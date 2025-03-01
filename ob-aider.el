;;; ob-aider.el --- Org Babel functions for Aider.el integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Keywords: literate programming, reproducible research, ai, aider
;; Homepage: https://github.com/yourusername/ob-aider
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4") (aider "0.1.0"))

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library enables the use of Aider.el within Org mode Babel.
;; It allows sending prompts to an already running Aider.el comint buffer
;; directly from Org mode source blocks.

;;; Requirements:

;; - Emacs 27.1 or later
;; - Org mode 9.4 or later
;; - aider.el (https://github.com/tninja/aider.el)

;;; Code:
(require 'ob)
(require 'aider)

(defgroup ob-aider nil
  "Org Babel functions for Aider.el integration."
  :group 'org-babel
  :prefix "ob-aider-")

(defcustom ob-aider-timeout 60
  "Timeout in seconds for waiting for Aider responses."
  :group 'ob-aider
  :type 'integer)

(defun ob-aider-find-buffer ()
  "Find the active Aider conversation buffer.
Returns nil if no buffer is found."
  (let ((buffer (aider--find-conversation-buffer)))
    (when (and buffer (buffer-live-p buffer))
      buffer)))

(defun ob-aider-send-prompt (prompt)
  "Send PROMPT to the active Aider conversation buffer.
Returns the buffer to which the prompt was sent, or nil if no buffer was found."
  (let ((buffer (ob-aider-find-buffer)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-max))
        (comint-send-string (get-buffer-process buffer) (concat prompt "\n")))
      buffer)))

(defun ob-aider-wait-for-response (buffer)
  "Wait for a response in the Aider BUFFER.
Returns the response text or nil if timeout occurs."
  (with-current-buffer buffer
    (let ((start-time (current-time))
          (start-point (point-max))
          (process (get-buffer-process buffer)))
      (while (and (process-live-p process)
                  (accept-process-output process 0.1)
                  (< (float-time (time-since start-time)) ob-aider-timeout)
                  (not (save-excursion
                         (goto-char start-point)
                         (re-search-forward comint-prompt-regexp nil t)))))
      (if (< (float-time (time-since start-time)) ob-aider-timeout)
          (save-excursion
            (goto-char start-point)
            (when (re-search-forward comint-prompt-regexp nil t)
              (buffer-substring-no-properties start-point (match-beginning 0))))
        (message "Timeout waiting for Aider response")
        nil))))

;;;###autoload
(defun org-babel-execute:aider (body params)
  "Execute a block of Aider code with org-babel.
This function is called by `org-babel-execute-src-block'.
BODY contains the prompt to send to Aider.
PARAMS are the parameters passed to the block."
  (let ((buffer (ob-aider-send-prompt body)))
    (if buffer
        (or (ob-aider-wait-for-response buffer)
            "No response received from Aider (timeout)")
      "No active Aider conversation buffer found")))

;; Register the language with Org Babel
(add-to-list 'org-babel-load-languages '(aider . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(provide 'ob-aider)
;;; ob-aider.el ends here
