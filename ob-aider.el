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

;;; Usage:

;; Add to your Emacs configuration:
;;
;; (with-eval-after-load 'org
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    (append org-babel-load-languages
;;            '((aider . t)))))
;;
;; Then create an Aider source block in your Org file:
;;
;; #+begin_src aider
;; Your prompt to Aider here...
;; #+end_src
;;
;; Execute the block with C-c C-c to send the prompt to the active Aider session.

;;; Code:
(require 'ob)
(require 'aider)

(defgroup ob-aider nil
  "Org Babel functions for Aider.el integration."
  :group 'org-babel)

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

(defun ob-aider-send-prompt (buffer prompt)
  "Send PROMPT to Aider BUFFER and return the response."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer))
          (comint-input-sender-no-newline nil)
          (comint-input-sender (function comint-simple-send)))
      (when proc
        ;; Go to the end of the buffer
        (goto-char (point-max))
        ;; Record the current position as the start of our prompt
        (let ((start-pos (point)))
          ;; Send the prompt
          (comint-send-string proc (concat prompt "\n"))
          ;; Wait for response
          (let ((timeout ob-aider-timeout)
                (response-complete nil))
            (while (and (> timeout 0) (not response-complete))
              (sleep-for 1)
              (setq timeout (1- timeout))
              ;; Check if Aider has finished responding
              ;; This is a heuristic - we look for the prompt pattern
              (goto-char (point-max))
              (setq response-complete 
                    (save-excursion
                      (forward-line -1)
                      (looking-at-p "^aider> $"))))
            ;; Extract the response
            (if response-complete
                (buffer-substring-no-properties 
                 (+ start-pos (length prompt) 1) ; Skip past our prompt
                 (save-excursion
                   (goto-char (point-max))
                   (forward-line -1)
                   (point)))
              (error "Timeout waiting for Aider response"))))))))

;;;###autoload
(defun org-babel-execute:aider (body params)
  "Execute a block of Aider code with org-babel.
This function is called by `org-babel-execute-src-block'.
BODY contains the prompt to send to Aider.
PARAMS are the parameters specified in the Org source block."
  (let ((buffer (ob-aider-find-buffer)))
    (if buffer
        (ob-aider-send-prompt buffer body)
      (error "No active Aider conversation buffer found"))))

;; Register the language with Org Babel
;; Note: Users should enable this in their own config
;; (add-to-list 'org-babel-load-languages '(aider . t))
;; (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(provide 'ob-aider)
;;; ob-aider.el ends here
