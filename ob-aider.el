;;; ob-aider.el --- Org Babel functions for Aider.el integration -*- lexical-binding: t; -*-

;; Author: Levi Strope <levi.strope@gmail.com>
;; Maintainer: Levi Strope <levi.strope@gmail.com>
;; Keywords: tools, convenience, languages, org, processes
;; URL: https://github.com/localredhead/ob-aider.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; This library enables the use of Aider.el within Org mode Babel.
;; It allows sending prompts to an already running Aider.el comint buffer
;; directly from Org mode source blocks.
;;
;; This integration enables seamless documentation of AI-assisted coding
;; sessions within Org mode documents, making it easier to create
;; reproducible workflows and tutorials.

;; Requirements:
;;
;; - Emacs 27.1 or later
;; - Org mode 9.4 or later
;; - aider.el (https://github.com/tninja/aider.el)

;; Usage:
;;
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
;; The response from Aider will be captured and displayed as the result.

;;; Code:
(require 'ob)
(require 'cl-lib)
;; Defer requiring aider until execution time
(defvar ob-aider-loaded-flag nil)

(defgroup ob-aider nil
  "Org Babel functions for Aider.el integration."
  :group 'org-babel
  :prefix "ob-aider-")

;; These custom variables are no longer needed

;; Removed async default setting as we're removing async functionality

(defcustom ob-aider-buffer nil
  "Manually specified Aider buffer to use.
When set, this buffer will be used instead of auto-detection."
  :group 'ob-aider
  :type '(choice (const :tag "Auto-detect" nil)
          (string :tag "Buffer name")))

(defun ob-aider-find-buffer ()
  "Find the active Aider conversation buffer.
Returns nil if no buffer is found."
  (let ((buffer-list (buffer-list)))
    (cl-find-if (lambda (buf)
                  (with-current-buffer buf
                    (let ((buf-name (buffer-name buf)))
                      (and (derived-mode-p 'comint-mode)
                           (get-buffer-process buf)
                           (or (string-match-p "\\*aider:" buf-name)
                               (string-match-p "aider:/Users/" buf-name)
                               (string-match-p "aider" buf-name))))))
                buffer-list)))

;; These functions are no longer needed since we're not waiting for responses

(defun ob-aider-send-prompt (buffer prompt)
  "Send PROMPT to Aider BUFFER and return a message.
This is a non-blocking implementation that returns immediately."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))
      (unless proc
        (error "No process found in Aider buffer"))

      ;; Go to the end of the buffer
      (goto-char (point-max))
      
      ;; Format multi-line prompts properly
      ;; If the prompt contains newlines, wrap it in curly braces
      (let ((formatted-prompt 
             (if (string-match-p "\n" prompt)
                 (concat "{\n" prompt "\n}")
               prompt)))
        ;; Send the prompt
        (comint-send-string proc (concat formatted-prompt "\n")))

      ;; Return a message indicating the prompt was sent
      "Prompt sent to Aider buffer. Check the buffer for response.")))

;;;###autoload
(defun org-babel-execute:aider (body params)
  "Execute a block of Aider code with org-babel.
This function is called by `org-babel-execute-src-block'.
BODY contains the prompt to send to Aider.
PARAMS are the parameters specified in the Org source block."
  (unless ob-aider-loaded-flag
    (require 'aider)
    (setq ob-aider-loaded t))

  (let ((buffer (ob-aider-find-buffer)))
    (unless buffer
      (user-error "No active Aider conversation buffer found"))

    ;; Non-blocking execution
    (message "Sending prompt to Aider buffer: %s" (buffer-name buffer))
    (ob-aider-send-prompt buffer body)))

;; Removed async function as it's no longer needed


;;;###autoload
(defun ob-aider-insert-source-block ()
  "Insert an Aider source block at point."
  (interactive)
  (insert "#+begin_src aider\n\n#+end_src")
  (forward-line -1))

(provide 'ob-aider)
;;; ob-aider.el ends here
