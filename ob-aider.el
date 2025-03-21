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

(defcustom ob-aider-default-async t
  "Whether to execute Aider blocks asynchronously by default."
  :group 'ob-aider
  :type 'boolean)

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
      ;; Send the prompt
      (comint-send-string proc (concat prompt "\n"))

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

  (let* ((buffer (ob-aider-find-buffer))
         (async (or (cdr (assq :async params))
                    ob-aider-default-async)))
    (unless buffer
      (user-error "No active Aider conversation buffer found"))

    (if async
        (org-babel-execute:aider-async body params buffer)
      ;; "Synchronous" execution - still non-blocking but with a different message
      (message "Sending prompt to Aider buffer: %s" (buffer-name buffer))
      (ob-aider-send-prompt buffer body))))

(defun org-babel-execute:aider-async (body params buffer)
  "Execute aider source block asynchronously.
BODY contains the prompt text, PARAMS are block parameters,
BUFFER is the aider buffer."
  (let ((aider-buffer (or buffer (ob-aider-find-buffer))))
    (if (not aider-buffer)
        (progn
          (message "Warning: No active Aider conversation buffer found")
          "No active Aider conversation buffer found")

      (with-current-buffer aider-buffer
        (let ((proc (get-buffer-process aider-buffer)))
          (if (not proc)
              (progn
                (message "Warning: No process found in Aider buffer %s" (buffer-name aider-buffer))
                "No process found in Aider buffer")

            ;; Go to the end of the buffer and send the prompt
            (goto-char (point-max))
            ;; Simply send the prompt and don't wait for a response
            (comint-send-string proc (concat body "\n"))

            ;; Log the buffer being used
            (message "Sent async prompt to Aider buffer: %s" (buffer-name aider-buffer))
            ;; Return a placeholder for async execution
            "Prompt sent to Aider buffer. Check the buffer for response."))))))


;;;###autoload
(defun ob-aider-insert-source-block ()
  "Insert an Aider source block at point."
  (interactive)
  (insert "#+begin_src aider\n\n#+end_src")
  (forward-line -1))

(provide 'ob-aider)
;;; ob-aider.el ends here
