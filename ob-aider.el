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
;; The response from Aider will be captured and displayed as the result.

;;; Code:
(require 'ob)
(require 'cl-lib)
;; Defer requiring aider until execution time
(defvar ob-aider-loaded nil)

(defgroup ob-aider nil
  "Org Babel functions for Aider.el integration."
  :group 'org-babel
  :prefix "ob-aider-")

(defcustom ob-aider-timeout 60
  "Timeout in seconds for waiting for Aider responses."
  :group 'ob-aider
  :type 'integer)

(defcustom ob-aider-response-delay 0.1
  "Delay in seconds between checks for Aider response completion."
  :group 'ob-aider
  :type 'float)

(defcustom ob-aider-default-async nil
  "Whether to execute Aider blocks asynchronously by default."
  :group 'ob-aider
  :type 'boolean)

(defun ob-aider-find-buffer ()
  "Find the active Aider conversation buffer.
Returns nil if no buffer is found."
  (let ((buffer-list (buffer-list)))
    (cl-find-if (lambda (buf)
                  (with-current-buffer buf
                    (and (derived-mode-p 'comint-mode)
                         (string-match-p "\\*aider:" (buffer-name buf))
                         (get-buffer-process buf))))
                buffer-list)))

(defun ob-aider-find-response-end-marker (buffer)
  "Find the end of the most recent response in the Aider BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (point-marker))))

(defun ob-aider-wait-for-response (buffer start-marker)
  "Wait for response in BUFFER after START-MARKER for `ob-aider-timeout` secs."
  (let ((end-time (+ (float-time) ob-aider-timeout))
        (response-received nil))
    (while (and (not response-received)
                (< (float-time) end-time))
      (with-current-buffer buffer
        (goto-char (point-max))
        ;; Check if the prompt indicator has appeared after our start marker
        (setq response-received (and (> (point) (marker-position start-marker))
                                     (save-excursion
                                       (goto-char (point-max))
                                       (forward-line -1)
                                       (looking-at-p "^aider> ")))))
      (unless response-received
        (sit-for ob-aider-response-delay)
        ;; Allow user to cancel with C-g
        (when quit-flag
          (setq quit-flag nil)
          (signal 'quit nil))))
    response-received))

(defun ob-aider-extract-response (buffer start-marker)
  "Extract the response text from BUFFER after START-MARKER."
  (with-current-buffer buffer
    (let ((response-text ""))
      (save-excursion
        (goto-char (marker-position start-marker))
        (forward-line 1) ;; Skip the line with the prompt
        (let ((response-start (point)))
          (goto-char (point-max))
          (forward-line -1) ;; Skip the final prompt line
          (setq response-text (buffer-substring-no-properties response-start (point)))))
      response-text)))

(defun ob-aider-send-prompt (buffer prompt)
  "Send PROMPT to Aider BUFFER and return the response."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))
      (unless proc
        (error "No process found in Aider buffer"))
      
      ;; Go to the end of the buffer
      (goto-char (point-max))
      ;; Record the current position as the start of our prompt
      (let ((start-marker (ob-aider-find-response-end-marker buffer)))
        ;; Send the prompt
        (comint-send-string proc (concat prompt "\n"))
        
        ;; Wait for response
        (if (ob-aider-wait-for-response buffer start-marker)
            ;; Extract and return the response
            (ob-aider-extract-response buffer start-marker)
          (error "Timeout waiting for Aider response"))))))

;;;###autoload
(defun org-babel-execute:aider (body params)
  "Execute a block of Aider code with org-babel.
This function is called by `org-babel-execute-src-block'.
BODY contains the prompt to send to Aider.
PARAMS are the parameters specified in the Org source block."
  (unless ob-aider-loaded
    (require 'aider)
    (setq ob-aider-loaded t))
  (let ((buffer (ob-aider-find-buffer))
        (async (cdr (assq :async params))))
    (if buffer
        (if async
            (org-babel-execute:aider-async body params buffer)
          ;; Synchronous execution
          (ob-aider-send-prompt buffer body))
      (user-error "No active Aider conversation buffer found"))))

(defun org-babel-execute:aider-async (body params buffer)
  "Execute aider source block asynchronously.
BODY contains the prompt text, PARAMS are block parameters,
BUFFER is the aider buffer."
  (let ((result-params (cdr (assq :result-params params))))
    ;; Return a placeholder for async execution
    (org-babel-insert-result "Executing asynchronously, see Aider buffer" result-params)
    
    ;; Send the prompt without waiting for response
    (with-current-buffer buffer
      (let ((proc (get-buffer-process buffer)))
        (unless proc
          (error "No process found in Aider buffer"))
        
        ;; Go to the end of the buffer
        (goto-char (point-max))
        ;; Send the prompt
        (comint-send-string proc (concat body "\n"))))
    
    ;; Return nil to prevent displaying internal details
    nil))


;;;###autoload
(defun ob-aider-insert-source-block ()
  "Insert an Aider source block at point."
  (interactive)
  (insert "#+begin_src aider\n\n#+end_src")
  (forward-line -1))

(provide 'ob-aider)
;;; ob-aider.el ends here
