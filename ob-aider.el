;;; ob-aider.el --- Org Babel functions for Aider.el integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Levi Strope <levi.strope@gmail.com>
;; Keywords: literate programming, reproducible research, ai, aider
;; URL: https://github.com/yourusername/ob-aider
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4") (aider "0.1.0"))

;; This file is not part of GNU Emacs.

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
(require 'aider)
(require 'cl-lib)

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
                    (and (eq major-mode 'comint-mode)
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
  "Wait for a response in BUFFER after START-MARKER for up to `ob-aider-timeout` seconds."
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
  (let ((buffer (ob-aider-find-buffer))
        (async (or (assq :async params) ob-aider-default-async)))
    (if buffer
        (if async
            (org-babel-execute:aider-async body params buffer)
          ;; Get the raw response
          (let ((result (ob-aider-send-prompt buffer body)))
            ;; Return the result as a string, not as Lisp data
            (org-babel-reassemble-table
             result
             (org-babel-pick-name (cdr (assq :colname-names params))
                                 (cdr (assq :colnames params)))
             (org-babel-pick-name (cdr (assq :rowname-names params))
                                 (cdr (assq :rownames params))))))
      (user-error "No active Aider conversation buffer found"))))

(defun org-babel-execute:aider-async (body params buffer)
  "Asynchronously execute aider source block with BODY and PARAMS in BUFFER."
  (let ((callback (cdr (assq :post params)))
        (result-params (cdr (assq :result-params params)))
        (result-type (cdr (assq :result-type params))))
    (run-with-timer
     0 nil
     (lambda ()
       (let ((result (condition-case err
                         (ob-aider-send-prompt buffer body)
                       (quit "Aider prompt execution cancelled")
                       (error (format "Error: %S" err)))))
         (when callback
           (funcall (intern callback) result)))))))

;; Register the language with Org Babel
(add-to-list 'org-babel-load-languages '(aider . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;;;###autoload
(defun ob-aider-insert-source-block ()
  "Insert an Aider source block at point."
  (interactive)
  (insert "#+begin_src aider\n\n#+end_src")
  (forward-line -1))

(provide 'ob-aider)
;;; ob-aider.el ends here
