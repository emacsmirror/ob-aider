;;; ob-aider.el --- Org Babel functions for Aider.el integration

;; Copyright (C) 2024 Your Name

;; Author: Your Name <your.email@example.com>
;; Keywords: org, babel, aider, ai
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.4") (aider "0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides Org Babel integration for Aider.el, allowing
;; users to send prompts to an already running Aider.el comint buffer
;; directly from Org mode source blocks.

;;; Code:
(require 'ob)
(require 'aider)

(defgroup ob-aider nil
  "Org Babel functions for Aider.el integration."
  :group 'org-babel)

(defcustom ob-aider-timeout 30
  "Timeout in seconds to wait for Aider response."
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

(defun ob-aider-wait-for-response (buffer start-marker)
  "Wait for and extract response from BUFFER starting from START-MARKER.
Returns the response text."
  (with-current-buffer buffer
    (let ((timeout ob-aider-timeout)
          (start-time (current-time)))
      ;; Wait for the prompt to reappear, indicating response is complete
      (while (and (< (float-time (time-since start-time)) timeout)
                  (not (input-pending-p))
                  (not (save-excursion
                         (goto-char (point-max))
                         (forward-line 0)
                         (looking-at "^aider> "))))
        (sleep-for 0.1))
      
      ;; Extract the response
      (buffer-substring-no-properties 
       start-marker
       (save-excursion
         (goto-char (point-max))
         (if (re-search-backward "^aider> " nil t)
             (match-beginning 0)
           (point-max)))))))

(defun org-babel-execute:aider (body params)
  "Execute a block of Aider code with org-babel.
This function is called by `org-babel-execute-src-block'.
BODY contains the prompt to send to Aider.
PARAMS are the parameters from the Org Babel source block."
  (let ((buffer (ob-aider-send-prompt body))
        (start-marker nil)
        (result nil))
    (unless buffer
      (error "No active Aider conversation buffer found"))
    
    (with-current-buffer buffer
      ;; Mark the current end of buffer as our starting point
      (goto-char (point-max))
      (setq start-marker (point-marker)))
    
    ;; Wait for and get the response
    (setq result (ob-aider-wait-for-response buffer start-marker))
    
    ;; Return the result
    result))

;; Register the language with Org Babel
(add-to-list 'org-babel-load-languages '(aider . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

(provide 'ob-aider)
;;; ob-aider.el ends here
