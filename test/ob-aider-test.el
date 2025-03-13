;;; ob-aider-test.el --- Tests for ob-aider.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Levi Strope

;; Author: Levi Strope <levi.strope@gmail.com>

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; Tests for ob-aider.el

;;; Code:

(require 'ert)
;; Make sure we can find ob-aider.el
(when (not (featurep 'ob-aider))
  (require 'ob-aider))

(ert-deftest ob-aider-test-loaded ()
  "Test that ob-aider loads properly."
  (should (featurep 'ob-aider)))

;; Add more basic tests that don't require an active aider session
(ert-deftest ob-aider-test-variables ()
  "Test that variables are properly defined."
  (should (boundp 'ob-aider-timeout))
  (should (boundp 'ob-aider-response-delay))
  (should (boundp 'ob-aider-default-async)))

(ert-deftest ob-aider-test-functions ()
  "Test that functions are properly defined."
  (should (fboundp 'ob-aider-find-buffer))
  (should (fboundp 'ob-aider-send-prompt))
  (should (fboundp 'org-babel-execute:aider)))

(provide 'ob-aider-test)
;;; ob-aider-test.el ends here
