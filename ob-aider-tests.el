;;; ob-aider-tests.el --- Tests for ob-aider.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Levi Strope <levi.strope@gmail.com>

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

(provide 'ob-aider-tests)
;;; ob-aider-tests.el ends here
