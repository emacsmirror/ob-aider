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
(require 'ob-aider)

(ert-deftest ob-aider-test-loaded ()
  "Test that ob-aider loads properly."
  (should (featurep 'ob-aider)))

;; Add more tests as needed

(provide 'ob-aider-tests)
;;; ob-aider-tests.el ends here
