;;; elmacro.el --- Store your named macros persistently!           -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/elmacro.el
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Package for organizing and executing named kbd macros.

;;; Code:

(require 'subr-x)

(defcustom elmacro-macro-file "~/.emacs.d/elmacro-macros.el"
  "File where macros are stored."
  :type 'string
  :group 'elmacro)

(defvar elmacro--named-functions '()
  "List of macro names.")

(defvar elmacro--tmp-buffer "*elmacro-tmp*"
  "Temporary buffer for storing macros.")

(defun elmacro-restore-sesstion ()
  "Restore macros from `elmacro-macro-file'."
  (interactive)
  (with-current-buffer (get-buffer-create elmacro--tmp-buffer)
    (insert-file-contents elmacro-macro-file)
    (unless (equal (string-trim (buffer-string)) "")
      (setq elmacro--named-functions (eval (car (read-from-string (format "'%s" (buffer-string))))))))
  (kill-buffer elmacro--tmp-buffer))

(defun elmacro-save-session ()
  "Save macros to `elmacro-macro-file'."
  (interactive)
  (with-temp-file elmacro-macro-file
    (insert (format "%s\n\n" elmacro--named-functions))))

(defun elmacro--restore-session-when-no-data ()
  "Restore session when `elmacro--named-functions' is empty."
  (unless elmacro--named-functions
    (elmacro-restore-sesstion)))

;;;###autoload
(defun elmacro-name-last-kbd-macro (symbol)
  "Name last kbd macro.
SYMBOL is the name of the macro."
  (interactive "SName for last kbd macro: ")
  (elmacro--restore-session-when-no-data)
  (name-last-kbd-macro symbol)
  (let ((kbd-macro (with-current-buffer (get-buffer-create elmacro--tmp-buffer)
                     (insert-kbd-macro symbol)
                     (buffer-string)))
        ;; (formatted-symbol (intern (replace-regexp-in-string " " "\ " (symbol-name symbol))))
        (macro-name (symbol-name symbol)))
    (kill-buffer elmacro--tmp-buffer)

    (add-to-list 'elmacro--named-functions `(,macro-name . ,kbd-macro))
    (elmacro-save-session)))

;;;###autoload
(defun elmacro-execute-macros ()
  "Execute macros."
  (interactive)
  (elmacro--restore-session-when-no-data)
  (let ((macro (intern (completing-read "Execute macro: " elmacro--named-functions nil t))))
    (unless (fboundp macro)
      (eval (cdr (assoc macro elmacro--named-functions))))
    (call-interactively macro)))

(defun elmacro-remove-macro ()
  "Remove macro."
  (interactive)
  (elmacro--restore-session-when-no-data)
  (let ((macro (intern (completing-read "Remove macro: " elmacro--named-functions nil t))))
    (setq elmacro--named-functions (delete (assoc macro elmacro--named-functions) elmacro--named-functions))
    (elmacro-save-session)))


(provide 'elmacro)

;;; elmacro.el ends here
