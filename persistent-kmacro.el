;;; persistent-kmacro.el --- Store your named macros persistently!           -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/persistent-kmacro.el
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.0.3

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

(defcustom persistent-kmacro-macro-file "~/.emacs.d/persistent-kmacro.el"
  "File where macros are stored."
  :type 'string
  :group 'persistent-kmacro)

(defcustom persistent-kmacro--include-projectile-name t
  "If non-nil, include projectile project name in macro name."
  :type 'boolean
  :group 'persistent-kmacro)

(defvar persistent-kmacro--named-functions '()
  "List of macro names.")

(defvar persistent-kmacro--tmp-buffer "*elmacro-tmp*"
  "Temporary buffer for storing macros.")


(defun persistent-kmacro--build-prefix-name ()
  "Build prefix name for macro."
  (if (and persistent-kmacro--include-projectile-name (fboundp 'projectile-project-name))
      (format "[%s] " (projectile-project-name))
    ""))

(defun persistent-kmacro-restore-sesstion ()
  "Restore macros from `persistent-kmacro-macro-file'."
  (interactive)
  (with-current-buffer (get-buffer-create persistent-kmacro--tmp-buffer)
    (insert-file-contents persistent-kmacro-macro-file)
    (unless (equal (string-trim (buffer-string)) "")
      (setq persistent-kmacro--named-functions (eval (car (read-from-string (format "'%s" (buffer-string))))))))
  (kill-buffer persistent-kmacro--tmp-buffer))

(defun persistent-kmacro-save-session ()
  "Save macros to `persistent-kmacro-macro-file'."
  (interactive)
  (with-temp-file persistent-kmacro-macro-file
    (insert (format "%s\n\n" persistent-kmacro--named-functions))))

(defun persistent-kmacro--restore-session-when-no-data ()
  "Restore session when `persistent-kmacro--named-functions' is empty."
  (unless persistent-kmacro--named-functions
    (persistent-kmacro-restore-sesstion)))

;;;###autoload
(defun persistent-kmacro-name-last-kbd-macro (symbol)
  "Name last kbd macro.
SYMBOL is the name of the macro."
  (interactive (list (read-string "Name for last kbd macro: " (persistent-kmacro--build-prefix-name))))
  (persistent-kmacro--restore-session-when-no-data)
  (name-last-kbd-macro symbol)
  (let ((kbd-macro (with-current-buffer (get-buffer-create persistent-kmacro--tmp-buffer)
                     (insert-kbd-macro symbol)
                     (buffer-string)))
        ;; (formatted-symbol (intern (replace-regexp-in-string " " "\ " (symbol-name symbol))))
        (macro-name (symbol-name symbol)))
    (kill-buffer persistent-kmacro--tmp-buffer)

    (add-to-list 'persistent-kmacro--named-functions `(,macro-name . ,kbd-macro))
    (persistent-kmacro-save-session)))

;;;###autoload
(defun persistent-kmacro-execute-macro ()
  "Execute macros."
  (interactive)
  (persistent-kmacro--restore-session-when-no-data)
  (let ((macro (intern (completing-read "Execute macro: " persistent-kmacro--named-functions nil t))))
    (unless (fboundp macro)
      (eval (cdr (assoc macro persistent-kmacro--named-functions))))
    (call-interactively macro)))

(defun persistent-kmacro-remove-macro ()
  "Remove macro."
  (interactive)
  (persistent-kmacro--restore-session-when-no-data)
  (let ((macro (intern (completing-read "Remove macro: " persistent-kmacro--named-functions nil t))))
    (setq persistent-kmacro--named-functions (delete (assoc macro elmacro--named-functions) elmacro--named-functions))
    (persistent-kmacro-save-session)))


(provide 'persistent-kmacro)

;;; persistent-kmacro.el ends here
