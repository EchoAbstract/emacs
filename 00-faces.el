;;; 00-faces.el --- Face customization -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Brian Wilson <brian@polytopes.me>
;;
;; Author: Brian Wilson <brian@polytopes.me>
;; URL: https://gihub.com/EchoAbstract/emacs
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; TODO

;;; Code:

;;; Functions of dubious value...

(defun baw-remove-frame-background ()
  "Remove the background color for the current frame."
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame)))


(defvar *baw-last-enabled-theme*
  nil
  "The last theme that was enabled.")

(defun baw-load-theme-advice (f theme &optional no-confirm no-enable)
  "Wraps F by disabling last theme before enabling THEME.

Pases NO-CONFIRM and NO-ENABLE to `load-theme'."
  (message (format "Disabling %s and enabling %s" *baw-last-enabled-theme* theme))
  (unless (null *baw-last-enabled-theme*)
    (disable-theme *baw-last-enabled-theme*))

  (setq *baw-last-enabled-theme* theme)
  (apply f theme no-confirm no-enable))

(advice-add 'load-theme
            :around
            #'baw-load-theme-advice)



;;;; Basic TODO Support
(defvar *baw/fixme-modes*
  '(c++-mode c-mode emacs-lisp-mode)
  "List of major modes to fontify TODO/NOTE Marks.")

(defun baw/add-todo-faces ()
  "Add my faces to specific modes."
  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("\\<\\(TODO\\)" 1 'font-lock-error-face t)
             ("\\<\\(NOTE\\)" 1 'font-lock-warning-face t))))
        *baw/fixme-modes*))

(add-hook 'after-init-hook #'baw/add-todo-faces)

;; TODO (brian): This is a test
;; NOTE (brian): This is not a test


;; (defface baw/font-lock-fixme-face
;;   'default
;;   "Face for TODO, FIXME, BUG, etc keywords.")

;;  (make-face 'font-lock-fixme-face)
;;  (make-face 'font-lock-note-face)
;;  (mapc (lambda (mode)
;; 	 (font-lock-add-keywords
;; 	  mode
;; 	  '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
;;             ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
;; 	fixme-modes)
;;  (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)



(provide '00-faces)
;;; 00-faces.el ends here
