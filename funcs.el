;;; funcs.el --- Collection of random lisp functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018, 2019, 2020 Brian Wilson <brian@polytopes.me>
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

(defun baw/maybe-load-file (file &optional log-p ignore-missing-p)
  "Load FILE only if it exists.

If LOG-P is true, indicate that we're trying to load the file.
If IGNORE-MISSING-P is true then don't warn if we can't find the file."

  (when log-p (message (format "INIT: Trying to load %s." file)))
  (if (or
       (file-exists-p file)
       (file-exists-p (concat file ".el"))
       (file-exists-p (concat file ".elc")))
      (load file)
    (unless ignore-missing-p (warn (concat "Can't load non-existent file: " file)))))

(defun baw/maybe-load-config (config-file &optional log-p)
  "Load CONFIG-FILE only if it exists.  Be noisey if LOG-P is true."
  (baw/maybe-load-file (concat user-emacs-directory config-file) log-p t))

(defvar *baw/org-scratch-buffer-name* "*org-notes*"
  "The name for our scratch org buffer.")

(defun baw/create-scratch-org-buffer ()
  "Create a new scratch `org-mode' buffer."
  (interactive)
  (save-excursion
    (switch-to-buffer (get-buffer-create *baw/org-scratch-buffer-name*))
    (org-mode)
    (insert "#+TITLE: Org-Mode scratch buffer for notes\n\n")
    (insert (concat "* Notes for " (format-time-string "%Y-%m-%d")))
    (insert "\n\n")))


(defun baw/remove-frame-background ()
  "Remove the background color for the current frame."
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame)))

(defvar *baw/last-enabled-theme*
  nil
  "The last theme that was enabled.")

(defun baw/load-theme-advice (f theme &optional no-confirm no-enable)
  "Wraps F by disabling last theme before enabling THEME.

Pases NO-CONFIRM and NO-ENABLE to `load-theme'."
  (message (format "Disabling %s and enabling %s" *baw/last-enabled-theme* theme))
  (unless (null *baw/last-enabled-theme*)
    (disable-theme *baw/last-enabled-theme*))

  (setq *baw/last-enabled-theme* theme)
  (apply f theme no-confirm no-enable))

(defun baw/safe-set-face-font (face font-family font-size)
  "Set the font for FACE to FONT-FAMILY (with FONT-SIZE) if it exists."
  (if (member font-family (font-family-list))
      (or (set-face-font face (concat font-family "-" (number-to-string font-size))) t)
    nil))

(defun baw/insert-shrug ()
  "Insert the shrug guy (¯\_(ツ)_/¯) at point."
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(defun baw/insert-commented-line ()
  "Insert a line of ─ characters prefixed with a comment."
  (interactive)
  (insert (concat comment-start
	   " ────────────────────────────────────────────────────────────────────────────")))

;; TODO(brian): This seems like the easiest way to handle this cross platform, but
;;              I should verify this.
(require 'eshell)

(defun baw/find-all-valid-cmds (cmds)
  "Return any runnable commands in CMDS."
  (remove-if-not 'eshell-search-path cmds))

(defun baw/find-first-valid-cmd (cmds)
  "Return the first runnable command in CMDS."
  (let ((candidates (baw/find-all-valid-cmds cmds)))
    (when candidates
      (car candidates))))

(provide 'funcs)
;;; funcs.el ends here
