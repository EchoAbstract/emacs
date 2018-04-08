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




(provide '00-faces)
;;; 00-faces.el ends here
