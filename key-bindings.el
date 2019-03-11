;;; key-bindings.el --- Emacs keybindings -*- lexical-binding: t; -*-
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



(global-unset-key (kbd "C-z")) ; Unset C-z so we don't get annoying hides :-)



;; Make emacs window moving feel like my tmux setup
(global-set-key (kbd "C-z h")       'windmove-left)
(global-set-key (kbd "C-z j")       'windmove-down)
(global-set-key (kbd "C-z k")       'windmove-up)
(global-set-key (kbd "C-z l")       'windmove-right)
(global-set-key (kbd "C-z <left>")  'windmove-left)
(global-set-key (kbd "C-z <down>")  'windmove-down)
(global-set-key (kbd "C-z <up>")    'windmove-up)
(global-set-key (kbd "C-z <right>") 'windmove-right)
(global-set-key (kbd "C-z b")       'windmove-left)
(global-set-key (kbd "C-z n")       'windmove-down)
(global-set-key (kbd "C-z p")       'windmove-up)
(global-set-key (kbd "C-z f")       'windmove-right)
(global-set-key (kbd "C-z o")       'other-window)
(global-set-key (kbd "C-z `")       'other-window)


;; GUI only
(when (display-graphic-p)
  (global-set-key (kbd "M-`") 'other-frame)
  (global-set-key (kbd "<mouse-3>") 'imenu)
  (global-set-key (kbd "M-0") 'suspend-frame)) ; Macalike


(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c g") 'magit)
  ; (global-set-key (kbd "M-i") 'indent-region)
  ; (global-set-key (kbd "M-p") 'previous-error)
  ; (global-set-key (kbd "M-n") 'next-error)

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-find-references)
(global-set-key (kbd "M-*") 'xref-pop-marker-stack)


(provide 'key-bindings)
;;; key-bindings.el ends here
