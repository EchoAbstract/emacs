;;; 10-expansion.el --- Text expansion and snippets -*- lexical-binding: t; -*-
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

;;;; BAW Insertions
;; TODO: Keybind these
(defun baw-insert-shrug ()
  "Insert the shrug guy (¯\_(ツ)_/¯) at point."
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

(defun baw-insert-commented-line ()
  "Insert a line of ─ characters prefixed with a comment."
  (interactive)
  (insert (concat comment-start
	   " ────────────────────────────────────────────────────────────────────────────")))

;;;; YASnippet
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "Ⓨ")
  :config (progn
            (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
            ;; From https://stackoverflow.com/questions/28487400/how-to-unset-yasnippets-tab-key
            ;; This illustrates how to redefine yas-expand to S-TAB.
            (define-key yas-minor-mode-map [backtab]     'yas-expand)

            ;; Strangely, just redefining one of the variations below won't work.
            ;; All rebinds seem to be needed.
            (define-key yas-minor-mode-map [(tab)]        nil)
            (define-key yas-minor-mode-map (kbd "TAB")    nil)
            (define-key yas-minor-mode-map (kbd "<tab>")  nil)

            (yas-global-mode 1))
  :defer 10)

(provide '10-expansion)
;;; 10-expansion.el ends here
