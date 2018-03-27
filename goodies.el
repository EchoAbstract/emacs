;;; goodies.el --- Not ready for prime time config stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Brian Wilson

;; Author: Brian Wilson <>
;; Keywords: internal, extensions

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

;; How often do I program in OCaml... once, twice a year?
;;
;; Why then is tuareg mode cluttering my init file?  Now it's split to a
;; separate config file  to keep the clutter down.

;;; Code:

(use-package clojure-mode
  :defer t
  :ensure t)

(use-package cider
  :disabled
  :ensure t)

(use-package slime-docker
  :defer t
  :ensure t)

;;; Apple's Swift Language
(use-package swift-mode
  :defer t
  :ensure t)

;;; OCaml Support
(use-package tuareg
  :defer t
  :ensure t)

(use-package kubernetes
  :disabled
  :ensure t)

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package easy-hugo
  :defer t
  :ensure t)


;;; Window splitting functions...

;; TODO (brian): Currently this fails with magit commits, since that's
;; technically an emacs client and doesn't go through the normal splitting
;; chain.  I feel like I could fix that by adding some magit-specific advice...
;; ¯\_(ツ)_/¯

(defvar *baw-modes-allowed-to-split*
  '("magit" "COMMIT_EDITMSG")
  "Regexps that match major modes allowed to split my windows")

(defun baw-major-mode-contains-p (partial-string)
  "Does the current major-mode contain PARTIAL-STRING?"
  (not
   (null
    (string-match-p (regexp-quote partial-string) (symbol-name major-mode)))))

(defun baw-should-split-sensibly-p ()
  "Determine if the current major mode is allowed to split"
  (not
   (null
    (seq-filter 'baw-major-mode-contains-p *baw-modes-allowed-to-split*))))

(defun baw-split-window (&optional window)
  "Maybe split the window sensibly"
  (cond ((baw-should-split-sensibly-p)
         (split-window-sensibly window))
        (t
         nil)))


(setq split-window-preferred-function 'baw-split-window)


;;; Functions of dubious value...

(defun baw-remove-frame-background ()
  "Remove the background color for the current frame."
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame)))


;;; Emoji hacks
(defun shrug ()
  "Insert the shrug guy (¯\_(ツ)_/¯) at point"
  (interactive)
  (insert "¯\\_(ツ)_/¯"))


(provide 'goodies)
;;; goodies.el ends here
