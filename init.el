;;; init.el --- Emacs configuration of Brian Wilson -*- lexical-binding: t; -*-
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


;;;; First things first
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Platform stuff
(defun darwin-init ()
  (interactive)
  (message "»»» INIT: Loading stuff for darwin.")
  (setq mac-command-modifier 'meta)           ; Set META-Key to be CMD
  (setq mac-option-modifier 'none)            ; Unset Option so we get fancy inputs

  (use-package exec-path-from-shell
	       :ensure t
	       :if (and (eq system-type 'darwin) (display-graphic-p))
	       :config
	       (progn
		 (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH" "JAVA_OPTS"))
		   (add-to-list 'exec-path-from-shell-variables var))
		 (exec-path-from-shell-initialize)

		 (setq user-mail-address (getenv "EMAIL"))

		 ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
		 ;; already initializes info, we need to explicitly add the $INFOPATH
		 ;; directories to `Info-directory-list'.  We reverse the list of info paths
		 ;; to prepend them in proper order subsequently
		 (with-eval-after-load 'info
		   (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
		     (when dir
		       (add-to-list 'Info-directory-list dir))))))

(message "»»» INIT: Fini darwin loading."))

(defun win-init ()
  (interactive)
  (message "»»» INIT: Loading stuff for Bill Gates."))


(defun unix-init ()
  (interactive)
  (message "»»» INIT: Loading stuff for UNIX™ (or not)."))

(cond ((eq system-type 'darwin)
       (darwin-init))
      ((eq system-type 'windows-nt)
       (win-init))
      (t
       (unix-init)))

(defun gui-init ()
  (interactive)
  (message "»»» INIT: Loading stuff for GUI.")
  ;; Set the frame title for Quantified Self Capture
  (setq frame-title-format " %b -- %m -- Emacs")

  ;; Frame commands
  (global-set-key (kbd "M-`") 'other-frame)

  ;; Imenu
  (global-set-key (kbd "<mouse-3>") 'imenu)

  ;; Toolbar/menubar
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (message "»»» INIT: Done with GUI loading."))


(defun tui-init ()
  (interactive)
  (message "»»» INIT: Loading stuff for TUI.")
  (message "»»» INIT: Done TUI loading."))

(if (display-graphic-p)
    (gui-init)
  (tui-init))


;; All of the common bits
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)   ; Make y/n prompts easier

(setq-default fill-column 78)           ; Wider fill by default
(setq-default indent-tabs-mode nil)     ; Prevent tabs by default
(setq-default tab-width 2)              ; If we are using tabs, make them small
(setq suggest-key-bindings t)           ; Let emacs teach me
(setq visible-bell t)                   ; No beeps!
(setq inhibit-startup-screen t)         ; I'll miss it, but it no longer works for me
(setq backup-inhibited t)		;disable backup
(setq auto-save-default nil)		;disable auto save


;; Make emacs window moving feel like my tmux setup
(global-unset-key (kbd "C-z")) ; Unset C-z so we don't get annoying hides :-)
(global-set-key (kbd "C-z h") 'windmove-left)
(global-set-key (kbd "C-z j") 'windmove-down)
(global-set-key (kbd "C-z k") 'windmove-up)
(global-set-key (kbd "C-z l") 'windmove-right)
(global-set-key (kbd "C-z <left>") 'windmove-left)
(global-set-key (kbd "C-z <down>") 'windmove-down)
(global-set-key (kbd "C-z <up>") 'windmove-up)
(global-set-key (kbd "C-z <right>") 'windmove-right)
(global-set-key (kbd "C-z b") 'windmove-left)
(global-set-key (kbd "C-z n") 'windmove-down)
(global-set-key (kbd "C-z p") 'windmove-up)
(global-set-key (kbd "C-z f") 'windmove-right)
(global-set-key (kbd "C-z o") 'other-window)
(global-set-key (kbd "C-z `") 'other-window)

(show-paren-mode 1)         ; I like to see my parens
(display-time)              ; Full-screen emacs without a time?
(column-number-mode 1)      ; What's my current column?
(display-battery-mode 1)    ; If i've got a battery it's nice to know if it's dead

;; Prevent custom stuff from ending up in a vc controlled file
(let ((custom-file-location (concat user-emacs-directory "custom.el")))
  (setq custom-file custom-file-location)
  (load custom-file-location t))

(defun safe-set-face-font (face font-family font-size)
  "Set the font for FACE to FONT-FAMILY (with FONT-SIZE) if it exists."
  (when (member font-family (font-family-list))
    (set-face-font face (concat font-family "-" (number-to-string font-size)))))

(safe-set-face-font 'default "Inconsolata" 11)
(safe-set-face-font 'variable-pitch "Symbola" 11)
;; (safe-set-face-font 'interface-variable-pitch "Inter UI" 11)

(message "»»» INIT: Init loading finished")


;;;;; Keybindings
;; (defun previous-blank-line ()
;;   "Moves to the previous line containing nothing but whitespace."
;;   (interactive)
;;   (search-backward-regexp "^[ \t]*\n")
;; )
;;
;; (defun next-blank-line ()
;;   "Moves to the next line containing nothing but whitespace."
;;   (interactive)
;;   (forward-line)
;;   (search-forward-regexp "^[ \t]*\n")
;;   (forward-line -1)
;; )
;;
;; Bind previous to  and next to 

(provide 'init)
;;; init.el ends here

