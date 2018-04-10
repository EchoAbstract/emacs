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
(defvar init/original-gc-cons-threshold nil
  "Placeholder for Emacs default `gc-cons-threshold'.")

(setq init/original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000) ; Temporarily bump up the gc threshold

;; Never load the older of byte-code vs. elisp
(setq load-prefer-newer t)

;; If we're profiling we want to start it as early as possible
(defvar init/profile-emacs-init-time t
  "Setup various things for profiling startup time.")

;; Disabled for now
(setq init/profile-emacs-init-time nil)

(when init/profile-emacs-init-time
  (setq use-package-compute-statistics t))

;; Set up `use-package` mode
;; This is how we load most of the other packages in our init
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; This is a profiling tool for this startup script.
;; It's pretty useful, even though the variables are
;; defined above, we need to wait for `use-package'
;; to be able to actually use it
(use-package esup
  :when init/profile-emacs-init-time
  :defer t
  :ensure t)

;;;; Utilites to help with the rest of the program
(defun init/maybe-load-file (file &optional log-p ignore-missing-p)
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

(defun init/maybe-load-config (config-file &optional log-p)
  "Load CONFIG-FILE only if it exists.  Be noisey if LOG-P is true."
  (init/maybe-load-file (concat user-emacs-directory config-file) log-p t))

(defun init/safe-set-face-font (face font-family font-size)
  "Set the font for FACE to FONT-FAMILY (with FONT-SIZE) if it exists."
  (when (member font-family (font-family-list))
    (set-face-font face (concat font-family "-" (number-to-string font-size)))))


(defvar *init/org-scratch-buffer-name* "*org-notes*"
  "The name for our scratch org buffer.")

(defun init/create-scratch-org-buffer ()
  "Create a new scratch `org-mode' buffer."
  (interactive)
  (save-excursion
    (switch-to-buffer (get-buffer-create *init/org-scratch-buffer-name*))
    (org-mode)
    (insert "#+TITLE: Org-Mode scratch buffer for notes\n\n")
    (insert (concat "* Notes for " (format-time-string "%Y-%m-%d")))
    (insert "\n\n")))


(defvar *baw-modes-allowed-to-split*
  '("magit" "COMMIT_EDITMSG")
  "Regexps that match major modes allowed to split my windows.")

(defun baw-major-mode-contains-p (partial-string)
  "Does the current `major-mode' contain PARTIAL-STRING?"
  (not
   (null
    (string-match-p (regexp-quote partial-string) (symbol-name major-mode)))))

(defun baw-should-split-sensibly-p ()
  "Determine if the current major mode is allowed to split."
  (not
   (null
    (seq-filter 'baw-major-mode-contains-p *baw-modes-allowed-to-split*))))

(defun baw-split-window (&optional window)
  "Maybe split the window sensibly.

Applies to the curren window unless WINDOW is specified."
  (cond ((baw-should-split-sensibly-p)
         (split-window-sensibly window))
        (t
         nil)))

(setq split-window-preferred-function 'baw-split-window)

;;;; Setup hooks
(defun init/gui-setup ()
  "Setup bits for GUI environments."
  (message "INIT: Loading GUI configuration")
  (init/safe-set-face-font 'variable-pitch "Symbola" 14)
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))

  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;; Set the frame title for Quantified Self Capture
  (setq frame-title-format " %b -- %m -- Emacs")

  ;; Frame commands
  (global-set-key (kbd "M-`") 'other-frame)

  ;; Imenu
  (global-set-key (kbd "<mouse-3>") 'imenu)

  ;; Toolbar/menubar
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))     ; Disable the toolbar

  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))   ; Disable the scrollbars

  (split-window-horizontally)
  (when (< (length command-line-args) 2)
    (switch-to-buffer "*scratch*"))
  (switch-to-buffer-other-window *init/org-scratch-buffer-name*)
  (toggle-frame-maximized))

(defun init/terminal-setup ()
  "Setup bits for terminals only."
  (message "INIT: Loading terminal configuration")
  (menu-bar-mode -1))


(defun init/common-setup ()
  "Setup bits that should always be applied."
  (message "INIT: Loading common configuration")

  (add-hook 'after-init-hook #'init/create-scratch-org-buffer)

  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode nil))    ; Disable the menubar (Doesn't impact os-x)

  (fset 'yes-or-no-p 'y-or-n-p)   ; Make y/n prompts easier

  (setq-default fill-column 78)           ; Wider fill by default
  (setq-default indent-tabs-mode nil)     ; Prevent tabs by default
  (setq-default tab-width 2)              ; If we are using tabs, make them small
  (setq suggest-key-bindings t)           ; Let emacs teach me
  (setq visible-bell t)                   ; No beeps!
  (setq inhibit-startup-screen t)         ; I'll miss it, but it no longer works for me

  (setq backup-by-copying t)      ; don't clobber symlinks
  (setq backup-directory-alist
	'(("." . "/tmp/emacs")))  ; don't litter my fs tree
  (setq delete-old-versions t)    ; limit how much space we take up
  (setq kept-new-versions 6)      ; keep more from this session
  (setq kept-old-versions 2)      ; keep less from last session
  (setq version-control t)        ; use versioned backups

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
  (global-hl-line-mode)       ; Highlight the current line

  ;; Prevent custom stuff from ending up in a vc controlled file
  (let ((custom-file-location (concat user-emacs-directory "custom.el")))
    (setq custom-file custom-file-location)
    (load custom-file-location t)))

;; macOS
(defun init/darwin-init ()
  "Define macOS specific initializations."
  (message "INIT: Loading macOS config")
  (setq mac-command-modifier 'meta)           ; Set META-Key to be CMD
  (setq mac-option-modifier 'none)            ; Unset Option so we get fancy inputs
  (if (display-graphic-p)
      (global-set-key (kbd "M-0") 'suspend-frame)) ; and rebind it to be sort-of mac like

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
              (add-to-list 'Info-directory-list dir)))))))

(defun init/windows-init ()
  "Define Windows specific initializations."
    (message "INIT: Loading Window config"))

(defun init/unix-init ()
    "Define other unix specific initializations."
    (message "INIT: Loading generic UNIX-ish config"))

;;;; Actually setup emacs

;; GUI vs. Terminal
;; This gets run last
(add-hook 'after-init-hook (lambda ()
                             (if (display-graphic-p)
                                 (init/gui-setup)
                               (init/terminal-setup))))

;; OS
(cond ((equal system-type 'darwin)
       (init/darwin-init))
      ((equal system-type 'windows-nt)
       (init/windows-init))
      (t
       (init/unix-init)))

;; Common
(init/common-setup)

;; Load all files
(message "INIT: Loading config files")

(init/maybe-load-config "00-faces" t)       ; Extra theme / face configuration
(init/maybe-load-config "05-packages" t)    ; Extra theme / face configuration
(init/maybe-load-config "10-expansion" t)   ; Text expansion / snippets
(init/maybe-load-config "20-org" t)         ; Org mode
(init/maybe-load-config "30-programming" t) ; Programming
(init/maybe-load-config "40-writing" t)     ; Document generation
(init/maybe-load-config "50-mail" t)        ; email
(init/maybe-load-config "90-work" t)        ; Work stuff

;; Reset the cons threshhold
(setq gc-cons-threshold init/original-gc-cons-threshold)

(message "Init loading finished")

(provide 'init)
;;; init.el ends here
