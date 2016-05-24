;;; init.el --- Emacs configuration of Brian Wilson -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016 Brian Wilson <brian@polytopes.me>
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


;; First things first, never load the older of byte-code vs. elisp
(setq load-prefer-newer t)

;; Need some cl here
(require 'cl)


;; Set up `use-package` mode
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; Next, let's load up our platform specific configs
(defun init/exec-path-config ()
  "Here lies the config for the exec path."
  )

(defun init/darwin-init ()
  "Define OS-X specific initializations, GUI-P indicates GUI is present."
  (setq mac-command-modifier 'meta)           ; Set META-Key to be CMD
  (setq mac-option-modifier 'none)            ; Unset Option so we get fancy inputs
  (global-unset-key (kbd "C-z"))              ; Unset C-z so we don't get annoying hides :-)
  (global-set-key (kbd "M-0") 'suspend-frame) ; and rebind it to be sort-of mac like
  (setq default-input-method "MacOSX"))


(when (equal (symbol-name system-type) "darwin")
  (use-package exec-path-from-shell
    :ensure t
    :if (and (eq system-type 'darwin) (display-graphic-p))
    :config
    (progn
      (when (string-match-p "/bash$" (getenv "SHELL"))
        ;; Use a non-interactive login shell.  A login shell, because my
        ;; environment variables are mostly set in `.zprofile'.
        (setq exec-path-from-shell-arguments '("-l")))

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
  (if (display-graphic-p)
      (init/darwin-init)))


;;; Look and feel

(defun init/setup-gui ()
  "Set up GUI."
  ;; Fonts
  (let ((fsize (if (equal (symbol-name system-type) "darwin")
                   "13"
                 "11")))
    (set-face-font 'default        (concat "M+ 1m light-" fsize))
    (set-face-font 'variable-pitch (concat "Noto Sans-" fsize))
    (set-face-font 'fixed-pitch    (concat "M+ 1m light-" fsize)))

  ;; specify fonts for all emoji characters
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))

  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;; Set the frame title for Quantified Self Capture
  (setq frame-title-format " %b -- %m -- Emacs")

  ;; I like this theme
  (load-theme 'misterioso t))


(defun init/setup-terminal ()
  "Set up the terminal the way we like it."
  (menu-bar-mode -1))

(add-hook 'after-init-hook (lambda ()
                             (if window-system
                                 (init/setup-gui)
                               (init/setup-terminal))))


;; I ♥ UNICODE (in hex at least)
(setq read-quoted-char-radix 16)

;; TODO: Setup company emoji complete
(use-package company-emoji
  :ensure t)

;; Toolbar/menubar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))     ; Disable the toolbar

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode nil))    ; Disable the menubar (Doesn't impact os-x)

(use-package form-feed
  :ensure t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'form-feed-mode))
  :diminish form-feed-mode)


;;; Overall Stuff

(fset 'yes-or-no-p 'y-or-n-p)   ; Make y/n prompts easier

;; General Vars
(setq-default fill-column 78)           ; Wider fill by default
(setq-default indent-tabs-mode nil)     ; Prevent tabs by default
(setq-default tab-width 2)              ; If we are using tabs, make them small
(setq suggest-key-bindings t)           ; Let emacs teach me
(setq visible-bell t)                   ; No beeps!


;; Toggle all the things!
(auto-insert-mode 1)        ; Prompt for templates, TODO: Better?
(show-paren-mode 1)         ; I like to see my parens
(display-time)              ; Full-screen emacs without a time?
(column-number-mode 1)      ; What's my current column?

;; Global packages
(use-package ido
  :ensure t
  :init (progn
          (setq ido-enable-flex-matching t)
          (setq ido-everywhere t)
          (setq ido-create-new-buffer 'always)
          (setq ido-ignore-extensions t)
          (ido-mode 1)))


;;; Programming
(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :init (progn
          (add-hook 'c++-mode-hook 'company-mode)
          (add-hook 'c-mode-hook 'company-mode)
          (add-hook 'obj-c-mode-hook 'company-mode)
          (global-company-mode))
  :config (progn
            (setq company-backends (delete 'company-semantic company-backends))
            (add-to-list 'company-backends 'company-rtags)
            (add-to-list 'company-backends 'company-emoji)
            (add-to-list 'company-backends 'company-irony))
  :diminish (company-mode . "©"))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :diminish (projectile-mode . "℗")
  :demand t)

(use-package magit
  :ensure t)

;;; C++
(use-package auto-complete-clang
  :ensure t)

(defun init/irony-mode-hook ()
  "Hook for irony mode initialization"
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(use-package irony
  :ensure t
  :init (progn
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'c-mode-hook 'irony-mode)
          (add-hook 'objc-mode-hook 'irony-mode))
  :config (progn
            (add-hook 'irony-mode-hook 'init/irony-mode-hook)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :ensure t)

(use-package rtags
  :if (eq system-type 'darwin)          ; For now, until ubu1604
  :ensure t
  :config (progn
            ;; (setq rtags-use-helm t)
            (rtags-enable-standard-keybindings)))

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

;;; Code:
;; Candidates for Dev mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Filter that ansi-colors compilation region."
  (read-only-mode 0)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 1))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-scroll-output t)

;; Enable subword mode for target major modes
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))


;; Shell Scripting
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; Sort these
(use-package go-mode :ensure t)
(use-package js2-mode :ensure t)
(use-package ag :ensure t)
(use-package web-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package glsl-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package clojure-mode :ensure t)
(use-package cider :ensure t)
(use-package geiser :ensure t)
(use-package markdown-mode :ensure t)
(use-package elisp-slime-nav :ensure t)
(use-package discover-my-major :ensure t)
(use-package org :ensure t)
(use-package f :ensure t)
(use-package web :ensure t)
(use-package kv :ensure t)

;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t                ; Parse documents to provide completion
                                        ; for packages, etc.
        TeX-auto-save t                 ; Automatically save style information
        TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
        TeX-electric-math '("\\(" "\\)")
        ;; Don't insert magic quotes right away.
        TeX-quote-after-quote t
        ;; Don't ask for confirmation when cleaning
        TeX-clean-confirm nil
        ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq-default TeX-master nil          ; Ask for the master file
                TeX-engine 'luatex      ; Use a modern engine
                ;; Redundant in 11.88, but keep for older AUCTeX
                TeX-PDF-mode t)

  ;; Move to chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

(use-package tex-buf                    ; TeX buffer management
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  ;; Teach TeX folding about KOMA script sections
  (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            (,(rx (0+ space) "\\subsection*{") 3)
                            (,(rx (0+ space) "\\subsubsection*{") 4)
                            (,(rx (0+ space) "\\minisec{") 5))
        ;; No language-specific hyphens please
        LaTeX-babel-hyphen nil)

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))    ; Easy math input

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :defer t
  :after latex
  :init (auctex-latexmk-setup))

;; (use-package auctex-skim                ; Skim as viewer for AUCTeX
;;   :load-path "lisp/"
;;   :commands (auctex-skim-select)
;;   :after tex
;;   :init (auctex-skim-select))


;; Oblong
(defvar g-speak-home
  (let ((g-speak-env (getenv "G_SPEAK_HOME")))
    (if g-speak-env
        g-speak-env
      "/opt/oblong/g-speak3.24"))
  "Location of g-speak root.")

(defvar g-speak-deps
  (let ((g-speak-deps-env (getenv "G_SPEAK_DEPS"))) ; TODO(brian): This can't be right...
    (if g-speak-deps-env
        g-speak-deps-env
      "/opt/oblong/deps-64-11"))
  "Location of g-speak deps.")

(defun init/install-oblong-hooks ()
  "Install any oblong specific hooks."
  (let ((hook (lambda ()
                (add-to-list 'flycheck-clang-include-path (concat g-speak-home "/include"))
                (add-to-list 'flycheck-clang-include-path (concat g-speak-deps "/include")))))
    (add-hook 'c++-mode-hook hook)
    (add-hook 'c-mode-hook hook)))

(defun init/set-oblong-persona ()
  "This is my Oblong persona."
  (progn
    (setq user-mail-address "bwilson@oblong.com")
    (setq calendar-latitude 42.34998)
    (setq calendar-longitude -71.04949)
    (setq calendar-location-name "Boston, MA")))

(defun init/maybe-load-oblong ()
  "Load oblong configs if on an oblong system."
  (let ((oblong-dir (concat user-emacs-directory "oblong")))
    (message (concat "Checking for oblong customizations @ " oblong-dir))
    (when (file-exists-p oblong-dir)
      (message "Oblong config required, loading...")
      (init/set-oblong-persona)
      (add-to-list 'load-path oblong-dir)
      (load-file (concat oblong-dir "/init.el"))
      (init/install-oblong-hooks)
      (message "Oblong load complete."))))

(init/maybe-load-oblong)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (rtags exec-path-from-shell use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
