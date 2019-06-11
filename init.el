;;; init.el --- Emacs configuration of Brian Wilson -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018, 2019 Brian Wilson <brian@polytopes.me>
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
(defun init-log (message)
  "Force log MESSAGE to be visible in messages buffer."
  (concat "--- INIT: " message))


; ────────────────────────────────────────────────────────────────────────────
(init-log "Loading Brian's functions")
; ────────────────────────────────────────────────────────────────────────────

(load (concat user-emacs-directory "funcs.el"))
(baw/maybe-load-file "~/src/elisp/loader" t t)

(when (fboundp 'baw/load-all)
  (baw/load-all))

; ────────────────────────────────────────────────────────────────────────────
(init-log "Emacs variable configuration")
; ────────────────────────────────────────────────────────────────────────────

;; Simple
(setq backup-by-copying t)       ; don't clobber symlinks
(setq backup-directory-alist'(("." . "/tmp/emacs")))
(setq default-major-mode 'text-mode)    ; Text-mode is better than fundamental
(setq delete-old-versions t)     ; limit how much space we take up
(setq indent-tab-width 2)        ; Yet another tab default of 2
(setq inhibit-startup-screen t)  ; I'll miss it, but it no longer works for me
(setq kept-new-versions 6)       ; keep more from this session
(setq kept-old-versions 2)       ; keep less from last session
(setq load-prefer-newer t)       ; Load .el if newer than .elc
(setq scroll-step 1)             ; Only scroll 1 new-line at EOF
(setq suggest-key-bindings t)    ; Teach me about new bindings
(setq version-control nil)       ; Don't version the backup files
(setq visible-bell t)            ; Don't beep

(setq-default fill-column 78)          ; Wider fill by default
(setq-default indent-tabs-mode nil)    ; Prevent tabs by default
(setq-default tab-width 2)             ; If we are using tabs, make them small

(setq dabbrev-always-check-other-buffers t)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)
(require 'dabbrev)

;; Complex

;; Prevent custom stuff from ending up in a vc controlled file
(let ((custom-file-location (concat user-emacs-directory "custom.el")))
  (setq custom-file custom-file-location)
  (load custom-file-location t))


; ────────────────────────────────────────────────────────────────────────────
(init-log "Other built-in customizations")
; ────────────────────────────────────────────────────────────────────────────

(column-number-mode 1)      ; What's my current column?
(display-battery-mode 1)    ; Hopefully this works without a battery?
(display-time)              ; Full-screen emacs without a time?
(global-subword-mode)       ; I prefer camel case...
(scroll-bar-mode -1)        ; No Scrollbars
(show-paren-mode 1)         ; I like to see my parens
(tool-bar-mode -1)          ; No toolbars

;; Don't make me type out `yes'
(fset 'yes-or-no-p 'y-or-n-p)   ; Make y/n prompts easier

;; Update copyright if able
(add-hook 'write-file-hooks 'copyright-update)

;; Don't prompt, just revert
(global-auto-revert-mode 1)

; ────────────────────────────────────────────────────────────────────────────
(init-log "GUI / TUI config")
; ────────────────────────────────────────────────────────────────────────────

(if (display-graphic-p)
    (progn
      (setq frame-title-format " %b -- %m -- Emacs"))
  (progn
    (menu-bar-mode nil)))


; ────────────────────────────────────────────────────────────────────────────
(init-log "Platform specific config (pre-packages)")
; ────────────────────────────────────────────────────────────────────────────

(cond ((equal system-type 'darwin)
       (init-log "Darwin/macOS")
       (setq mac-command-modifier 'meta) ; Set META-Key to be CMD
       (setq mac-option-modifier 'none)) ; Unset Option so we get fancy inputs
      ((equal system-type 'windows-nt)
       (init-log "Windows"))
      (t
       (init-log "UNIX/Linux")))


; ────────────────────────────────────────────────────────────────────────────
(init-log "Loading/configuring packages")
; ────────────────────────────────────────────────────────────────────────────
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish :ensure t)

(use-package ivy
  :ensure t
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq magit-completing-read-function 'ivy-completing-read))
  :diminish (ivy-mode . "[i]"))

;; Info mode additions
(use-package info-colors
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 'Info-mode-hook		; After Info-mode has started
              (lambda ()
                (setq Info-additional-directory-list Info-default-directory-list)))
    (add-hook 'Info-selection-hook 'info-colors-fontify-node)))

(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-mode +1)
    ; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index"))
  :diminish (projectile-mode . "[p]"))

(use-package ag
  :defer t
  :ensure t)

(use-package discover-my-major
  :defer t
  :ensure t)

(use-package neotree
  :defer t
  :ensure t)

(use-package multi-term
  :defer t
  :ensure t)

(use-package magit
  :ensure t
  :init (progn
            (setq magit-diff-options (quote ("--word-diff")))
            (setq magit-diff-refine-hunk 'all))
  :config (progn
            (defun magit-quit-session ()
              "Quit the magit buffer"
              (interactive)
              (kill-buffer))
            (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
  :defer 10)

(use-package expand-region
  :ensure t
  :config (progn
            (global-set-key (kbd "C-=") 'er/expand-region)))

;; LSP
(use-package lsp
  :commands lsp
  :config (progn
            (require 'lsp-clients)
            (setq lsp-prefer-flymake nil)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            (setq lsp-ui-sideline-enable nil)))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config (progn
            (setq company-lsp-cache-candidates t)))


;; Text mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (set (make-local-variable 'dabbrev-case-fold-search) t)
	    (set (make-local-variable 'dabbrev-case-replace) t)
      (text-mode-hook-identify)
      (turn-on-auto-fill)))

(use-package pandoc-mode :defer t :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.md.html\\'" . markdown-mode))))

;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :defer t
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


(use-package flycheck
  :defer t
  :ensure t
  :diminish (flycheck-mode . "[f]"))

;; (use-package flycheck
;;   :defer t
;;   :ensure t
;;   :diminish (flycheck-mode . "F")
;;   :init (progn
;;           (add-hook 'after-init-hook #'global-flycheck-mode)
;;           (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
;;           (setq flycheck-c/c++-clang-executable "~/LLVM/latest/bin/clang")
;;           (setq flycheck-c/c++-clang-executable "~/LLVM/latest/bin/clang")
;;           (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
;;   :config (progn
;;           ;; enable typescript-tslint checker
;;           (flycheck-add-mode 'typescript-tslint 'web-mode)))

(use-package company
  :ensure t
  :init (progn
          (add-hook 'after-init-hook #'global-company-mode))
  :config (progn
            (setq company-idle-delay                2
                  company-minimum-prefix-length     2
                  company-show-numbers              t
                  company-tooltip-limit             20
                  company-tooltip-align-annotations t
                  ;; From the info page
                  ;; If you set this value to nil, you may also want to set
                  ;; ‘company-dabbrev-ignore-case’ to any value other than ‘keep-prefix’.
                  company-dabbrev-downcase        nil)
            (setq company-backends (delete 'company-semantic company-backends))
            ;; (global-set-key (kbd "M-/") 'company-complete)
            (global-set-key (kbd "C-M-i") 'company-complete))
  :diminish (company-mode . "[c]"))


;;;; shell-scripting
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(add-hook 'shell-mode-hook 'lsp)

;;;; elisp
(use-package f :defer t :ensure t)               ; Modern File API
(use-package kv :defer t :ensure t)              ; Modern Key-Value API

(use-package elisp-slime-nav
  :defer t
  :ensure t)

(use-package eros
  :defer t
  :ensure t
  :config (eros-mode t))

;;;; Lisp programming
(use-package geiser :defer t :ensure t)
(use-package paredit :defer t :ensure t)

;;;; Building
(use-package cmake-mode
  :defer t
  :ensure t)

(use-package meson-mode
  :defer t
  :ensure t)

;;;; Data interchange formats
(use-package yaml-mode
  :defer t
  :ensure t)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Filter that ansi-colors compilation region."
  (read-only-mode 0)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 1))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package cquery
  :ensure t
  :defer t)

(setq init/cpp-other-file-alist
      '(("\\.cc$" (".hh" ".h"))
        ("\\.hh$" (".cc" ".C"))
        ("\\.c$" (".h"))
        ("\\.h$" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m"))
        ("\\.C$" (".h" ".hh" ".H"))
        ("\\.H$" (".C" ".CC"))
        ("\\.CC$" (".HH" ".H" ".hh" ".h"))
        ("\\.HH$" (".CC"))
        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hh" ".h" ".hpp"))
        ("\\.hpp$" (".cpp"))))
;; (setq init/cpp-other-file-alist
;;   '(("\\.cpp\\'" (".h" ".hpp" ".hh"))
;;     ("\\.hpp\\'" (".cpp"))
;;     ("\\.cc\\'" (".hh" ".h"))
;;     ("\\.hh\\'" (".cc" ".cpp" ".C"))
;;     ("\\.c\\'" (".h"))
;;     ("\\.C\\'" (".h"))
;;     ("\\.h\\'" (".cpp" ".cc" ".c" ".C"))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq ff-ignore-include t) ; I don't want this to jump to includes
            (setq ff-other-file-alist init/cpp-other-file-alist)
            (setq compilation-scroll-output t)
            (setq c-basic-offset 2)
            (setq c-default-style "gnu")
            (lsp)))

(use-package modern-cpp-font-lock
  :ensure t
  :config (modern-c++-font-lock-global-mode t))

(use-package glsl-mode :defer t :ensure t)
(use-package clang-format :defer t :ensure t)
;;;; Web

(use-package rainbow-mode
  :ensure t)

(defun init/js-common-hooks ()
  "Common code after JS modes load."
  (subword-mode 1)
  (setq js2-basic-offset 2)
  (lsp))

(use-package js2-mode
  :defer t
  :ensure t
  :init (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
            (setq-default js2-basic-offset 2)
            (setq js-indent-level 2)
            (add-hook 'js2-mode-hook #'init/js-common-hooks)))

(use-package vue-mode
  :defer t
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))))


(use-package typescript-mode
  :defer t
  :ensure t
  :init (progn
          (setq-default typescript-indent-level 2)
          (add-hook 'typescript-mode-hook 'lsp)
          (add-hook 'typescript-mode-hook (lambda ()
                                            (setq typescript-indent-level 2)
                                            (subword-mode)
                                            (lsp)))
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))))

(use-package web :ensure t :defer t)             ; Make web requests

(defun init/web-mode-hook ()
  "Brian's Web Mode hook."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (lsp))


(use-package web-mode                            ; Mixing HTML and Scripts
  :defer t
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
          (add-hook 'web-mode-hook #'init/web-mode-hook)))

;;;; Python
(setq python-shell-interpreter "ipython3")
(setq python-shell-completion-native-enable nil)
(add-hook 'python-mode-hook 'lsp)

;;;; golang

(use-package go-mode :defer t :ensure t)
(use-package flymake-go :defer t :ensure t)
(use-package go-complete :defer t :ensure t
  :config (progn
            (add-hook 'completion-at-point-functions 'go-complete-at-point)))
(use-package go-eldoc :defer t :ensure t)
(use-package go-guru :defer t :ensure t)
(use-package go-impl :defer t :ensure t)
(use-package go-imports :defer t :ensure t)


;;;; Ops / Admin
(use-package docker :defer t :ensure t)
(use-package dockerfile-mode :defer t :ensure t)
(use-package systemd :defer t :ensure t)
(use-package launchctl :defer t :ensure t)

;;;; Random...
(use-package clojure-mode
  :defer t
  :ensure t)

(use-package cider
  :disabled
  :ensure t)

(use-package slime-docker
  :defer t
  :ensure t)

;; Apple's Swift Language
(use-package swift-mode
  :defer t
  :ensure t)

;; OCaml Support
(use-package tuareg
  :defer t
  :ensure t)

(use-package kubernetes
  :disabled
  :ensure t)

(use-package sly
  :defer t
  :ensure t)


; ────────────────────────────────────────────────────────────────────────────
(init-log "Fonts n' Themes")
; ────────────────────────────────────────────────────────────────────────────

;; These are some stale notes that I don't remember what they are...
;; Make font bigger, and Install these 5 packages (rebecca-theme-20180324.821,
;; nova-theme-20180530.1501, hemisu-theme-20130508.1844,
;; dakrone-theme-20170801.1933, dakrone-light-theme-20170808.2140)? (y or n) y

;; Some notes on themes
;; Poet is a good document theme, but it can get a little tiresome on the eyes
;; tsdh-light is also a good light theme
;; hemisu-theme is also a favorite, however
;; the lab-themes have both a light and a dark varient that I've been enjoying.

(when (fboundp 'baw/load-theme-advice)
  (advice-add 'load-theme
              :around
              #'baw/load-theme-advice))

(use-package hemisu-theme :ensure t)
(load-theme 'hemisu-light)

(use-package olivetti
  :ensure t
  :defer t
  :preface
  (defun olivetti-config ()
    (variable-pitch-mode 1)
 	  (olivetti-mode 1)
 	  (olivetti-set-width 0.75))
  :hook ((text-mode . olivetti-config)
          (Info-mode . (lambda () (olivetti-mode)))))

(baw/safe-set-face-font 'default "Source Code Pro" 12)
(baw/safe-set-face-font 'variable-pitch "Symbola" 12)

;; Dashboard
(use-package dashboard
  :ensure t
  :demand
  :preface
  (defun my/dashboard-banner ()
    "Dashboard banner."
    (setq dashboard-banner-logo-title (when (fboundp 'baw/formatted-date-time-string)
                                        (baw/formatted-date-time-string))))
  :config
  ;; (setq dashboard-startup-banner 'logo)
  ;; (setq dashboard-startup-banner "~/.emacs.d/oblong/ob-logo-dark.png")
  (setq dashboard-startup-banner "~/.emacs.d/logo.png")
  (setq dashboard-banner-logo-title (my/dashboard-banner)); "Brian is at work, on ...")
  (dashboard-setup-startup-hook))
  ;; :hook ((after-init     . dashboard-refresh-buffer)
  ;;       (dashboard-mode . my/dashboard-banner)))



; ────────────────────────────────────────────────────────────────────────────
(init-log "Loading key bindings")
; ────────────────────────────────────────────────────────────────────────────

(baw/maybe-load-config "key-bindings" t)

; ────────────────────────────────────────────────────────────────────────────
(init-log "Loading org stuff")
; ────────────────────────────────────────────────────────────────────────────

(baw/maybe-load-config "org" t)


; ────────────────────────────────────────────────────────────────────────────
(init-log "Maybe loading work files")
; ────────────────────────────────────────────────────────────────────────────

(baw/maybe-load-config "work" t)        ; Work stuff


; ────────────────────────────────────────────────────────────────────────────
(init-log "Init loading finished")
; ────────────────────────────────────────────────────────────────────────────


; ────────────────────────────────────────────────────────────────────────────
(init-log "Maybe loading server?")
; ────────────────────────────────────────────────────────────────────────────
(load "server")
(when (not (server-running-p))
  (init-log "Starting server")
  (server-start))

(provide 'init)
;;; init.el ends here
