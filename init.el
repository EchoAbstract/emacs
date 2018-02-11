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


(setq init/original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000) ; Temporarily bump up the gc threshold

;; First things first, never load the older of byte-code vs. elisp
(setq load-prefer-newer t)

;; Need some cl here
(require 'cl)


;; Set up `use-package` mode
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(defvar init/profile-emacs-init-time t
  "Setup various things for profiling startup time")

;; I've managed to get this down below 1 second for startup time
;; So... no need to profile
;; Total User Startup Time: 0.920sec     Total Number of GC Pauses: 1     Total GC Time: 0.059sec
(setq init/profile-emacs-init-time nil)

(when init/profile-emacs-init-time
  (setq use-package-compute-statistics t))


;; Here lies useful functions for the rest of this file

(defun init/maybe-load-file (file)
  "Load FILE only if it exists."
  (if (or
       (file-exists-p file)
       (file-exists-p (concat file ".el"))
       (file-exists-p (concat file ".elc")))
      (load file)
    (warn (concat "Can't load non-existent file: " file))))

(defun init/maybe-load-config (config-file)
  "Load CONFIG-FILE only if it exists."
  (init/maybe-load-file (concat user-emacs-directory config-file)))


;; Next, let's load up our platform specific configs

(defun init/exec-path-config ()
  "Here lies the config for the exec path."
  )

(defun init/darwin-init ()
  "Define OS-X specific initializations, GUI-P indicates GUI is present."
  (setq mac-command-modifier 'meta)           ; Set META-Key to be CMD
  (setq mac-option-modifier 'none)            ; Unset Option so we get fancy inputs
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

(defun init/safe-set-face-font (face font-family font-size)
  "If font family is found use it for face (with given size)."
  (when (member font-family (font-family-list))
    (set-face-font face (concat font-family "-" (number-to-string font-size)))))

(defun init/setup-gui ()
  "Set up GUI."

  ;; TODO: Add code to cycle through fonts I like
  (let* ((defualt-fixed-font-name "Monoisome")
         (defualt-variable-font-name "Symbola")
         (mac-fixed-font-name defualt-fixed-font-name)
         (linux-fixed-font-name defualt-fixed-font-name)
         (mac-variable-font-name defualt-variable-font-name)
         (linux-variable-font-name defualt-variable-font-name))

    ;; Mac fonts
    (when (equal (symbol-name system-type) "darwin")
      (let ((fsize 12))
        (init/safe-set-face-font 'default mac-fixed-font-name fsize)
        (init/safe-set-face-font 'variable-pitch  mac-variable-font-name (+ fsize 4))
        (init/safe-set-face-font 'fixed-pitch mac-fixed-font-name fsize)))

    ;; Linux fonts
    (when (equal (symbol-name system-type) "gnu/linux")
      (let ((fsize 13))
        (init/safe-set-face-font 'default linux-fixed-font-name fsize)
        (init/safe-set-face-font 'variable-pitch linux-variable-font-name fsize)
        (init/safe-set-face-font 'fixed-pitch  linux-fixed-font-name fsize))))

  ;; There is a bug with the next two bits of code on Fedora 27
  ;; (and likely ubuntu 18.04) where XFT bails trying to load
  ;; a font with color information.  Possible solutions are to build
  ;; emacs without XFT support, or *maybe* to build with Cairo support.

  ;; specify fonts for all emoji characters
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))

  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;; Set the frame title for Quantified Self Capture
  (setq frame-title-format " %b -- %m -- Emacs"))


(defun init/setup-terminal ()
  "Set up the terminal the way we like it."
  (menu-bar-mode -1)
  (load-theme 'misterioso t))           ; TODO maybe time to switch this up

(add-hook 'after-init-hook (lambda ()
                             (if window-system
                                 (init/setup-gui)
                               (init/setup-terminal))))

;; GUI Themes

;; I really like this theme, but it has some usability bugs
;; so I should fork and fix
;; (use-package monotropic-theme
;;   :ensure t
;;   :config
;;   (if window-system
;;       (load-theme 'monotropic t)))

(use-package dracula-theme
	     :ensure t
	     :config
	     (if window-system
           (load-theme 'dracula t)))



;; I ♥ UNICODE (in hex at least)
(setq read-quoted-char-radix 16)

;; Make sure that we can diminish modes
(use-package diminish :ensure t)

;; On platforms with working emoji this enables
;; slack-style :emoji: substitutions
(use-package company-emoji
  :defer t
  :ensure t)

;; Toolbar/menubar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))     ; Disable the toolbar

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode nil))    ; Disable the menubar (Doesn't impact os-x)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))   ; Disable the scrollbars

;; This replaces `^L' characters with lines
(use-package form-feed
  :defer t
  :ensure t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'form-feed-mode))
  :diminish form-feed-mode)

;; This is a profiling tool for this startup script.
;; It's pretty useful
(use-package esup
  :when init/profile-emacs-init-time
  :defer t
  :ensure t)



;;; Overall Stuff

(fset 'yes-or-no-p 'y-or-n-p)   ; Make y/n prompts easier

;; General Vars
(setq-default fill-column 78)           ; Wider fill by default
(setq-default indent-tabs-mode nil)     ; Prevent tabs by default
(setq-default tab-width 2)              ; If we are using tabs, make them small
(setq suggest-key-bindings t)           ; Let emacs teach me
(setq visible-bell t)                   ; No beeps!
(setq inhibit-startup-screen t)         ; I'll miss it, but it no longer works for me

;; Get rid of the ~ files
(setq backup-by-copying t)      ; don't clobber symlinks
(setq backup-directory-alist
      '(("." . "/tmp/emacs")))  ; don't litter my fs tree
(setq delete-old-versions t)    ; limit how much space we take up
(setq kept-new-versions 6)      ; keep more from this session
(setq kept-old-versions 2)      ; keep less from last session
(setq version-control t)        ; use versioned backups

;; Keybindings
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

;; Frame commands
(global-set-key (kbd "M-`") 'other-frame)

;; Imenu
(global-set-key (kbd "<mouse-3>") 'imenu)

;; Finding corresponding files
(defun baw-ff-find-other-file (&optional in-other-window)
  "Wrapper around FF-FIND-OTHER-FILE that ignores includes."
  (interactive)
  (ff-find-other-file in-other-window t))

(global-set-key (kbd "<f9>") 'baw-ff-find-other-file)
(global-set-key (kbd "M-<f9>") (lambda () (interactive) (baw-ff-find-other-file t)))

;; Toggle all the things!
(auto-insert-mode 1)        ; Prompt for templates, TODO: Better?
(show-paren-mode 1)         ; I like to see my parens
(display-time)              ; Full-screen emacs without a time?
(column-number-mode 1)      ; What's my current column?

;; Global packages
; Swiper, counsel, ivy, counsel-projectile
(use-package ivy
  :ensure t
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq magit-completing-read-function 'ivy-completing-read))
  :diminish (ivy-mode . "ⓘ"))


(use-package swiper
  :ensure t
  :init (progn
          (global-set-key (kbd "C-s") 'swiper)))

(use-package counsel
  :ensure t
  :init (progn
          (global-set-key (kbd "M-x") 'counsel-M-x)
          (global-set-key (kbd "C-x C-f") 'counsel-find-file)
          (global-set-key (kbd "<f1> f") 'counsel-describe-function)
          (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
          (global-set-key (kbd "<f1> l") 'counsel-find-library)
          (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
          (global-set-key (kbd "<f2> u") 'counsel-unicode-char)))

(use-package counsel-projectile
  :ensure t
  :init (progn
          (counsel-projectile-mode 1)))

; Better mode line
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/mode-width 'full)
  ;; this makes sure that the mode line doesn't go off the screen
  (setq sml/name-width 40)
  (sml/setup))

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

(use-package ag
  :defer t
  :ensure t)
(use-package discover-my-major
  :defer t
  :ensure t)
(use-package neotree
  :defer t
  :ensure t)

;; Let's try yasnippet again
(use-package yasnippet
  :ensure t
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

(use-package yasnippet-snippets
  :ensure t
  :config (yas-reload-all)
  :after yasnippet)


;;; Programming

;; elisp
(use-package f :defer t :ensure t)               ; Modern File API
(use-package kv :defer t :ensure t)              ; Modern Key-Value API

(use-package elisp-slime-nav
  :defer t
  :ensure t)

(use-package eros
  :defer t
  :ensure t
  :config (eros-mode t))

;; Global Programming
(use-package flycheck
  :defer t
  :ensure t)

(use-package company
  :defer t
  :ensure t
  :init (progn
          (add-hook 'c++-mode-hook 'company-mode)
          (add-hook 'c-mode-hook 'company-mode)
          (add-hook 'obj-c-mode-hook 'company-mode)
          (global-company-mode))
  :config (progn
            (setq company-backends (delete 'company-semantic company-backends))
            (add-to-list 'company-backends 'company-rtags)
            (add-to-list 'company-backends 'company-emoji))
  :diminish (company-mode . "©"))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :diminish (projectile-mode . "℗")
  :demand t)

(use-package magit
  :ensure t
  :config (progn
            (defadvice magit-status (around magit-fullscreen activate)
              "Make magit-status run alone in a frame."
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))

            (defun magit-quit-session ()
              "Restore the previous window configuration and kill the magit buffer."
              (interactive)
              (kill-buffer)
              (jump-to-register :magit-fullscreen))

            (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
  :defer 10)


(use-package expand-region
  :defer t
  :ensure t
  :config (progn
            (global-set-key (kbd "C-=") 'er/expand-region)))

;;; Build systems
(use-package cmake-mode
  :defer t
  :ensure t)

(use-package meson-mode
  :defer t
  :ensure t)

;;; Data interchange formats
(use-package yaml-mode
  :defer t
  :ensure t)

;;; C++
(use-package rtags
  :load-path "~/src/rtags/build/src/"
  :defer 10
  :config (progn
            (define-key c-mode-base-map (kbd "M-.")
              (function rtags-find-symbol-at-point))
            (define-key c-mode-base-map (kbd "M-,")
              (function rtags-find-references-at-point))
            ;; install standard rtags keybindings. Do M-. on the symbol below to
            ;; jump to definition and see the keybindings.
            ;; company completion setup
            (setq rtags-autostart-diagnostics t)
            (rtags-diagnostics)
            (setq rtags-completions-enabled t)
            ;; (push 'company-rtags company-backends)
            ;; (global-company-mode)
            (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
            ;; (setq rtags-use-helm t)
            (setq rtags-use-ivy t)
            (rtags-enable-standard-keybindings)))


(defun setup-rtags-flycheck ()
  "Make sure that flycheck doesn't override RTags"
  (interactive)
  (flycheck-mode)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(use-package flycheck-rtags
  :config (progn
            (add-hook 'c-mode-common-hook #'setup-rtags-flycheck))
  :after rtags)

;; Prettier Compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Filter that ansi-colors compilation region."
  (read-only-mode 0)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 1))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq init/cpp-other-file-alist
  '(("\\.cpp\\'" (".h" ".hpp" ".hh"))
    ("\\.hpp\\'" (".cpp" ".cc"))
    ("\\.cc\\'" (".h" ".hh"))
    ("\\.hh\\'" (".cc" ".cpp"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".cpp" ".cc" ".c"))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (subword-mode 1)           ; Enable subword mode
            (setq ff-ignore-include t) ; I don't want this to jump to includes
            (setq ff-other-file-alist init/cpp-other-file-alist)
            (setq compilation-scroll-output t)))

(use-package glsl-mode :defer t :ensure t)
(use-package clang-format :defer t :ensure t)

;; Shell Scripting
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; Go
(use-package go-mode :defer t :ensure t)
(use-package flymake-go :defer t :ensure t)
(use-package go-complete :defer t :ensure t
  :config (progn
            (add-hook 'completion-at-point-functions 'go-complete-at-point)))
(use-package go-eldoc :defer t :ensure t)
(use-package go-guru :defer t :ensure t)
(use-package go-impl :defer t :ensure t)
(use-package go-imports :defer t :ensure t)
(use-package go-projectile :defer t :ensure t)

;;; Web Programming
(use-package js2-mode
  :defer t
  :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))))
(use-package tide :ensure t :defer t)            ; TypeScript
(use-package web :ensure t :defer t)             ; Make web requests
(use-package web-mode                            ; Mixing HTML and Scripts
  :defer t
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))))


;;; Lisp programming
(use-package geiser :defer t :ensure t)
(use-package paredit :defer t :ensure t)

;;; Ops / Admin
(use-package docker :defer t :ensure t)
(use-package dockerfile-mode :defer t :ensure t)
(use-package systemd :defer t :ensure t)
(use-package launchctl :defer t :ensure t)



;;; Document generation

(use-package ox-pandoc
  :ensure t
  :after ox)

(use-package org
  :defer 5
  :ensure org-plus-contrib
  ;; :ensure t
  :config
  (progn
    (setq org-capture-templates
          '(;; other entries
            ("j" "Journal entry" plain
             (file+datetree+prompt "~/personal/journal.org")
             "%K - %<%H:%M:%S>\n%a\n%i\n%?\n")
            ;; other entries
            ))
    (global-set-key (kbd "C-c c") 'org-capture)))

(use-package pandoc-mode :defer t :ensure t)
(use-package markdown-mode :defer t :ensure t)


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


;; External config files

(init/maybe-load-config "goodies")      ; Experimental / not ready stuff
(init/maybe-load-config "work")         ; Work stuff



;; Get customization autogenerated stuff out of init.el
(let ((custom-file-location (concat user-emacs-directory "custom.el")))
  (setq custom-file custom-file-location)
  (load custom-file-location t))


;; Reset the cons threshhold
(setq gc-cons-threshold init/original-gc-cons-threshold)
