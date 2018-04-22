;;; 30-programming.el --- Programming modes -*- lexical-binding: t; -*-
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

;;;; Common
(use-package flycheck
  :defer t
  :ensure t
  :diminish (flycheck-mode . "Ⓕ")
  :init (progn
          (add-hook 'after-init-hook #'global-flycheck-mode)
          (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
          (setq flycheck-c/c++-clang-executable "~/LLVM/latest/bin/clang")
          (setq flycheck-c/c++-clang-executable "~/LLVM/latest/bin/clang")
          (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))))

(use-package company
  :ensure t
  :init (progn
          (add-hook 'after-init-hook #'global-company-mode))
  :config (progn
            (setq company-idle-delay              2
                  company-minimum-prefix-length   2
                  company-show-numbers            t
                  company-tooltip-limit           20
                  ;; From the info page
                  ;; If you set this value to nil, you may also want to set
                  ;; ‘company-dabbrev-ignore-case’ to any value other than ‘keep-prefix’.
                  company-dabbrev-downcase        nil)
            (setq company-backends (delete 'company-semantic company-backends))
            (global-set-key (kbd "M-/") 'company-complete)
            (global-set-key (kbd "C-M-i") 'company-complete))
  :diminish (company-mode . "Ⓒ"))


;;;; shell-scripting
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

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

;;;; C++

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

(use-package modern-cpp-font-lock
  :ensure t
  :config (modern-c++-font-lock-global-mode t))

(use-package glsl-mode :defer t :ensure t)
(use-package clang-format :defer t :ensure t)


;;;; Web

;; For CSS and other programming stuff we like to
;; see colors
(use-package rainbow-mode
  :ensure t)

(defun init/js-common-hooks ()
  "Common code after JS modes load."
  (subword-mode 1)
  (setq js2-basic-offset 2))

(use-package js2-mode
  :defer t
  :ensure t
  :init (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
            (setq-default js2-basic-offset 2)
            (add-hook 'js2-mode-hook #'init/js-common-hooks)))

;; TypeScript
(defun init/setup-tide-mode ()
  "Setup TypeScript Interactive Development Environment for Emacs."
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(defun init/tide-mode-before-save-hook ()
  "If we're in TypeScript mode, format before saving."
  (when (eq major-mode 'typescript)
    (tide-format-before-save)))

(use-package tide
  :ensure t
  :init (progn
          (add-hook 'typescript-mode-hook #'init/setup-tide-mode))
  :config (progn

            ;; aligns annotation to the right hand side
            (setq company-tooltip-align-annotations t) ; FIXME(brian): This feels wrong...

            ;; formats the buffer before saving
            (add-hook 'before-save-hook #'init/tide-mode-before-save-hook)
            (add-hook 'typescript-mode-hook #'init/setup-tide-mode))
  :defer t)

(use-package web :ensure t :defer t)             ; Make web requests

(use-package web-mode                            ; Mixing HTML and Scripts
  :defer t
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))))

;;;; Python
(use-package elpy
  :ensure t
  :after python
  :config
  (elpy-enable)
  )


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

(provide '30-programming.)
;;; 30-programming.el ends here
