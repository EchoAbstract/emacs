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
  (setq default-input-method "MacOSX")
  (add-hook 'after-init-hook (lambda () (setq visible-bell nil)))) ; Visible Bell breaks on El Capitan, fix it here


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
  (set-face-font 'default        "M+ 1m light-13")
  (set-face-font 'variable-pitch "Noto Sans-13")
  (set-face-font 'fixed-pitch    "M+ 1m light-13")

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


;;; Let's start enabling modes

;;; Overall Stuff

(use-package helm                       ; Powerful minibuffer input framework
  :ensure t
  :bind (
         ;; Replace built-in commands with more powerful Helm variants
         ([remap find-file] . helm-find-files)
         ([remap switch-to-buffer] . helm-mini)
         ([remap execute-extended-command] . helm-M-x)
         ([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)
         ([remap apropos-command] . helm-apropos)
         ([remap occur] . helm-occur)
         ;; Additional helm commands
         ("C-c f l" . helm-locate-library)
         ("C-c f s" . helm-for-files)
         ("C-c f r" . helm-recentf)
         ("C-c h l" . helm-resume)
         ("C-c h m" . helm-man-woman)
         ("C-c i C" . helm-colors)
         ("C-c j t" . helm-imenu))
  :init
  (helm-mode 1)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded! Get rid of it ASAP!"))
  :config
  (setq helm-split-window-in-side-p t
        ;; Fuzzy matching
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-imenu-fuzzy-match t
        ;; Use recentf to manage file name history
        helm-ff-file-name-history-use-recentf t
        ;; Find libraries from `require', etc.
        helm-ff-search-library-in-sexp t
        ;; Don't automatically jump to imenu candidate if only one match,
        ;; because it makes the behaviour of this command unpredictable, and
        ;; prevents me from getting an overview over the buffer if point is on a
        ;; matching symbol.
        helm-imenu-execute-action-at-once-if-one nil)

  (when (eq system-type 'darwin)
    ;; Replace locate with spotlight for `helm-for-files'
    (setq helm-for-files-preferred-list
          (append (delq 'helm-source-locate
                        helm-for-files-preferred-list)
                  '(helm-source-mac-spotlight))))
  :diminish helm-mode)

;;; Programming
(use-package flycheck
	     :ensure t)

(use-package company
	     :ensure t
	     :init (progn
		     (add-hook 'c++-mode-hook 'company-mode)
		     (add-hook 'c-mode-hook 'company-mode)
		     (add-hook 'obj-c-mode-hook 'company-mode))
	     :config (progn
		       (setq company-backends (delete 'company-semantic company-backends))
		       (add-to-list 'company-backends 'company-rtags)
		       (add-to-list 'company-backends 'company-emoji)
		       (add-to-list 'company-backends 'company-irony)))

(use-package projectile
	     :ensure t
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
	     :ensure t
	     :config
	     (progn
	       (setq rtags-use-helm t)
	       (rtags-enable-standard-keybindings)))

(use-package cmake-ide
	     :ensure t
	     :config
	     (cmake-ide-setup))

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
