;;; 05-packages.el --- Base package loading -*- lexical-binding: t; -*-
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

(use-package diminish :ensure t)

(use-package ivy
  :ensure t
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq magit-completing-read-function 'ivy-completing-read))
  :diminish (ivy-mode . "Ⓘ"))

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

(provide '05-packages)
;;; 05-packages.el ends here
