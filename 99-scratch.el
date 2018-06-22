;;; 99-scratch.el --- Misc testing code              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Brian Wilson

;; Author: Brian Wilson <bwilson@oblong.com>
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

(use-package emms
  :ensure t
  :config
  (progn
    (require 'emms-setup)
    (emms-all)
    (emms-default-players)
    (setq emms-source-file-default-directory "~/Music/MP3"))
  :defer t)

(use-package request-deferred
  :ensure t
  :defer t)

(use-package airplay
  :ensure t
  :defer t)


(provide '99-scratch)
;;; 99-scratch.el ends here
