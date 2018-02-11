;;; work.el --- Load work specific stuff             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Brian Wilson

;; Author: Brian Wilson <>
;; Keywords: internal

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

;; This is a place holder for the current work's specific config

;;; Code:

;; Oblong
(defvar g-speak-home
  (let ((g-speak-env (getenv "G_SPEAK_HOME")))
    (if g-speak-env
        g-speak-env
      "/opt/oblong/g-speak4.4"))
  "Location of g-speak root.")

(defvar g-speak-deps
  (let ((g-speak-deps-env (getenv "G_SPEAK_DEPS"))) ; TODO(brian): This can't be right...
    (if g-speak-deps-env
        g-speak-deps-env
      "/opt/oblong/deps-64-12"))
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
      (init/maybe-load-file (concat oblong-dir "/init.el"))
      (init/install-oblong-hooks)
      (init/maybe-load-file (concat oblong-dir "/spruce.el"))
      (message "Oblong load complete."))))

(init/maybe-load-oblong)

(provide 'work)
;;; work.el ends here
