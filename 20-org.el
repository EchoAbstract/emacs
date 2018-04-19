;;; 20-org.el --- org-mode config -*- lexical-binding: t; -*-
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

(add-to-list 'load-path "~/src/org-mode/lisp")
(add-to-list 'load-path "~/src/org-mode/contrib/lisp" t)

(defun init/set-org-faces ()
  "Make `org-mode' pretty."
  (interactive)
  (let ((foreground "#333333")
        (background "#DFDBC3"))
    (custom-set-faces
     ;; `(org-default ((t (:foreground ,foreground :background ,background))))

     ;; `(org-agenda-calendar-event ((t ())))
     ;; `(org-agenda-calendar-sexp ((t ())))
     ;; `(org-agenda-clocking ((t ())))
     ;; `(org-agenda-column-dateline ((t ())))
     ;; `(org-agenda-current-time ((t ())))
     ;; `(org-agenda-date ((t ())))
     ;; `(org-agenda-date-today ((t ())))
     ;; `(org-agenda-date-weekend ((t ())))
     ;; `(org-agenda-diary ((t ())))
     ;; `(org-agenda-dimmed-todo-face ((t ())))
     ;; `(org-agenda-done ((t ())))
     ;; `(org-agenda-filter-category ((t ())))
     ;; `(org-agenda-filter-effort ((t ())))
     ;; `(org-agenda-filter-regexp ((t ())))
     ;; `(org-agenda-filter-tags ((t ())))
     ;; `(org-agenda-restriction-lock ((t ())))
     ;; `(org-agenda-structure ((t ())))
     ;; `(org-archived ((t ())))
     ;; `(org-block ((t ())))
     ;; `(org-block-begin-line ((t ())))
     ;; `(org-block-end-line ((t ())))
     ;; `(org-checkbox ((t ())))
     ;; `(org-checkbox-statistics-done ((t ())))
     ;; `(org-checkbox-statistics-todo ((t ())))
     ;; `(org-clock-overlay ((t ())))
     ;; `(org-code ((t ())))
     ;; `(org-column ((t ())))
     ;; `(org-column-title ((t ())))
     ;; `(org-date ((t ())))
     ;; `(org-date-selected ((t ())))
     ;; `(org-document-info ((t ())))
     ;; `(org-document-info-keyword ((t ())))
     ;; `(org-document-title ((t ())))
     ;; `(org-done ((t ())))
     ;; `(org-drawer ((t ())))
     ;; `(org-ellipsis ((t ())))
     ;; `(org-footnote ((t ())))
     ;; `(org-formula ((t ())))
     ;; `(org-headline-done ((t ())))
     ;; `(org-hide ((t ())))
     ;; `(org-latex-and-related ((t ())))
     ;; `(org-level-1 ((t (:foreground ,foreground :background ,background))))
     ;; `(org-level-2 ((t ())))
     ;; `(org-level-3 ((t ())))
     ;; `(org-level-4 ((t ())))
     ;; `(org-level-5 ((t ())))
     ;; `(org-level-6 ((t ())))
     ;; `(org-level-7 ((t ())))
     ;; `(org-level-8 ((t ())))
     ;; `(org-link ((t ())))
     ;; `(org-list-dt ((t ())))
     ;; `(org-macro ((t ())))
     ;; `(org-meta-line ((t ())))
     ;; `(org-mode-line-clock ((t ())))
     ;; `(org-mode-line-clock-overrun ((t ())))
     ;; `(org-priority ((t ())))
     ;; `(org-property-value ((t ())))
     ;; `(org-quote ((t ())))
     ;; `(org-scheduled ((t ())))
     ;; `(org-scheduled-previously ((t ())))
     ;; `(org-scheduled-today ((t ())))
     ;; `(org-sexp-date ((t ())))
     ;; `(org-special-keyword ((t ())))
     ;; `(org-table ((t ())))
     ;; `(org-tag ((t ())))
     ;; `(org-tag-group ((t ())))
     ;; `(org-target ((t ())))
     ;; `(org-time-grid ((t ())))
     ;; `(org-todo ((t ())))
     ;; `(org-upcoming-deadline ((t ())))
     ;; `(org-verbatim ((t ())))
     ;; `(org-verse ((t ())))
     ;; `(org-warning ((t ())))
     )))

(add-hook 'after-init-hook #'init/set-org-faces)


(provide '20-org)
;;; 20-org.el ends here
