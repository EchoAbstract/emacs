;;; 50-mail.el --- Utilities for mail -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2018 Brian Wilson <brian@polytopes.me>
;;
;; Author: Brian Wilson <brian@polytopes.me>
;; URL: https://gihub.com/EchoAbstract/emacs
;; Keywords: mail

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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(load-library "mu4e")

(setq mu4e-contexts
 `( ,(make-mu4e-context
     :name "ObMail"
     :match-func (lambda (msg) (when msg
       (string-prefix-p "/ObMail" (mu4e-message-field msg :maildir))))
     :vars '(
       (mu4e-trash-folder . "/ObMail/[Gmail].Trash")
       (mu4e-refile-folder . "/ObMail/[Gmail].All Mail")
       ))
    ))


(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
    (concat
     "flag:unread maildir:/ObMail/INBOX"
     ))
  (mu4e-alert-enable-mode-line-display)
  (defun gjstein-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display)
    )
  (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
  )

(defun remove-nth-element (nth list)
  "Remove the NTH element from LIST."
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))
(setq mu4e-marks (remove-nth-element 5 mu4e-marks))
(add-to-list 'mu4e-marks
     '(trash
       :char ("d" . "▼")
       :prompt "dtrash"
       :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
       :action (lambda (docid msg target)
                 (mu4e~proc-move docid
                    (mu4e~mark-check-target target) "-N"))))

;; Include a bookmark to open all of my inboxes
(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "All Inboxes"
        :query "maildir:/ObMail/INBOX"
        :key ?i))

;; This allows me to use 'helm' to select mailboxes
(setq mu4e-completing-read-function 'completing-read)
;; Why would I want to leave my message open after I've sent it?
(setq message-kill-buffer-on-exit t)
;; Don't ask for a 'context' upon opening mu4e
(setq mu4e-context-policy 'pick-first)
;; Don't ask to quit... why is this the default?
(setq mu4e-confirm-quit nil)


;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587
    smtpmail-auth-credentials
    '(("smtp.gmail.com" 587 "bwilson@oblong.com@gmail.com" nil)))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


(provide '50-mail)
;;; 50-mail.el ends here

