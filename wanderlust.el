(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write drafts in wanderlust." t)

;; IMAP, gmail:
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "bwilson@oblong.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl

      ;; For non ascii-characters in folder-names
      elmo-imap4-use-modified-utf7 t

      ;; How do we store passwords?
      elmo-passwd-storage-type 'auth-source)

;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "bwilson@oblong.com"
      wl-smtp-posting-server "smtp.gmail.com"
      wl-local-domain "gmail.com"
      wl-message-id-domain "smtp.gmail.com")

(setq wl-from "Brian Wilson <bwilson@oblong.com>"

      ;; All system folders (draft, trash, spam, etc) are placed in the
      ;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
      wl-default-folder "%inbox"
      wl-draft-folder   "%[Gmail]/Drafts"
      wl-trash-folder   "%[Gmail]/Trash"
      ;; The below is not necessary when you send mail through Gmail's SMTP server,
      ;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
      ;; wl-fcc            "%[Gmail]/Sent"

      ;; Mark sent messages as read (sent messages get sent back to you and
      ;; placed in the folder specified by wl-fcc)
      wl-fcc-force-as-read    t

      ;; For auto-completing foldernames
      wl-default-spec "%"

      ;; Not sure what this does
      wl-folder-check-async t
      wl-other-frame t
      wl-draft t
      wl-stay-folder-window t                       ;; show the folder pane (left)
      wl-folder-window-width 25                     ;; toggle on/off with 'i'

      ;; hide many fields from message buffers
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^Message-Id:"
        "^\\(Posted\\|Date\\):"
        )
      wl-message-sort-field-list
      '("^From"
        "^Organization:"
        "^X-Attribution:"
        "^Subject"
        "^Date"
        "^To"
        "^Cc")
      wl-auto-save-drafts-interval nil)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(setq mime-view-type-subtype-score-alist
  '(((text . plain) . 4)
    ((text . enriched) . 3)
    ((text . html) . 2)
    ((text . richtext) . 1)))

(require 'bbdb)
(bbdb-initialize 'wl)
(bbdb-mua-auto-update-init 'wl)


;; Disable v key in the view mode
(add-hook
 'mime-view-mode-hook
 '(lambda ()
    "Disable 'v' for mime-play."
    ;; Key bindings
    (local-set-key [?v] () )
    ))

;; Refile with a single keypress
;; From: https://emacs-fu.blogspot.com/2009/09/wanderlust-tips-and-tricks.html
(defun baw/wl-summary-refile (&optional folder)
  "refile the current message to FOLDER; if FOLDER is nil, use the default"
  (interactive)
  (wl-summary-refile (wl-summary-message-number) folder)
  (wl-summary-next)
  (message "refiled to %s" folder))

;; (define-key wl-summary-mode-map (kbd "b x") ;; => Project X
;;   '(lambda()(interactive)(djcb-wl-summary-refile ".project-x")))

;; (define-key wl-summary-mode-map (kbd "b y") ;; => Project Y
;;   '(lambda()(interactive)(djcb-wl-summary-refile ".project-y")))

(define-key wl-summary-mode-map (kbd "b a") ;; => Archive
  '(lambda()(interactive)(baw/wl-summary-refile "%[Gmail]/All Mail")))
