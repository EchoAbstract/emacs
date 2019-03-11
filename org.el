;;; org.el ---  Setup Org environment
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(eval-after-load "org-agenda"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))

     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)))

(define-key global-map [(control meta ?r)] 'org-capture)

(custom-set-variables
 '(org-completion-use-ido t)
 '(org-agenda-files (quote ("~/Dropbox/org/"
                            "~/Dropbox/org/oblong/")))
 '(org-agenda-file-regexp "^agenda-.*\\.org$")
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-capture-templates
   (quote (("n"
            "Notes" entry
            (file+headline "~/Dropbox/org/notes.org" "Notes")
            "* %u %?")
           ("g"
            "Guile" entry
            (file+headline "~/Dropbox/org/agenda-guile.org" "Tasks")
            "* TODO %?\n  %u")
           ("p"
            "Personal" entry
            (file+headline "~/Dropbox/org/agenda-personal.org" "Tasks")
            "* TODO %?\n  %u")
           ("o"
            "Oblong" entry
            (file+headline "~/Dropbox/org/oblong/agenda-oblong.org" "Tasks")
            "* TODO %?\n  %u")
           ("e"
            "Emacs" entry
            (file+headline "~/Dropbox/org/oblong/agenda-emacs.org" "Tasks")
            "* TODO %?\n  %u")))))

;;; org.el ends here
