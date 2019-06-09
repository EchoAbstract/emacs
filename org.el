;;; org.el ---  Setup Org environment
(require 'cl)

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

(setq org-directory
      (if (file-exists-p "~/Dropbox/org")
          "~/Dropbox/org"
        "/tmp"))

(defun baw/agenda-file (file)
  "FILE, but with agenda dir."
  (concat org-directory "/" file))

(defalias 'baw/setcust 'customize-set-variable)

(baw/setcust 'org-completion-use-ido t)

(baw/setcust 'org-agenda-files
             (cl-remove-if-not 'file-exists-p
                               (list org-directory
                                     (baw/agenda-file "oblong"))))

(baw/setcust 'org-agenda-file-regexp "^agenda-.*\\.org$")
(baw/setcust 'org-default-notes-file (baw/agenda-file "notes.org"))
(baw/setcust 'org-agenda-ndays 7)
(baw/setcust 'org-deadline-warning-days 14)
(baw/setcust 'org-agenda-show-all-dates t)
(baw/setcust 'org-agenda-skip-deadline-if-done t)
(baw/setcust 'org-agenda-skip-scheduled-if-done t)
(baw/setcust 'org-agenda-start-on-weekday nil)
(baw/setcust 'org-reverse-note-order t)
(baw/setcust 'org-fast-tag-selection-single-key 'expert)
(baw/setcust 'org-agenda-custom-commands
             (list '("d" todo "DELEGATED" nil)
                   '("c" todo "DONE|DEFERRED|CANCELLED" nil)
                   '("w" todo "WAITING" nil)
                   '("W" agenda "" ((org-agenda-ndays 21)))
                   '("A" agenda ""
                     ((org-agenda-skip-function
                       (lambda nil
                         (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
                      (org-agenda-ndays 1)
                      (org-agenda-overriding-header "Today's Priority #A tasks: ")))
                   '("u" alltodo ""
                     ((org-agenda-skip-function
                       (lambda nil
                         (org-agenda-skip-entry-if 'scheduled 'deadline
                                                   'regexp "\n]+>")))
                      (org-agenda-overriding-header "Unscheduled TODO entries: ")))))

(baw/setcust 'org-capture-templates
             `(("n"
                "Notes" entry
                (file+headline ,(baw/agenda-file "notes.org") "Notes")
                "* %u %?")
               ("g"
                "Guile" entry
                (file+headline ,(baw/agenda-file "agenda-guile.org") "Tasks")
                "* TODO %?\n  %u")
               ("p"
                "Personal" entry
                (file+headline ,(baw/agenda-file "agenda-personal.org") "Tasks")
                "* TODO %?\n  %u")
               ("o"
                "Oblong" entry
                (file+headline ,(baw/agenda-file "agenda-oblong.org") "Tasks")
                "* TODO %?\n  %u")
               ("e"
                "Emacs" entry
                (file+headline ,(baw/agenda-file "agenda-emacs.org") "Tasks")
                "* TODO %?\n  %u")))

;;; org.el ends here
