;;; org.el ---  Setup Org environment
(require 'cl-seq)

;; Set the file association
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Set our global keymap
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; Future work I really need two modes, one for writing and one for reading.
;;; The reading mode should use olivetti and should change the colors and hide
;;; more symbols and markup.  The writing mode should be full-width and should
;;; not hide the markup.


;; Look and Feel
;; TODO (baw): this should probably be a buffer var
;;   (add-hook 'emacs-lisp-mode-hook
;;            (lambda ()
;;              (push '("<=" . ?≤) prettify-symbols-alist)))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "«")
                                       ("#+END_SRC" . "»")
                                       ("#+begin_src" . "«")
                                       ("#+end_src" . "»")))
                                       ;; (">=" . "≥")
                                       ;; ("=>" . "⇨")))
;; This exposes the symbol when we're in it
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook (prettify-symbols-mode))

;; Just display the emphasis, not the markup
(setq org-hide-emphasis-markers t)

;; Use indentation with nested levels
(setq org-startup-indented t
      org-src-tab-acts-natively t)

;; Font wrangling
(add-hook 'org-mode-hook (variable-pitch-mode 1)) ; Default to proportional font

(unless (eq (baw/which-x-toolkit) 'athena)
  (let* ((headline-font (baw/find-first-available-font '("Go Sans"
                                                         "Inter UI"
                                                         "Inter"
                                                         "Noto Sans"
                                                         "Helvetica Neue"
                                                         "Helvetica")))
         (variable-tuple `(:font ,headline-font))
         (base-color (face-foreground 'default nil 'default))
         (headline`(:inherit default :weight bold :foreground ,base-color)))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0
                                          :underline nil)))))

    (custom-theme-set-faces
     'user
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-document-info ((t (:foreground "dark orange"))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-link ((t (:foreground "royal blue" :underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))))




;; Other
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
