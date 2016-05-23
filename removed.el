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
