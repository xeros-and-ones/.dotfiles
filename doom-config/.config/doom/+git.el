;;; +git.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(after! forge
  ;; (push '("git.dummy.com" "git.dummy.com/api/v3" "git.dummy.com" forge-github-repository)
  ;;       forge-alist)

  ;; TEMP
  ;; (setq ghub-use-workaround-for-emacs-bug 'force)

  ;; Only show issues and pullreqs assigned to me. Toggle it off here.
  ;; (+my/forge-toggle-all-issues-and-pullreqs)
  )

(use-package! magit-delta
  :after magit
  :init
  (when (executable-find "delta")
    (add-hook! magit-mode #'magit-delta-mode))
  :config
  (setq magit-delta-default-light-theme "GitHub")
  )

(after! magit-todos
  (setq magit-todos-exclude-globs '("third-party/*" "third_party/*")))


;; magit-todos uses hl-todo-keywords
(custom-theme-set-faces! doom-theme
  `(hl-todo :foreground ,(doom-color 'bg)))
(after! hl-todo
  (setq hl-todo-color-background t
        hl-todo-keyword-faces
        `(("TODO"  . ,(doom-color 'orange))
          ("HACK"  . ,(doom-color 'orange))
          ("TEMP"  . ,(doom-color 'orange))
          ("DONE"  . ,(doom-color 'green))
          ("NOTE"  . ,(doom-color 'green))
          ("DONT"  . ,(doom-color 'red))
          ("DEBUG"  . ,(doom-color 'red))
          ("FAIL"  . ,(doom-color 'red))
          ("FIXME" . ,(doom-color 'red))
          ("XXX"   . ,(doom-color 'blue))
          ("XXXX"  . ,(doom-color 'blue)))))
