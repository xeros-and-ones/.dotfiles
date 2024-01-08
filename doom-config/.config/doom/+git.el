;;; +git.el -*- lexical-binding: t; -*-

(after! magit
  (setq magit-save-repository-buffers nil
        git-commit-style-convention-checks nil
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk 'all
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  ;; Add git-credential-manager-core support
  (add-hook 'magit-process-prompt-functions
            'magit-process-git-credential-manager-core)

  ;; fix magit prompt for midway auth
  (appendq! magit-process-password-prompt-regexps '("Kerberos authentication failed.  Password:"))

  (magit-wip-after-apply-mode t)
  (magit-wip-before-change-mode t))


(after! forge
  (push '("git.dummy.com" "git.dummy.com/api/v3" "git.dummy.com" forge-github-repository)
        forge-alist)

  ;; TEMP
  (setq ghub-use-workaround-for-emacs-bug 'force)

  ;; Only show issues and pullreqs assigned to me. Toggle it off here.
  ;; (+xero/forge-toggle-all-issues-and-pullreqs)
  )


(after! git-link
  (setq git-link-open-in-browser nil
        git-link-use-commit t)

  ;; For some company still uses http git server
  (add-to-list 'git-link-remote-alist
               '("git\\.dummy\\.com" git-link-github-http))
  (add-to-list 'git-link-commit-remote-alist
               '("git\\.dummy\\.com" git-link-commit-github-http))

  ;; OVERRIDE
  (advice-add #'git-link--select-remote :override #'git-link--read-remote))


(after! magit-todos
  (setq magit-todos-exclude-globs '("third-party/*" "third_party/*")))


;; magit-todos uses hl-todo-keywords
(custom-theme-set-faces! doom-theme
  `(hl-todo :foreground ,(doom-color 'bg))
  `(magit-diff-file-heading :background ,(doom-blend 'blue 'base0 0.2))
  `(magit-diff-file-heading-highlight :background ,(doom-blend 'blue 'base0 0.5))
  ;; ediff
  `(ediff-current-diff-A :foreground ,(doom-color 'red)   :background ,(doom-blend 'red 'base0 0.2))
  `(ediff-current-diff-B :foreground ,(doom-color 'green) :background ,(doom-blend 'green 'base0 0.2))
  `(ediff-current-diff-C :foreground ,(doom-color 'blue)  :background ,(doom-blend 'blue 'base0 0.2))
  `(ediff-current-diff-Ancestor :foreground ,(doom-color 'teal)  :background ,(doom-blend 'teal 'base0 0.2)))


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
