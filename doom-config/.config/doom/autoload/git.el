;;; $DOOMDIR/autoload/git.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vc/git-browse-commit (arg)
  "Open the website for the current version controlled file. Fallback to
repository root."
  (interactive "P")
  (require 'git-link)
  (let ((git-link-open-in-browser (not arg)))
    (git-link-commit (git-link--select-remote))))

;;;###autoload
(defun git-link-github-http (hostname dirname filename branch commit start end)
  (format "http://%s/%s/blob/%s/%s"
          hostname
          dirname
          (or branch commit)
          (concat filename
                  (when start
                    (concat "#"
                            (if end
                                (format "L%s-%s" start end)
                              (format "L%s" start)))))))

;;;###autoload
(defun git-link-commit-github-http (hostname dirname commit)
  (format "http://%s/%s/commit/%s"
          hostname
          dirname
          commit))

;;;###autoload
(defun magit-blame--git-link-commit (arg)
  "Git link commit go to current line's magit blame's hash"
  (interactive "P")
  (require 'git-link)
  (cl-letf (((symbol-function 'word-at-point)
             (symbol-function 'magit-blame-copy-hash)))
    (let ((git-link-open-in-browser (not arg)))
      (git-link-commit (git-link--read-remote)))))


(defvar forge-show-all-issues-and-pullreqs t
  "If nil, only show issues and pullreqs assigned to me.")

;;;###autoload
(defun +my/forge-toggle-all-issues-and-pullreqs ()
  "Toggle the forge section which only shows the issues and
pullreqs assigned to me."
  (interactive)
  (setq forge-insert-default '(forge-insert-pullreqs forge-insert-issues))
  (setq forge-insert-assigned '(forge-insert-assigned-pullreqs forge-insert-assigned-issues))
  (if forge-show-all-issues-and-pullreqs
      (progn
        (setq forge-show-all-issues-and-pullreqs nil)
        (remove-hook! 'magit-status-sections-hook #'forge-insert-issues nil t)
        (remove-hook! 'magit-status-sections-hook #'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t))
    (progn
      (setq forge-show-all-issues-and-pullreqs t)
      (remove-hook! 'magit-status-sections-hook #'forge-insert-assigned-issues nil t)
      (remove-hook! 'magit-status-sections-hook #'forge-insert-assigned-pullreqs nil t)
      (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
      (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues nil t)))

  ;; refresh magit-status buffer
  (magit-refresh))
