;;; autoload/git.el -*- lexical-binding: t; -*-


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

(defun magit-blame-get-hash ()
  "Code copied from magit-blame-copy-hash"
  (oref (magit-current-blame-chunk) orig-rev))

;;;###autoload
(defun magit-blame--git-link-commit (arg)
  "Git link commit go to current line's magit blame's hash"
  (interactive "P")
  (require 'git-link)
  (cl-letf (((symbol-function 'word-at-point)
             (symbol-function 'magit-blame-get-hash)))
    (let ((git-link-open-in-browser (not arg)))
      (git-link-commit (git-link--read-remote)))))


(defvar forge-show-all-issues-and-pullreqs t
  "If nil, only show issues and pullreqs assigned to me.")

;; refresh magit-status buffer
(magit-refresh))
