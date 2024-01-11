;;; private/my-checker/autoload/checker.el -*- lexical-binding: t; -*-

;;;###autoload
(defun reset-flycheck (&rest _)
  (flycheck-mode -1)
  (flycheck-mode +1))

;;;###autoload
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))
