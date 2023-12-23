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

;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")
