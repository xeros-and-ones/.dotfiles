;;; autoload/ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun toggle-display-line-numbers-type ()
  (interactive)
  (if display-line-numbers-type
      (setq display-line-numbers-type nil)
    (setq display-line-numbers-type t))
  (revert-buffer-no-confirm))

;; a function to toggle 'transparency'
;;;###autoload
(defun toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha-background))
         (active (if (numberp alpha) alpha (car alpha))))
    (set-frame-parameter
     nil 'alpha-background
     (if (eql active 100)
         85 100))))
