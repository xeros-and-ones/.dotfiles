;;; +compilation-mode.el -*- lexical-binding: t; -*-

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'compile-command)
;;                  (format "python3 %s" (file-name-nondirectory buffer-file-name)))))

;; (add-hook 'python-ts-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'compile-command)
;;                  (format "python3 %s" (file-name-nondirectory buffer-file-name)))))

;; (add-hook 'java-ts-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'compile-command)
;;                  (format "javac %s && java %s" (file-name-nondirectory buffer-file-name) (file-name-nondirectory buffer-file-name)))))

;; (defun java-compile-interactive ()
;;   "Interactive compilation with separate compile and run phases."
;;   (interactive)
;;   (let* ((src-file (file-name-nondirectory buffer-file-name))
;;          (class-name (file-name-base buffer-file-name))
;;          (compile-cmd (format "javac %s" src-file))
;;          (run-args (read-from-minibuffer "Run arguments (leave empty to skip execution): "))
;;          (full-cmd (if (string-empty-p run-args)
;;                        compile-cmd
;;                      (format "%s && java %s %s" compile-cmd class-name run-args))))
;;     (set (make-local-variable 'compile-command)
;;          (format full-cmd)
;;          )))

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<leader>cc") 'java-compile-interactive)))
