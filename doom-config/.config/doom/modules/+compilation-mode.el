;;; +compilation-mode.el -*- lexical-binding: t; -*-

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "python3 %s" (file-name-nondirectory buffer-file-name)))))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "python3 %s" (file-name-nondirectory buffer-file-name)))))

;; (add-hook 'java-ts-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'compile-command)
;;                  (format "javac %s && java %s" (file-name-nondirectory buffer-file-name) (file-name-nondirectory buffer-file-name)))))


(add-hook 'csharp-ts-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "dotnet run %s" (file-name-nondirectory buffer-file-name)))))

(add-hook 'csharp-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "dotnet run %s" (file-name-nondirectory buffer-file-name)))))
