;;; ~/.doom.d/autoload/python.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Imports               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defun +python/copy-pytest-cmd ()
  "Copy pytest cmd."
  (interactive)
  (message (kill-new
            (concat "pytest "
                    (file-relative-name (buffer-file-name) (doom-project-root))
                    "::"
                    (which-function)
                    ))))

;;;###autoload
(defun +python/copy-unittest-cmd ()
  "Copy pytest cmd."
  (interactive)
  (message (kill-new
            (concat "python "
                    (file-relative-name (buffer-file-name) (doom-project-root))
                    " "
                    (which-function)
                    ))))

;;;###autoload
(defun +python/copy-python-cmd ()
  "Copy pytest cmd."
  (interactive)
  (message (kill-new
            (concat "python3 " (file-relative-name (buffer-file-name) (doom-project-root))))))

;;;###autoload
(defun +python/copy-pudb-python-cmd ()
  "Copy pytest cmd."
  (interactive)
  (message (kill-new
            (concat "python3 -m pudb.run "
                    (file-relative-name (buffer-file-name) (doom-project-root))))))

;;;###autoload
(defun +python/copy-pudb-pytest-cmd ()
  "Copy pytest cmd."
  (interactive)
  (message (kill-new
            (concat "pytest --pdbcls pudb.debugger:Debugger --pdb --capture=no "
                    (file-relative-name (buffer-file-name) (doom-project-root))
                    "::"
                    (which-function)
                    ))))

;; ;;;###autoload
;; (defun lsp-pylsp-locate-venv ()
;;   "Look for virtual environments local to the workspace."
;;   (interactive)
;;   (or lsp-pyright-python-executable-cmd
;;       (-when-let (venv-base-directory (locate-dominating-file default-directory "venv/"))
;;         (concat venv-base-directory "venv"))
;;       (-when-let (venv-base-directory (locate-dominating-file default-directory ".venv/"))
;;         (concat venv-base-directory ".venv"))))

;; ;;;###autoload
;; (defun get-python-path ()
;;   "Look for python executable cmd to the workspace."
;;   (interactive)
;;   (or (executable-find (f-expand "bin/python" (lsp-pylsp-locate-venv)))
;;       (with-no-warnings
;;         (executable-find lsp-pyright-python-executable-cmd))))

;;;###autoload
(defun get-python-path ()
  "Get the path of Python executable."
  (interactive)
  (let ((venv-path (getenv "VIRTUAL_ENV"))
        (py-path nil))
    ;; Decide which python executable to use for mypy
    (if venv-path
        (setq py-path (concat venv-path "/bin/python"))
      (progn
        (setq py-path (executable-find "python3"))
        (unless py-path
          (setq py-path (executable-find "python"))
          (unless py-path
            (setq py-path "python")))))
    (message "Python path: %s" py-path)
    py-path))
