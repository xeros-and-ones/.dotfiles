;;; private/my-python/config.el -*- lexical-binding: t; -*-

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


(map!
 (:after python
  :localleader
  :map python-mode-map
  (:prefix ("v" . "ENV")
           "c" #'conda-env-activate
           "C" #'conda-env-deactivate
           "v" #'poetry-venv-toggle
           "P" #'pyvenv-workon
           "p" #'pyvenv-activate))
 (:after pyenv-mode
         (:map pyenv-mode-map
               "C-c C-s" nil
               "C-c C-u" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(after! python
  (setq python-indent-offset 4
        python-shell-interpreter "python"
        pippel-python-command "python")
  (when (file-exists-p! "~/.conda")
    (setq conda-env-home-directory (expand-file-name "~/.conda"))))



(after! lsp-pyright
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-diagnostic-mode "workspace"
        lsp-pyright-python-executable-cmd "python3"
        lsp-pyright-typechecking-mode "basic"
        lsp-pyright-auto-search-paths t
        lsp-pyright-use-library-code-for-types t))



(after! pipenv
  (setq pipenv-with-projectile t)
  ;; Override pipenv--clean-response to trim color codes
  (defun pipenv--clean-response (response)
    "Clean up RESPONSE from shell command."
    (replace-regexp-in-string "\n\\[0m$" "" (s-chomp response)))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pipenv-activate pipenv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))


(after! conda
  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(conda-env-activate conda-env-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

(after! poetry
  (remove-hook 'python-mode-hook #'poetry-tracking-mode)
  (dolist (func '(poetry-venv-workon poetry-venv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

;; For pytest-mode
(set-evil-initial-state! '(comint-mode) 'normal)


(after! dap-python
  (setq dap-python-executable "python3"
        dap-python-debugger 'debugpy))
