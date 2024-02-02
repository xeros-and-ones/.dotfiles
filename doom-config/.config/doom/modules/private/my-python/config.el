;;; private/my-python/config.el -*- lexical-binding: t; -*-

(map!
 (:after python
  :localleader
  :map python-mode-map
  (:prefix "t"
   :desc "Copy python breakpoint" "b" #'+python/copy-pdb-breakpoint-of-current-line
   :desc "Copy python cmd" "p" #'+python/copy-python-cmd
   :desc "Copy pytest cmd" "y" #'+python/copy-pytest-cmd
   :desc "Copy unittest cmd" "u" #'+python/copy-unittest-cmd)
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
        python-shell-interpreter "python3"
        pippel-python-command "python3")
  (when (file-exists-p! "~/.conda")
    (setq conda-env-home-directory (expand-file-name "~/.conda"))))

;; (after! lsp-pylsp
;;   ;; disable live-mode for mypy
;;   (lsp-register-custom-settings `(("pylsp.plugins.pylsp_mypy.enabled" t)))
;;   (lsp-register-custom-settings `(("pylsp.plugins.pylsp_mypy.live_mode" t)))
;;   (lsp-register-custom-settings `(("pylsp.plugins.pylsp_mypy.report_progress" nil)))
;;   (lsp-register-custom-settings `(("pylsp.plugins.pylsp_mypy.pythonExecutable" (get-python-path) t)))
;;   (lsp-register-custom-settings `(("pylsp.plugins.ruff.enabled" t)))
;;   (lsp-register-custom-settings `(("pylsp.plugins.ruff.lineLength" 100)))

;;   ;; ignore some linting info
;;   (setq lsp-pylsp-plugins-black-enabled nil
;;         lsp-pylsp-configuration-sources ["ruff"]
;;         lsp-pylsp-plugins-autopep8-enabled nil
;;         lsp-pylsp-plugins-yapf-enabled nil
;;         lsp-pylsp-plugins-pylint-enabled nil
;;         lsp-pylsp-plugins-pyflakes-enabled nil
;;         lsp-pylsp-plugins-pycodestyle-enabled nil
;;         lsp-pylsp-plugins-pydocstyle-enabled nil
;;         lsp-pylsp-plugins-mccabe-enabled nil
;;         lsp-pylsp-plugins-jedi-completion-enabled t
;;         lsp-pylsp-plugins-jedi-completion-fuzzy t
;;         lsp-pylsp-plugins-isort-enabled nil
;;         lsp-pylsp-plugins-rope-completion-enabled t
;;         lsp-pylsp-plugins-rope-autoimport-enabled nil))


(after! lsp-pyright
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-diagnostic-mode "workspace"
        lsp-pyright-python-executable-cmd "python3"))



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
