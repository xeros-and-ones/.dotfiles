;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+os")
(load! "+git")
(load! "+misc")
(load! "+text")
(load! "+prog")
(load! "+ui")
(load! "+keys")
(cond
  ((modulep! :tools lsp +eglot) (load! "+eglot"))
  ((modulep! :tools lsp) (load! "+lsp")))

(setq user-full-name "Mohamed Tarek"
      user-mail-address "mohamed96tarek@hotmail.com")

(setq org-directory "~/org/")

(setq native-comp-jit-compilation t)

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)

;; Delete the selection when pasting
(delete-selection-mode 1)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

(add-hook! 'find-file-hook #'+my/find-file-check-make-large-file-read-only-hook)

(setq clipetty-tmux-ssh-tty "tmux show-environment -g SSH_TTY")

;; check minified-file
(add-to-list 'magic-mode-alist (cons #'+my/check-minified-file 'fundamental-mode))

(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ("^\\*keycast.*" :size 50 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("^\\*grep\\*$" :size 0.35)
                    ("^\\*pytest\\*" :size 0.35)
                    ("^\\*Anaconda\\*$" :size 0.35)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)
                    ))

;; Manually edit .local/custom.el will break doom updates
(when (file-directory-p custom-file)
  (message (concat "Please delete " custom-file ". And customization in config.el and +ui.el.")))

(custom-set-variables
 '(warning-suppress-log-types '((lsp-mode) (iedit)))
 '(warning-suppress-types '((iedit))))

(setq lsp-pylsp-plugins-black-enabled nil)
(setq lsp-pylsp-plugins-flake8-enabled nil)
(setq lsp-pylsp-plugins-isort-enabled nil)
(setq lsp-pylsp-plugins-autopep8-enabled nil)
(setq lsp-pylsp-plugins-yapf-enabled nil)
(setq lsp-pylsp-plugins-ruff-enabled t)
(setq lsp-pylsp-plugins-ruff-lineLength 100)
(setq lsp-pylsp-plugins-ruff-format "I")
(setq lsp-pylsp-plugins-pyflakes-enabled nil)
(setq lsp-pylsp-plugins-pycodestyle-enabled nil)
(setq lsp-pylsp-plugins-pydocstyle-enabled nil)
(setq lsp-pylsp-plugins-mccabe-enabled nil)
(setq lsp-pylsp-plugins-mypy-enabled nil)

(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'web-mode-engines-alist '("\\.jinja2\\'" . "django"))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-indentation t))

(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer)
  (push 'prodigy-mode evil-snipe-disabled-modes))
