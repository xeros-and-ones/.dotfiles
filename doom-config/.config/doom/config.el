;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+keys")

(setq user-full-name "Mohamed Tarek"
      user-mail-address "mohamed96tarek@hotmail.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'semibold)
      doom-variable-pitch-font (font-spec :family "Rubik" :size 15 :weight 'medium)
      doom-symbol-font (font-spec :family "JetbrainsMono Nerd Font" :size 14))

;; --------------------- Theme -----------------------------
(setq doom-theme 'doom-gruvbox)


;; --------------------- Options --------------------------
(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")
(setq native-comp-jit-compilation t)
(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq confirm-kill-emacs nil) ; Disable exit confirmation.


;; ------------------ Config ----------------------------
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
(setq lsp-pylsp-plugins-mypy-enabled t)


(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'web-mode-engines-alist '("\\.jinja2\\'" . "django"))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-indentation t))

