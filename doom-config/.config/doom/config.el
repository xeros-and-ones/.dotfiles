;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
(load! "modules/+ui.el")
(load! "modules/+misc.el")
(load! "modules/+magit.el")
(load! "modules/+org.el")
(load! "modules/+keys.el")
(load! "modules/+drag-stuff.el")
(load! "modules/+debugger.el")
(load! "modules/+lsp.el")
(load! "modules/+python.el")
(load! "modules/+tree-sitter.el")

;; Basic identity and performance
(setq user-full-name "Mohamed Tarek"
      user-mail-address "m96tarek@gmail.com"
      doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil
      delete-trailing-lines t)

;; Editing behavior
(delete-selection-mode 1)  ;; Delete selection when pasting
(setq-default fill-column 120
              delete-by-moving-to-trash t
              delete-trailing-lines t)

;; Security/annoyance reduction
(advice-add 'risky-local-variable-p :override #'ignore)
(custom-set-variables
 '(warning-suppress-log-types '((lsp-mode) (iedit))))

(custom-set-variables
 '(warning-suppress-log-types '((lsp-mode) (iedit)))
 '(warning-suppress-types '((iedit))))


;; Treemacs behavior
(after! treemacs (treemacs-follow-mode 1))


(setq which-key-use-C-h-commands t)

(add-to-list 'auto-mode-alist '("\\.inl\\'" . +cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'web-mode-engines-alist '("\\.jinja2\\'" . "django"))
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-indentation t))


(setq markdown-fontify-code-blocks-natively t)

(remove-hook 'text-mode-hook #'auto-fill-mode)


(after! markdown-mode
  (advice-add #'markdown-follow-thing-at-point :around #'doom-set-jump-a))

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


(setq! compilation-read-command t)
