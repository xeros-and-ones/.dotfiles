;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;;
;;
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;;
;;
;;
;;
;; Importing custom config files
(load! "+ui")
(load! "+git")
(load! "+prog")
(load! "+text")
(load! "+os")
(load! "+lsp")
(load! "+keys")
;;
;;
;;
;;
;;
;; doom config
(setq user-full-name "Mohamed Tarek"
      user-mail-address "m96tarek@gmail.com"
      native-comp-jit-compilation t
      doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)
;;
(setq! IS-LINUX t)
;;
;;
;;
;;
;; Delete the selection when pasting
(delete-selection-mode 1)
;;
;;
;;
(setq +workspaces-on-switch-project-behavior t)
;;
;;
;;
(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
;;
;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)
;;
;;
;;
;;
(add-hook! 'find-file-hook #'+xero/find-file-check-make-large-file-read-only-hook)
;;
;;
;;
(setq clipetty-tmux-ssh-tty "tmux show-environment -g SSH_TTY")
;;
;;
;;
;;
;; check minified-file
(add-to-list 'magic-mode-alist (cons #'+xero/check-minified-file 'fundamental-mode))
;;
;;
;; Manually edit .local/custom.el will break doom updates
(when (file-directory-p custom-file)
  (message (concat "Please delete " custom-file ". And customization in config.el and +ui.el.")))
;;
;;
;;
;;
(custom-set-variables
 '(warning-suppress-log-types '((lsp-mode) (iedit)))
 '(warning-suppress-types '((iedit))))
;;
;;
;;
;; 'evil-snipe' config
(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer)
  (push 'prodigy-mode evil-snipe-disabled-modes))
;;
;;
;;
;;
;; 'web-mode' config
(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'web-mode-engines-alist '("\\.jinja2\\'" . "django"))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-indentation t))
;;
;;
;;
;;
;;
;; Use chrome to browse
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (cond
       ;; https://github.com/ztlevi/dotty-config/blob/main/bin/launch-browser
       ((executable-find "launch-browser"  "launch-browser"))
       ((executable-find "google-chrome-stable") "google-chrome-stable")
       ((executable-find "/usr/bin/google-chrome-stable") "/usr/bin/google-chrome-stable")
       ((executable-find "google-chrome") "google-chrome")))
;;
;;
;;
;;
;;
(use-package! screenshot
  :defer t)
;;
;;
;;
;;
;;
(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project))
;;
;;
;;
;;
;;
(use-package! keycast
  :defer t)
;;
;;
;;
;;
;;
(after! ssh-deploy
  (setq ssh-deploy-automatically-detect-remote-changes 1))
;;
;;
;;
;;
(setq evil-cross-lines t
      evil-split-window-below t
      evil-vsplit-window-right t
      ;; Implicit /g flag on evil ex substitution, because I less often want the
      ;; default behavior.
      evil-ex-substitute-global t)
;;
;;
;;
;;
(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))
;;
;;
(use-package! evil-string-inflection :after evil)
;;
;;
;;
;;
(use-package! imenu-list
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))
;;
;;
;;
;;
(add-hook! 'better-jumper-post-jump-hook #'recenter)
;;
;;
;;
;;
(after! nav-flash
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))
;;
;;
;;
;;
;;
;; Use ) key to toggle it
(after! dired
  ;; Rust version ls
  (when-let (eza (executable-find "eza"))
    (setq insert-directory-program eza)
    (setq dired-listing-switches (string-join (list "-ahl" "--group-directories-first") " ")))
  )
;;
;;
;;
;;
;;
(after! ranger
  (setq ranger-hide-cursor t
        ranger-show-hidden 'format
        ranger-deer-show-details nil)

  (defun ranger-copy-relative-path ()
    "Copy the current file path relative to `default-directory path."
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively 'dired-copy-filename-as-kill)))

  (defun ranger-close-and-kill-inactive-buffers ()
    "ranger close current buffer and kill inactive ranger buffers"
    (interactive)
    (ranger-close)
    (ranger-kill-buffers-without-window))
  ;; do not kill buffer if exists in windows
  (defun ranger-disable ()
    "Interactively disable ranger-mode."
    (interactive)
    (ranger-revert)))
;;
;;
;;
;; config & binding for 'tmux-pane'
;;
;;
;; (use-package! tmux-pane
;;   :unless (display-graphic-p)
;;   :defer t
;;   :config
;;   (defvar my-tmux-pane-mode-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "C-t k")
;;         (lambda () (interactive) (tmux-pane--windmove "up"  "-U")))
;;       (define-key map (kbd "C-t j")
;;         (lambda () (interactive) (tmux-pane--windmove "down"  "-D")))
;;       (define-key map (kbd "C-t h")
;;         (lambda () (interactive) (tmux-pane--windmove "left" "-L")))
;;       (define-key map (kbd "C-t l")
;;         (lambda () (interactive) (tmux-pane--windmove "right" "-R")))
;;       (define-key map (kbd "C-t C-k")
;;         (lambda () (interactive) (tmux-pane--windmove "up"  "-U")))
;;       (define-key map (kbd "C-t C-j")
;;         (lambda () (interactive) (tmux-pane--windmove "down"  "-D")))
;;       (define-key map (kbd "C-t C-h")
;;         (lambda () (interactive) (tmux-pane--windmove "left" "-L")))
;;       (define-key map (kbd "C-t C-l")
;;         (lambda () (interactive) (tmux-pane--windmove "right" "-R")))
;;       map))
;;
;;   (define-minor-mode my-tmux-pane-mode
;;     "Seamlessly navigate between tmux pane and emacs window"
;;     :init-value nil
;;     :global t
;;     :keymap 'my-tmux-pane-mode-map)
;;
;;   :hook (after-init . my-tmux-pane-mode))

;;
;;
;;
;;
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CITRE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! citre
  :defer t
  :init
  (require 'citre-config)
  ;; `doom-large-file-size-alist' controls when controls when Doom activates so-long-minor-mode
  ;; to cut down on features in large files
  ;; (setq large-file-warning-threshold nil)
  (map!
   :nv "gE"  #'citre-peek
   :nv "ge"  #'citre-jump
   :nv "gp"  #'citre-jump-back
   (:after citre-peek
           (:map citre-peek-keymap
                 "M-j" #'citre-peek-next-definition
                 "M-k" #'citre-peek-prev-definition
                 "M-S-j" #'citre-peek-next-line
                 "M-S-k" #'citre-peek-prev-line
                 "M-l" #'citre-peek-jump))
   :leader
   (:prefix "c"
    :desc "Citre update tags file" "t" #'citre-update-this-tags-file
    :desc "Citre edit tags file" "T" #'citre-edit-tags-file-recipe))
  :config
  ;; better jump set marker
  (advice-add #'citre-jump :around #'doom-set-jump-a)

  (remove-hook! 'citre-after-jump-hook #'citre-recenter-and-blink)
  (add-hook 'citre-after-jump-hook #'+nav-flash-blink-cursor-maybe-h)
  (setq
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'package-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   ;; citre-auto-enable-citre-mode-modes '(prog-mode)
   ))
;;
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (modulep! :completion vertico)
  ;; Fix jump issue for vertico, https://github.com/hlissner/doom-emacs/issues/5386
  (dolist (func '(+default/search-project))
    (advice-add func :around #'doom-set-jump-a)))

(when (modulep! :completion ivy)
  (after! (:and ivy ivy-prescient)
    (setq ivy-prescient-retain-classic-highlighting t))

  (after! ivy-posframe
    (setq ivy-posframe-border-width 5)

    ;; Use minibuffer to display ivy functions
    (dolist (fn '(+ivy/switch-workspace-buffer
                  ivy-switch-buffer))
      (setf (alist-get fn ivy-posframe-display-functions-alist) #'ivy-display-function-fallback)))

  (after! ivy-rich
    (plist-put! ivy-rich-display-transformers-list
                'ivy-switch-buffer
                '(:columns
                  ((ivy-switch-buffer-transformer (:width 60))
                   (ivy-rich-switch-buffer-size (:width 7))
                   (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                   (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                   (ivy-rich-switch-buffer-project (:width 15 :face success))
                   (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                  :predicate
                  (lambda (cand) (get-buffer cand)))))

  (after! counsel
    ;; counsel-rg-base-command is configurable
    (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
          counsel-describe-function-function 'helpful-callable
          counsel-describe-variable-function 'helpful-variable)))
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRODIGY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! prodigy
  (set-evil-initial-state!
    '(prodigy-mode)
    'normal))
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! eshell
  ;; eshell-mode imenu index
  (add-hook! 'eshell-mode-hook (setq-local imenu-generic-expression '(("Prompt" " λ \\(.*\\)" 1))))

  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
  (defun eshell/ft (&optional arg) (treemacs arg))

  (defun eshell/up (&optional pattern)
    (let ((p (locate-dominating-file
              (f-parent default-directory)
              (lambda (p)
                (if pattern
                    (string-match-p pattern (f-base p))
                  t)))
             ))
      (eshell/pushd p)))
  )


(after! term
  ;; term-mode imenu index
  (add-hook! 'term-mode-hook (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1)))))
