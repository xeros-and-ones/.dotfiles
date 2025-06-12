;;; +ui.el -*- lexical-binding: t; -*-

;;; +ui.el -*- lexical-binding: t; -*-

;; Theme and Font Configuration
(setq doom-theme 'doom-gruvbox
      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 16 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "CaskaydiaCove Nerd Font")
      doom-symbol-font (font-spec :family "CaskaydiaCove Nerd Font" :size 16 :weight 'bold)
      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 19)
      display-line-numbers-type 'relative
      +workspaces-on-switch-project-behavior t)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; Modeline Configuration
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit (- fill-column 10)
        doom-modeline-modal-icon nil
        evil-normal-state-tag   (propertize "NORMAL")
        evil-emacs-state-tag    (propertize "EMACS")
        evil-insert-state-tag   (propertize "INSERT")
        evil-motion-state-tag   (propertize "MOTION")
        evil-visual-state-tag   (propertize "VISUAL")
        evil-operator-state-tag (propertize "OPERATOR")))

;; Cursor and Evil UI
(setq evil-emacs-state-cursor `(box ,(doom-color 'violet)))
(unless (display-graphic-p)
  (setq evil-insert-state-cursor 'box)
  (defun my-change-window-divider ()
    (ignore-errors
      (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│))))
  (add-hook 'window-configuration-change-hook #'my-change-window-divider))

;; Custom Faces
(custom-theme-set-faces! doom-theme
  ;; Basic syntax highlighting
  `(font-lock-doc-face :italic t :foreground ,(doom-color 'blue))
  `(font-lock-comment-face :italic t :foreground ,(doom-color 'grey))
  '(font-lock-builtin-face :italic t)
  '(font-lock-type-face :italic t)
  '(font-lock-function-name-face :weight bold :slant italic)

  ;; UI elements
  `(doom-modeline-debug-visual :background ,(doom-blend 'red 'bg 0.3))
  `(mode-line :background ,(doom-blend 'dark-blue 'bg 0.6))
  `(mode-line-inactive :background ,(doom-color 'bg-alt))
  `(hl-line :background ,(doom-color 'bg-alt))
  `(vertical-border :foreground ,(doom-color 'bg-alt))
  `(show-paren-match :background ,(doom-blend 'teal 'bg 0.6) :foreground ,(doom-color 'base1))

  ;; Language-specific
  `(web-mode-jsx-depth-1-face :background ,(doom-blend 'teal 'bg 0.1))
  `(web-mode-jsx-depth-2-face :background ,(doom-blend 'teal 'bg 0.2))
  `(web-mode-jsx-depth-3-face :background ,(doom-blend 'teal 'bg 0.3))
  `(web-mode-jsx-depth-4-face :background ,(doom-blend 'teal 'bg 0.4))
  `(web-mode-jsx-depth-5-face :background ,(doom-blend 'teal 'bg 0.5))

  '(markdown-header-face-1 :inherit 'org-level-1)
  '(markdown-header-face-2 :inherit 'org-level-2)
  '(markdown-header-face-3 :inherit 'org-level-3)

  '(tide-hl-identifier-face :inherit 'lsp-face-highlight-read)

  ;; Corfu Completion
  `(corfu-current :background ,(doom-blend 'dark-blue 'bg 0.6) :foreground "ffffff" :weight bold)
  `(corfu-border :background ,(doom-blend 'dark-blue 'bg 0.6))

  ;; Diagnostics
  `(flyspell-incorrect :underline ,(doom-color 'red))
  `(flyspell-duplicate :underline ,(doom-color 'orange))
  `(flymake-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-error :underline (:style wave :color ,(doom-color 'red)))
  `(flycheck-info :underline (:style wave :color ,(doom-color 'green)))
  `(lsp-ui-peek-highlight :foreground ,(doom-color 'blue))

  ;; Debugging
  `(breakpoint-enabled :background ,(doom-color 'red) :foreground ,(doom-lighten (doom-color 'red) 0.5))
  `(dap-ui-pending-breakpoint-face :background ,(doom-color 'red) :foreground "white")
  `(dap-ui-verified-breakpoint-face :background ,(doom-blend 'red 'bg 0.2)))
(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; Terminal-specific settings
(unless (display-graphic-p)
  (custom-set-faces!
    `(mode-line-inactive :background ,(doom-darken (doom-color 'bg-alt) 0.05) :foreground ,(doom-color 'fg))))

;; Buffer Management
(after! ibuffer
  (setq-hook! 'ibuffer-hook ibuffer-formats
              '((mark modified read-only locked " "
                 (name 50 18 :left :elide)
                 " "
                 (size 9 -1 :right)
                 " "
                 (mode 16 16 :left :elide)
                 " " filename-and-process)
                (mark " "
                      (name 16 -1)
                      " " filename)))
  (after! all-the-icons-ibuffer
    (setq all-the-icons-ibuffer-icon-size 1.0
          all-the-icons-ibuffer-human-readable-size t)
    (all-the-icons-ibuffer-mode 1)))

;; Process Menu
(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID" 7 t)
                                     ("Status" 7 t)
                                     ("Buffer" 15 t)
                                     ("TTY" 12 t)
                                     ("Command" 0 t)]))

;; Window Management
(after! centered-window
  (setq cwm-centered-window-width 160))

;; Popup Rules
(set-popup-rules!
  '(("^\\*helpful" :size 0.35)
    ("^\\*Ibuffer\\*$" :size 0.35)
    ("^\\*info.*" :size 80 :side right)
    ("^\\*Man.*" :size 80 :side right)
    ("^\\*keycast.*" :size 50 :side right)
    ("^\\*Customize" :actions display-buffer)
    ("^\\*edit-indirect" :size 0.6)
    ("^\\*grep\\*$" :size 0.35)
    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)))
