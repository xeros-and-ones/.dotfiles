;;; private/my/+ui.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 13 :weight 'semibold)
      doom-variable-pitch-font (font-spec :family "Rubik" :size 15 :weight 'medium)
      doom-symbol-font (font-spec :family "FiraCode Nerd Font" :size 13))

(setq display-line-numbers-type 'relative)

(setq-default fill-column 120
              delete-trailing-lines t)

;; (set-frame-parameter nil 'alpha-background 80)
;; (add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq doom-theme 'doom-gruvbox)

(setq evil-emacs-state-cursor `(box ,(doom-color 'violet)))

(setq doom-modeline-modal-icon nil
      evil-normal-state-tag   (propertize "NORMAL")
      evil-emacs-state-tag    (propertize "EMACS" )
      evil-insert-state-tag   (propertize "INSERT")
      evil-motion-state-tag   (propertize "MOTION")
      evil-visual-state-tag   (propertize "VISUAL")
      evil-operator-state-tag (propertize "OPERATOR"))


;; Update window divider in terminal
;; https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
(unless (display-graphic-p)
  (setq evil-insert-state-cursor 'box)
  (defun my-change-window-divider ()
    (ignore-errors
      (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│)
        ;; (set-window-display-table (selected-window) display-table)
        )))
  (add-hook 'window-configuration-change-hook #'my-change-window-divider))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-major-mode-icon t
        ;; My mac vsplit screen won't fit
        doom-modeline-window-width-limit (- fill-column 10)))

(setq +workspaces-on-switch-project-behavior t)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; Faces need to postpone renderring
;; custom-set-faces! doesn't work properly when you switch doom themes
(custom-theme-set-faces! doom-theme
  `(hl-line :background ,(doom-color 'bg-alt)) ; sometimes ranger doesn't show hl-line color
  `(doom-modeline-debug-visual :background ,(doom-blend 'red 'base0 0.3))
  `(mode-line :background ,(doom-blend 'dark-blue 'base0  0.6))
  `(mode-line-inactive :background ,(doom-color 'bg-alt))
  `(vertical-border :foreground ,(doom-color 'bg-alt))
  '(font-lock-doc-face :italic t)
  '(font-lock-comment-face :italic t)
  '(font-lock-builtin-face :italic t)
  '(font-lock-type-face :italic t)
  `(show-paren-match :background ,(doom-blend 'teal 'base0 0.6) :foreground ,(doom-color 'base1))
  '(tide-hl-identifier-face :inherit 'lsp-face-highlight-read)
  `(breakpoint-enabled :background ,(doom-color 'red) :foreground ,(doom-lighten (doom-color 'red) 0.5))
  `(dap-ui-pending-breakpoint-face :background ,(doom-color 'red) :foreground "white")
  `(dap-ui-verified-breakpoint-face :background ,(doom-blend 'red 'base0 0.2))
  `(lsp-ui-peek-highlight :foreground ,(doom-color 'blue))
  `(ivy-posframe-border :background ,(doom-color 'blue))
  `(magit-diff-file-heading :background ,(doom-blend 'blue 'base0 0.2))
  `(magit-diff-file-heading-highlight :background ,(doom-blend 'blue 'base0 0.5))
  '(markdown-header-face-1 :inherit 'org-level-1)
  '(markdown-header-face-2 :inherit 'org-level-2)
  '(markdown-header-face-3 :inherit 'org-level-3)
  `(web-mode-jsx-depth-1-face :background ,(doom-blend 'teal 'base0 0.1))
  `(web-mode-jsx-depth-2-face :background ,(doom-blend 'teal 'base0 0.2))
  `(web-mode-jsx-depth-3-face :background ,(doom-blend 'teal 'base0 0.3))
  `(web-mode-jsx-depth-4-face :background ,(doom-blend 'teal 'base0 0.4))
  `(web-mode-jsx-depth-5-face :background ,(doom-blend 'teal 'base0 0.5))
  `(flyspell-incorrect :underline ,(doom-color 'red))
  `(flyspell-duplicate :underline ,(doom-color 'orange))
  `(flymake-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-error :underline (:style wave :color ,(doom-color 'red)))
  `(flycheck-info :underline (:style wave :color ,(doom-color 'green)))
  `(ein:cell-input-area :background ,(doom-blend 'red 'base0 0.15))
  `(ein:cell-input-prompt :background ,(doom-color 'red) :foreground ,(doom-color 'base0) :bold t)
  `(font-lock-comment-face :foreground ,(doom-color 'blue))
  `(font-lock-doc-face :foreground ,(doom-color 'blue)))

(custom-theme-set-faces! 'doom-gruvbox
  ;; ediff
  `(ediff-current-diff-A :foreground ,(doom-color 'red)   :background ,(doom-blend 'red 'base0 0.2))
  `(ediff-current-diff-B :foreground ,(doom-color 'green) :background ,(doom-blend 'green 'base0 0.2))
  `(ediff-current-diff-C :foreground ,(doom-color 'blue)  :background ,(doom-blend 'blue 'base0 0.2))
  `(ediff-current-diff-Ancestor :foreground ,(doom-color 'teal)  :background ,(doom-blend 'teal 'base0 0.2))
  )

;; for terminal
(unless (display-graphic-p)
  (custom-set-faces!
    `(mode-line-inactive :background ,(doom-darken (doom-color 'bg-alt) 0.05) :foreground ,(doom-color 'fg))))

;; (when IS-MAC
;;   ;; enable ligatures support
;;   ;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;   (ignore-errors
;;     (mac-auto-operator-composition-mode)))


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
                      " " filename))))

(use-package! all-the-icons-ibuffer
  :after ibuffer
  :init (all-the-icons-ibuffer-mode 1)
  )

(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))

(after! centered-window
  (setq cwm-centered-window-width 160))
