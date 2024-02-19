;;; +ui.el -*- lexical-binding: t; -*-

;; setting the 'theme' and enabling 'bold' and 'italic'
(setq doom-theme 'doom-gruvbox
      doom-themes-enable-bold t
      doom-themes-enable-italic t)

(setq evil-emacs-state-cursor `(box ,(doom-color 'violet)))

;; setting 'fonts' for doom ui
(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 14 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "CaskaydiaCove Nerd Font") ; inherits `doom-font''s :size
      doom-symbol-font (font-spec :family "CaskaydiaCove Nerd Font" :size 14 :weight 'bold)
      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 19))


(setq +workspaces-on-switch-project-behavior t)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; enabling 'relative' line numbers
(setq display-line-numbers-type t)


;; 'fill-column' for line wraping
(setq-default fill-column 120)
;;
;;
;;
;; 'doom-modeline' config
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-major-mode-icon t
        ;; My mac vsplit screen won't fit
        doom-modeline-window-width-limit (- fill-column 10)))

(setq doom-modeline-modal-icon nil
      evil-normal-state-tag   (propertize "NORMAL")
      evil-emacs-state-tag    (propertize "EMACS" )
      evil-insert-state-tag   (propertize "INSERT")
      evil-motion-state-tag   (propertize "MOTION")
      evil-visual-state-tag   (propertize "VISUAL")
      evil-operator-state-tag (propertize "OPERATOR"))

(custom-theme-set-faces! doom-theme
  `(doom-modeline-debug-visual :background ,(doom-blend 'red 'bg 0.3))
  `(mode-line :background ,(doom-blend 'dark-blue 'bg  0.6))
  `(mode-line-inactive :background ,(doom-color 'bg-alt)))
;;
;;
;;
;; 'popup' rules
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
;;
;;
;;
;; 'evil' ui config
(setq evil-emacs-state-cursor `(box ,(doom-color 'violet)))

(unless (display-graphic-p)
  (setq evil-insert-state-cursor 'box)
  (defun my-change-window-divider ()
    (ignore-errors
      (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│)
        ;; (set-window-display-table (selected-window) display-table)
        )))
  (add-hook 'window-configuration-change-hook #'my-change-window-divider))
;;
;;
;;
;;---------------------------------------------------------------------------------------
;; setting custom 'faces'
;;---------------------------------------------------------------------------------------


(custom-theme-set-faces! doom-theme
  `(font-lock-doc-face :italic t :foreground ,(doom-color 'blue))
  `(font-lock-comment-face :italic t :foreground ,(doom-color 'blue))
  '(font-lock-builtin-face :italic t)
  '(font-lock-type-face :italic t)
  '(font-lock-function-name-face :weight bold :slant italic)
  `(web-mode-jsx-depth-1-face :background ,(doom-blend 'teal 'bg 0.1))
  `(web-mode-jsx-depth-2-face :background ,(doom-blend 'teal 'bg 0.2))
  `(web-mode-jsx-depth-3-face :background ,(doom-blend 'teal 'bg 0.3))
  `(web-mode-jsx-depth-4-face :background ,(doom-blend 'teal 'bg 0.4))
  `(web-mode-jsx-depth-5-face :background ,(doom-blend 'teal 'bg 0.5))
  `(flyspell-incorrect :underline ,(doom-color 'red))
  `(flyspell-duplicate :underline ,(doom-color 'orange))
  `(flymake-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-error :underline (:style wave :color ,(doom-color 'red)))
  `(flycheck-info :underline (:style wave :color ,(doom-color 'green)))
  '(markdown-header-face-1 :inherit 'org-level-1)
  '(markdown-header-face-2 :inherit 'org-level-2)
  '(markdown-header-face-3 :inherit 'org-level-3)
  `(breakpoint-enabled :background ,(doom-color 'red) :foreground ,(doom-lighten (doom-color 'red) 0.5))
  `(dap-ui-pending-breakpoint-face :background ,(doom-color 'red) :foreground "white")
  `(dap-ui-verified-breakpoint-face :background ,(doom-blend 'red 'bg 0.2))
  `(lsp-ui-peek-highlight :foreground ,(doom-color 'blue))
  `(hl-line :background ,(doom-color 'bg-alt)) ; sometimes ranger doesn't show hl-line color
  `(vertical-border :foreground ,(doom-color 'bg-alt))
  `(show-paren-match :background ,(doom-blend 'teal 'bg 0.6) :foreground ,(doom-color 'base1))
  '(tide-hl-identifier-face :inherit 'lsp-face-highlight-read)
  `(ivy-posframe-border :background ,(doom-color 'blue))
  `(ein:cell-input-area :background ,(doom-blend 'red 'bg 0.15))
  `(ein:cell-input-prompt :background ,(doom-color 'red) :foreground ,(doom-color 'bg) :bold t))


(defface breakpoint-enabled '((t)) "Breakpoint face.")

;;
;;
;;
;; for terminal
(unless (display-graphic-p)
  (custom-set-faces!
    `(mode-line-inactive :background ,(doom-darken (doom-color 'bg-alt) 0.05) :foreground ,(doom-color 'fg))))
;;----------------------------------------------------------------------------------
;;
;;
;;
;; 'ibuffer' config
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
;;
;;
;;
;; 'process-menu' ui
(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))
;;
;;
;;
;; 'centered-window'
(after! centered-window
  (setq cwm-centered-window-width 160))


;; (after! persp-mode
;;   (defun display-workspaces-in-minibuffer ()
;;     (with-current-buffer " *Minibuf-0*"
;;       (erase-buffer)
;;       (insert (+workspace--tabline))))
;;   (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;   (+workspace/display))

(setq which-key-use-C-h-commands 't)
