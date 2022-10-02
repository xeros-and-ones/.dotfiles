;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Mohamed Tarek"
      user-mail-address "mohamed96tarek@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "FiraCode NerdFont" :size 15 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 15 :weight 'semi-bold))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(add-hook! 'org-mode-hook #'solaire-mode)
(setq mixed-pitch-variable-pitch-cursor nil)



;;Keybinds
(map! :i "M-y" #'yank-pop)
(map! :i "M-Y" #'consult-yank-pop)
(map! :n "SPC e" #'neotree-toggle)

;;theme config
;; (set-frame-parameter nil 'alpha-background 70) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 70)) ; For all new frames henceforth

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; (defun load-doom-gruvbox-theme (frame)
;;   (select-frame frame)
;;   (load-theme doom-theme t))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions #'load-doom-theme)
;;   (load-theme doom-theme t))

;; modeline config
(after! doom-modeline
  (setq doom-modeline-bar-width 4
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-modal-icon nil
        evil-normal-state-tag   (propertize "NORMAL")
        evil-emacs-state-tag    (propertize "EMACS")
        evil-insert-state-tag   (propertize "INSERT")
        evil-motion-state-tag   (propertize "MOTION")
        evil-visual-state-tag   (propertize "VISUAL")
        evil-replace-state-tag   (propertize "REPLACE")
        evil-operator-state-tag (propertize "OPERATOR")))


;;lsp-mode config
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-ui-sideline-enable t)
(setq lsp-lens-enable t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-signature-auto-activate t)
(setq lsp-signature-render-documentation t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)

;; Dashboard config
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("██╗░░██╗███████╗██████╗░░█████╗░"
            "╚██╗██╔╝██╔════╝██╔══██╗██╔══██╗"
            "░╚███╔╝░█████╗░░██████╔╝██║░░██║"
            "░██╔██╗░██╔══╝░░██╔══██╗██║░░██║"
            "██╔╝╚██╗███████╗██║░░██║╚█████╔╝"
            "╚═╝░░╚═╝╚══════╝╚═╝░░╚═╝░╚════╝░"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))
