;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Mohamed Tarek"
      user-mail-address "mohamed96tarek@hotmail.com")
;;------------------------ fonts -----------------------------
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14 :weight 'semibold)
      doom-variable-pitch-font (font-spec :family "Rubik" :size 15 :weight 'medium)
      doom-symbol-font (font-spec :family "FiraCode Nerd Font" :size 14))

;; --------------------- Theme -----------------------------
(setq doom-theme 'doom-gruvbox)


;; --------------------- Options --------------------------
(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")
(setq native-comp-jit-compilation t)
(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq confirm-kill-emacs nil) ; Disable exit confirmation.


;; ------------------ Config ----------------------------
(after! lsp-ui-mode
  :init
  (setq lsp-ui-doc-mode 1))

(setq lsp-ui-sideline-update-mode 'line
      lsp-ui-sideline-delay 0.5
      lsp-ui-doc-show-with-cursor t
      lsp-ui-doc-show-with-mouse t)


;; ------------------ Mappings --------------------------
(map! :leader
      :desc "Open Treemacs" "e" #'treemacs)
