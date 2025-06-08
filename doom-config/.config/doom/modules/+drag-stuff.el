;;; +drag-stuff.el -*- lexical-binding: t; -*-

(use-package! drag-stuff
  :defer t  ; Important for Doom's lazy loading
  :init
  (drag-stuff-global-mode 1)
  :config
  ;; Exception modes
  (dolist (mode '(term-mode vterm-mode eshell-mode pdf-view-mode))
    (add-to-list 'drag-stuff-except-modes mode))

  ;; Settings
  (setq drag-stuff-modify-undone nil
        drag-stuff-delay 0.02
        drag-stuff-highlight t)

  ;; Safer keybindings (these work in both terminal and GUI)
  (map! :n "M-C-h"  #'drag-stuff-left
        :n "M-C-l" #'drag-stuff-right
        :n "M-C-j"  #'drag-stuff-down
        :n "M-C-k"    #'drag-stuff-up))
