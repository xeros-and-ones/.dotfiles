;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +orderless +icons +dabbrev)  ; Modern in-buffer completion
       (vertico +icons)                    ; Better minibuffer completion

       :ui
       doom
       doom-dashboard
       hl-todo
       indent-guides
       ligatures
       modeline
       ophints
       (popup +defaults)
       tabs
       (treemacs +lsp)
       (vc-gutter +pretty)
       vi-tilde-fringe
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       rotate-text
       snippets

       :emacs
       (dired +icons)
       electric
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +everywhere +childframe)

       :tools
       (debugger +lsp)
       (docker +lsp)
       editorconfig
       (eval +overlay)
       lookup
       (lsp +peek)
       magit
       make
       pdf
       (terraform +lsp)
       tree-sitter

       :os
       (:if (featurep :system 'macos) macos)
       tty

       :lang
       (cc +lsp +tree-sitter)
       data
       emacs-lisp
       (go +lsp +tree-sitter)
       (graphql +lsp)
       (json +lsp +tree-sitter)
       (java +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (kotlin +lsp)
       (lua +lsp +tree-sitter)
       (markdown +grip)
       (nix +tree-sitter)
       (org +pretty)
       (python +lsp +tree-sitter +pyright)
       (rust +lsp +tree-sitter)
       (scala +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (yaml +lsp +tree-sitter)

       :config
       (default +bindings +smartparens))

(setq custom-file (expand-file-name "custom.el" doom-local-dir))
(load custom-file 'no-error 'no-message)

(add-to-list 'default-frame-alist '(alpha-background . 85)) ; For all new frames henceforth
(add-to-list 'default-frame-alist '(fullscreen . maximized))
