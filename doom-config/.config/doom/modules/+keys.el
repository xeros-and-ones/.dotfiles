;;; +keys.el -*- lexical-binding: t; -*-

(defun my/fold-class-methods-and-return ()
  "Fold all class methods EXCEPT the current one."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (execute-kbd-macro (kbd "2 z m")))  ; Fold everything
  (execute-kbd-macro (kbd "z o"))
  (execute-kbd-macro (kbd "z z")))      ; Unfold at current position

;;; Terminal Key Fixes
;; Distinguish C-i from TAB (important for terminal users)
(when (display-graphic-p)
  (define-key input-decode-map "\C-i" [C-i])
  (map! "<C-i>" #'evil-jump-forward))

;;; Critical Overrides (take precedence over all other modes)
(map! :map override  ;; general-override-mode-map
      ;; Essential text operations
      "M-y"   #'+default/yank-pop
      "M-p"   (λ! (projectile-invalidate-cache nil) (projectile-find-file))
      "C-]"   #'yas-expand

      ;; Buffer Management
      "M-q" #'kill-current-buffer
      ;; fix OS window/frame navigation/manipulation keys
      "M-w" #'delete-window
      "M-W" #'delete-frame
      "M-n" #'+default/new-buffer
      "M-N" #'make-frame
      "C-M-f" #'toggle-frame-fullscreen

      ;; Navigation
      "C-'"   #'toggle-input-method
      "C-S-n" #'dap-next
      "<xterm-paste>" #'xterm-paste-with-delete-region)


;;; Core Keybindings
(map!
 ;; Basic Navigation
 :i "C-j" #'next-line
 :i "C-k" #'previous-line
 :gi "C-h" #'backward-char
 :gi "C-l" #'forward-char
 :gnmvi "C-e" #'doom/forward-to-last-non-comment-or-eol
 :gnmvi "C-a" #'doom/backward-to-bol-or-indent

 ;; Conflict with vertico
 :g "C-SPC" nil :g "C-@" nil
 :gn "C-t" nil

 ;; Window Management
 "C-S-j" #'evil-scroll-line-down
 "C-S-k" #'evil-scroll-line-up
 "C-S-h" #'+tabs:previous-or-goto
 "C-S-l" #'+tabs:next-or-goto
 "M-h"   #'evil-window-left
 "M-l"   #'evil-window-right
 "M-j"   #'evil-window-down
 "M-k"   #'evil-window-up
 "C-<left>" #'evil-window-increase-width
 "C-<right>" #'evil-window-decrease-width
 "C-<up>" #'evil-window-increase-height
 "C-<down>" #'evil-window-decrease-height


 ;; Text Editing
 "M-z" #'undo
 "M-Z" #'redo
 "M-c" #'evil-yank
 "M-v" #'evil-paste-after
 "M-s" #'evil-write-all
 "M-/" #'evilnc-comment-or-uncomment-lines
 "M-a" #'mark-whole-buffer


 ;; frame-local font scaling
 "M-0"   #'doom/reset-font-size
 "M-="   #'doom/increase-font-size
 "M--"   #'doom/decrease-font-size

 ;; Search
 "M-i" #'display-which-function
 "M-f" #'consult-line
 "C-s" #'consult-line


 ;; Macros
 "<M-m>" #'kmacro-call-macro)

;;; Leader Keybindings
(map! :leader

      :desc "Fold Close 2" "z" #'my/fold-class-methods-and-return

      ;; Code
      (:prefix "c"                      ; code
       :desc "Check grammar"          "g" #'langtool-check-buffer
       :desc "Done Check grammar"     "G" #'langtool-check-done
       (:when (not (modulep! :tools lsp +eglot))
         :desc "LSP organize imports"   "I" #'lsp-organize-imports
         :desc "LSP workspace restart"  "R" #'lsp-workspace-restart
         :desc "Treemacs references"    "D" #'lsp-treemacs-references))

      ;; File
      (:prefix "f"
               "n" #'+default/yank-filename
               "s" #'evil-write-all)

      ;; Git
      (:prefix "g"
               "w" #'magit-wip-log-worktree)

      (:prefix "h"                      ; help
               "C" #'helpful-command)

      (:prefix "o"                      ; open
       :desc "Kill ring"             "k" #'+default/yank-pop
       :desc "Imenu list"            "i" #'imenu-list
       :desc "Open link"             "x" #'link-hint-open-link
       :desc "Toggle Treemacs"       "p" #'+treemacs/toggle
       :desc "Open link at point"    "X" #'link-hint-open-link-at-point)

      ;; Toggle
      (:prefix "t"
               "c" #'rainbow-mode
               "l" #'toggle-display-line-numbers-type
               "C" #'centered-window-mode
               "d" #'toggle-debug-on-error
               "T" #'toggle-transparency
               "S" #'size-indication-mode
               "i" #'lsp-inlay-hints-mode
               "v" #'visual-line-mode)

      (:prefix "i"                      ; insert
               "v" #'add-dir-local-variable
               "o" #'symbol-overlay-put
               "q" #'symbol-overlay-remove-all)

      (:prefix "s"                      ; search
       :desc "Comments" "c" #'imenu-comments
       :desc "Search Project (hidden)" "h" #'+default/search-project-with-hidden-files)

      (:prefix "p"                      ; project
               "n" #'+default/yank-project-name)

      (:prefix "e"                      ;error
               "d" #'posframe-delete-all)

      (:prefix "TAB"
       :desc "Load worksapce from file" "L" #'+workspace/load
       :desc "Swap left"  "h" #'+workspace/swap-left
       :desc "Swap right" "l" #'+workspace/swap-right))

;;; Mode-Specific Keybindings

(after! markdown-mode
  (map! :map markdown-mode-map
        :nv [tab] #'markdown-cycle
        :localleader
        "ih" #'+my/markdown-highlight
        "F" #'+my/markdown-copy-fix))


;;; Package Configurations
(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer
        evil-snipe-override-evil-repeat-keys t)
  (push 'prodigy-mode evil-snipe-disabled-modes))


(map!
 (:prefix "C-x"
  :n "e"  #'pp-eval-last-sexp)
 (:prefix "C-c"
  :desc "Text properties at point" :nmv "f" (cmd! (message "%S" (text-properties-at (point))))))

(map!
 ;; ====================
 ;; MAJOR MODE BINDINGS
 ;; ====================

 ;; Programming Modes
 (:map prog-mode-map
  :i "TAB" #'doom/dumb-indent
  :i "<backtab>" #'doom/dumb-dedent)

 ;; Outline Mode
 (:after outline
  :map outline-mode-map
  :n "C-k" nil  ; Unbind
  :n "C-j" nil) ; Unbind

 ;; Elisp Mode
 (:after elisp-mode
  :map emacs-lisp-mode-map
  :n "gh" #'helpful-at-point)

 ;; Org Mode
 (:after org
         ;; Evil-Org
         (:after evil-org
          :map evil-org-mode-map
          :i "C-d" nil :i "C-t" nil :i "C-h" nil :i "C-k" nil) ; Unbinds

         (:map org-mode-map
          :localleader
          "z" #'org-add-note
          "L" #'org-toggle-link-display))

 ;; Markdown Mode
 (:after markdown-mode
         (:map evil-markdown-mode-map
          :i "C-d" nil) ; Unbind

         (:map markdown-mode-map
          :desc "Markdown Cycle" :nv [tab] #'markdown-cycle
          :desc "Insert item below" :ni "<C-return>" #'+org/insert-item-below
          :desc "Insert item above" :ni "<S-C-return>" #'+org/insert-item-above
          (:localleader
           "-" #'org-table-insert-hline
           "|" #'org-table-create-or-convert-from-region
           (:prefix ("i" . "Insert")
                    "r" #'markdown-table-insert-row
                    "c" #'markdown-table-insert-column))))


 ;; ====================
 ;; LSP/COMPLETION BINDINGS
 ;; ====================

 ;; LSP UI
 (:after lsp-ui
  (:map lsp-ui-mode-map
        "C-j" #'lsp-ui-doc-mode)

  (:map lsp-ui-peek-mode-map
        "h" #'lsp-ui-peek--select-prev-file
        "j" #'lsp-ui-peek--select-next
        "k" #'lsp-ui-peek--select-prev
        "l" #'lsp-ui-peek--select-next-file)

  :nv "gb" #'lsp-ui-peek-jump-backward)
 ;; Vertico (if enabled)
 (:when (modulep! :completion vertico)
   (:after vertico
    :map vertico-map
    "C-j" #'vertico-next
    "C-k" #'vertico-previous
    "C-M-RET" #'+vertico/embark-preview
    "C-n" #'vertico-next
    "C-M-j" #'+vertico/next-candidate-preview
    "C-S-n" #'vertico-next-group
    "C-p" #'vertico-previous
    "A-v" #'vertico-scroll-down
    "C-v" #'vertico-scroll-up
    "C-M-k" #'+vertico/previous-candidate-preview
    "C-S-p" #'vertico-previous-group
    )

   (:after minibuffer
    :map minibuffer-local-map
    "M-RET" #'vertico-exit-input))

 (:after yasnippet
  :map yas-keymap
  "TAB" nil  ; Ensure no conflict with doom/dumb-indent
  "<backtab>" nil)


 ;; ====================
 ;; UTILITY MODE BINDINGS
 ;; ====================

 ;; iEdit
 (:after iedit
  :map iedit-mode-occurrence-keymap
  "M-D" nil) ; Unbind

 ;; Edebug
 (:after edebug
  :map edebug-mode-map
  "c" #'edebug-go-mode)

 ;; Magit
 (:after magit-mode
  :map magit-mode-map
  "M-p" nil "M-n" nil "M-w" nil ; Unbinds
  :nv "$" #'magit-process-buffer
  "C-c r" #'code-review-forge-pr-at-point)

 (:after magit-diff
  :map magit-diff-mode-map
  "C-o" #'magit-diff-visit-file-other-window)

 (:after magit-blame
  :map magit-blame-mode-map
  :n "o" #'magit-blame--git-link-commit)

 ;; Terminals
 (:after vterm
  :map vterm-mode-map
  "M-e" nil ; Unbind
  "M-w" #'+workspace/close-window-or-workspace)


 ;; ====================
 ;; CORE FUNCTIONALITY
 ;; ====================

 ;; Evil Ex Completion
 (:after evil-vars
  :map evil-ex-completion-map
  "C-b" nil ; Unbind
  "C-k" #'kill-line
  "C-d" #'delete-forward-char)

 ;; Info Mode
 (:after evil-collection-info
  :map Info-mode-map
  "/" #'Info-search
  "?" #'Info-search-backward)

 ;; Minibuffer (global)
 (:after minibuffer
  :map minibuffer-local-map
  "C-t" #'marginalia-cycle
  "C-k" #'kill-line))
