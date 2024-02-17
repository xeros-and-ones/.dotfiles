;;; +keys.el -*- lexical-binding: t; -*-


(when (featurep :system 'macos) (setq mac-command-modifier 'meta
                                      mac-option-modifier  'alt))

;; Distinguish C-i from TAB
(when (display-graphic-p)
  (define-key input-decode-map "\C-i" [C-i])
  (map! "<C-i>" #'evil-jump-forward))


(map!
 ;; overrides other minor mode keymaps (just for non-evil)
 (:map override ;; general-override-mode-map
       "M-q"   #'kill-current-buffer
       "M-p"   (λ! (projectile-invalidate-cache nil) (projectile-find-file))
       "M-y"   #'+default/yank-pop
       "C-]"   #'yas-expand
       "C-'"   #'toggle-input-method
       "<xterm-paste>" #'xterm-paste-with-delete-region
       "C-S-n" #'dap-next
       "C-M-h" #'drag-stuff-left
       "C-M-l" #'drag-stuff-right
       "C-M-j" #'drag-stuff-down
       "C-M-k" #'drag-stuff-up
       ;; window management
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
       "C-<down>" #'evil-window-decrease-height)

 ;; fix OS window/frame navigation/manipulation keys
 "M-w" #'delete-window
 "M-W" #'delete-frame
 "M-n" #'+default/new-buffer
 "M-N" #'make-frame
 "C-M-f" #'toggle-frame-fullscreen
 "M-t" #'transpose-words
 "M-i" #'display-which-function
 :nv "gD" #'xref-find-definitions-other-window
 :nv "gr" #'+lookup/references
 ;; Conflict with vertico
 :g "C-SPC" nil :g "C-@" nil
 :gn "C-t" nil
 ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
 ;; it imposes some other functionality and overhead we don't need)
 "M-z" #'undo
 "M-Z" #'redo
 "M-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "M-v" #'yank-with-delete-region
 "M-s" #'evil-write-all
 ;; frame-local font scaling
 "M-0"   #'doom/reset-font-size
 "M-="   #'doom/increase-font-size
 "M--"   #'doom/decrease-font-size
 ;; Conventional text-editing keys & motions
 "M-a" #'mark-whole-buffer
 :gni [M-RET]       #'+default/newline-below
 :gni [M-S-RET]     #'+default/newline-above
 :gi  [M-backspace] #'backward-kill-word
 ;; Searching
 (:when (modulep! :completion vertico)
   "M-f" #'consult-line
   "C-s" #'consult-line)
 (:when (modulep! :completion ivy)
   "M-f" #'swiper
   "C-s" #'swiper)
 "M-e"    #'persp-switch-to-buffer
 ;; Comment
 "M-/" (cmd! (save-excursion (comment-line 1)))
 :n "M-/" #'evilnc-comment-or-uncomment-lines
 :v "M-/" #'evilnc-comment-operator
 ;; Others
 :m [tab] nil
 "C-M-\\" #'indent-region-or-buffer
 "M-m"    #'kmacro-call-macro
 )

(if (display-graphic-p)
    (map!
     ;; M-[ does not work in terminal
     "M-[" #'better-jumper-jump-backward
     "M-]" #'better-jumper-jump-forward)
  (map!
   :g "<mouse-4>" #'evil-scroll-line-up
   :g "<mouse-5>" #'evil-scroll-line-down
   ))

(map!
 ;; Unix text-editing keys & motions
 :gi "C-j" #'next-line
 :gi "C-k" #'previous-line
 :gi "C-h" #'backward-char
 :gi "C-l" #'forward-char

 :v "C-r"   #'+my/evil-quick-replace
 :v "DEL" (kbd "\"_d")
 :v "<del>" (kbd "\"_d")
 :v "<backspace>" (kbd "\"_d")
 :nmv "-" #'better-jumper-jump-backward
 :nmv "=" #'better-jumper-jump-forward
 :gnmvi "C-e" #'doom/forward-to-last-non-comment-or-eol
 :gnmvi "C-a" #'doom/backward-to-bol-or-indent
 (:prefix "z"
  :n "o" #'ts-fold-open-recursively)
 (:prefix "C-x"
  :n "e"  #'pp-eval-last-sexp)
 (:prefix "C-c"
  :desc "Text properties at point" :nmv "f" (cmd! (message "%S" (text-properties-at (point))))))

;; leader/localleader is not compatible with :gnvmi
(map! :leader
      (:prefix-map ("a" . "app")
       "s" #'prodigy
       :desc "List process" "p" #'list-processes
       "x" #'align-regexp)
      (:prefix "b"                      ; buffer
               "m" #'list-bookmarks
               "h" #'+doom-dashboard/open
               "r" #'revert-buffer-no-confirm
               "R" #'reload-buffer-no-confirm)
      (:prefix "c"                      ; code
       :desc "Check grammar"          "g" #'langtool-check-buffer
       :desc "Done Check grammar"     "G" #'langtool-check-done
       (:when (modulep! :tools lsp +eglot)
         :desc "Eglot workspace restart"  "R" #'eglot-reconnect
         :desc "Eglot organize imports"   "I" #'eglot-code-action-organize-imports
         :desc "Eglot quickfix" "q" #'eglot-code-action-quickfix
         )
       (:when (not (modulep! :tools lsp +eglot))
         :desc "LSP organize imports"   "I" #'lsp-organize-imports
         :desc "LSP workspace restart"  "R" #'lsp-workspace-restart
         :desc "Treemacs references"    "D" #'lsp-treemacs-references))
      (:prefix "TAB"
       :desc "Switch workspace" "TAB" #'+workspace/switch-to
       :desc "Load worksapce from file" "L" #'+workspace/load
       :desc "Swap left"  "h" #'+workspace/swap-left
       :desc "Swap right" "l" #'+workspace/swap-right)
      (:prefix "f"                      ; file
       :desc "Yank filename" "n" #'+default/yank-filename
       :desc "Save all" "s" #'evil-write-all
       :desc "Deer"     "j" #'deer)
      (:prefix "n"                      ; notes
       :desc "Take screenshot" "p" #'screenshot)
      (:prefix "g"                      ; git
               (:after smerge-mode
                :desc "Smerge" "s" smerge-basic-map)
               :desc "Browse file or region" "oo" #'git-link
               :desc "Magit browse commit"   "oc" #'+vc/git-browse-commit
               :desc "Magit wip worktree"    "w"  #'magit-wip-log-worktree)
      (:prefix "h"                      ; help
               "C" #'helpful-command)
      (:prefix "o"                      ; open
       :desc "Kill ring"             "k" #'+default/yank-pop
       :desc "Imenu list"            "i" #'imenu-list
       :desc "Open link"             "x" #'link-hint-open-link
       :desc "Open link at point"    "X" #'link-hint-open-link-at-point
       :desc "Docker open apps"      ";" #'+docker/reveal-in-apps
       (:when (featurep :system 'macos)
         :desc "Reveal in default program"  "f" #'+macos/open-in-default-program
         :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
         :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
         :desc "Reveal in Terminal"         "t" #'+macos/reveal-in-terminal
         :desc "Reveal project in Terminal" "T" #'+macos/reveal-project-in-terminal
         :desc "Reveal file in Apps"        "," #'+shell/reveal-in-apps
         :desc "Reveal project in Apps"     "." #'+shell/reveal-project-in-apps)
       (:when (featurep :system 'linux)
         :desc "Reveal in default program"  "f" #'+shell/open-in-default-program
         :desc "Reveal in Finder"           "o" #'+shell/reveal-in-finder
         :desc "Reveal project in Finder"   "O" #'+shell/reveal-project-in-finder
         :desc "Reveal in Terminal"         "t" #'+shell/reveal-in-terminal
         :desc "Reveal project in Terminal" "T" #'+shell/reveal-project-in-terminal
         :desc "Reveal file in Apps"        "," #'+shell/reveal-in-apps
         :desc "Reveal project in Apps"     "." #'+shell/reveal-project-in-apps))
      (:prefix "i"                      ; insert
               "v" #'add-dir-local-variable
               "o" #'symbol-overlay-put
               "q" #'symbol-overlay-remove-all)
      (:prefix "p"                      ; project
               "n" #'+default/yank-project-name
               )
      (:prefix "e"                      ;error
               "d" #'posframe-delete-all)
      (:prefix "t"                      ; toggle
       :desc "Pomodoro timer" "t" #'pomm
       "c" #'rainbow-mode
       "C" #'centered-window-mode
       "d" #'toggle-debug-on-error
       "l" #'toggle-display-line-numbers-type
       "k" #'keycast-log-mode
       "T" #'toggle-transparency
       "S" #'size-indication-mode
       "i" #'highlight-indent-guides-mode
       "v" #'visual-line-mode)
      (:prefix-map ("j" . "jump")
                   "j" #'avy-goto-char-timer
                   "l" #'avy-goto-line
                   "b" #'avy-pop-mark
                   "t" #'yas-describe-tables)
      (:prefix "s"                      ; search
       :desc "Comments" "c" #'imenu-comments
       :desc "Search Project (hidden)" "h" #'+default/search-project-with-hidden-files))

(map!
 (:map prog-mode-map
  :i "TAB" #'doom/dumb-indent
  :i "<backtab>" #'doom/dumb-dedent)
 (:after ranger
         (:map ranger-normal-mode-map
               "M-1" nil "M-2" nil "M-3" nil "M-4" nil "M-5" nil "M-6" nil "M-7" nil "M-8" nil "M-9" nil "M-0" nil
               "g"   nil
               "q" #'ranger-close-and-kill-inactive-buffers
               "f" #'find-file
               "F" #'dired-narrow                 ; use `; g` to quit dired-narrow
               "M-g" #'ranger-go
               "yr" #'ranger-copy-relative-path
               "C-TAB" #'ranger-next-tab
               "C-S-TAB" #'ranger-prev-tab
               "U" #'dired-unmark-all-files
               "u" #'dired-unmark
               "(" #'dired-hide-details-mode
               "+" #'dired-create-directory))
 (:after outline
  :map outline-mode-map
  :n "C-k" nil
  :n "C-j" nil)
 (:after elisp-mode
  :map emacs-lisp-mode-map
  :n "gh" #'helpful-at-point)
 (:after lsp-ui
  :map lsp-ui-mode-map
  "C-j" #'lsp-ui-doc-mode)
 (:after lsp-ui-peek
  :map lsp-ui-peek-mode-map
  "h" #'lsp-ui-peek--select-prev-file
  "j" #'lsp-ui-peek--select-next
  "k" #'lsp-ui-peek--select-prev
  "l" #'lsp-ui-peek--select-next-file)
 :nv "gb" #'lsp-ui-peek-jump-backward
 (:after org
         (:map org-mode-map
          :localleader
          "z" #'org-add-note
          "L" #'org-toggle-link-display))
 (:after iedit
         (:map iedit-mode-occurrence-keymap
               "M-D" nil))
 (:after edebug
         (:map edebug-mode-map "c" #'edebug-go-mode))
 (:after evil-org
         (:map evil-org-mode-map
          :i "C-d" nil :i "C-t" nil :i "C-h" nil :i "C-k" nil))
 (:after markdown-mode
         (:map evil-markdown-mode-map
          :i "C-d" nil)
         (:map markdown-mode-map
          :desc "Markdown Cycle" :nv [tab] #'markdown-cycle
          :desc "Insert item below" :ni "<C-return>"  #'+org/insert-item-below
          :desc "Insert item above" :ni "<S-C-return>" #'+org/insert-item-above
          (:localleader
           :desc "highlight" "ih" #'+my/markdown-highlight
           :desc "Fix Copy"           "F" #'+my/markdown-copy-fix
           :desc "Insert header line" "-" #'org-table-insert-hline
           :desc "Crete Table from region" "|" #'org-table-create-or-convert-from-region
           :desc "Edit" "x" (+my/simulate-key "C-c C-s")
           (:prefix ("i" . "Insert")
                    "r" #'markdown-table-insert-row
                    "c" #'markdown-table-insert-column))))
 (:when (modulep! :completion vertico)
   (:after vertico
    :map vertico-map
    "C-j"   #'vertico-next
    "C-k"   #'vertico-previous
    "C-M-RET" #'+vertico/embark-preview
    "C-n"   #'vertico-next
    "C-M-j" #'+vertico/next-candidate-preview
    "C-S-n" #'vertico-next-group
    "C-p"   #'vertico-previous
    "A-v"   #'vertico-scroll-down
    "C-v"   #'vertico-scroll-up
    "C-M-k" #'+vertico/previous-candidate-preview
    "C-S-p" #'vertico-previous-group))
 (:when (modulep! :completion ivy)
   (:after wgrep
    :map wgrep-mode-map
    :nv "gr" #'ivy-occur-revert-buffer
    :n "RET" #'ivy-occur-press-and-switch)
   (:after ivy
    :map ivy-occur-grep-mode-map
    "<backspace>" #'ivy-occur-delete-candidate
    :nv "gr" #'ivy-occur-revert-buffer
    :map ivy-minibuffer-map
    "TAB" #'ivy-partial-or-done
    "<M-return>" #'ivy-immediate-done
    "C-b" nil
    "C-r" #'ivy-reverse-i-search      ; similar to ivy-restrict-to-matches
    "C-j" #'ivy-call-and-recenter
    "C-k" #'ivy-kill-line
    "C-v" #'ivy-scroll-up-command
    "A-v" #'ivy-scroll-down-command))
 (:after minibuffer
  :map minibuffer-local-map
  (:when (modulep! :completion vertico)
    "M-RET" #'vertico-exit-input)
  "C-t" #'marginalia-cycle
  "C-k" #'kill-line)
 (:after magit-mode
         (:map magit-mode-map "M-p" nil "M-n" nil "M-w" nil
          :nv "$" #'magit-process-buffer
          "C-c r" #'code-review-forge-pr-at-point))
 (:after magit-diff
         (:map magit-diff-mode-map            ; for magit diff/rev mode
               "C-o" #'magit-diff-visit-file-other-window))
 (:after magit-blame
         (:map magit-blame-mode-map
          :n "o" #'magit-blame--git-link-commit))
 (:after evil-vars
  :map evil-ex-completion-map
  "C-b" nil
  "C-k" #'kill-line
  "C-d" #'delete-forward-char)
 (:after evil-collection-info
  :map Info-mode-map
  "/" #'Info-search
  "?" #'Info-search-backward)
 (:after company
         (:map company-active-map
               "TAB" nil [tab] nil [backtab] nil
               "C-j" #'company-select-next
               "C-k" #'company-select-previous
               "C-SPC" #'company-complete-selection))
 (:after vterm
         (:map vterm-mode-map
               "M-e" nil
               "M-w" #'+workspace/close-window-or-workspace))
 (:after term
         (:map term-raw-map
          :i "M-v" #'term-paste)))
