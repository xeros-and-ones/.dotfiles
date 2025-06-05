;;; +debugger.el -*- lexical-binding: t; -*-


(when (modulep! :tools debugger)
  (defun +my/dap-start ()
    (interactive)
    (dap-mode 1)
    (call-interactively #'dap-debug))

  (defun +my/dap-delete-output-and-stderr-buffers ()
    (doom/kill-matching-buffers " stderr*" (buffer-list))
    (doom/kill-matching-buffers " out*" (buffer-list)))


  (after! dap-mode
    (setq dap-auto-configure-features '(sessions locals expressions controls tooltip))
    (setq lsp-enable-dap-auto-configure nil)

    ;; use M-u to exit dap-hydra
    (after! dap-hydra
      (defhydra+ dap-hydra () ("M-u" nil)))

    ;; Toggle dap-hydra whenever breakpoint is triggered
    ;; (add-hook 'dap-stopped-hook
    ;;           (lambda (arg) (call-interactively #'dap-hydra)))
    )

  (map! :leader
        (:prefix ("d" . "debug")
         :desc "Start debugger" "d" #'+my/dap-start
         :desc "Start last debugger" "D" #'dap-debug-last
         :desc "Remove DAP outpput buffers" "K" #'+my/dap-delete-output-and-stderr-buffers
         (:prefix-map ("b" . "breakpoint")
                      "b" #'dap-breakpoint-toggle
                      "c" #'dap-breakpoint-condition)
         "B" #'dap-ui-breakpoints
         "h" #'dap-hydra
         "r" #'dap-debug-restart
         "l" #'dap-ui-locals
         "e" #'dap-ui-expressions
         "a" #'dap-ui-expressions-add
         "R" #'dap-ui-expressions-remove
         "f" #'dap-switch-stack-frame
         "q" #'dap-disconnect
         "s" #'dap-ui-sessions
         "k" #'dap-delete-session
         "K" #'dap-delete-all-sessions
         "S" #'realgud-short-key-mode)))
