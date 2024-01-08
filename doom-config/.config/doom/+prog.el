;;; +prog.el -*- lexical-binding: t; -*-

;;
;;
;;
;; 'graphql' config
(use-package! graphql-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode)))
;;
;;
;;
;;
(use-package! format-all
  :hook (emacs-lisp-mode . format-all-mode)
  :defer t)
;;
;;
;;
(add-hook! '(web-mode-hook html-mode-hook) (setq-local format-all-formatters '(("HTML" prettier))))
(add-hook! 'typescript-mode-hook (setq-local format-all-formatters '(("TypeScript" prettier))))
(add-hook! 'rjsx-mode-hook (setq-local format-all-formatters '(("JavaScript" prettier))))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))
;;
;;
;;
;;
;; 'bazel' config
(use-package! bazel-mode
  :defer t)
;;
;;
;;
;; add .inc & .inl to c++ objc-mode
(add-to-list 'auto-mode-alist '("\\.inl\\'" . +cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))
;;
;;
;;
;; 'company' completion config
(after! company
  ;; (setq company-idle-delay 0.2)
  (setq company-format-margin-function #'company-detect-icons-margin))
;;
;;
;;
;; 'which-func' config
(use-package! which-func
  :defer t
  :commands which-function)
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! projectile
  (setq compilation-read-command nil)   ; no prompt in projectile-compile-project
  ;; . -> Build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s"
                                    :compile "cmake --build build --config Debug --target all -j 14 --"
                                    :test "ctest -j14 -C Debug -T test --output-on-failure")

  ;; set projectile-known-projects after magit
  ;; (after! magit
  ;;   (update-projectile-known-projects))
  )
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++")
  (when IS-LINUX
    (quickrun-set-default "c++" "c++/g++")))



(when (modulep! :tools debugger)
  (defun +xero/dap-start ()
    (interactive)
    (dap-mode 1)
    (call-interactively #'dap-debug))

  (defun +xero/dap-delete-output-and-stderr-buffers ()
    (doom/kill-matching-buffers " stderr*" (buffer-list))
    (doom/kill-matching-buffers " out*" (buffer-list)))

  ;; (add-hook! dap-mode-hook ((tooltip-mode 1)))

  (after! dap-mode
    ;; (setq dap-auto-configure-features '(sessions locals expressions controls tooltip))
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
         (:prefix ("b" . "breakpoint")
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
