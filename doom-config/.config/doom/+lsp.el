;;; +lsp.el -*- lexical-binding: t; -*-

;;
;;
;;
;;
;; C++/c
(after! lsp-clangd
  (setq lsp-clients-clangd-args '("-j=3"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2)
  (after! dap-mode
    (require 'dap-codelldb)))

;;
;;
;;
;; Typescript
(setq lsp-clients-typescript-init-opts
      '(:importModuleSpecifierPreference "relative"))



;;
;;
;;
;; lsp-ui & lsp-mode
;; Use format-all by default
(setq +format-with-lsp nil)

(setq +lsp-prompt-to-install-server 'quiet)

(after! lsp-mode
  (setq lsp-log-io nil
        lsp-file-watch-threshold 4000
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable t
        lsp-headerline-breadcrumb-segments '(file symbols)
        lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method Enum Interface
                                       Function Variable Constant Struct Event Operator TypeParameter)
        )
  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]build\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories))
  )

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-lens-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100))
