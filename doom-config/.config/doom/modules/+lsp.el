;;; +lsp.el -*- lexical-binding: t; -*-

;;; +lsp.el -*- lexical-binding: t; -*-

;; ====================
;; Core LSP Configuration
;; ====================
(after! lsp-mode
  ;; General Settings
  (setq lsp-log-io nil
        +format-with-lsp nil
        +lsp-prompt-to-install-server 'quiet
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable t
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method
                                       Enum Interface Function Variable Constant
                                       Struct Event Operator TypeParameter))

  ;; Ignored directories
  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]build\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories))


  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; ====================
;; Language-Specific Configurations
;; ====================

;; Clangd (C/C++)
(after! lsp-clangd
  (setq lsp-clients-clangd-args '("-j=3"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2)
  (after! dap-mode (require 'dap-codelldb)))

;; TypeScript
(setq lsp-clients-typescript-init-opts
      '(:importModuleSpecifierPreference "relative"))

;; Lua
(after! lsp-lua
  (setq lsp-lua-hint-enable t
        lsp-lua-diagnostics-globals ["vim"]
        lsp-lua-telemetry-enable nil
        lsp-lua-workspace-max-preload 10000
        lsp-lua-workspace-preload-file-size 1000))

;; Nix
(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
(use-package! lsp-nix
  :custom (lsp-nix-nil-formatter ["alejandra" "--quiet"]))
(use-package! nix-mode
  :custom (nix-nixfmt-bin "alejandra-the-quiet"))

;; JAVA
(after! lsp-java
  (setq lsp-java-vmargs
        `("-XX:+UseParallelGC"
          "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Xmx4G")
        lsp-enable-indentation nil
        lsp-java-completion-max-results 50
        lsp-java-progress-reports :disabled
        lsp-java-autobuild-enabled nil))

(after! java-mode
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil))

;; ====================
;; UI Configuration
;; ====================
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-lens-enable t))

(setq! +tree-sitter-hl-enabled-modes #'tree-sitter-hl-mode)
