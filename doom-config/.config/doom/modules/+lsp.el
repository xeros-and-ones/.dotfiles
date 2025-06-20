;;; +lsp.el -*- lexical-binding: t; -*-

;; ====================
;; Core LSP Configuration
;; ====================
(after! lsp-mode
  ;; General Settings
  (setq lsp-log-io nil
        +format-with-lsp nil
        lsp-lens-mode t
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
    (push dir lsp-file-watch-ignored-directories)))

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
        `("-XX:GCTimeRatio=9"
          ;; G1GC
          "-XX:+UseG1GC"
          "-XX:MaxGCPauseMillis=200"
          ;; ZGC
          ;; "-XX:+UseZGC"
          ;; "-XX:ZAllocationSpikeTolerance=5.0"
          ;; Shenandoah
          ;; "-XX:+UseShenandoahGC"
          ;; "-XX:ShenandoahGCMode=iu"
          ;;
          "-XX:+HeapDumpOnOutOfMemoryError"
          "-XX:SoftRefLRUPolicyMSPerMB=50"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Djava.awt.headless=true"
          "-Xms1G"
          "-Xmx4G")
        lsp-java-completion-max-results 50
        lsp-java-progress-reports nil
        lsp-java-autobuild-enabled nil
        lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.46.1/jdt-language-server-1.46.1-202504011455.tar.gz"))

;; ====================
;; UI Configuration
;; ====================
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-include-signature t
        lsp-modeline-workspace-status-enable nil
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions nil))

