;;; private/xero-checker/config.el -*- lexical-binding: t; -*-

(if (modulep! :checkers syntax)
    (map! :leader
          (:prefix-map ("e" . "error")
           :desc "Next error"      "n" #'flycheck-next-error
           :desc "Previous error"  "p" #'flycheck-previous-error
           :desc "Explain error"   "e" #'flycheck-explain-error-at-point
           :desc "List errors"     "l" #'flycheck-list-errors
           :desc "Lsp list errors" "L" #'consult-lsp-diagnostics
           :desc "Verify setup"    "v" #'flycheck-verify-setup))
  (map! :leader
        (:prefix-map ("e" . "error")
         :desc "Next error"      "n" #'flymake-goto-next-error
         :desc "Previous error"  "p" #'flymake-goto-prev-error
         :desc "Explain error"   "e" #'flymake-show-diagnostic
         :desc "List errors"     "l" #'flymake-show-diagnostics-buffer
         :desc "Lsp list errors" "L" #'consult-flymake
         :desc "List project error" "P" #'flymake-show-project-diagnostics
         :desc "Verify setup"    "v" #'flymake-running-backends)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package! wucuo
;;   :defer t
;;   :init
;;   (add-hook! (js2-mode rjsx-mode go-mode c-mode c++-mode) #'wucuo-start))


(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  typescript-tslint javascript-jshint handlebars
                  eglot json-jsonlist json-python-json
                  c/c++-clang c/c++-cppcheck c/c++-gcc c/c++-googlelint
                  python-mypy python-pylint python-pycompile
                  ;; Disable Perl for Coral Config file
                  perl
                  ))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; ======================== JS & TS ========================
  ;; https://github.com/hlissner/doom-emacs/blob/c2f8476c8641fcc9a1371d873ed3b5924952a059/modules/lang/javascript/config.el#L109

  ;; ======================== CC ========================
  (require 'flycheck-google-cpplint)
  (setq flycheck-c/c++-googlelint-executable "cpplint")
  (flycheck-add-next-checker 'c/c++-gcc '(t . c/c++-googlelint))

  (setq flycheck-c/c++-gcc-executable "gcc-7"
        flycheck-gcc-include-path '("/usr/local/inclue"))

  (add-hook! c++-mode-hook
    (setq flycheck-gcc-language-standard "c++11"
          flycheck-clang-language-standard "c++11"))
  )

;; (defun disable-flycheck-mode ()
;;   (flycheck-mode -1))
;; (add-hook! (emacs-lisp-mode) 'disable-flycheck-mode)
