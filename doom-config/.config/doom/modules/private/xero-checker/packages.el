;; -*- no-byte-compile: t; -*-
;;; private/xero-checker/packages.el


(when (modulep! :checkers syntax)
  (package! flycheck-google-cpplint
    :recipe
    (:host github :repo "flycheck/flycheck-google-cpplint")))
