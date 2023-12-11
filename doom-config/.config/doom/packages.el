;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; disabled packages
(disable-packages! solaire-mode
                   osx-trash
                   realgud
                   realgud-trepan-ni
                   ccls
                   tide
                   swiper
                   forge
                   code-review
                   anaconda-mode
                   company-anaconda
                   lsp-python-ms
                   pyimport)

(package! adoc-mode)
(package! tldr)
(package! edit-indirect)
(package! link-hint)
(package! symbol-overlay)
(package! pomm)
(package! org-appear)

(package! format-all)
(package! keycast)
(package! evil-string-inflection)
(package! all-the-icons-ibuffer)
(package! dired-narrow)
(package! git-link)
(package! magit-delta)
(package! citre)
(package! imenu-list)
(package! go-translate)
;; (package! org-roam-ui)

;; programming
(package! bazel-mode :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
(package! graphql-mode)
(package! protobuf-mode)
(package! gn-mode)
(when (modulep! :tools lsp +eglot)
  (package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb"))
  (package! eglot-java)
  )
(when (not (modulep! :tools lsp +eglot))
  (package! lsp-docker))
