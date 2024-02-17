;; -*- no-byte-compile: t; -*-
;;; packages.el

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

;; text
(package! tldr)
(package! edit-indirect)
(package! link-hint)
(package! symbol-overlay)
(package! pomm)
(package! org-appear)

;; misc
(package! keycast)
(package! evil-string-inflection)
(package! all-the-icons-ibuffer)
(package! dired-narrow)
(package! git-link)
(package! citre)
(package! imenu-list)
;; (package! tmux-pane)
(package! go-translate)

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
