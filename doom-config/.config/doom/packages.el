;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; disabled packages
(disable-packages! osx-trash
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
;;
;;
;;
;; adding our desired 'packages' here
(package! git-link)
(package! all-the-icons-ibuffer)
(package! tldr)
(package! edit-indirect)
(package! link-hint)
(package! symbol-overlay)

(package! keycast)
(package! evil-string-inflection)
(package! all-the-icons-ibuffer)
(package! dired-narrow)

(package! imenu-list)
(package! format-all)

(package! pomm)
(package! org-appear)

(package! citre)
(package! bazel-mode
  :recipe
  (:host github :repo "bazelbuild/emacs-bazel-mode"))

(package! graphql-mode)

(package! breadcrumb
  :recipe
  (:host github :repo "joaotavora/breadcrumb"))
