;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; disabled packages
(disable-packages! osx-trash
                   realgud
                   realgud-trepan-ni
                   ccls
                   tide
                   ;; forge
                   code-review
                   anaconda-mode
                   company-anaconda
                   lsp-python-ms
                   pyimport)


(package! citre)
(package! imenu-list)
(package! all-the-icons-ibuffer)
(package! drag-stuff)
(package! vim-file-locals
  :recipe (:host github :repo "abougouffa/emacs-vim-file-locals"))
