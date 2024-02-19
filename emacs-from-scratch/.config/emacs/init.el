;;; init.el --- Personal configuration file -*- lexical-binding: t; no-byte-compile: t; -*-
;; NOTE: init.el is now generated from readme.org.  Please edit that file instead

(defconst config-org (locate-user-emacs-file "README.org"))
(defconst config-el (locate-user-emacs-file "config.el"))

(unless (file-exists-p config-el)
(require 'org)
(org-babel-tangle-file config-org config-el))

(load-file config-el)
;;; init.el ends here
