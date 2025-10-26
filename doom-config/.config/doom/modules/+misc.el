;;; +misc.el -*- lexical-binding: t; -*-
;;;
;;; Core Settings
;;; Browser Configuration
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (cond
       ((executable-find "thorium-browser") "thorium-browser")
       ((executable-find "firefox") "firefox")
       ((executable-find "floorp") "floorp")
       ((executable-find "google-chrome-stable") "google-chrome-stable")
       ((executable-find "google-chrome") "google-chrome")
       ((executable-find "/usr/bin/google-chrome-stable") "/usr/bin/google-chrome-stable")))

;;; Navigation & Editing
(setq evil-cross-lines t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-ex-substitute-global t
      evil-escape-key-sequence "jk"
      evil-escape-delay 0.1)

(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))

(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer))

(when (modulep! :completion vertico)
  ;; Fix jump issue for vertico, https://github.com/hlissner/doom-emacs/issues/5386
  (dolist (func '(+default/search-project))
    (advice-add func :around #'doom-set-jump-a)))

;;; Project Management
(after! projectile
  (setq compilation-read-command nil)
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s"
                                    :compile "cmake --build build --config Debug --target all -j 14 --"
                                    :test "ctest -j14 -C Debug -T test --output-on-failure"))

;;; Terminal & Shell
(after! eshell
  ;; eshell-mode imenu index
  (add-hook! 'eshell-mode-hook (setq-local imenu-generic-expression '(("Prompt" " λ \\(.*\\)" 1))))

  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
  (defun eshell/ft (&optional arg) (treemacs arg))

  (defun eshell/up (&optional pattern)
    (let ((p (locate-dominating-file
              (f-parent default-directory)
              (lambda (p)
                (if pattern
                    (string-match-p pattern (f-base p))
                  t)))
             ))
      (eshell/pushd p))))

(after! term
  (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1))))

;;; Tools & Utilities
(after! imenu-list
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t)))

  ;; Optional keybindings
  (map! :leader
        :prefix "t"
        "i" #'imenu-list-smart-toggle))

(after! quickrun
  (when (featurep :system 'linux)
    (quickrun-set-default "c++" "c++/g++")))

(after! citre
  (require 'citre-config)
  (setq citre-project-root-function #'projectile-project-root
        citre-default-create-tags-file-location 'package-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (map! :nv "gE" #'citre-peek
        :nv "ge" #'citre-jump
        :nv "gp" #'citre-jump-back
        (:after citre-peek
                (:map citre-peek-keymap
                      "M-j" #'citre-peek-next-definition
                      "M-k" #'citre-peek-prev-definition
                      "M-S-j" #'citre-peek-next-line
                      "M-S-k" #'citre-peek-prev-line
                      "M-l" #'citre-peek-jump))
        :leader
        (:prefix "c"
         :desc "Citre update tags file" "t" #'citre-update-this-tags-file
         :desc "Citre edit tags file" "T" #'citre-edit-tags-file-recipe))

  (advice-add #'citre-jump :around #'doom-set-jump-a)
  (remove-hook 'citre-after-jump-hook #'citre-recenter-and-blink)
  (add-hook 'citre-after-jump-hook #'+nav-flash-blink-cursor-maybe-h))


;;; Hooks
(add-hook! 'better-jumper-post-jump-hook #'recenter)
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'highlight-indent-guides-mode)
