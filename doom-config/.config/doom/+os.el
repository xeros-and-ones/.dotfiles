;;; ~/.doom.d/+os.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +os/read-apps ()
  "Applications collection used for `+shell--open-with' method.
To add executable: Idea -> Tools -> Create Command Line Launcher"
  (let ((shell-apps '("idea" "code -g" "pycharm" "clion")))
    (completing-read "Select Applications:" shell-apps)))

(defun get-filename-with-line-number ()
  (concat (concat (buffer-file-name) ":")
          (number-to-string (line-number-at-pos))))

(when IS-MAC
  (+macos--open-with reveal-in-finder nil default-directory)
  (+macos--open-with reveal-project-in-finder nil
                     (or (doom-project-root) default-directory))

  (+shell--open-with reveal-in-apps (+os/read-apps)
                     (string-join `("'" ,(get-filename-with-line-number) "'")))
  (+shell--open-with reveal-project-in-apps (+os/read-apps)
                     (or (doom-project-root) default-directory))

  (+macos--open-with reveal-in-typora "typora" buffer-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LINUX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when IS-LINUX
  (defvar linux-terminal (cond ((executable-find "kitty") "kitty")
                               ((executable-find "konsole") "konsole")))

  (defun linux-terminal-args (dir)
    (cond ((executable-find "kitty") (concat "--working-directory='" dir "'"))
          ((executable-find "konsole") (concat "--workdir='" dir "'"))))


  (defvar linux-finder (cond ((executable-find "xdg-open") "xdg-open")
                             ((executable-find "gvfs-open") "gvfs-open")))

  (+shell--open-with open-in-default-program linux-finder buffer-file-name)

  (+shell--open-with reveal-in-finder linux-finder default-directory)
  (+shell--open-with reveal-project-in-finder linux-finder
                     (or (doom-project-root) default-directory))

  (+shell--open-with reveal-in-apps (+os/read-apps)
                     (string-join `("'" ,(buffer-file-name) "'")))
  (+shell--open-with reveal-project-in-apps (+os/read-apps)
                     (or (doom-project-root) default-directory))

  (+shell--open-with reveal-in-terminal linux-terminal (linux-terminal-args default-directory))
  (+shell--open-with reveal-project-in-terminal linux-terminal
                     (linux-terminal-args (or (doom-project-root) default-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete to trash
(setq delete-by-moving-to-trash t)

;; using trash over rm
(when (executable-find "trash")
  (os--trash-setup))
