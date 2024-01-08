;;; +os.el -*- lexical-binding: t; -*-

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
                   (linux-terminal-args (or (doom-project-root) default-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete to trash
(setq delete-by-moving-to-trash t)

;; using trash over rm
(when (executable-find "trash")
  (os--trash-setup))
