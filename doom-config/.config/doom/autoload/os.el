;;; autoload/os.el -*- lexical-binding: t; -*-

;;;###autoload
(defun async-shell-command-no-window (command)
  "Requisite Documentation"
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command nil nil)))

;;;###autoload
(defadvice async-shell-command-no-window (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

;;;###autoload
(defun display-which-function ()
  (interactive)
  (message (which-function)))


;;;###autoload
(defun +shell-open-with (&optional app-name args container app-window-name)
  "Open shell application."
  (interactive)
  (let* ((process-connection-type nil))
    (if (string= "" app-window-name) (setq app-window-name app-name))
    (if container
        (setq command (format "docker exec --user user %s %s %s" container app-name args))
      (setq command (format "%s %s" app-name args)))
    (async-shell-command-no-window command)
    (message command)
    (when IS-LINUX
      (shell-command (concat "wmctrl -a \"" app-window-name "\" ")))))

;;;###autoload
(defun notify-current-line-number ()
  (alert (concat "line number " (number-to-string (line-number-at-pos))) :severity 'low))

;;;###autoload
(defmacro +shell--open-with (id &optional app args)
  `(defun ,(intern (format "+shell/%s" id)) ()
     (interactive)
     (+shell-open-with ,app ,args)
     (notify-current-line-number)))

;;;###autoload
(defmacro +docker--open-with (id &optional app args container app-window-name)
  `(defun ,(intern (format "+docker/%s" id)) ()
     (interactive)
     (+shell-open-with ,app ,args ,container ,app-window-name)
     (notify-current-line-number)))
