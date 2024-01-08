;;; autoload/misc.el -*- lexical-binding: t; -*-

;;; private/my/autoload/misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun yank-with-delete-region ()
  (interactive)
  (when (evil-visual-state-p)
    (delete-region (region-beginning) (region-end)))
  (yank))

;;;###autoload
(defun xterm-paste-with-delete-region (event)
  (interactive "e")
  (when (evil-visual-state-p)
    (delete-region (region-beginning) (region-end)))
  (xterm-paste event))

;;;###autoload
(defun doom/toggle-comment-region-or-line ()
  "Comments or uncomments the whole region or if no region is
selected, then the current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(define-inline +xero/prefix-M-x (prefix)
  (inline-quote
   (lambda () (interactive)
     (setq unread-command-events (string-to-list ,prefix))
     (call-interactively #'execute-extended-command))))

;;;###autoload
(define-inline +xero/simulate-key (key)
  (inline-quote
   (lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key))))))


;;;###autoload
(defun +default/yank-project-name ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (require 'f)
  (message "Copied project name to clipboard: %s"
           (kill-new (f-filename (doom-project-root)))))

;;;###autoload
(defun +default/search-project-with-hidden-files ()
  (interactive)
  (let ((counsel-rg-base-command "rg -zS --no-heading --line-number --color never --hidden %s . ")
        (consult-ripgrep-command (concat  "rg --null --line-buffered --color=ansi --max-columns=1000 "
                                          "--hidden --no-heading --line-number . -e ARG OPTS")))
    (+default/search-project)))

;;;###autoload
(defun +default/search-workspace (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
         (if arg
             (read-directory-name "Search Workspace: ")
           (expand-file-name (concat (doom-project-root) "/..")))))
    (call-interactively
     (cond ((modulep! :completion vertico) #'+vertico/project-search-from-cwd)
           ((modulep! :completion ivy)  #'+ivy/project-search-from-cwd)
           ((modulep! :completion helm) #'+helm/project-search-from-cwd)
           (#'rgrep)))))


;; ;;;###autoload
;; (defvar +xero/repo-root-list '("~" "~/Code" "~/Builds" "~/.config")
;;   "personal repo root to scan git projects")

;; ;;;###autoload
;; (defun update-projectile-known-projects ()
;;   (interactive)
;;   (require 'magit)
;;   (setq magit-repository-directories '(("~/Code" . 3)))
;;   ;; Workplace folder has some permission error on macos
;;   (let ((workspace-dir "~/Code/"))
;;     (when (file-directory-p workspace-dir)
;;       (dolist (dir (directory-files workspace-dir t))
;;         (when (and
;;                (file-readable-p dir)
;;                (not (string-equal ".." (substring dir -2)))
;;                (not (string-equal "." (substring dir -1))))
;;           (appendq! magit-repository-directories `((,dir . 2)))))))
;;   (let (magit-repos
;;         magit-abs-repos
;;         (home (expand-file-name "~")))
;;     ;; append magit repos at root with depth 1
;;     (dolist (root +xero/repo-root-list)
;;       (setq magit-abs-repos (append magit-abs-repos (magit-list-repos-1 root 1))))
;;     (setq magit-abs-repos (append magit-abs-repos (magit-list-repos)))

;;     ;; convert abs path to relative path (HOME)
;;     (dolist (repo magit-abs-repos)
;;       (push (concat "~/" (file-relative-name repo "~")) magit-repos))
;;     (setq projectile-known-projects magit-repos)))

;;;###autoload
(defun +xero/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (replace-regexp-in-string "/" "\\/" (regexp-quote (buffer-substring-no-properties beg end)) t t)))
      (setq command-string (format "1,$s /%s/%s/g" selection selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

;;;###autoload
(defun +xero/markdown-copy-fix ()
  (interactive)
  (let ((case-fold-search nil))
    (dolist (pair '(("<pre.*>" . "```python")
                    ("<\/pre>" . "```")
                    ("^> " . "")
                    ("^>" . "")
                    ("\\[<svg.*</svg>\\]([^)]*)" . "")
                    ("\\\\\\*" . "*")
                    ("\\\\\\#" . "#")
                    ))
      (goto-char (point-min))
      ;; if you need regexp, use search-forward-regexp
      (while (re-search-forward (car pair) nil t)
        (replace-match (cdr pair))))))


;;;###autoload
(defun imenu-comments ()
  "Imenu display comments."
  (interactive)
  (require 'evil-nerd-commenter)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (cond ((modulep! :completion vertico)   (consult-imenu))
          ((modulep! :completion ivy)       (counsel-imenu)))))


;; if the first line is too long, enable fundamental by default
;;;###autoload
(defun get-nth-line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
        (- (line-end-position)
           (line-beginning-position)))))

;;;###autoload
(defun +xero/check-minified-file ()
  (and
   (not (when (buffer-file-name)
          (member (file-name-extension (buffer-file-name))
                  '("org" "md" "markdown" "txt" "rtf"))))
   (cl-loop for i from 1 to (min 30 (count-lines (point-min) (point-max)))
            if (> (get-nth-line-length i) 1000)
            return t
            finally return nil)))


;;;###autoload
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;;; Scratch frame
(defvar +xero--scratch-frame nil)

(defun cleanup-scratch-frame (frame)
  (when (eq frame +xero--scratch-frame)
    (with-selected-frame frame
      (setq doom-fallback-buffer-name (frame-parameter frame 'old-fallback-buffer))
      (remove-hook 'delete-frame-functions #'cleanup-scratch-frame))))

;;;###autoload
(defun open-scratch-frame (&optional fn)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (let* ((frame-title-format "")
         (preframe (cl-loop for frame in (frame-list)
                            if (equal (frame-parameter frame 'name) "scratch")
                            return frame))
         (frame (unless preframe
                  (make-frame `((name . "scratch")
                                (width . 120)
                                (height . 24)
                                (transient . t)
                                (internal-border-width . 10)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (undecorated . t)
                                ,(if IS-LINUX '(display . ":0")))))))
    (setq +xero--scratch-frame (or frame posframe))
    (select-frame-set-input-focus +xero--scratch-frame)
    (when frame
      (with-selected-frame frame
        (if fn
            (call-interactively fn)
          (with-current-buffer (switch-to-buffer "*scratch*")
            ;; (text-scale-set 2)
            (when (eq major-mode 'fundamental-mode)
              (emacs-lisp-mode)))
          (redisplay)
          (set-frame-parameter frame 'old-fallback-buffer doom-fallback-buffer-name)
          (setq doom-fallback-buffer-name "*scratch*")
          (add-hook 'delete-frame-functions #'cleanup-scratch-frame))))))

;;;###autoload
(defun +default/yank-filename  ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (buffer-file-name)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

;;;###autoload
(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))
