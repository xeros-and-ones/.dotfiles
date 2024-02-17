;;; config.el -*- lexical-binding: t; -*-

(load! "+os")
(load! "+git")
(load! "+misc")
(load! "+text")
(load! "+prog")
(load! "+ui")
(load! "+keys")
(cond
 ((modulep! :tools lsp +eglot) (load! "+eglot"))
 ((modulep! :tools lsp) (load! "+lsp")))

;; doom config
(setq user-full-name "Mohamed Tarek"
      user-mail-address "m96tarek@gmail.com"
      native-comp-jit-compilation t
      doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)
;;
(setq! IS-LINUX t)

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)

(setq-default fill-column 120
              delete-trailing-lines t)

(add-to-list 'doom-detect-indentation-excluded-modes 'org-mode)

;; Delete the selection when pasting
(delete-selection-mode 1)
(setq! treemacs-follow-mode t)
;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

(add-hook! 'find-file-hook #'+my/find-file-check-make-large-file-read-only-hook)

(setq clipetty-tmux-ssh-tty "tmux show-environment -g SSH_TTY")

;; check minified-file
(add-to-list 'magic-mode-alist (cons #'+my/check-minified-file 'fundamental-mode))

(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ("^\\*keycast.*" :size 50 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("^\\*grep\\*$" :size 0.35)
                    ("^\\*pytest\\*" :size 0.35)
                    ("^\\*Anaconda\\*$" :size 0.35)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)
                    ))

;; Manually edit .local/custom.el will break doom updates
(when (file-directory-p custom-file)
  (message (concat "Please delete " custom-file ". And customization in config.el and +ui.el.")))

(custom-set-variables
 '(warning-suppress-log-types '((lsp-mode) (iedit)))
 '(warning-suppress-types '((iedit))))

;; Load system profile for different machines and work config
(dolist (config '("~/.config/doom/local.el"
                  "~/dev/work/dots/local.el"))
  (let ((config-file (file-truename config)))
    (when (file-exists-p config-file)
      (load-file config-file))))

;;
;;
;;
;; Font Ligatures
(when (display-graphic-p)
  (use-package! ligature
    :config
    (ligature-set-ligatures 'prog-mode  '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                                          ;; =:= =!=
                                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                                          ;; ;; ;;;
                                          (";" (rx (+ ";")))
                                          ;; && &&&
                                          ("&" (rx (+ "&")))
                                          ;; !! !!! !. !: !!. != !== !~
                                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                                          ;; ?? ??? ?:  ?=  ?.
                                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                                          ;; %% %%%
                                          ("%" (rx (+ "%")))
                                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                                          ;; |->>-||-<<-| |- |== ||=||
                                          ;; |==>>==<<==<=>==//==/=!==:===>
                                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                                          "-" "=" ))))
                                          ;; \\ \\\ \/
                                          ("\\" (rx (or "/" (+ "\\"))))
                                          ;; ++ +++ ++++ +>
                                          ("+" (rx (or ">" (+ "+"))))
                                          ;; :: ::: :::: :> :< := :// ::=
                                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                                          "="))))
                                          ;; .. ... .... .= .- .? ..= ..<
                                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                                          ;; *> */ *)  ** *** ****
                                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                                          ;; www wwww
                                          ("w" (rx (+ "w")))
                                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                                          ;; << <<< <<<<
                                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                                          "-"  "/" "|" "="))))
                                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                                          ;; >> >>> >>>>
                                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                                       (+ "#"))))
                                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                                          ;; __ ___ ____ _|_ __|____|_
                                          ("_" (rx (+ (or "_" "|"))))
                                          ;; Fira code: 0xFF 0x12
                                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                                          ;; Fira code:
                                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                                          ;; The few not covered by the regexps.
                                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))))

(plist-put! +ligatures-extra-symbols
            :and           nil
            :or            nil
            :for           nil
            :not           nil
            :true          nil
            :false         nil
            :int           nil
            :float         nil
            :str           nil
            :bool          nil
            :list          nil
            )
