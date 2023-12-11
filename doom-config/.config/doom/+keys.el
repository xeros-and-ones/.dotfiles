;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; ------------------ Mappings --------------------------
(map! :leader
      :desc "Open Treemacs" "e" #'treemacs)
(map!
 (:map override
       "S-l" #'+tabs:next-or-goto
       "S-h" #'+tabs:previous-or-goto))
