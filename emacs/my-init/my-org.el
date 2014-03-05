;; some orgmode config

(provide 'my-org)

(setq org-agenda-files (quote ("~/gtd.org")))
(setq org-completion-use-ido t)
(setq org-format-latex-options (quote (:foreground default :background default :scale 1.2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))