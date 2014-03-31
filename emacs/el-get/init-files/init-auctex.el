(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq LaTeX-command "pdflatex")
; spell check
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(add-hook 'LaTex-mode-hook 'flyspell-buffer)
