;; my personal dired setup.

;; set up single dired mode
(require 'dired-single)
(require 'dired-isearch)
(require 'dired-details)
(dired-details-install)
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; set up dired-isearch
	    (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
	    (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
	    (define-key dired-mode-map (kbd "M-C-s") 'dired-isearch-forward-regexp)
	    (define-key dired-mode-map (kbd "M-C-r") 'dired-isearch-backward-regexp)
	    ;; set up dired-single
	    (define-key dired-mode-map (kbd "RET") 'dired-single-buffer)
	    (define-key dired-mode-map (kbd "e")   'dired-single-buffer)
	    (define-key dired-mode-map (kbd "<mouse-1>") 'dired-single-buffer-mouse)
	    (define-key dired-mode-map (kbd "^")
	      (lambda ()
		(interactive)
		(dired-single-buffer "..")))
	    (define-key dired-mode-map (kbd "<backspace>")
	      (lambda ()
		(interactive)
		(dired-single-buffer "..")))

	    (setq dired-single-use-magic-buffer t)
	    (setq dired-single-magic-buffer-name "*dired")))
(global-set-key (kbd "C-x d")
		'dired-single-magic-buffer)

;; set up dired-x
(add-hook 'dired-load-hook
	  (lambda () 
	    (load "dired-x")
	    ))
(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-omit-mode 1)
	    (setq dired-omit-files
		  (concat dired-omit-files "\\|^\\..+$"))
	    ))

(provide 'my-dired)