;; My own keybinding.
(provide 'my-keybinding)

;; Note the F5-F9 and F11, F12 is not binded.
;; and also C-c is reserved for personal use.

;; fullscreen F11
(global-set-key [f11] 'toggle-fullscreen)
;; menubar F10
(global-set-key [f10] 'menu-bar-mode)

;; open dired
(global-set-key [f1] 'dired-single-magic-buffer)
;; open file
(global-set-key [f2] 'ido-find-file)
;; buffer switch F3
(global-set-key [f3] 'ido-switch-buffer)
;; close buffer
(global-set-key [f4] 'kill-buffer-and-window)

;; reload file
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))
;; compile F5
;(global-set-key [f5] 'compile)
;; next error F6

;; 
;; 

;; go to line 
;(global-set-key "\M-g" )

;; use escape to cancel keyboard input
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; switch between window
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; keep C-SPC for input method. fcitx
(global-set-key (kbd "C-SPC") 'nil)

;; ebib bibtex insertion
(global-set-key "\C-ce" 'ebib)

;; vi navigation keybinding
(global-set-key "\M-h" 'backward-char)
(global-set-key "\M-j" 'next-line)
(global-set-key "\M-k" 'previous-line)
(global-set-key "\M-l" 'forward-char)

;; next/pre page
;(global-set-key "\M-"

;; window split
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-2" 'split-window-vertically)
(global-set-key "\M-3" 'split-window-horizontally)
(global-set-key "\M-4" 'delete-window)

;; buffer switch, default is C-x b
;(global-set-key "\M"

;; buffer kill, default is C-x k
;(global-set-key "\M" 'kill-buffer)

;; dired mode, default is C-x d
;(global-set-key "\M

