;;; Set up some general small items

(provide 'my-general)

;; no toolbar
(if (display-graphic-p)
    (tool-bar-mode -1))
(menu-bar-mode -1)
;; no splash screen
(setq inhibit-splash-screen t)
;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
;; display line and column number
(column-number-mode t)
(global-linum-mode t)
;; disable linum in certain modes.
(setq linum-disabled-modes-list '(shell-mode eshell-mode complilation-mode))
(defun linum-on()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))
;; scrollbar right
(customize-set-variable 'scroll-bar-mode 'right)
;; display date and time
(display-time-mode 1)
(setq display-time-format "%m/%d %a %H:%M")

;; Fullscreen at setup
;(defun toggle-fullscreen (&optional f)
;  (interactive)
;  (let ((current-value (frame-parameter nil 'fullscreen)))
;    (set-frame-parameter nil 'fullscreen
;			 (if (equal 'fullboth current-value)
;			     (if (boundp 'old-fullscreen) old-fullscreen nil)
;			   (progn (setq old-fullscreen current-value)
;				  'fullboth)))))
;
;(add-hook 'window-setup-hook 'toggle-fullscreen)

(setq user-mail-address "hi@chunlianglyu.com")
(setq user-full-name "Chunliang LYU")

;; save bookmarks in .emacs.d directory
(setq bookmark-default-file "~/.emacs.d/.bookmarks")
;; save all semantic db files into .emacs.d
(make-directory "~/.emacs.d/.semantic" t)
(setq semanticdb-default-save-directory "~/.emacs.d/.semantic")
;; backup file config
(make-directory "~/.emacs.d/.backups" t)
(setq
   backup-by-copying t
   backup-directory-alist '(("." . "~/.emacs.d/.backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)
;; auto save file config
(setq auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/.saves-")
(make-directory "~/.emacs.d/.autosaves" t)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/.autosaves/\\1" t)))

;; Making buffer names unique
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; close buffer when shell/gdb exits
(defun kill-buffer-when-exit ()
  (let ((current-process (ignore-errors (get-buffer-process (current-buffer)))))
    (when current-process
      (set-process-sentinel current-process
			    (lambda (watch-process change-state)
			      (when (string-match "\\(finished\\|exited\\)" change-state)
				(kill-buffer (process-buffer watch-process))))))))
;(add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
;(add-hook 'term-mode-hook 'kill-buffer-when-exit)
(add-hook 'shell-mode-hook 'kill-buffer-when-exit)

;; display matched parent
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; auto reload file if modified out from emacs.
(global-auto-revert-mode)

;; follow version controled symbolic file
(setq vc-follow-symlinks t)

;; turn off alarm
(setq ring-bell-function 'ignore)

;; remember the last place
(setq save-place-file "~/.emacs.d/.saved-places")
(require 'saveplace)
(setq-default save-place t)

(add-to-list 'load-path "~/.emacs.d/packages/tomorrow-theme")
(require 'color-theme-tomorrow)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/tomorrow-theme")
(load-theme 'tomorrow-night-bright t)
