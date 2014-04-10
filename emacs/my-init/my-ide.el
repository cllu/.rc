;; Programming Environment.
;; also include AUCTex config

;; tabs are evil
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; goodbye trailing spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; whitespace mode
;; (setq whitespace-display-mappings
;;       '(
;;         (space-mark 32 [183] [46])
;;         (newline-mark 10 [8629 10])
;;         (tab-mark 9 [8594 9])
;;         ))

;; css two spaces indent
(setq css-indent-offset 2)

;; javascript
(setq js-indent-level 2)

;; c indent style
(setq-default c-basic-offset 4)

;; spell check
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")

;; Matlab/octave
;; \\' match the end of filename
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; zshrc
(add-to-list 'auto-mode-alist '("zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("alias\\'" . shell-script-mode))
;; set indentation to 2 spaces
(setq sh-basic-offset 2)

;; For Drupal Development
(when (require 'drupal-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode)))

;; set up parenthesis for paren completion
(when (require 'parenthesis nil t)
  ;; in latex writing, ()[]{}$$ always comes together
  (add-hook 'LaTeX-mode-hook (lambda() (parenthesis-register-keys "([{$" LaTeX-mode-map))))

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(eval-after-load "tramp"
  '(progn
     (defvar sudo-tramp-prefix
       "/sudo::"
       (concat "Prefix to be used by sudo commands when building tramp path "))

     (defun sudo-file-name (filename) (concat sudo-tramp-prefix filename))

     (defun sudo-find-file (filename &optional wildcards)
       "Calls find-file with filename with sudo-tramp-prefix prepended"
       (interactive "fFind file with sudo ")
       (let ((sudo-name (sudo-file-name filename)))
         (apply 'find-file
                (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

     (defun sudo-reopen-file ()
       "Reopen file as root by prefixing its name with sudo-tramp-prefix and by clearing buffer-read-only"
       (interactive)
       (let*
           ((file-name (expand-file-name buffer-file-name))
            (sudo-name (sudo-file-name file-name)))
         (progn
           (setq buffer-file-name sudo-name)
           (rename-buffer sudo-name)
           (setq buffer-read-only nil)
           (message (concat "Set file name to " sudo-name)))))

     (global-set-key "\C-x+" 'sudo-find-file)
     (global-set-key "\C-x!" 'sudo-reopen-file)))

(require 'tramp)

;; editor-config
(load "editorconfig")

;; newline-and-indent
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'html-mode-hook 'set-newline-and-indent)
(add-hook 'css-mode-hook 'set-newline-and-indent)
(add-hook 'javascript-mode-hook 'set-newline-and-indent)

(provide 'my-ide)
