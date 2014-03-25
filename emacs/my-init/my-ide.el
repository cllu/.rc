;; Programming Environment.
;; also include AUCTex config
(provide 'my-ide)

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

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/package/snippets"))
(yas-global-mode 1)

;; Matlab/octave
;; \\' match the end of filename
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; zshrc
(add-to-list 'auto-mode-alist '("zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("alias\\'" . shell-script-mode))
;; set indentation to 2 spaces
(setq sh-basic-offset 2)

;; php
(require 'php-mode)

;; For Drupal Development
(when (require 'drupal-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode)))

;; CEDET config
;(when (require 'cedet nil t)
;  ;; (global-ede-mode 1)
;  (semantic-load-enable-code-helpers)
;  (require 'semantic-ia)
;  ;; (require 'semantic-gcc)
;  ;; use semantic tag folding when in gui
;  (when (and window-system (require 'semantic-tag-folding nil 'noerror))
;    (global-semantic-tag-folding-mode 1)))

;; ECB config
(setq ecb-options-version "2.40")
(when (require 'ecb nil t)
  (setq ecb-tip-of-the-day nil))

;; Auctex
; manual install
;(add-to-list 'load-path "~/.emacs.d/package/auctex")
;(add-to-list 'load-path "~/.emacs.d/package/auctex/preview")
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)
;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;(setq LaTeX-command "pdflatex")
; spell check
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(add-hook 'LaTex-mode-hook 'flyspell-buffer)

;; ebib
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(setq ebib-index-display-fields (quote (title)))
(setq ebib-index-window-size 15)
(setq ebib-layout 60)
(setq ebib-timestamp-format "%Y-%m-%d %A")
(setq ebib-use-timestamp t)


;; set up parenthesis for paren completion
(when (require 'parenthesis nil t)
  ;; in latex writing, ()[]{}$$ always comes together
  (add-hook 'LaTeX-mode-hook (lambda() (parenthesis-register-keys "([{$" LaTeX-mode-map))))


;; set up auto-complete for use.
;(when (require 'auto-complete)
;  (global-auto-complete-mode t))

;(require 'auto-complete-python)

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; CUDA mode
(autoload 'cuda-mode "cuda-mode.el")
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;; sass mode
(require 'haml-mode)
(require 'sass-mode)
(require 'scss-mode)
(setq-default scss-compile-at-save nil)

;; scala mode
(add-to-list 'load-path "~/.emacs.d/package/scala-mode2")
(require 'scala-mode2)

;; nginx mode
(require 'nginx-mode)

;; git .gitigmore mode
(require 'gitignore-mode)

;; Dockerfile
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


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

;;; smex for M-x completion
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(require 'ag)

;; nginx
(require 'nginx-mode)

(add-to-list 'load-path "~/.emacs.d/package/js2-mode")
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 2)

;; patch js2-mode to support json file
;; see https://code.google.com/p/js2-mode/issues/detail?id=50
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(defadvice js2-reparse (before json)
  (setq js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)

(defadvice js2-parse-statement (around json)
  (if (and (= tt js2-LC)
           js2-buffer-file-name
           (string-equal (substring js2-buffer-file-name -5) ".json")
           (eq (+ (save-excursion
                    (goto-char (point-min))
                    (back-to-indentation)
                    (while (eolp)
                      (next-line)
                      (back-to-indentation))
                    (point)) 1) js2-ts-cursor))
      (setq ad-return-value (js2-parse-assign-expr))
    ad-do-it))
(ad-activate 'js2-parse-statement)
;; end patch to js2-mode

;; editor-config
(load "editorconfig")
