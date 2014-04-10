(require 'evil)
(evil-mode 1)

;; underscore should be regarded as word character, like vim
;; this affects the */# search on things like function_name
(modify-syntax-entry ?_ "w")

;;; esc quits almost everything
;; http://stackoverflow.com/a/10166400/693110
;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;;; keybinding
;; buffer
(define-key evil-normal-state-map "gb" 'helm-buffers-list)
(define-key evil-normal-state-map "g;" 'helm-M-x)
;; find file in project, need helm and helm-ls-git
;; the helm-mini sources are customized in init-helm-ls-git.el
(define-key evil-normal-state-map "gp" 'helm-mini)
;; find file, will call `locate`
(define-key evil-normal-state-map "gf" 'helm-for-files)
