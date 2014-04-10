(require 'helm-misc)

(require 'helm-ls-git)

;(add-to-list 'helm-mini-default-sources 'helm-c-source-ls-git-status t)
;(add-to-list 'helm-mini-default-sources 'helm-c-source-ls-git t)

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-c-source-ls-git-status
                                  helm-c-source-ls-git
                                  helm-source-buffer-not-found))
