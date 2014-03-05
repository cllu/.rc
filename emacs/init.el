(add-to-list 'load-path "~/.emacs.d/my-init")  ; my init files

(require 'my-install)  ; some cutomization between different machine.
(require 'my-general)  ; general settings, overall looking
(require 'my-dired)    ; all dired settings
(require 'my-org)      ; org-mode setting
(require 'my-ide)      ; CEDET, ECB, Auctex, drupal, etc.
(require 'my-calendar)
(require 'my-keybinding)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d8b2fc4540c8e562cfad979233188809fa27956b7d229bc585dc06916e5552b0" "84fc6441d9a2de3b6fec586ff2cf89d12bb78ccf9f85b3908658f00da5a0f20d" "4d66773cc6d32566eaf2c9c7ce11269d9eb26e428a1a4fa10e97bae46ff615da" "a64e1e2ead17a9322f6011f6af30f41bd6c2b3bbbf5e62700c8c3717aac36cbf" "5e2ade7f65d9162ca2ba806908049fb37d602d59d90dc3a08463e1a042f177ae" "d05303816026cec734e26b59e72bb9e46480205e15a8a011c62536a537c29a1a" default)))
 '(scroll-bar-mode (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
