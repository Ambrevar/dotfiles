;;; Evil+Package-menu-mode
(evil-set-initial-state 'package-menu-mode 'motion)
(evil-define-key 'motion package-menu-mode-map "q" 'quit-window)
(evil-define-key 'motion package-menu-mode-map "i" 'package-menu-mark-install)
(evil-define-key 'motion package-menu-mode-map "U" 'package-menu-mark-upgrades)
(evil-define-key 'motion package-menu-mode-map "u" 'package-menu-mark-unmark)
(evil-define-key 'motion package-menu-mode-map "d" 'package-menu-mark-delete)
(evil-define-key 'motion package-menu-mode-map "x" 'package-menu-execute)

(provide 'init-evil-package)
