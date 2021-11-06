;; information
;; (emacs-version)
;; (message user-emacs-directory)
;; get information about keypress
;; C-h k
;; get information about prefix keypress
;; [C-w] C-h
;; view all major and minor key bindings in a buffer
;; C-h m

;; vars
(setq is-mac-gui
      (and
       (eq system-type 'darwin)
       (eq window-system 'ns)
       )
      )
(setq is-terminal (eq window-system nil))

;; default emacs interface overrides
(tool-bar-mode -1) ;; no default emacs toolbar
(toggle-scroll-bar -1) ;; no scrollbars
(set-default 'truncate-lines t)
(tab-bar-mode 1) ;; enable tab bar (note: not working with emacs-plus on mac)
(setq inhibit-startup-message t) ;; hide the splash page
(setq ring-bell-function 'ignore) ;; disable constant bell dinging

;; Mac-specific modifiers: use Command as Meta, and leave option unchanged
(when is-mac-gui
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (global-set-key (kbd "M-s") 'save-buffer) ;; overrides default reverse-i-search
  (global-set-key (kbd "M-w") 'delete-window) ;; overrides default M-w kill-ring-save/"Copy"
  (global-set-key (kbd "M-c") 'kill-ring-save) ;; overrides default M-c capitalize-word
  (global-set-key (kbd "M-l") 'load-file) ;; overrides default M-l downcase-word
  )

;; terminal-specific settings
(when is-terminal
  (xterm-mouse-mode t) ;; enable mouse clicking and region selection
  ;; enable mouse wheel scrolling
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  )

;; global editor behavior
(windmove-default-keybindings) ;; use shift + arrow to move in windows
(electric-pair-mode 1) ;; automatic "" () [] {} matching
(show-paren-mode 1) ;; highlight matching () []

;; From https://systemcrafters.cc/advanced-package-management/using-straight-el/

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package for package configuration
(straight-use-package 'use-package)

;; ensure use-package installs all packages without requiring :straight t (ex: (use-package evil :straight t))
(setq straight-use-package-by-default t)

;; install packages
(use-package org
  :defer t)
(use-package magit
  :defer t)

(use-package gruvbox-theme
  :config
  (when (not is-terminal)
    (load-theme 'gruvbox-light-soft t)
    )
  )

(use-package which-key
  :config
  (which-key-mode)
  )

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; necessary for evil-collection
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  ;; (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

;; use a package just for this session with M-x straight-use-package
;; upgrade active packages with M-x straight-pull-all
;; activate upgraded versions of packages without restarting Emacs with M-x straight-check-all
;; remove unused repository folders with M-x straight-remove-unused-repos
