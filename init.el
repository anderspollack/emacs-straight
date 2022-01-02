;; information
;; (emacs-version)
;; (message user-emacs-directory)
;; get information about keypress
;; C-h k
;; get information about prefix keypress
;; [C-w] C-h
;; view all major and minor key bindings in a buffer
;; C-h m

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Anders Pollack"
      user-mail-address "pollack.anders@gmail.com")

;; system vars
(setq is-mac-gui (and (eq system-type 'darwin) (eq window-system 'ns))
      is-terminal (eq window-system nil))

;; default emacs interface overrides
(set-default 'truncate-lines t) ;; truncate long lines instead of wrap at window edge
(tab-bar-mode 1) ;; enable tab bar (note: not working with emacs-plus on mac)
(winner-mode 1) ;; enable winner
(setq inhibit-startup-message t) ;; hide the splash page
(setq ring-bell-function 'ignore) ;; disable constant bell dinging
(setq meta-prefix-char nil) ;; disable ESC as meta prefix since we have atlt/option

;; Mac-specific modifiers: use Command as Meta, and leave option unchanged
(when is-mac-gui
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (global-set-key (kbd "M-s") 'save-buffer) ;; SAVE: overrides default reverse-i-search
  (global-set-key (kbd "M-w") 'delete-window) ;; CLOSE: overrides default M-w kill-ring-save/"Copy"
  (global-set-key (kbd "M-c") 'kill-ring-save) ;; COPY: overrides default M-c capitalize-word
  (global-set-key (kbd "M-v") 'yank) ;; PASTE: overrides default M-v scroll-down-command/inverse of C-v
  (global-set-key (kbd "M-l") 'load-file) ;; LOAD: overrides default M-l downcase-word
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
  (set-face-attribute 'region nil :background "brightblue")
  )

;; gui-specific settings
(when (not is-terminal)
  ;; default font family and size in the current and all future frames
  (set-face-attribute 'default nil :font "SF Mono-13")
  (set-frame-font "SF Mono-13" nil t)
  ;; Setup initial and default frame sizes
  (setq initial-frame-alist
	(append (list '(fullscreen . maximized))))
  (setq default-frame-alist
	(append (list '(width  . 90)
                      '(height . 52)
                      '(top . 50)
                      '(left . 30)
                      '(tool-bar-lines . 0)
                      '(vertical-scroll-bars . nil))))
  ;; make fringes wider so magit arrows don't look weird
  (fringe-mode 12)
  ;; thicken window dividers for easier mouse resizing
  ;; (window-divider-mode)
  ;; (setq
  ;;  window-divider-default-bottom-width 3
  ;;  window-divider-default-right-width 3
  ;;  )
  (tool-bar-mode -1) ;; no default emacs toolbar
  (toggle-scroll-bar -1) ;; no scrollbars
  (global-hl-line-mode 1) ;; enable current line highlight
  )

;; global editor behavior
(setq scroll-conservatively 1)
;; (global-display-line-numbers-mode)
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
;;(setq straight-use-package-by-default t)

;; install packages
(use-package org
  :defer t)

(use-package magit
  :straight t
  :defer t)

(use-package git-gutter
  :straight t
  :defer t
  :config
  (global-git-gutter-mode 1)
  )

(use-package gruvbox-theme
  :straight t
  :config
  (when (not is-terminal)
    (load-theme 'gruvbox-light-soft t)
    )
  )

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  )

(use-package evil
  :straight t
  :init
  (setq evil-esc-delay 0) ;; prevent evil-modee from waiting t send ESC (for default use as meta-prefix-char)
  (setq evil-want-keybinding nil) ;; necessary for evil-collection
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  ;; (setq evil-want-C-u-scroll t) ;; not enabled - keepin C-u available
  (setq evil-respect-visual-line-mode t) ;; move up and down over text with soft line breaks
  (add-hook 'with-editor-mode-hook 'evil-insert-state) ;; ensure magit commit message initializes in insert mode
  :config
  (evil-mode)
  )

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package vterm
  :straight t
  :defer t)

;; gemini + gopher browser
(use-package elpher
  :straight t
  :defer t)

;; typing practice
(use-package speed-type
  :straight t
  :defer t)

;; use a package just for this session with M-x straight-use-package
;; upgrade active packages with M-x straight-pull-all
;; activate upgraded versions of packages without restarting Emacs with M-x straight-check-all
;; remove unused repository folders with M-x straight-remove-unused-repos
