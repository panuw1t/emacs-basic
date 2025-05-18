(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(load-theme 'wombat)
(scroll-bar-mode -1)
(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))
(tool-bar-mode -1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)
(global-so-long-mode)
(savehist-mode 1)
(repeat-mode 1)
(global-superword-mode)                 ;for camelCase and snake_case

(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))


(setq global-auto-revert-non-file-buffers t)
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq eshell-scroll-to-bottom-on-input 'this)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq ibuffer-movement-cycle t)
(setq ibuffer-old-time 72)
(setq indent-tabs-mode nil)
(setq-default tab-width 2)
(setq kill-do-not-save-duplicates t)
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(setq savehist-file (expand-file-name "history" user-emacs-directory))
(setq bookmark-save-flag 1)
(setq load-prefer-newer t)
(setq help-window-select t)  ; Switch to help buffers automatically

;;for scrolling
(setq scroll-conservatively 10
      scroll-margin 15)

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)

(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)
