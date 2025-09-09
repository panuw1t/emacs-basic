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
(global-superword-mode -1)                 ;for camelCase and snake_case
(winner-mode 1)

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
(setq-default indent-tabs-mode nil)
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


;; compilation
(setopt compilation-always-kill t)
(setopt compilation-environment
        (list (concat "PATH=" (getenv "HOME") "/.bun/bin:" (getenv "PATH"))))

(add-hook 'compilation-mode-hook
          (lambda ()
						(visual-line-mode -1)
            (setq truncate-lines t)))

(with-eval-after-load 'compile
  ;; set cursor to follow compilation output
  (setq compilation-scroll-output t)

	(add-to-list 'compilation-error-regexp-alist 'gradle-kotlin)
	(add-to-list 'compilation-error-regexp-alist-alist
							 '(gradle-kotlin
								 "^e: file://\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
								 1 2 3)))

;; color for compile buffer ?
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; project.el
(with-eval-after-load 'project
	(add-to-list 'project-switch-commands '(project-compile "compile")))

;; visual line mode
(with-eval-after-load 'simple
  (define-key visual-line-mode-map [remap move-beginning-of-line] #'move-beginning-of-line)
  (define-key visual-line-mode-map [remap move-end-of-line] #'move-end-of-line)
  (define-key visual-line-mode-map [remap kill-line] #'kill-line))

(global-visual-line-mode 1)

;; org mode
(with-eval-after-load 'org
	(global-set-key (kbd "C-c l") #'org-store-link)
	(global-set-key (kbd "C-c a") #'org-agenda)
	(global-set-key (kbd "C-c c") #'org-capture))

;; completion
(setq-default read-file-name-completion-ignore-case t)
