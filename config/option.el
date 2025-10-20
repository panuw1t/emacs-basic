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
(blink-cursor-mode 0)

(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))


(setq global-auto-revert-non-file-buffers t)
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
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

;;for scrolling
;; (setq scroll-conservatively 10
;;       scroll-margin 15)

(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; global keymap
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
(keymap-global-set "M-o" #'other-window)

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

(require 'ansi-color) ;; color for compile buffer ?
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
  (keymap-global-set "C-c l" #'org-store-link)
  (keymap-global-set "C-c a" #'org-agenda)
  (keymap-global-set "C-c c" #'org-capture))

;; completion
(setq-default read-file-name-completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)

;; whitespace-mode
;; (setq whitespace-line-column 80)
(setq whitespace-style '(face tabs tab-mark))

(defvar my-whitespace-excluded-modes
  '(makefile-gmake-mode)
  "List of modes where `my-whitespace-mode` should not be enabled.")

(defun my-whitespace-mode ()
  (setq show-trailing-whitespace t)
  (unless (memq major-mode my-whitespace-excluded-modes)
    (whitespace-mode)))
(add-hook 'prog-mode-hook 'my-whitespace-mode)

;; isearch
;; c-s c-h c-h for doc
(setq isearch-repeat-on-direction-change t)
(setq isearch-wrap-pause 'no)


;; hippie expand
(keymap-global-set "M-/" #'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-line
                                         try-expand-list
                                         try-complete-file-name-partially
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill))

;; eshell
(setq eshell-scroll-to-bottom-on-input nil)
(setq eshell-scroll-to-bottom-on-output nil)
(setq eshell-scroll-show-maximum-output nil)
(defun my-eshell-recenter ()
  (interactive)
  (recenter-top-bottom 0))
(defun my-hook-eshell ()
  (local-set-key (kbd "C-l") 'my-eshell-recenter))
(add-hook 'eshell-mode-hook 'my-hook-eshell)

(defun my-zoxide (&optional args)
  (if (not args)
      (eshell/cd "~")
    (let ((dir (string-trim (shell-command-to-string (format "zoxide query %s" (shell-quote-argument args))))))
      (eshell/cd dir))))

(defun my-eshell-zoxide-add-pwd ()
  (when (and (boundp 'eshell-last-dir-ring)
             (eshell/pwd))
    (start-process "zoxide-add" nil "zoxide" "add" (eshell/pwd))))

(add-hook 'eshell-directory-change-hook #'my-eshell-zoxide-add-pwd)

;; window managerment
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)
;; (setq help-window-select t)  ; Switch to help buffers automatically

(setq display-buffer-alist `(((or (major-mode . help-mode) (major-mode . helpful-mode))
                              (display-buffer-reuse-mode-window display-buffer-reuse-window display-buffer-pop-up-window)
                              (mode . (helful-mode help-mode)))
                             (,(rx "*" (or "compilation" "eshell") "*")
                              (display-buffer-reuse-mode-window display-buffer-in-direction)
                              (mode . (compilation-mode eshell-mode))
                              (direction . bottom)
                              (window-height . 0.3))
                             (,(rx "*" (or "*xref*" "*grep*" "*Occur*") "*")
                              (display-buffer-reuse-window)
                              (inhibit-same-window . nil))
                             ("magit: .*"
                              (display-buffer-same-window)
                              (window . root)
                              (window-height . 1.0)
                              (inhibit-same-window . nil))
                             ))
