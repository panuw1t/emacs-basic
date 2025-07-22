;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(defun my/load-config (name)
  "Load a config file from `user-emacs-directory/config/` given NAME (without extension)."
  (let ((file (expand-file-name (concat name ".el")
                                (expand-file-name "config/" user-emacs-directory))))
    (if (file-exists-p file)
        (load file)
      (message "⚠️ Config file not found: %s" file))))

(my/load-config "option")
(my/load-config "my-completion")

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; editing
(use-package meow
  :config
  (my/load-config "meow-setup"))

(use-package key-chord
  :after meow
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit))

;; ui
(use-package all-the-icons
  :if (display-graphic-p))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h x" . helpful-command)))

(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package which-key
  :config
  (which-key-mode))


;;(my/load-config "smooth-scroll")

;; completion
(use-package vertico
  :custom
   (vertico-cycle t)
  :init
  (vertico-mode))

;; (use-package orderless
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package marginalia
;;   :bind (:map minibuffer-local-map
;;          ("M-A" . marginalia-cycle))
;;   :init
;;   (marginalia-mode))

;; (use-package corfu
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   :init
;;   (global-corfu-mode)
;;   (corfu-popupinfo-mode))

;; (use-package cape			;backend for completion
;;   :bind ("C-c p" . cape-prefix-map)
;;   :init
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev))

;; lsp + tree-sitter
(use-package eglot
  :hook ((c-mode          . eglot-ensure)
         (c++-mode        . eglot-ensure)
         (js-mode         . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (kotlin-ts-mode  . eglot-ensure)
         (js-ts-mode  . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(kotlin . ("kotlin-language-server"))
               '(kotlin-ts-mode . ("kotlin-language-server")))
  (setq eglot-autoshutdown t)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
	)

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . typescript))
  (add-to-list 'tree-sitter-major-mode-language-alist '(kotlin-ts-mode . kotlin))
  (add-to-list 'tree-sitter-major-mode-language-alist '(js-ts-mode . javascript)))

(use-package tree-sitter-langs
  :config
  (setq treesit-language-source-alist '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
																				(javascript "https://github.com/tree-sitter/tree-sitter-javascript"))))

(use-package tree-sitter-indent)
(use-package tree-sitter-ispell)
;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package meow-tree-sitter
  :config
  (meow-tree-sitter-register-defaults))


;;misc
;; (use-package smartparens
;;   :hook ((prog-mode . smartparens-mode)))

(use-package kotlin-mode)

(use-package magit)
