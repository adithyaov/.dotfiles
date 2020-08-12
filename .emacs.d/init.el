;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Remapping bindings to the max
(global-set-key (kbd "C-c SPC") 'set-mark-command)

(global-set-key (kbd "C-l") 'delete-backward-char)
(global-set-key (kbd "M-l") 'backward-kill-word)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-f") 'forward-char)
;; (define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-n" 'isearch-repeat-forward)
(global-set-key (kbd "C-b") 'backward-char)
;; (define-key isearch-mode-map "\C-b" 'isearch-repeat-backward)
(define-key isearch-mode-map "\C-p" 'isearch-repeat-backward)
(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; (global-set-key (kbd "C-j") 'backward-delete-char)
;; (global-set-key (kbd "C-d") 'backward-delete-char)
;; (global-set-key (kbd "M-d") 'ivy-backward-kill-word)

(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
(setq search-whitespace-regexp ".*?")
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(progn
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))

(defun goto-prog-dir ()
  "Change directory to the Desktop/LinuxWorkStation/Prog"
  (interactive)
  (cd "/mnt/c/Users/mota/Desktop/LinuxWorkStation/Prog"))

(defalias 'open 'find-file)
(defalias 'openo 'find-file-other-window)
(defalias 'prog 'goto-prog-dir)

(defconst HOME "/home/creed")

(define-key key-translation-map (kbd "C-c x") (kbd "C-x"))

;; (global-set-key (kbd "C-f") 'forward-word)
;; (global-set-key (kbd "M-f") 'forward-char)
;; (global-set-key (kbd "C-b") 'backward-word)
;; (global-set-key (kbd "M-b") 'backward-char)

(setq-default fill-column 80)

(use-package ahk-mode
  :ensure t)

(use-package column-enforce-mode
  :ensure t
  :config (global-column-enforce-mode t))

(use-package eshell-git-prompt
  :ensure t
  :config (eshell-git-prompt-use-theme 'robbyrussell))

(use-package csharp-mode
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :config (setq highlight-indent-guides-method 'fill))

;; Themes START --------------------------------------------

(use-package rebecca-theme :ensure t)
(use-package dracula-theme :ensure t)
(use-package srcery-theme :ensure t)

(load-theme 'srcery t)

;; Themes END ----------------------------------------------

(use-package impatient-mode
  :ensure t
  :config
  (defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer))))

(setq ring-bell-function 'ignore)

(use-package expand-region
  :ensure t
  :config
  (pending-delete-mode t)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "<C-return>") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c g") 'counsel-git))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-tags-on-save nil
	tags-revert-without-query t
	haskell-indentation-mode nil
	haskell-compile-cabal-build-command "cabal v2-build"))

(use-package hindent
  :ensure t
  :config (add-hook 'haskell-mode-hook #'hindent-mode))

; (use-package flymake-hlint
;   :ensure t
;   :config (add-hook 'haskell-mode-hook 'flymake-hlint-load))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy))

(use-package org
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package avy
  :ensure t
  :config
  (setq avy-keys '(?a ?s ?d ?f ?q ?w ?e ?r ?n ?m ?g ?h ?j ?k ?l))
  (setq avy-background t)
  (setq avy-orders-alist '((avy-goto-word-0 . avy-order-closest)))
  (setq avy-all-windows nil)
  (global-set-key (kbd "M-RET") 'avy-goto-word-0))

(windmove-default-keybindings 'meta)

(setq projectile-project-search-path '("/mnt/c/Users/mota/Desktop/LinuxWorkStation/Prog/"))

(defun open-my-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

(defun smarter-move-point ()
  "Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
If point is already there, move to the end of line.
Effectively toggle between the first non-whitespace character,
the beginning of the line and the end of the line."
  (interactive)

  (let ((orig-point (point)))
    (if (= orig-point (point-at-bol))
      (move-end-of-line 1)
      (progn (back-to-indentation)
	     (when (= orig-point (point))
	       (move-beginning-of-line 1))))))

(global-set-key (kbd "C-o") 'smarter-move-point)

(defun buffer-list-switch ()
  "Switch to buffer list and activate the window"
  (interactive)
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*" 0))
)

(global-set-key (kbd "C-x C-b") 'buffer-list-switch)

(use-package hydra
  :ensure t
  :config
  (global-set-key
    (kbd "C-c m")
    (defhydra hydra-movement (:body-pre (set-mark-command nil))
      "Hydra navigation"
      ("f"   forward-char)
      ("F"   forward-word)
      ("b"   backward-char)
      ("B"   backward-word)
      ("p"   previous-line)
      ("n"   next-line)
      ("P"   backward-paragraph)
      ("N"   forward-paragraph)
      ("a"   crux-move-beginning-of-line)
      ("e"   move-end-of-line)
      ("q"   nil)
      ("v"   scroll-up-command)
      ("V"   scroll-down-command)
      ("m"   avy-goto-word-0 :color blue)
      )))

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ===================================================================
;; Haskell auto show core script

(defun produce-core (file-path &optional ghc-path args)
  (let* ((check-nil (lambda (x y) (if x x y)))
	 (gp (funcall check-nil ghc-path "ghc"))
	 (aa (funcall check-nil args ""))
	 (cmd (concat gp " " file-path " -ddump-simpl -ddump-to-file " aa))
	 (so (shell-command-to-string cmd))
	 (res (with-temp-buffer (insert-file-contents (concat (substring file-path 0 -2) "dump-simpl") nil) (buffer-string)))
	 (cb (current-buffer)))
    (progn (with-help-window "*core*" (princ res))
	   (switch-to-buffer "*core*")
	   (haskell-mode)
	   (switch-to-buffer cb))))

(defun current-core ()
  (interactive)
  (produce-core (buffer-file-name) "ghc" "-dsuppress-all -O2"))

;; ===================================================================
;; Sync dotfiles
(defun dotfile-sync (fp)
  (let ((git
	(concat "git --git-dir=" HOME "/.dotfiles/ --work-tree=" HOME)))
    (progn
      (shell-command (concat git " add " fp))
      (shell-command (concat git " commit -m 'Update " fp "'")))))

(defun dotfiles-sync ()
  (interactive)
  (let* ((git
	  (concat "git --git-dir=" HOME "/.dotfiles/ --work-tree=" HOME))
	 (tf-cmd (concat git " ls-files" " --modified"))
	 (tf-string (shell-command-to-string tf-cmd))
	 (tf-list (split-string tf-string "\n")))
    (progn
      (dolist (f tf-list)
	(unless (= (length f) 0)
	  (dotfile-sync f)))
      (shell-command (concat git " push")))))
