;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(blink-cursor-mode 1)
(global-hl-line-mode 1)

;; Display time
(display-time-mode 1)

;; Conservative scrolling
(setq scroll-preserve-screen-position 'always)

;; Ignore ding
(setq ring-bell-function 'ignore)

;; Set scratch default text to ""
(setq initial-scratch-message "")

;; Onsave hook, remove spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Key bindings
(global-set-key (kbd "M-o") 'other-window)
(define-key isearch-mode-map "\C-n" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-p" 'isearch-repeat-backward)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)

; Use Alt+Arrow to jump to different windows
(windmove-default-keybindings 'meta)

; Default white space to match anything
(setq search-whitespace-regexp ".*?")
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

; Hilight matching parenthesis
(progn
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))

;; Goto program dir when you type "prog"
(defun goto-prog-dir ()
  "Change directory to the Desktop/LinuxWorkStation/Prog"
  (interactive)
  (cd "/mnt/c/Users/mota/Desktop/LinuxWorkStation/Prog"))
(defalias 'prog 'goto-prog-dir)

;; Set the HOME constant
(defconst HOME (getenv "HOME"))

;; Set column length to 80
(setq-default fill-column 80)

;; Imports
(straight-use-package 'ahk-mode)
(straight-use-package 'column-enforce-mode)
(straight-use-package 'eshell-git-prompt)
(straight-use-package 'csharp-mode)
(straight-use-package 'highlight-indent-guides)
(straight-use-package 'solarized-theme)
(straight-use-package 'impatient-mode)
(straight-use-package 'ivy)
(straight-use-package 'swiper)
(straight-use-package 'counsel)
(straight-use-package 'magit)
(straight-use-package 'haskell-mode)
(straight-use-package 'hindent)
(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)
(straight-use-package 'multiple-cursors)
(straight-use-package 'avy)
(straight-use-package 'hydra)
(straight-use-package 'rust-mode)
(straight-use-package 'highlight-function-calls)
(straight-use-package 'org-tree-slide)
(straight-use-package 'org)
(straight-use-package 'expand-region)
(straight-use-package
 '(ox-reveal :type git :host github :repo "yjwen/Org-Reveal"))
(straight-use-package 's)
(straight-use-package 'company)
(straight-use-package 'company-fuzzy)

(progn
  (require 'company)
  (require 'company-fuzzy)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-eclim-auto-save nil)
  (setq company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-/") 'company-dabbrev)
  (global-company-fuzzy-mode 1)
  (setq company-fuzzy-prefix-ontop t))

(require 'ox-reveal)

(progn
  (require 'org)
  (define-key org-mode-map (kbd "<C-return>") 'er/expand-region))

;; Hilight text that extends beyond a certain column
(progn
  (require 'column-enforce-mode)
  (global-column-enforce-mode t))

;; Git prompt in eshell
(progn
  (require 'eshell-git-prompt)
  (eshell-git-prompt-use-theme 'robbyrussell))

;; Show indentation block
(progn
  (require 'highlight-indent-guides)
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Load theme
(progn
  (require 'solarized-theme)
  (load-theme 'solarized-dark t))

;; Look at markdown in a clean format
(progn
  (require 'impatient-mode)
  (defun markdown-html (buffer)
    (princ
     (with-current-buffer buffer
       (format "<!DOCTYPE html><html><title>Impatient Markdown</title>\
<xmp theme=\"united\" style=\"display:none;\"> %s  </xmp>\
<script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>"
	       (buffer-substring-no-properties (point-min) (point-max))))
     (current-buffer))))

(progn
  (require 'expand-region)
  (pending-delete-mode t)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "<C-return>") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

(progn
  (require 'ivy)
  (require 'counsel)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c g") 'counsel-git))

(progn
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status))

(progn
  (require 'haskell-mode)
  (setq haskell-tags-on-save nil)
  (setq tags-revert-without-query t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (setq haskell-compile-cabal-build-command "cabal v2-build"))


(progn
  (require 'hindent)
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; =================================================================
;; Try to work with both, hindent and CPP

(progn
  (require 'hindent)
  (setq alist-haskell-cpp
	'(("INLINE_LATE" . "INLINE [0]")
	  ("INLINE_NORMAL" . "INLINE [1]")
	  ("INLINE_EARLY" . "INLINE [2]")))
  (defun haskell-desugar-cpp-decl (assoc-arr)
    (interactive)
    (let ((start-end (hindent-decl-points)))
      (when start-end
	(let ((beg (car start-end))
	      (end (cdr start-end)))
	  (dolist (elem assoc-arr)
	    (replace-string (car elem) (cdr elem) nil beg end))))))
  (defun haskell-sugar-cpp-decl (assoc-arr)
    (interactive)
    (let ((start-end (hindent-decl-points)))
      (when start-end
	(let ((beg (car start-end))
	      (end (cdr start-end)))
	  (dolist (elem assoc-arr)
	    (replace-string (cdr elem) (car elem) nil beg end))))))
  (defun hindent-reformat-decl-cpp (&optional assoc-arr)
    (interactive)
    (progn
      (unless assoc-arr (setq assoc-arr alist-haskell-cpp))
      (haskell-desugar-cpp-decl assoc-arr)
      (hindent-reformat-decl)
      (haskell-sugar-cpp-decl assoc-arr)))
  (defun hindent-reformat-decl-or-fill-cpp (justify)
    (interactive (progn
		   (barf-if-buffer-read-only)
		   (list (if current-prefix-arg 'full))))
    (if (hindent-in-comment)
	(fill-paragraph justify t)
      (hindent-reformat-decl-cpp)))
  (define-key hindent-mode-map
    [remap fill-paragraph] #'hindent-reformat-decl-or-fill-cpp))

;; =================================================================

(progn
  (require 'projectile)
  (require 'ivy)
  (require 'counsel)
  (require 'counsel-projectile)
  (projectile-mode +1)
  (counsel-projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path
	'("/mnt/c/Users/mota/Desktop/LinuxWorkStation/Prog/")))

(progn
  (require 'multiple-cursors)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(progn
  (require 'avy)
  (setq avy-keys '(?a ?s ?d ?f ?q ?w ?e ?r ?n ?m ?g ?h ?j ?k ?l))
  (setq avy-background t)
  (setq avy-orders-alist '((avy-goto-word-0 . avy-order-closest)))
  (setq avy-all-windows nil)
  (global-set-key (kbd "M-RET") 'avy-goto-word-0))

(defun open-my-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

;; Use C-a to move to beginning of line and first indentation
(defun smarter-move-point ()
  "Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line"
  (interactive)

  (let ((orig-point (point)))
    (back-to-indentation)
    (if (= orig-point (point))
	(move-beginning-of-line 1))))

; Bind C-a to smarter-move-point
(global-set-key (kbd "C-a") 'smarter-move-point)

;; Open buffer list/switch
(defun buffer-list-switch ()
  "Switch to buffer list and activate the window"
  (interactive)
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*" 0)))

; Bind C-x C-b to buffer-list-switch
(global-set-key (kbd "C-x C-b") 'buffer-list-switch)

(progn
  (require 'hydra)
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
     ("m"   avy-goto-word-0 :color blue))))


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

;; ===================================================================
;; cabal check open-repl/close
(defun cabalcc (main &rest args)
  (interactive "srepl: ")
  (let* ((cmd (lambda (x) (concat "(echo :q | cabal repl " x  ")" )))
	 (and-cmd (lambda (x) (concat " && " (funcall cmd x))))
	 (result (funcall cmd main))
	 (full-cmd
	  (dolist (element args result)
	    (setq result (concat result (funcall and-cmd element))))))
    (projectile-with-default-dir (projectile-project-root)
      (compile full-cmd))))

(defun cc-streamly ()
  (interactive)
  (cabalcc "streamly"))

;; Highlight emacs function calls
(progn
  (require 'highlight-function-calls)
  (add-hook 'emacs-lisp-mode-hook 'highlight-function-calls-mode))


;; ===================================================================
;; Load custom file
;; Should be at the end
(progn
  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Cool ivy completion, Copied from StackOverflow and modified accordingly
(progn

  (require 's)
  (require 'ivy)

  ;; Is there a better way to get the expansions?
  (defun ivy-complete ()
    (interactive)
    (dabbrev--reset-global-variables)
    (let* ((sym (thing-at-point 'symbol :no-properties))
	   (wor (thing-at-point 'word :no-properties))
	   (candidates-sym (dabbrev--find-all-expansions sym t))
	   (candidates-wor
	    (if (s-equals? sym wor)
		nil
	      (progn
		(dabbrev--reset-global-variables)
		(dabbrev--find-all-expansions wor t))))
	   (bounds-sym (bounds-of-thing-at-point 'symbol))
	   (bounds-wor (bounds-of-thing-at-point 'word))
	   (candidates (append candidates-sym candidates-wor)))
      (when (not (null candidates))
	(let* ((found-match (ivy-read "matches: " candidates
				      :preselect (thing-at-point 'word)
				      :sort t
				      :initial-input (concat wor " ")))
	       (bounds (if (s-prefix? wor found-match) bounds-wor bounds-sym)))
	  (progn (delete-region (car bounds) (cdr bounds))
		 (insert found-match)))))))
