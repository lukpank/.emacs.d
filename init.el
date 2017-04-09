;;; Local changes
;; TODO remove go helm integration?

(setq markdown-command "webpage")

;;; Add local lisp directory to search path

(let ((path (expand-file-name "~/.emacs.d/lisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))

;;; Initialize external packages including the melpa repository

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      gnutls-verify-error t)

(package-initialize)

(require 'use-package)

;;; Use more efficient buffer/file selection

(use-package counsel
  :ensure nil
  :init
  (setq ivy-use-virtual-buffers t
	ivy-magic-tilde nil)
  (recentf-mode 1)
  :bind
  (("C-c r" . ivy-resume)
   ("C-c i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c L" . counsel-locate)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . ivy-switch-buffer)
   ("C-s" . swiper))
  :config
  (ivy-mode 1)
  (counsel-mode 1))

;;; Use more more efficient changing windows

(use-package ace-window
  :ensure nil
  :bind
  ("C-x o" . ace-window))

(use-package windmove
  :ensure nil
  :demand
  :bind
  (("s-J" . windmove-down)
   ("s-K" . windmove-up)
   ("s-H" . windmove-left)
   ("s-L" . windmove-right))
  :config
  (windmove-default-keybindings))

(use-package spaces
  :ensure nil
  ;; customize mode-line-format to add: "(" sp-current-space ")"
  :bind
  ("C-c b" . sp-switch-space))

;;; Allow for Undo/Redo of window manipulations (such as C-x 1)

(winner-mode 1)

;;; Remind of keys than can follow in a key sequence

(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

;;; Context aware insertion of pairs of parenthesis

(use-package smartparens
    :ensure nil
    :defer)

;;; Edit with multiple cursors

(use-package multiple-cursors
  :ensure nil
  :bind
  (("C-c n" . mc/mark-next-like-this)
   ("C-c p" . mc/mark-previous-like-this)))

;;; Appearance

(setq inhibit-startup-screen t)
(set-scroll-bar-mode 'right)
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

(defun set-frame-font-inconsolata (size &optional frames)
  "Set font to Inconsolata:pixelsize=SIZE:antialias=true:autohint=false.
Argument FRAMES has the same meaning as for `set-frame-font'"
  (interactive "n[Inconsolata] size: ")
  (set-frame-font (format "Inconsolata:pixelsize=%d:antialias=true:autohint=true" size) nil frames))

(defun set-frame-font-go-mono (size &optional frames)
  "Set font to Go mono:pixelsize=SIZE:antialias=true:autohint=false.
Argument FRAMES has the same meaning as for `set-frame-font'"
  (interactive "n[Go mono] size: ")
  (set-frame-font (format "Go mono:pixelsize=%d:antialias=true:autohint=true" size) nil frames))

(eval-after-load 'firebelly-theme
  '(custom-theme-set-faces
    'firebelly
    '(font-lock-comment-delimiter-face ((t (:foreground "#505050"))))))

(use-package powerline
  :ensure nil
  :defer)

(defun my-make-frame-function(frame)
  (with-selected-frame frame ;; needed for spacemacs powerline colors to be applied properly
    (when (not (featurep 'spacemacs-dark-theme))
      (require 'spacemacs-dark-theme nil t)
      (when (require 'powerline nil t)
	(powerline-center-theme)))))

(defun my-light-theme ()
  "Switch to Spacemacs light theme."
  (interactive)
  (when (require 'spacemacs-light-theme nil t)
    (enable-theme 'spacemacs-light)
    (disable-theme 'spacemacs-dark)))

(defun my-dark-theme ()
  "Switch to Spacemacs dark theme."
  (interactive)
  (when (require 'spacemacs-dark-theme nil t)
    (enable-theme 'spacemacs-dark)
    (disable-theme 'spacemacs-light)))


(when window-system
  (my-make-frame-function (selected-frame)))

(add-hook 'after-make-frame-functions
	  'my-make-frame-function)

;;; Convenience functions and aliases

(defun am ()
  "Change dictionary to american."
  (interactive)
  (setq ispell-local-dictionary "american"))

(defun pl ()
  "Change dictionary to polish."
  (interactive)
  (setq ispell-local-dictionary "polish"))

(defalias 'fi 'set-frame-font-inconsolata)
(defalias 'fm 'set-frame-font-go-mono)
(defalias 'st 'magit-status)
(defalias 'ir 'ispell-region)
(defalias 'md 'markdown-mode)

;;; Bind keys

(global-set-key "\C-ck" 'compile)
(global-set-key "\C-cq" 'bury-buffer)
(global-set-key "\C-cs" 'shell)

(use-package magit
  :ensure nil
  :bind ("C-c m" . magit-status))

;;; C mode

(setq-default c-basic-offset 8)
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "k&r"))
      company-async-timeout 5		; completion may be slow
      rtags-completions-enabled t)

(use-package rtags
  ;; need to install all three: rc rdm rp for jump to definition to work
  :ensure nil
  :defer
  :config
  (rtags-enable-standard-keybindings nil "C-c R"))

(use-package cmake-ide
  :ensure nil
  :after cc-mode
  :init
  :config
  (cmake-ide-setup))

(defun my-c-c++-mode-hook-fn ()
  (set (make-local-variable 'company-backends) '(company-rtags))
  (company-mode)
  (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "M-,") 'rtags-location-stack-back)
  (local-set-key "\C-i" 'my-indent-or-complete)
  (local-set-key (kbd "<tab>") 'my-indent-or-complete)
  (local-set-key "\C-\M-i" 'my-indent-or-complete))

(add-hook 'c-mode-hook 'my-c-c++-mode-hook-fn)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook-fn)

;;; Lisp and Emacs lisp modes

;; in emacs 25.1: M-. runs xref-find-definitions,  M-, jumps back
(global-set-key (kbd "C-c e l") 'find-library)

;; see https://common-lisp.net/project/slime/doc/html/Loading-Swank-faster.html
;; for how to prepare the core
(setq slime-lisp-implementations `((sbcl ("sbcl" "--core" ,(expand-file-name "~/local/lisp/sbcl.core-for-slime"))))
      slime-default-lisp 'sbcl)

(use-package paredit
  :ensure nil
  :defer)

(use-package rainbow-delimiters
  :ensure nil
  :defer)

(defun my-emacs-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function) 'lisp-indent-function)
  (paredit-mode 1)
  (show-paren-mode 1)
  (rainbow-delimiters-mode 1))

(defun my-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  (paredit-mode 1)
  (show-paren-mode 1)
  (rainbow-delimiters-mode 1))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook-fn)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook-fn)

;;; JS mode

(setq js-indent-level 8)

;;; Go mode

(defun my-indent-or-complete ()
  "Complete or indent if nothing to complete."
  (interactive)
  (if (or (looking-at "\\w")
	  (looking-back "^\\|\s"))
      (indent-for-tab-command)
    (company-complete)))

(defun my-go-electric-brace ()
  "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
  (interactive)
  (if (not (looking-back " "))
      (insert "{")
    (insert "{")
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (insert "}")
      (indent-according-to-mode))))

(defun my-go-list-packages ()
  "Return list of Go packages."
  (split-string
   (with-temp-buffer
     (shell-command "go list ... 2>/dev/null" (current-buffer))
     (buffer-substring-no-properties (point-min) (point-max)))
   "\n"))

(defun my-godoc-package ()
  "Display godoc for given package (with completion)."
  (interactive)
    (godoc (ivy-read "Godoc package: " (my-go-list-packages))))

(use-package flymake-go
  :ensure nil
  :after go-mode
  :init
  (setq flymake-no-changes-timeout 30))

(use-package go-eldoc
  :ensure nil
  :defer)

(use-package company-go
  :ensure nil
  :defer)

(use-package go-guru
  :ensure nil
  :defer)

(use-package go-mode
  :ensure nil
  :init
  (setq gofmt-command "goimports"     ; use goimports instead of gofmt
	go-fontify-function-calls nil ; fontifing names of called functions is too much for me
	company-idle-delay nil)
  :bind
  (:map go-mode-map
   ("M-." . go-guru-definition)
   ("M-*" . xref-pop-marker-stack) ; it is already bound to M-, in emacs 25.1
   ("C-c d" . godoc-at-point)
   ("C-c g" . godoc)
   ("C-c h" . go-guru-hl-identifier)
   ("C-c P" . my-godoc-package)
   ("{" . my-go-electric-brace)
   ("C-i" . my-indent-or-complete)
   ("C-M-i" . my-indent-or-complete))
  :config
  ;; run gofmt/goimports when saving the file
  (add-hook 'before-save-hook 'gofmt-before-save)

  (defun my-go-mode-hook-fn ()
    (go-eldoc-setup)
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)
    (smartparens-mode 1)
    (setq imenu-generic-expression
	  '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
	    ("func" "^func *\\(.*\\) {" 1))))

  (add-hook 'go-mode-hook 'my-go-mode-hook-fn))

;; Go/speedbar integration

(eval-after-load 'speedbar
  '(speedbar-add-supported-extension ".go"))

;;; Python

(use-package company-jedi
  :ensure nil
  :defer)

(defun my-python-mode-hook-fn ()
  (set (make-local-variable 'company-backends) '(company-jedi))
  (company-mode)
  (smartparens-mode 1)
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-*") 'jedi:goto-definition-pop-marker)
  (local-set-key "\C-i" 'my-indent-or-complete))

(add-hook 'python-mode-hook 'my-python-mode-hook-fn)

;;; Yasnippet and abbrev mode

(setq-default abbrev-mode 1)

(use-package yasnippet
  :ensure nil
  :init
  (yas-global-mode 1)
  :bind
  (:map yas-minor-mode-map
	("C-c & t" . yas-describe-tables)
	("C-c & &" . org-mark-ring-goto)))

;;; Org mode

(use-package org-bullets
  :ensure nil
  :defer)

(use-package org
  :ensure nil
  :init
  (setq org-default-notes-file "~/org/notes.org"
	org-highlight-latex-and-related '(latex)
	org-bullets-bullet-list '("●" "○" "✸" "✿")
	org-ellipsis "…"
	org-catch-invisible-edits 'smart
	gnuplot-program "pyxplot")
  (defun my-org-timer-done ()
    (shell-command "echo Timer timed out; date &"))
  :bind
  (("C-c a" . org-agenda)
   ("C-c B" . org-iswitchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :config
  (add-hook 'org-timer-done-hook 'my-org-timer-done)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (require 'ox-beamer))

;;; Set keys from H-a to H-z to switch to buffers from a register from a to z

(defalias 'pr 'point-to-register)

(require 'cl)
(let ((character ?a))
  (while (<= character ?z)
    (let ((func-name-symbol (make-symbol (format "my-switch-to-register-%c" character))))
      (global-set-key (kbd (format "H-%c" character))
		      (eval (list 'defun func-name-symbol '()
				  (format "switch to buffer of register %c" character)
				  '(interactive)
				  `(let ((r (get-register ,character)))
				     (if (and (markerp r) (marker-buffer r))
					 (switch-to-buffer (marker-buffer r))
				       (jump-to-register ,character)))))))
    (incf character)))

;;; Rest

(defun my-eww-scale-adjust ()
  "Slightly bigger font but text shorter than text."
  (interactive)
  (text-scale-adjust 0)
  (text-scale-adjust 1)
  (eww-toggle-fonts)
  (split-window-right)
  (eww-toggle-fonts)
  (other-window 1)
  (sleep-for 1)
  (delete-window))

;;; Use separate custom file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
