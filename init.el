;;; Below are fragments from my Emacs configuration file
;;; (`~/.emacs.d/init.el`).

;;; <!-- more -->

;;; The newest version of this config is available from a [github
;;; repo](https://github.com/lukpank/.emacs.d).

;;; I recommend watching the following video by BuildFunThings called
;;; [My GNU Emacs configuration for programming](https://www.youtube.com/watch?v=I28jFkpN5Zk).

;;; When I want to ensure all of the packages (on a new machine) I set
;;; `use-package-always-ensure` below to `t` start Emacs and then set
;;; it back to `nil`.


;;; Basic settings
;;; ==============


;;; Directory with local Emacs lisp files
;;; -------------------------------------

;;; I add a directory to the lisp search path where I can add my own
;;; lisp code and (now less often) downloaded lisp code which is not
;;; available through [MELPA](https://melpa.org). Then I initialize
;;; secure downloading from GNU and MELPA archives of Emacs packages.


(let ((path (expand-file-name "~/.emacs.d/lisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))


;;; Add MELPA package list
;;; ----------------------

;;; You can install many Emacs packages from [MELPA](https://melpa.org)
;;; repository. To add MELPA to the package list add the following to your
;;; `~/.emacs.d/init.el` file


(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stb" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      gnutls-verify-error t)

(package-initialize)

(setq use-package-always-ensure nil)
(require 'use-package)


;;; This also turns on checking TLS certificates (in both possible modes)
;;; with `tls-program` set to only the first value from the default value
;;; (for more info
;;; see
;;; [Your Text Editor Is Malware](https://glyph.twistedmatrix.com/2015/11/editor-malware.html)).

;;; Now you can list available packages by running `M-x list-packages`.
;;; Mark packages you want to install by pressing `i` and later press `x`
;;; to install all marked packages (the necessary dependencies will be
;;; installed automatically).

;;; Note: The last line above requires installing package named
;;; `use-package` to work.


;;; Other settings
;;; --------------

(setq recentf-max-saved-items 100)


;;; Workaround for security vulnerability in Emacs >= 21.1 and < 25.3
;;; -----------------------------------------------------------------
;;;
;;;  See [Changes in Emacs 25.3](https://www.gnu.org/software/emacs/news/NEWS.25.3)

(eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))


;;; Productivity
;;; ============


;;; More efficient buffer/file selection
;;; ------------------------------------


(use-package helm
  :init
  (setq helm-split-window-default-side 'other)
  (helm-mode 1)
  :config
  (define-key helm-find-files-map
    (kbd "<backtab>") #'helm-select-action)
  (define-key helm-find-files-map
    (kbd "C-i")  #'helm-execute-persistent-action)
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-f" . helm-find-files)
   ("C-c o" . helm-occur)
   ("C-x b" . helm-mini)
   ("C-x r b" . helm-bookmarks)
   ("C-h a" . helm-apropos)
   ("C-h d" . helm-info-at-point)
   ("C-c L" . helm-locate)
   ("C-c r" . helm-resume)
   ("C-c i" . helm-imenu)))

(use-package helm-swoop
  :bind
  (("C-s" . helm-swoop-without-pre-input)
   ("C-S-s" . helm-swoop)))

(use-package helm-descbinds
  :init
  (helm-descbinds-mode))

(use-package helm-git-grep
  :bind
  (("C-c j" . helm-git-grep)
   ("C-c J" . helm-git-grep-at-point)))

(use-package helm-ls-git
  :bind
  (("C-c g" . helm-ls-git-ls)))

(use-package helm-make
  :bind
  (("C-c K" . helm-make)))

(use-package helm-c-yasnippet
  :bind
  (("C-c y" . helm-yas-complete)))

(use-package rg
  :bind
  (("C-c R" . rg)))

(use-package treemacs
  :bind
  (("C-c t" . treemacs)
   ("s-a" . treemacs)))

;;; Cycle through buffers' history

(use-package buffer-flip
  :bind
  (("s-v" . buffer-flip)
   :map buffer-flip-map
   ("s-v" . buffer-flip-forward)
   ("s-V" . buffer-flip-backward)
   ("C-g" . buffer-flip-abort)))

;;; `fzf` and `lcd` for finding files and directories

(defun my-lcd ()
  (interactive)
  (fzf/start default-directory
             (fzf/grep-cmd "lcd" "-l %s")))

(use-package fzf
  :init
  (autoload 'fzf/start "fzf")
  :bind
  (("C-c f" . fzf)
   ("C-c d" . my-lcd)))


;;; Window selection enhancements
;;; -----------------------------


(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package windmove
  :demand
  :bind
  (("C-s-n" . windmove-down)
   ("C-s-p" . windmove-up)
   ("C-s-b" . windmove-left)
   ("C-s-f" . windmove-right))
  :config
  (windmove-default-keybindings))

(use-package eyebrowse
  :config
  (eyebrowse-mode))

;;; Allow for Undo/Redo of window manipulations (such as `C-x 1`)

(winner-mode 1)


;;; In buffer movement enhancements
;;; -------------------------------


;;; Remind of keys than can follow in a key sequence

(use-package which-key
  :config
  (which-key-mode))

;;; Type prefix and wait to select one of the with a single or two letters

(use-package avy
  :bind
  ("C-:" . avy-goto-char-timer))

;;; Bind key `o` to selection of links by a single or two letters

(use-package ace-link
  :config
  (ace-link-setup-default))

;;; Select from visible errors by a single letter

(use-package avy-flycheck
  :bind
  ("C-c '" . avy-flycheck-goto-error))

;;; Go to last change in the buffer

(use-package goto-chg
  :bind
  ("C-c G" . goto-last-change))


;;; Editing enhancements
;;; ---------------------


;;; Context aware insertion of pairs of parenthesis

(use-package smartparens
  :defer)

;;; Edit with multiple cursors

(use-package multiple-cursors
  :bind
  (("C-c n" . mc/mark-next-like-this)
   ("C-c p" . mc/mark-previous-like-this)))

;;; Fix trailing spaces but only in modified lines

(use-package ws-butler
  :defer
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))


;;; Convenience functions, aliases, and key bindings
;;; ------------------------------------------------


;; Convenience functions and aliases

(defun am ()
  "Change dictionary to american."
  (interactive)
  (setq ispell-local-dictionary "american"))

(defun pl ()
  "Change dictionary to polish."
  (interactive)
  (setq ispell-local-dictionary "polish"))

(defalias 'st #'magit-status)
(defalias 'ir #'ispell-region)
(defalias 'md #'markdown-mode)

;; Bind keys

(global-set-key "\C-ck" #'compile)
(global-set-key "\C-cq" #'bury-buffer)

(use-package shell-pop
  :init
  (setq shell-pop-full-span t)
  :bind (("C-c s" . shell-pop)))

(use-package helm-mt
  :bind (("C-c S" . helm-mt)))

(use-package magit
  :bind ("C-c m" . magit-status))

(use-package git-messenger
  :bind ("C-c M" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
	git-messenger:use-magit-popup t))


;;; Switching buffers
;;; -----------------


;;; Set keys from `s-s a` to `s-s z` to switch to buffers from a register from a to z

(defalias 'pr #'point-to-register)

(defun my-switch-to-register ()
  "Switch to buffer given by a register named by last character
of the key binding used to execute this command."
  (interactive)
  (let* ((v (this-command-keys-vector))
	 (c (aref v (1- (length v))))
	 (r (get-register c)))
    (if (and (markerp r) (marker-buffer r))
	(switch-to-buffer (marker-buffer r))
      (jump-to-register c))))

(setq my-switch-to-register-map (make-sparse-keymap))

(let ((character ?a))
  (while (<= character ?z)
    (define-key my-switch-to-register-map
      (format "%c" character) #'my-switch-to-register)
    (setq character (1+ character))))

(global-set-key (kbd "s-s") my-switch-to-register-map)


;;; Programming languages
;;; =====================


;;; C and C++
;;; ---------

;;; The following Emacs packages [from MELPA](#add-melpa-package-list)
;;; need to be installed: [cmake-ide](https://melpa.org/#/cmake-ide),
;;; [company](https://melpa.org/#/company),
;;; [rtags](https://melpa.org/#/rtags), and
;;; [company-rtags](https://melpa.org/#/company-rtags).  Package
;;; `cmake-ide` automatically configures other C++ Emacs packages
;;; (here `company` and `rtags`) when you open a C/C++ file from a
;;; project which uses [cmake](https://cmake.org) to build.

(defconst my-cc-style
  '("k&r"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-mode" my-cc-style)

(setq-default c-basic-offset 8)
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(c++-mode . "my-cc-mode")
			(other . "k&r"))
      company-async-timeout 5		; completion may be slow
      rtags-completions-enabled t)

(use-package rtags
  :defer
  :config
  (rtags-enable-standard-keybindings nil "C-c R"))

(use-package company-rtags
  :defer)

(use-package cmake-ide
  :after cc-mode
  :init
  :config
  (cmake-ide-setup))

(defun my-c-c++-mode-hook-fn ()
  (set (make-local-variable 'company-backends) '(company-rtags))
  (company-mode)
  (local-set-key (kbd "M-.") #'rtags-find-symbol-at-point)
  (local-set-key (kbd "M-,") #'rtags-location-stack-back)
  (local-set-key "\C-i" #'company-indent-or-complete-common)
  (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  (local-set-key "\C-\M-i" #'company-indent-or-complete-common))

(add-hook 'c-mode-hook #'my-c-c++-mode-hook-fn)
(add-hook 'c++-mode-hook #'my-c-c++-mode-hook-fn)

;;; Now:

;;; 1. Install [clang](http://clang.llvm.org/) compiler or more accurately
;;;    `libclang` library (package `libclang-dev` or may be newer
;;;    `libclang-X.Y-dev` under Debian) which is required by `rtags`.

;;; 2. Under Emacs having `rtags` package installed press `M-:` and
;;;    evaluate expression `(require 'rtags)` and then press `M-x` and
;;;    run command `rtags-install` (should work if you have
;;;    `llvm-config` in your path) and wait until it compiles to 100%.
;;;    The `rtags-install` command needs to be rerun if you install
;;;    newer `rtags` from MELPA and the below commands do not work
;;;    complaining about protocol mismatch.

;;; 3. Usefull `rtags` functions (use `C-c r C-h` to see these and other key bindings)

;;;    | Key       | Function
;;;    |-----------|----------
;;;    | `C-c r .` | `rtags-find-symbol-at-point`
;;;    | `C-c r [` | `rtags-location-stack-back`
;;;    | `C-c r ,` | `rtags-find-references-at-point`
;;;    | `C-c r /` | `rtags-find-all-references-at-point`
;;;    |           | `rtags-find-references-current-file`
;;;    |           | `rtags-find-references-current-dir`
;;;    | `C-c r v` | `rtags-find-virtuals-at-point`
;;;    | `C-c r ;` | `rtags-find-file` (in the current project no metter in which directory)


;;; Lisp and Emacs lisp
;;; -------------------


;; in emacs 25.1: M-. runs xref-find-definitions,  M-, jumps back
(global-set-key (kbd "C-c e l") #'find-library)

(setq slime-lisp-implementations '((sbcl ("sbcl")))
      slime-default-lisp 'sbcl
      slime-contribs '(slime-fancy))

(let ((path (expand-file-name "/usr/local/share/doc/HyperSpec/")))
  (if (file-accessible-directory-p path)
      (setq common-lisp-hyperspec-root (concat "file://" path))))

(use-package paredit
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :defer)

(use-package paren-face
  :defer)

(defun my-emacs-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function) #'lisp-indent-function)
  (paredit-mode 1)
  (local-set-key (kbd "C-c S") (global-key-binding (kbd "M-s")))
  (local-set-key (kbd "C-c C-z")
		 (lambda () (interactive) (switch-to-buffer "*scratch*")))
  (show-paren-mode 1)
  (paren-face-mode))

(use-package slime-company
  :defer)

(use-package slime
  :init
  (slime-setup '(slime-fancy slime-company slime-cl-indent)))

(defun my-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function)
       #'common-lisp-indent-function)
  (paredit-mode 1)
  (local-set-key (kbd "C-c S") (global-key-binding (kbd "M-s")))
  (show-paren-mode 1)
  (paren-face-mode)
  (set (make-local-variable 'company-backends) '(company-slime))
  (company-mode)
  (local-set-key "\C-i" #'company-indent-or-complete-common))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook-fn)
(add-hook 'lisp-mode-hook #'my-lisp-mode-hook-fn)


;;; JavaScript
;;; ----------


(setq js-indent-level 8)


;;; Go
;;; --

;;; <div class="warning">
;;; This is my new Go setup (partially) based on <a
;;; href="https://github.com/golang/go/wiki/gopls">gopls</a> (which is
;;; still in alpha stage) and may not work for you if that is the case
;;; try my <a href="#old-go-setup">old Go setup
;;; (below)</a>. <code>gopls</code> supports Go modules outside of
;;; <code>GOPATH</code> (some Go tools, for example <code>guru</code>
;;; <a href="https://github.com/golang/go/issues/24661">does not</a>).
;;; </div>

;;; I use the following setup for the [go-mode] in my
;;; `~/.emacs.d/init.el`. This adds syntax highlighting but without
;;; fontifing names of called functions, autocompletion and info on
;;; called function in mode line, auto formatting of the code on save
;;; with adding of missing imports ([goimports]).

;;; It is quite long as I define two interactive functions:

;;; 1. `my-go-electric-brace` which is bind to `{` key and inserts an
;;; indented pair of braces (if previous character is a space,
;;; otherwise it inserts single opening brace),

;;; 2. `my-godoc-package` which is bind to `C-c P` key and display
;;; documentation for a package choosen from a list of installed
;;; packages.

;;; [go-mode]: https://github.com/dominikh/go-mode.el
;;; [goimports]: https://godoc.org/golang.org/x/tools/cmd/goimports

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
  (godoc (helm :sources (helm-build-sync-source "Go packages"
			  :candidates (my-go-list-packages))
	       :buffer "*godoc packages*")))

(use-package lsp-mode
  :commands lsp)

(use-package company-lsp
  :defer)

(use-package go-guru
  :defer)

(use-package company
  :defer)

(use-package go-mode
  :init
  (setq gofmt-command "goimports"     ; use goimports instead of gofmt
	go-fontify-function-calls nil ; fontifing names of called
				      ; functions is too much for me
	company-idle-delay nil)	; avoid auto completion popup, use TAB
				; to show it
  :bind
  (:map go-mode-map
	("C-c d" . lsp-describe-thing-at-point)
	("C-c g" . godoc)
	("C-c P" . my-godoc-package)
	("{" . my-go-electric-brace)
	("C-i" . company-indent-or-complete-common)
	("C-M-i" . company-indent-or-complete-common)
   )
  :config
  (require 'go-guru)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'smartparens-mode)
  ;; run gofmt/goimports when saving the file
  (add-hook 'before-save-hook #'gofmt-before-save))

;; Go/speedbar integration

(eval-after-load 'speedbar
  '(speedbar-add-supported-extension ".go"))


;;; Now, in go buffers you can use `M-.` to jump to the definition of
;;; the identifier at point (use `M-,` to jump back as for normal tags
;;; in Emacs 25.1) and you can also use `C-c C-d` for a short
;;; description of the identifier at point (actually it is constantly
;;; displayed in the mode line by enabled lsp support). You can
;;; use `C-c d` for a longer description of the identifier at point.

;;; For this to work you have to

;;; 1. After adding above to your emacs config file see how to
;;;    [install from MELPA all required packages](#add-melpa-package-list).
;;;    Or just install [go-mode], [go-guru], [company-lsp].

;;; 2. Install Go compiler. Under Debian you install `golang-go` package
;;;    (but in Debian 9 Stretch it is 1.7 while in Debian 8 Jessie it is
;;;    1.3.3 compared to the current 1.12, so you may
;;;    consider
;;;    [downloading the current version of Go](https://golang.org/dl/)). Otherwise
;;;    search for the package for your system or
;;;    see [Getting started](https://golang.org/doc/install).

;;; 3. Install [gopls](https://github.com/golang/go/wiki/gopls)
;;;    with

;;;    ```
;;;    $ go get -u golang.org/x/tools/cmd/gopls
;;;    ```

;;; 4. Install [goimports] which can be installed from Debian package
;;;    `golang-go.tools` or with

;;;    ```
;;;    $ go get -u golang.org/x/tools/cmd/goimports
;;;    ```

;;; 5. Install [guru](https://godoc.org/golang.org/x/tools/cmd/guru)
;;;    with

;;;    ```
;;;    $ go get -u golang.org/x/tools/cmd/guru
;;;    ```

;;; 6. Add your `$GOPATH/bin` to your `PATH` environment variable (or
;;;    copy the `gopls`, `goimports`, and `guru` executables from
;;;    `$GOPATH/bin` to some directory which is in your `PATH`).

;;; See also
;;; [Go, pls stop breaking my editor - GopherCon SG 2019](https://www.youtube.com/watch?v=gZ7N3HulAb0)
;;; and [Writing Go in Emacs](http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/)
;;; for more info.

;;; [company-lsp]: https://melpa.org/#/company-lsp
;;; [go-guru]: https://melpa.org/#/go-guru


;;; {{old-go.el}}


;;; Python
;;; ------

(use-package company-jedi
  :defer)

(defun my-python-mode-hook-fn ()
  (set (make-local-variable 'company-backends) '(company-jedi))
  (company-mode)
  (smartparens-mode 1)
  (local-set-key (kbd "M-.") #'jedi:goto-definition)
  (local-set-key (kbd "M-,") #'jedi:goto-definition-pop-marker)
  (local-set-key "\C-i" #'company-indent-or-complete-common))

(add-hook 'python-mode-hook #'my-python-mode-hook-fn)


;;; Nim
;;; ---

(use-package lsp-mode
  :commands lsp
  :config
  ;; Register `nimlsp` from https://github.com/PMunch/nimlsp
  (add-to-list 'lsp-language-id-configuration '(nim-mode . "nim"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nimlsp"))
		    :major-modes '(nim-mode)
		    :server-id 'nim-ls)))

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character))

(use-package nim-mode
  :init
  (add-hook 'nim-mode-hook 'highlight-indent-guides-mode))


;;; Rust
;;; ----


(use-package cargo
  :defer)

(use-package racer
  :defer)

(use-package rust-mode
  :init
  (setq company-tooltip-align-annotations t
	rust-format-on-save t)
  :config
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :bind
  (:map rust-mode-map
   ("C-i" . company-indent-or-complete-common)))


;;; Language server with Vala support
;;; ---------------------------------

(use-package lsp-mode
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(vala-mode . "vala"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
				     '("vala-language-server"))
                    :major-modes '(vala-mode)
                    :server-id 'vala-ls)))

(use-package company-lsp
  :defer)


;;; Meson build system
;;; ------------------

(use-package meson-mode
  :init
  (setq meson-indent-basic 4))


;;; Vala
;;; ----

(use-package dumb-jump
  :defer)

(defun my-vala-mode-hook-fn ()
  (setq c-basic-offset 4
	tab-width 8
	indent-tabs-mode nil)
  (set (make-local-variable 'company-backends) '(company-lsp))
  (company-mode 1)
  (dumb-jump-mode)
  (local-set-key "\C-i" #'company-indent-or-complete-common)
  (lsp))

(use-package vala-mode
  :config
  (add-hook 'vala-mode-hook #'my-vala-mode-hook-fn))


;;; Dart
;;; ----


(defun my-dart-goto ()
  (interactive)
  (xref-push-marker-stack)
  (dart-goto))

(use-package dart-mode
  :init
  (let ((path (expand-file-name
	       "~/local/src/flutter/bin/cache/dart-sdk/")))
    (if (file-accessible-directory-p path)
	(setq dart-sdk-path path)))
  (setq dart-enable-analysis-server t)
  :bind
  (:map dart-mode-map
   ("M-." . my-dart-goto)
   ("M-/" . dabbrev-expand)
   ("C-i" . company-indent-or-complete-common)
   ("C-M-i" . company-indent-or-complete-common))
  :config

  (defun my-dart-mode-hook-fn ()
    (smartparens-mode 1)
    (flycheck-mode 1))

  (add-hook 'dart-mode-hook #'my-dart-mode-hook-fn))


;;; PHP
;;; ---


(use-package php-mode
  :defer)

(use-package company-php
  :defer)

(defun my-php-mode-hook-fn()
  (when (require 'company-php nil t)
    (set (make-local-variable 'company-backends)
	 '(company-ac-php-backend))
    (company-mode t)
    (local-set-key (kbd "M-.") #'ac-php-find-symbol-at-point)))

(add-hook 'php-mode-hook #'my-php-mode-hook-fn)


;;; TypeScript
;;; ----------


(defun my-setup-tide-mode ()
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (company-mode 1))

(use-package typescript-mode
  :init
  (setq typescript-indent-level 2)
  :bind
  (:map typescript-mode-map
   ("C-i" . company-indent-or-complete-common)
   ("C-M-i" . company-indent-or-complete-common)))

(use-package tide
  :config
  (add-hook 'before-save-hook #'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'my-setup-tide-mode))

(use-package ng2-mode
  :defer)


;;; Other modes
;;; ===========


;;; Yasnippet and abbrev mode
;;; -------------------------


(setq-default abbrev-mode 1)

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :bind
  (:map yas-minor-mode-map
	("C-c & t" . yas-describe-tables)
	("C-c & &" . org-mark-ring-goto)))


;;; Web mode
;;; --------


(defun my-web-mode-hook-fn()
  (cond
   ((string= web-mode-engine "php")
    (my-php-mode-hook-fn))))

(use-package web-mode
  :init
  (add-hook 'web-mode-hook #'my-web-mode-hook-fn)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  :bind
  (:map web-mode-map
	("C-i" . company-indent-or-complete-common)))


;;; CSS
;;; ---


(use-package rainbow-mode
  :defer)

(add-hook 'css-mode-hook #'rainbow-mode)


;;; Org mode
;;; --------


(use-package org-bullets
  :defer)

(use-package org
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
  (add-hook 'org-timer-done-hook #'my-org-timer-done)
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (require 'ox-beamer))


;;; Search engines
;;; --------------


(use-package engine-mode
  :config
  (engine-mode t)
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"))


;;; EWW browser
;;; -----------

(setq browse-url-browser-function #'eww-browse-url)

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


;;; API documentation
;;; -----------------

(when (require 'devdocs-lookup nil t)
  (devdocs-setup))


;;; Appearance and custom file
;;; ==========================


;;; Appearance
;;; ----------

;;; Minimalistic look

(setq inhibit-startup-screen t
      frame-title-format (list "[" user-login-name "@" (system-name) "] %b")
      ediff-window-setup-function #'ediff-setup-windows-plain)
(set-scroll-bar-mode 'right)
(menu-bar-mode 0)
(tool-bar-mode 0)

;;; Easy switching between some fonts

(setq my-font-list '("Fantasque Sans Mono" "Go mono" "IBM 3270"
		     "Inconsolata" "Monofur" "Monoid" "mononoki"))

(defun my-set-frame-font (font-name size &optional frames)
  "Set font to one of the fonts from `my-font-list'
Argument FRAMES has the same meaning as for `set-frame-font'"
  (interactive
   (list (or (helm :prompt "Font name: "
		   :resume 'noresume
		   :sources (helm-build-sync-source "Fonts"
			      :candidates my-font-list)
		   :buffer "*font selection*")
	     (signal 'quit nil))
	 (read-number "Font size: ")))
  (set-frame-font
   (format "%s:pixelsize=%d:antialias=true:autohint=true" font-name size)
   nil frames))

(global-set-key (kbd "C-c F") #'my-set-frame-font)

;;; Fancy mode line and some themes

(use-package zenburn-theme
  :defer)

(use-package leuven-theme
  :defer)

;;; Easy switching between themes

(defun my-helm-themes-after ()
  (set-face-background 'scroll-bar (face-background 'fringe)))

(use-package helm-themes
  :bind
  (("C-c T" . helm-themes))
  :config
  ;; need to update powerline after changing theme
  (advice-add 'helm-themes :after #'my-helm-themes-after))

;;; Toggle between dark and light themes with a key

(setq my-dark-theme 'zenburn
      my-light-theme 'leuven)

(defun my-toggle-theme ()
  "Toggle between dark and light themes"
  (interactive)
  (let ((dark-p (custom-theme-enabled-p my-dark-theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (if dark-p
	(load-theme my-light-theme t)
      (load-theme my-dark-theme t)))
  (my-helm-themes-after))

(global-set-key (kbd "C-S-<f6>") #'my-toggle-theme)

(my-toggle-theme)

(defun my-frame-setup-fn (&optional frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg"
			 (or frame (selected-frame)))))

(add-hook 'after-make-frame-functions #'my-frame-setup-fn)
(add-hook 'window-setup-hook #'my-frame-setup-fn)

;;; My customization for some used themes

(eval-after-load 'firebelly-theme
  '(custom-theme-set-faces
    'firebelly
    '(font-lock-comment-delimiter-face ((t (:foreground "#505050"))))))

(eval-after-load 'nimbus-theme
  '(custom-theme-set-faces
    'nimbus
    '(region ((t (:background "#505050"))))))


;;; Use separate custom file
;;; ------------------------


(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))
