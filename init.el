;;; Below are fragments from my Emacs configuration file
;;; (`~/.emacs.d/init.el`).

;;; <!--more-->

;;; The newest version of this config is available from a [github
;;; repo](https://github.com/lukpank/.emacs.d).

;;; I recommend watching the following video by BuildFunThings called
;;; [My GNU Emacs configuration for programming](https://www.youtube.com/watch?v=I28jFkpN5Zk).

;;; If you put this config on a new machine with no Emacs packages
;;; installed then you will be asked if you want automatically install
;;; all packages used below.

;;; Later, if you add new packages you can temporary set
;;; `use-package-always-ensure` below to `t` start Emacs and then set
;;; it back to `nil` (or always have it set to `t`, if you prefer).

;;; **Note** that for Emacs 25 you first need to install new ELPA
;;; signing key as described
;;; [here](https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html).


;;; Basic settings
;;; --------------


;;; ### Directory with local Emacs lisp files ###

;;; I add a directory to the lisp search path where I can add my own
;;; lisp code and (now less often) downloaded lisp code which is not
;;; available through [MELPA](https://melpa.org). Then I initialize
;;; secure downloading from GNU and MELPA archives of Emacs packages.


(let ((path (expand-file-name "~/.emacs.d/lisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))


;;; ### Add MELPA package list ###

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

;;; This also turns on checking TLS certificates (in both possible
;;; modes) with `tls-program` set to only the first value from the
;;; default value (for more info see [Your Text Editor Is
;;; Malware](https://glyph.twistedmatrix.com/2015/11/editor-malware.html)).

;;; Now you can list available packages by running `M-x list-packages`.
;;; Mark packages you want to install by pressing `i` and later press `x`
;;; to install all marked packages (the necessary dependencies will be
;;; installed automatically).

;;; Now, load [use-package](https://github.com/jwiegley/use-package/)
;;; package or propose automatic installation if it is not yet
;;; installed.

(unless (require 'use-package nil t)
  (if (not (yes-or-no-p (concat "Refresh packages, install use-package and"
				" other packages used by init file? ")))
      (error "you need to install use-package first")
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t)))

;;; After loading `use-package` we can use it to configure other
;;; packages.


;;; ### Workaround for security vulnerability in Emacs >= 21.1 and < 25.3 ###
;;;
;;;  See [Changes in Emacs 25.3](https://www.gnu.org/software/emacs/news/NEWS.25.3)

(eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))


;;; Productivity
;;; ------------


;;; ### Key discoverability ###

;;; If you type a prefix key (such as `C-x r`) and wait some time then
;;; display window with keys that can follow.

(use-package which-key
  :config
  (which-key-mode))

;;; List of personal key bindings

(global-set-key (kbd "C-c h b") 'describe-personal-keybindings)


;;; ### More efficient buffer/file selection ###

(setq recentf-max-saved-items 100)

(global-set-key "\C-cq" #'bury-buffer)

(use-package flx
  :after ivy)

(use-package counsel
  :init
  (setq ivy-use-virtual-buffers t
	ivy-re-builders-alist
	'((counsel-git-grep . ivy--regex-plus)
	  (counsel-rg . ivy--regex-plus)
	  (swiper . ivy--regex-plus)
	  (swiper-all . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  :config
  (counsel-mode 1)
  :bind
  (("C-c E" . counsel-flycheck)
   ("C-c f" . counsel-fzf)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c L" . counsel-locate)
   ("C-c o" . counsel-outline)
   ("C-c r" . counsel-rg)
   ("C-c R" . counsel-register)
   ("C-c T" . counsel-load-theme)))

;; Install [ripgrep](https://github.com/BurntSushi/ripgrep) (rg) for
;; `counsel-rg` to work properly.


;;; File explorer sidebar

(use-package treemacs
  :bind
  (("C-c t" . treemacs)
   ("s-a" . treemacs)))

;;; Cycle through buffers' history

(use-package buffer-flip
  :bind
  (("H-f" . buffer-flip)
   :map buffer-flip-map
   ("H-f" . buffer-flip-forward)
   ("H-F" . buffer-flip-backward)
   ("C-g" . buffer-flip-abort)))


;;; ### Window selection enhancements ###

(use-package ace-window
  :init
  (setq aw-scope 'frame ; limit to single frame
	aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

(use-package windmove
  :demand
  :bind
  (("H-w j" . windmove-down)
   ("H-w k" . windmove-up)
   ("H-w h" . windmove-left)
   ("H-w l" . windmove-right))
  :config
  (windmove-default-keybindings))

(use-package windswap
  :demand
  :bind
  (("H-w J" . windswap-down)
   ("H-w K" . windswap-up)
   ("H-w H" . windswap-left)
   ("H-w L" . windswap-right))
  :config
  (windswap-default-keybindings))

;;; Allow for Undo/Redo of window manipulations (such as `C-x 1`)

(winner-mode 1)


;;; ### In buffer movement enhancements ###

;;; Improved in buffer search

(use-package swiper
  :bind
  (("C-s" . swiper)
   ("C-S-s" . swiper-thing-at-point)))

;;; Type substring and wait to select one of its visible occurrences
;;; (even in other windows) with a single or two letters.

(use-package avy
  :bind
  ("H-." . avy-goto-char-timer))

;;; Bind key `o` to selection of a link in help or info buffers by a
;;; single or two letters.

(use-package ace-link
  :config
  (ace-link-setup-default))

;;; Select from visible errors by a single letter

(use-package avy-flycheck
  :bind
  ("C-c '" . avy-flycheck-goto-error))

;;; Go to last change in the current buffer

(use-package goto-chg
  :bind
  ("C-c G" . goto-last-change))


;;; ### Editing enhancements ###

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
  :hook (prog-mode . ws-butler-mode))


;;; ### Spell checking ###

(setq ispell-dictionary "american")

(defun my-american-dict ()
  "Change dictionary to american."
  (interactive)
  (setq ispell-local-dictionary "american")
  (flyspell-mode 1)
  (flyspell-buffer))

(defun my-polish-dict ()
  "Change dictionary to polish."
  (interactive)
  (setq ispell-local-dictionary "polish")
  (flyspell-mode 1)
  (flyspell-buffer))

(defalias 'ir #'ispell-region)


;;; ### Shell and terminal ###

(use-package shell-pop
  :init
  (setq shell-pop-full-span t)
  :bind (("C-c s" . shell-pop)))

(use-package vterm
  :defer)

(use-package vterm-toggle
  :bind (("H-z" . vterm-toggle)
	 ("H-F" . vterm-toggle-forward)
	 ("H-B" . vterm-toggle-backward)))


;;; ### Git ###

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package git-commit
  :hook (git-commit-mode . my-american-dict))

(use-package git-messenger
  :bind ("C-x G" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
	git-messenger:use-magit-popup t))


;;; ### Switching buffers ###

;;; Bind keys from `s-s a` to `s-s z` to switch to buffers from a
;;; register from `a` to `z`.

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

(dolist (character (number-sequence ?a ?z))
  (define-key my-switch-to-register-map
    (char-to-string character) #'my-switch-to-register))

(global-set-key (kbd "H-r") my-switch-to-register-map)


;;; ### Yasnippet and abbrev mode ###

(setq-default abbrev-mode 1)

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :bind
  (:map yas-minor-mode-map
	("C-c & t" . yas-describe-tables)
	("C-c & &" . org-mark-ring-goto)))

(use-package yasnippet-snippets
  :defer)

(use-package ivy-yasnippet
  :bind
  (("C-c y" . ivy-yasnippet)))


;;; Programming languages
;;; ---------------------


;;; ### Common settings for programming modes ###

;;; For modes using [company](https://company-mode.github.io/) for tab
;;; completion add

(use-package company
  :init
  (setq company-idle-delay nil  ; avoid auto completion popup, use TAB
				; to show it
	company-tooltip-align-annotations t)
  :hook (after-init . global-company-mode)
  :bind
  (:map prog-mode-map
	("C-i" . company-indent-or-complete-common)
	("C-M-i" . counsel-company)))

;;; For modes that also use Language Server Protocol from
;;; [lsp-mode](https://github.com/emacs-lsp/lsp-mode) add

(use-package company-lsp
  :defer)

(use-package lsp-mode
  :commands lsp
  ;; reformat code and add missing (or remove old) imports
  :hook ((before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports))
  :bind (("C-c d" . lsp-describe-thing-at-point)
	 ("C-c e n" . flymake-goto-next-error)
	 ("C-c e p" . flymake-goto-prev-error)
	 ("C-c e r" . lsp-find-references)
	 ("C-c e R" . lsp-rename)
	 ("C-c e i" . lsp-find-implementation)
	 ("C-c e t" . lsp-find-type-definition)))

;;; Compilation

(global-set-key "\C-ck" #'compile)


;;; [my common settings for programming modes]: #common-settings-for-programming-modes


;;; ### C and C++ ###

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
;;;    `libclang` development package (package `libclang-dev` or may be newer
;;;    `libclang-X-dev` under Debian) which is required by `rtags`.

;;; 2. Under Emacs having `rtags` package installed press `M-:` and
;;;    evaluate expression `(require 'rtags)` and then press `M-:` and
;;;    evaluate expression `(rtags-install nil
;;;    '("-DRTAGS_NO_ELISP_FILES=true"))` (should work if you have
;;;    `llvm-config` or `llvm-config-X` in your `PATH`) and wait until
;;;    it compiles to 100%.  The `rtags-install` command needs to be
;;;    rerun if you install newer `rtags` from MELPA and the below
;;;    commands do not work complaining about protocol mismatch.

;;; 3. Usefull `rtags` functions (use `C-c R C-h` to see these and other key bindings)

;;;    | Key       | Function
;;;    |-----------|----------
;;;    | `C-c R .` | `rtags-find-symbol-at-point`
;;;    | `C-c R [` | `rtags-location-stack-back`
;;;    | `C-c R ,` | `rtags-find-references-at-point`
;;;    | `C-c R /` | `rtags-find-all-references-at-point`
;;;    |           | `rtags-find-references-current-file`
;;;    |           | `rtags-find-references-current-dir`
;;;    | `C-c R v` | `rtags-find-virtuals-at-point`
;;;    | `C-c R ;` | `rtags-find-file` (in the current project no metter in which directory)


;;; ### Lisp and Emacs lisp ###

;; in emacs 25.1: M-. runs xref-find-definitions,  M-, jumps back
(global-set-key (kbd "C-c e l") #'find-library)

(setq slime-lisp-implementations '((sbcl ("sbcl")))
      slime-default-lisp 'sbcl
      slime-contribs '(slime-fancy))

(let ((path "/usr/local/share/doc/HyperSpec/"))
  (if (file-accessible-directory-p path)
      (setq common-lisp-hyperspec-root (concat "file://" path))))

(use-package paredit
  :hook (eval-expression-minibuffer-setup . paredit-mode))

(use-package paren-face
  :defer)

(defun my-emacs-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function) #'lisp-indent-function)
  (paredit-mode 1)
  (local-set-key (kbd "C-c S") (global-key-binding (kbd "M-s")))
  (local-set-key (kbd "C-c C-z")
		 (lambda () (interactive) (switch-to-buffer "*scratch*")))
  (local-set-key (kbd "C-M-i") #'counsel-company)
  (show-paren-mode 1)
  (paren-face-mode))

(use-package slime-company
  :defer)

(use-package slime
  :demand
  :config
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


;;; ### JavaScript ###

(setq js-indent-level 8)


;;; <span id="go"></span>
;;; ###  Go (golang) ###

;;; <div class="warning">
;;; This is my new Go setup (partially) based on <a
;;; href="https://github.com/golang/tools/blob/master/gopls/README.md">gopls</a> (which is
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
;;; with adding of missing imports.

;;; It is quite long as I define two interactive functions:

;;; 1. `my-go-electric-brace` which is bind to `{` key and inserts an
;;; indented pair of braces (if previous character is a space,
;;; otherwise it inserts single opening brace),

;;; 2. `my-godoc-package` which is bind to `C-c P` key and display
;;; documentation for a package choosen from a list of installed
;;; packages.

;;; [go-mode]: https://github.com/dominikh/go-mode.el

;;; For **tab completion** and **lsp** support also add [my common
;;; settings for programming modes].

(defun my-go-electric-brace ()
  "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
  (interactive)
  (insert "{")
  (when (looking-back " {")
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (insert "}")
      (indent-according-to-mode))))

(defun my-godoc-package ()
  "Display godoc for given package (with completion)."
  (interactive)
  (godoc (ivy-read "Package: " (go-packages) :require-match t)))

(use-package go-guru
  :after go-mode)

(use-package go-mode
  :init
  (setq go-fontify-function-calls nil)  ; fontifing names of called
					; functions is too much for me
  :bind
  (:map go-mode-map
	("C-c e g" . godoc)
	("C-c P" . my-godoc-package)
	("{" . my-go-electric-brace))
  :hook ((go-mode . lsp)
	 (go-mode . smartparens-mode)))

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

;;; 2. Install Go compiler. Under Debian you may install `golang` package
;;;    (but in Debian 10 Buster it is 1.11 compared to the current 1.13,
;;;    so you may consider
;;;    [downloading the current version of Go](https://golang.org/dl/)). Otherwise
;;;    search for the package for your system or
;;;    see [Getting started](https://golang.org/doc/install).

;;; 3. Install [gopls](https://github.com/golang/tools/blob/master/gopls/README.md) with
;;;    (but in your home directory, not inside some Go module)

;;;    ```
;;;    $ GO111MODULE=on go get golang.org/x/tools/gopls@latest
;;;    ```

;;; 4. Install [guru](https://godoc.org/golang.org/x/tools/cmd/guru)
;;;    with

;;;    ```
;;;    $ go get -u golang.org/x/tools/cmd/guru
;;;    ```

;;; 5. Add your `$GOPATH/bin` to your `PATH` environment variable (or
;;;    copy the `gopls` and `guru` executables from
;;;    `$GOPATH/bin` to some directory which is in your `PATH`).

;;; See also
;;; [Go, pls stop breaking my editor - GopherCon SG 2019](https://www.youtube.com/watch?v=gZ7N3HulAb0)
;;; and [Writing Go in Emacs](http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/)
;;; for more info.

;;; [company-lsp]: https://melpa.org/#/company-lsp
;;; [go-guru]: https://melpa.org/#/go-guru


;;; {{old-go.el}}


;;; ### Python ###

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


;;; ### Rust ###

;;; If you have Emacs 26 than you need [Rustup] and using Rustup you
;;; should install components [rls](https://github.com/rust-lang/rls)
;;; and `rust-src` with the command
;;;
;;; ```
;;; $ rustup component add rls rust-src
;;; ```
;;;
;;; For **tab completion** and **lsp** support add [my common settings
;;; for programming modes] and then add

;;; [Rustup]: https://www.rust-lang.org/learn/get-started

(when (>= emacs-major-version 26)
  (defun my-rustic-mode-hook-fn ()
    "needed for lsp-format-buffer to indent with 4 spaces"
    (setq tab-width 4
	  indent-tabs-mode nil))

  (use-package rustic
    :init
    ;; to use rustic-mode even if rust-mode also installed
    (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
    :hook (rustic-mode . my-rustic-mode-hook-fn)))

;;; But if you have Emacs older than 26 than you should install
;;; [Rustup] and [racer](https://github.com/racer-rust/racer) and with
;;; Rustup install component `rust-src` with the command
;;;
;;; ```
;;; $ rustup component add rust-src
;;; ```
;;;
;;; for **tab completion** add [my common settings for programming
;;; modes] and then add

(when (< emacs-major-version 26)
  (use-package cargo
    :defer)

  (use-package racer
    :defer)

  (use-package rust-mode
    :init
    (setq rust-format-on-save t)
    :hook ((rust-mode . company-mode)
	   (rust-mode . cargo-minor-mode)
	   (rust-mode . racer-mode)
	   (rust-mode . eldoc-mode))))


;;; ### Meson build system ###

(use-package meson-mode
  :defer
  :init
  (setq meson-indent-basic 4))


;;; ### Vala ###

(use-package lsp-mode
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(vala-mode . "vala"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
				     '("vala-language-server"))
                    :major-modes '(vala-mode)
                    :server-id 'vala-ls)))

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
  :hook (vala-mode . my-vala-mode-hook-fn))


;;; ### Dart ###

;;; NOTE: `pub` and `dart` must be in `PATH` for lsp to start in
;;; dart-mode.

;;; For **tab completion** and **lsp** support also add [my common
;;; settings for programming modes].

(use-package dart-mode
  :init
  (setq lsp-dart-analysis-sdk-dir "~/local/flutter/bin/cache/dart-sdk/")
  :hook ((dart-mode . smartparens-mode)
	 (dart-mode . lsp)))

(use-package flutter
  :after dart-mode
  :init
  (setq flutter-sdk-path "~/local/flutter/")
  :bind
  (:map dart-mode-map
        ("C-M-x" . #'flutter-run-or-hot-reload)))


;;; ### PHP ###

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


;;; ### TypeScript ###

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
  :after typescript-mode
  :hook ((before-save . tide-format-before-save)
	 (typescript-mode . my-setup-tide-mode)))

(use-package ng2-mode
  :defer)


;;; Other modes
;;; -----------


;;; ### Web mode ###

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


;;; ### CSS ###

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))


;;; ### Org mode ###

(use-package org
  :init
  (setq org-default-notes-file "~/org/notes.org"
	org-highlight-latex-and-related '(latex)
	org-hide-leading-stars t
	org-ellipsis "…"
	org-catch-invisible-edits 'smart
	gnuplot-program "pyxplot")
  (defun my-org-timer-done ()
    (shell-command "echo Timer timed out; date &"))
  :bind
  (("C-c A" . org-agenda)
   ("C-c B" . org-switchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :hook ((org-timer-done . my-org-timer-done))
  :config
  (require 'ox-beamer))


;;; ### Search engines ###

(use-package engine-mode
  :config
  (engine-mode t)
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"))


;;; ### WWW browsers ###

(setq my-browsers
      '(("Firefox" . browse-url-firefox)
	("Chromium" . browse-url-chromium)
	("EWW" . eww-browse-url)))

(defun my-browse-url (&rest args)
  "Select the prefered browser from a menu before opening the URL."
  (interactive)
  (let ((browser (ivy-read "WWW browser: " my-browsers :require-match t)))
    (apply (cdr (assoc browser my-browsers)) args)))

(setq browse-url-browser-function #'my-browse-url)

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


;;; ### Mail ###

(use-package notmuch
  :defer
  :init
  (setq notmuch-search-oldest-first nil))


;;; ### API documentation ###

(when (require 'devdocs-lookup nil t)
  (devdocs-setup))


;;; Appearance and custom file
;;; --------------------------


;;; ### Minimalistic look ###

(setq inhibit-startup-screen t
      frame-title-format "%b"
      ediff-window-setup-function #'ediff-setup-windows-plain)
(add-to-list 'global-mode-string
	     `(t (,(user-login-name) "@" system-name " ")) t)
(set-scroll-bar-mode 'right)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)


;;; ### Easy switching between some fonts ###

(setq my-font-list '("Fantasque Sans Mono" "Go mono" "IBM 3270"
		     "Inconsolata" "Iosevka" "Monofur" "Monoid" "mononoki"))

(defun my-set-frame-font (font-name size &optional frames)
  "Set font to one of the fonts from `my-font-list'
Argument FRAMES has the same meaning as for `set-frame-font'"
  (interactive
   (list (ivy-read "Font name: " my-font-list)
	 (read-number "Font size: ")))
  (set-frame-font
   (format "%s:pixelsize=%d:antialias=true:autohint=true" font-name size)
   nil frames))

(global-set-key (kbd "C-c F") #'my-set-frame-font)


;;; ### Toggle between dark and light themes with a key ###

;;; Toggle between [my own dark and light
;;; themes](https://lupan.pl/lupan-themes/) if available in
;;; `~/.emacs.d/lupan-themes` otherwise falls back to another themes.

(let ((path (expand-file-name "~/.emacs.d/lupan-themes")))
  (when (file-accessible-directory-p path)
    (add-to-list 'load-path path t)))

(if (require 'lupan-themes nil t)
    (setq my-dark-theme 'lupan-dark
	  my-light-theme 'lupan-light)
  (setq my-dark-theme 'apropospriate-dark
	my-light-theme 'apropospriate-light))

(defun my-select-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun my-toggle-theme ()
  "Toggle between dark and light themes"
  (interactive)
  (let ((dark-p (custom-theme-enabled-p my-dark-theme)))
    (my-select-theme (if dark-p my-light-theme my-dark-theme))))

(global-set-key (kbd "C-S-<f6>") #'my-toggle-theme)

(use-package apropospriate-theme
  :defer)

(if (or (require 'lupan-themes nil t)
	(require 'apropospriate-theme nil t))
    (my-toggle-theme))

(defun my-frame-setup-fn (&optional frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg"
			 (or frame (selected-frame)))))

(add-hook 'after-make-frame-functions #'my-frame-setup-fn)
(add-hook 'window-setup-hook #'my-frame-setup-fn)


;;; ### Hyper bindings

(dolist (k '(("H-1" . "C-x 1")
	     ("H-2" . "C-x 2")
	     ("H-3" . "C-x 3")
	     ("H-0" . "C-x 0")
	     ("H-b" . "C-x b")
	     ("H-g" . "C-x g")
	     ("H-k" . "C-x k")
	     ("H-o" . "C-x o")))
  (global-set-key (kbd (car k))
		  (lookup-key (current-global-map) (kbd (cdr k)))))


;;; ### Use separate custom file ###

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))
