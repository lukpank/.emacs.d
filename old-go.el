;;; Old Go setup
;;; ------------

;;; <div class="warning">
;;; This is my old Go setup, you may try the <a href="#go">new setup
;;; (above)</a> based (partially) on <a
;;; href="https://github.com/golang/go/wiki/gopls">gopls</a> if the
;;; new one does not work for you try this one.
;;; </div>

;;; I used the following setup for the [go-mode] in my `~/.emacs.d/init.el`. This
;;; adds syntax highlighting but without fontifing names of called
;;; functions, autocompletion and [eldoc] support, auto formatting of the
;;; code on save with adding of missing imports ([goimports]).

;;; It is quite long as I define two interactive functions:

;;; 1. `my-go-electric-brace` which is bind to `{` key and inserts an
;;; indented pair of braces (if previous character is a space,
;;; otherwise it inserts single opening brace),

;;; 2. `my-godoc-package` which is bind to `C-c P` key and display
;;; documentation for a package choosen from a list of installed
;;; packages.

;;; [go-mode]: https://github.com/dominikh/go-mode.el
;;; [eldoc]: http://emacswiki.org/emacs/ElDoc
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

(use-package flycheck
  :defer)

(use-package go-eldoc
  :defer)

(use-package company-go
  :defer)

(use-package go-guru
  :defer)

(use-package go-mode
  :init
  (setq gofmt-command "goimports"     ; use goimports instead of gofmt
	go-fontify-function-calls nil ; fontifing names of called
				      ; functions is too much for me
	company-idle-delay nil)
  :config
  (require 'go-guru)
  :bind
  (:map go-mode-map
   ("M-." . go-guru-definition)
   ("C-c d" . godoc-at-point)
   ("C-c g" . godoc)
   ("C-c h" . go-guru-hl-identifier)
   ("C-c P" . my-godoc-package)
   ("{" . my-go-electric-brace)
   ("C-i" . company-indent-or-complete-common)
   ("C-M-i" . company-indent-or-complete-common))
  :config
  ;; run gofmt/goimports when saving the file
  (add-hook 'before-save-hook #'gofmt-before-save)

  (defun my-go-mode-hook-fn ()
    (go-eldoc-setup)
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)
    (smartparens-mode 1)
    (flycheck-mode 1)
    (setq imenu-generic-expression
	  '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
	    ("func" "^func *\\(.*\\) {" 1))))

  (add-hook 'go-mode-hook #'my-go-mode-hook-fn))

;; Go/speedbar integration

(eval-after-load 'speedbar
  '(speedbar-add-supported-extension ".go"))


;;; Now, in go buffers you can use `M-.` to jump to the definition of
;;; the identifier at point (use `M-,` to jump back as for normal tags
;;; in Emacs 25.1) and you can also use `C-c C-d` for a short
;;; description of the identifier at point (actually it is constantly
;;; displayed in the mode line by enabled Go [eldoc] support). You can
;;; use `C-c d` for a longer description of the identifier at point.

;;; For this to work you have to

;;; 1. Install [from MELPA](#add-melpa-package-list) the following Emacs
;;;    packages: [go-mode], [company-go], [go-eldoc], and [go-guru].

;;; 2. Install Go compiler. Under Debian you install `golang-go` package
;;;    (but in Debian 9 Stretch it is 1.7 while in Debian 8 Jessie it is
;;;    1.3.3 compared to the current 1.11, so you may
;;;    consider
;;;    [downloading the current version of Go](https://golang.org/dl/)). Otherwise
;;;    search for the package for your system or otherwise
;;;    see [Getting started](https://golang.org/doc/install).

;;; 3. Install [godef](https://godoc.org/github.com/rogpeppe/godef)
;;;    with

;;;    ```
;;;    $ go get github.com/rogpeppe/godef
;;;    ```

;;; 4. Install [goimports] which can be installed from Debian package
;;;    `golang-go.tools` or with

;;;    ```
;;;    $ go get golang.org/x/tools/cmd/goimports
;;;    ```

;;; 5. Install [gocode](https://github.com/stamblerre/gocode) (a fork
;;;    with Go modules support) which can be installed with

;;;    ```
;;;    $ go get -u github.com/stamblerre/gocode
;;;    ```

;;; 6. Install [guru](https://godoc.org/golang.org/x/tools/cmd/guru)
;;;    with

;;;    ```
;;;    $ go get golang.org/x/tools/cmd/guru
;;;    ```

;;; 7. Add your `$GOPATH/bin` to your `PATH` environment variable (or copy
;;;    the `godef`, `goimports`, `gocode`, and `guru` executables from
;;;    `$GOPATH/bin` to some directory which is in your `PATH`).

;;; See also
;;; [Writing Go in Emacs](http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/)
;;; for more info.

;;; [company-go]: https://melpa.org/#/company-go
;;; [go-eldoc]: https://melpa.org/#/go-eldoc
;;; [go-guru]: https://melpa.org/#/go-guru
