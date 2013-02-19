;;; Emacs configuration
;;; - diogoriba

;;; Load paths
(let ((default-directory  emacs-elisp-root))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; Disable version control
(setq vc-handled-backends nil)
(require 'mo-git-blame) ; little extra for git blame, cause it's shit without the little extra
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;; Tramp for editing remote documents
(require 'tramp)
(if (eq system-type 'windows-nt)
	(progn
	  (setq putty-path (concat emacs-root "/putty/"))
	  (setenv "PATH" (concat putty-path ";" (getenv "PATH")))
	  (setq exec-path (cons putty-path exec-path))
	  (setq tramp-default-method "plink")
	  )
;;else
    (setq tramp-default-method "scp")
)

;;; Dired goodies
(require 'dirtree)

;;; Themes/Layout
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default inhibit-startup-message t)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default font-lock-maximum-decoration t)
(setq-default search-highlight t)
(setq-default query-replace-highlight t)
(setq-default require-final-newline t)
(setq-default kill-whole-line t)
(line-number-mode t)
(column-number-mode t)
(menu-bar-mode 0)
;(tool-bar-mode 0)
;(set-scroll-bar-mode 'right)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

(require 'color-theme)
(require 'color-theme-standard)
(require 'color-theme-zenburn)
(require 'color-theme-scintilla)
;;(require 'color-theme-almost-monokai)
;;(require 'color-theme-solarized)

(defun normal-color ()
  (interactive)
  (color-theme-standard)
  (color-theme-zenburn)
  (toggle-truncate-lines 0)
  (if (eq (window-system) 'w32) (set-default-font "Consolas-11"))
  "normal-color"
)

(defun presentation-color ()
  (interactive)
  (color-theme-standard)
  (color-theme-scintilla)
  (visual-line-mode t)
  (if (eq (window-system) 'w32) (set-default-font "Consolas-16"))
  "presentation-color"
)

(if (> (display-color-cells) 16)
  (normal-color)
  ;;else
  (progn
	(toggle-truncate-lines 0)
	(message "Terminal does not support themes")
	)
)

;;; Browsing
(defconst chrome-path "/opt/google/chrome/chrome")
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program chrome-path)

;;; Buffer switching
(autoload 'ibuffer "ibuffer" "List buffers." t)
(iswitchb-mode t)
(global-set-key (kbd "C-M-b") 'ibuffer)
(global-set-key (kbd "C-M-n") 'iswitchb-buffer)

;;; Copy/Paste
(cua-mode t)
(transient-mark-mode t)
(setq cua-keep-region-after-copy t)
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "M-w") 'copy-region-as-kill)
(global-set-key (kbd "C-q") 'yank)
(global-set-key (kbd "M-q") 'yank-pop)
(setq x-select-enable-clipboard t)

;;; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(defun indent-all ()
  "Indent the whole buffer"
  (interactive)
  (indent-region (point-min) (point-max))
)
(global-set-key (kbd "M-<tab>") 'indent-all) ;review binding. m-tab changes window in w32

;;; Text utilities
;; Better open line
(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode)
)
(global-set-key (kbd "C-M-o") 'open-line-above)

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode)
)
(global-set-key (kbd "C-o") 'open-line-below)

;; Highlight matching paranthesis next to cursor
(require 'paren)
(show-paren-mode t)

;; Clear (or cut) all the text in the buffer
(defun clear-buffer ()
  "Kill all text in current buffer"
  (interactive)
  (clipboard-kill-region 1 (point-max))
  (begining-of-buffer)
)

;; Goto line (M-g)
(global-set-key (kbd "M-g") 'goto-line)
;; Replace string (C-S-s)
(global-set-key (kbd "C-S-s") 'replace-string)
(global-set-key (kbd "C-S-M-s") 'replace-regexp)
;; Comment region (C-c c)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
;; Newline+indent when you hit ENTER
;(global-set-key "\C-m" 'newline-and-indent)
;; Other window (C-TAB)
(global-set-key (kbd "C-<tab>") 'other-window)
;; Isearch
(global-set-key (kbd "<f3>") 'isearch-repeat-forward)
;; Reload file
(global-set-key (kbd "<f5>") 'revert-buffer)
;; Clear buffer
(global-set-key (kbd "<f6>") 'clear-buffer)

;;; mode-specific hooks
;; switched from skeleton-pair to autopair due to cleaning capabilities
(require 'autopair)
(autopair-global-mode t)
(setq autopair-autowrap t)

;; Ruby
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(defun my-ruby-mode-hook ()
  (setq tab-width 2)
  (setq indent-tabs-mode t)
  (add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(class\\|module\\|def\\|do\\|{\\)" "\\(end\\|end\\|end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))
  (hs-minor-mode t)
)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; C/C++/C#
(require 'csharp-mode)
(defun my-c-mode-hook ()
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
  (define-key c-mode-map (kbd "<tab>")
    (lambda ()
      (interactive)
      (insert "\t")
      )
    )
  (push ?{ (getf autopair-dont-pair :code))
  (setq parens-require-spaces nil)
  (hs-minor-mode t)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'csharp-mode-hook 'my-c-mode-hook)

;; F#
;(require 'fsharp)
;(add-to-list 'auto-mode-alist '("\\.fs[xs]?$" . fsharp-mode))

;;; XML/HTML
(require 'hl-tags-mode)
(defun my-close-tag-helper ()
  (interactive)
  (sgml-close-tag)
  (backward-sexp)
)

(defun my-close-tag ()
  "Close a XML tag when applicable"
  (interactive)
  (let ((flag nil))
    (save-excursion
      (goto-char (- (point) 2))
      (if (looking-at "/>") (setq flag 1))
    )
    (if (null flag) (my-close-tag-helper))
  )
)

(defun my-xml-mode-hook ()
  (setq tab-width 4)
  (setq sgml-basic-offset 4)
  (setq indent-tabs-mode t)
  (define-key sgml-mode-map ">"
    (lambda ()
      (interactive)
      (insert ">")
      (my-close-tag)
    )
  )
  (setq autopair-dont-activate t)
  (hl-tags-mode t)
)
(add-to-list 'auto-mode-alist '("\\.x[ms]l$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.[xd]?html?$" . sgml-mode))
(add-hook 'sgml-mode-hook 'my-xml-mode-hook)

(require 'zencoding-mode)
(require 'jsm)
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . jsm-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-hook 'jsm-mode-hook
		  (lambda ()
			(imenu-add-menubar-index)
			(hs-minor-mode t)))

;;; Org mode
(require 'org)
(require 'htmlize)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(defun my-org-mode-hook ()
  (setq default-buffer-file-coding-system 'utf-8-with-signature-dos)
)
(add-hook 'org-mode-hook 'my-org-mode-hook)
(setq org-todo-keywords
    '(
		(sequence "TODO(t)" "|" "DONE(d)")
		(sequence "STARTED(s!)" "VERIFY(v)" "BLOCKED(b!)" "|" "DONE(d!)")
		(sequence "|" "CANCELED(c!)" "DELEGATED(l!)")
	)
)
(setq org-directory "~/org/")
(setq org-archive-location (concat org-directory "archive/archive.org"))
(setq org-fast-selection-single-key t)
(setq org-fast-tag-selection-include-todo t)
(setq org-return-follows-link t)
(setq org-log-done 'time)
(setq org-export-html-xml-declaration "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
(setq org-export-html-style-include-scripts nil)
(setq org-export-html-style (concat "<style type=\"text/css\">" (with-temp-buffer (insert-file-contents (concat org-directory "stylesheet.css"))(buffer-string)) "</style>"))
(setq org-agenda-exporter-settings
	'((ps-number-of-columns 2)
    (ps-landscape-mode t)
    (org-agenda-add-entry-text-maxlines 5)
    (htmlize-output-type 'css))
)

(load-library "find-lisp")
(defun reload-org-files ()
  "Repopulate org-agenda-files with the .org files in the org folder"
  (setq org-agenda-files (find-lisp-find-files org-directory "\.org"))
)
(setq org-agenda-start-on-weekday 0)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-ndays 7)

;; Agenda/Remember
(defun custom-org-agenda ()
  "Kill all text in current buffer"
  (interactive)
  (reload-org-files)
  (org-agenda)
)
(global-set-key (kbd "<f4>") 'custom-org-agenda)

;;; Others
;; Shell
(require 'cmd-mode)
(if (eq system-type 'windows-nt)
	(progn
	  (require 'vc-x86-env)
	  (global-set-key (kbd "<f7>") 'vc-x86-env))
;else
    (progn
	  (require 'multi-term)
	  (setq multi-term-program "/bin/bash")
	  (global-set-key (kbd "<f7>") 'multi-term)
	  (defun my-term-mode-hook () 
		(setq autopair-dont-activate t))
	  (add-hook 'term-mode-hook 'my-term-mode-hook))
)
