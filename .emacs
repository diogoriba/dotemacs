(require 'bytecomp)
(require 'find-lisp)
(defconst emacs-root (getenv "HOME"))
(defconst emacs-elisp-root (concat emacs-root "/.elisp"))
(defconst emacs-config-file (concat emacs-root ".emacs"))
(defconst emacs-config-loader (concat emacs-root "diogoriba.config.el"))

(defvar config-files (append (list (concat emacs-root "/" emacs-config-file) (concat emacs-root "/" emacs-config-loader)) (find-lisp-find-files emacs-elisp-root "\\.el$")))

(defun files-need-compiling (files)
  (interactive)
  (let ((list ()))
	(dolist (f files list)
	  (let ((compiled-f (byte-compile-dest-file f)))
		(if (or
			 (not (file-exists-p compiled-f)) ;No compiled version of this file
			 (file-newer-than-file-p f compiled-f) ;Outdated compiled version of this file
			 (equal (nth 4 (file-attributes f)) (list 0 0))) ;File has never been accessed
			(push f list)
		  )
		)
	  )
	)
  )

(setq files-to-be-compiled
  (files-need-compiling config-files))

(defun compile-lisp-files ()
  (dolist (f files-to-be-compiled) (byte-compile-file f))
  )
(add-hook 'kill-emacs-hook 'compile-lisp-files)

(load-file emacs-config-loader)
