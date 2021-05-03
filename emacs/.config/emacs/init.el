;; Author: Sai Pandian
;; Email:  saipandian97@gmail.com

;; BOILERPLATE
;; --------------------------------------
(let ((file-name-handler-alist nil))

;; set gc cons high when using minibuffer
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold default-gc-cons-threshold))
(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)


;; INSTALLING STRAIGHT & USE-PACKAGE
;; --------------------------------------
(setq straight-check-for-modifications nil)
(setq straight-enable-package-integration nil)
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
   (unless (file-exists-p bootstrap-file)
     (with-current-buffer
         (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
       (goto-char (point-max))
       (eval-print-last-sexp)))
   (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)

;; LOAD ORG FILE
;; --------------------------------------
(defun my/tangle-dotfiles ()
   "If the current file is this file, the code blocks are tangled"
   (when (equal (buffer-file-name) (expand-file-name "~/dotfiles/emacs/.config/emacs/init-org.org"))
     (org-babel-tangle nil "~/dotfiles/emacs/.config/emacs/init-compiled/init-org.el")
     (byte-compile-file "~/dotfiles/emacs/.config/emacs/init-compiled/init-org.el")))
(add-hook 'after-save-hook #'my/tangle-dotfiles)

(load-file "~/dotfiles/emacs/.config/emacs/init-compiled/init-org.elc")


;; SET CUSTOM.EL LOCATION
;; --------------------------------------
(setq custom-file (expand-file-name
		   "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

)
