;; Author: Sai Pandian
;; Email:  saipandian97@gmail.com

;; BOILERPLATE
;; --------------------------------------
(let ((file-name-handler-alist nil))

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil
      package--init-file-ensured t)


;; ADDING PACKAGE REPOSITORIES
;; --------------------------------------
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp")


;; INSTALLING USE-PACKAGE
;; --------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; LOAD ORG FILE
;; --------------------------------------
(defun my/tangle-dotfiles ()
   "If the current file is this file, the code blocks are tangled"
   (when (equal (buffer-file-name) (expand-file-name "~/dotfiles/emacs/.emacs.d/init-org.org"))
     (org-babel-tangle nil "~/dotfiles/emacs/.emacs.d/init-org.el")
     (byte-compile-file "~/dotfiles/emacs/.emacs.d/init-org.el")))
(add-hook 'after-save-hook #'my/tangle-dotfiles)

(load-file "~/dotfiles/emacs/.emacs.d/init-org.elc")


;; SET CUSTOM.EL LOCATION
;; --------------------------------------
(setq custom-file (expand-file-name
		   "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; BOILERPLATE
;; --------------------------------------
(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216
		     gc-cons-percentage 0.1)))

)
