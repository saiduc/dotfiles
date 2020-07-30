;; Author: Sai Pandian
;; Email:  saipandian97@gmail.com

(let ((file-name-handler-alist nil))

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; LOAD ORG FILE
;; --------------------------------------

;; This tangles the org file
(defun my/tangle-dotfiles ()
   "If the current file is this file, the code blocks are tangled"
   (when (equal (buffer-file-name) (expand-file-name "~/dotfiles/emacs/.emacs.d/init-org.org"))
     (org-babel-tangle nil "~/dotfiles/emacs/.emacs.d/init-org.el")
     (byte-compile-file "~/dotfiles/emacs/.emacs.d/init-org.el")))
(add-hook 'after-save-hook #'my/tangle-dotfiles)

(load-file "~/dotfiles/emacs/.emacs.d/init-org.elc")


(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216
		     gc-cons-percentage 0.1)))

;; STUFF EMACS ADDS
;; --------------------------------------
)
