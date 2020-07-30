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
(package-initialize)
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


;; BOILERPLATE
;; --------------------------------------
(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216
		     gc-cons-percentage 0.1)))


;; STUFF EMACS ADDS
;; --------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org yasnippet-snippets writeroom-mode which-key vterm vimrc-mode use-package smex projectile perspective pdf-tools org-gcal org-bullets markdown-mode jupyter iedit flycheck-cython exec-path-from-shell evil-surround evil-numbers evil-magit evil-commentary evil-collection ess elpy doom-themes doom-modeline diminish dashboard cython-mode csv-mode counsel conda company-reftex company-c-headers company-auctex autopair auto-package-update auctex-latexmk))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t :height 1.5)))
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(persp-selected-face ((t (:foreground "#FD7CC5")))))

)
