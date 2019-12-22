;;   _____       _ _____             
;;  / ____|     (_)  __ \            
;; | (___   __ _ _| |  | |_   _  ___ 
;;  \___ \ / _` | | |  | | | | |/ __|
;;  ____) | (_| | | |__| | |_| | (__       Sai Pandian
;; |_____/ \__,_|_|_____/ \__,_|\___|      github.com/saiduc
;; ------------------------------------------------------------------------

(let ((file-name-handler-alist nil))

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; LOAD ORG FILE
;; --------------------------------------

;; This 'compiles' the org init file so it is quicker to load
(defun my/tangle-dotfiles ()
   "If the current file is this file, the code blocks are tangled"
   (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/init/init-org.org"))
     (org-babel-tangle nil "~/.emacs.d/init/init-org.el")
     (byte-compile-file "~/.emacs.d/init/init-org.el")))
(add-hook 'after-save-hook #'my/tangle-dotfiles)

(load-file "~/.emacs.d/init/init-org.elc")

;; SET BY EMACS
;; --------------------------------------
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(company-preview ((t (:foreground "darkgray" :underline t))))
 ;; '(company-preview-common ((t (:inherit company-preview))))
 ;; '(company-tooltip ((t (:background "black" :foreground "white"))))
 ;; '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 ;; '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 ;; '(company-tooltip-selection ((t (:background "darkgray" :foreground "white"))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ido-mode dracula-theme auctex company which-key use-package spaceline smex pdf-tools iedit exec-path-from-shell evil-commentary evil-collection esup elpy ein diminish dashboard conda company-reftex company-auctex autopair auctex-latexmk atom-one-dark-theme)))
 '(pdf-tools-handle-upgrades nil))
;; --------------------------------------

(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216
		     gc-cons-percentage 0.1)))
)
