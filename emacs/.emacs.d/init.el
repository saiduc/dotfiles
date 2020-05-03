;;  $$$$$$\            $$\ $$$$$$$\
;; $$  __$$\           \__|$$  __$$\
;; $$ /  \__| $$$$$$\  $$\ $$ |  $$ |$$\   $$\  $$$$$$$\
;; \$$$$$$\   \____$$\ $$ |$$ |  $$ |$$ |  $$ |$$  _____|
;;  \____$$\  $$$$$$$ |$$ |$$ |  $$ |$$ |  $$ |$$ /
;; $$\   $$ |$$  __$$ |$$ |$$ |  $$ |$$ |  $$ |$$ |
;; \$$$$$$  |\$$$$$$$ |$$ |$$$$$$$  |\$$$$$$  |\$$$$$$$\   Sai Pandian
;;  \______/  \_______|\__|\_______/  \______/  \_______|  github.com/saiduc
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
   (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/init-org.org"))
     (org-babel-tangle nil "~/.emacs.d/init-org.el")
     (byte-compile-file "~/.emacs.d/init-org.el")))
(add-hook 'after-save-hook #'my/tangle-dotfiles)

(load-file "~/.emacs.d/init-org.elc")

(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216
		     gc-cons-percentage 0.1)))

)

;; STUFF EMACS ADDS
;; --------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documents/workstuff/University/Year 3/Semester 2/RevisionSchedule.org"))
 '(package-selected-packages
   '(autopair evil-org doom-modeline helm-mode c-eldoc yasnippet-snippets which-key vterm vimrc-mode use-package spaceline smex projectile perspective pdf-tools org-bullets org markdown-mode jupyter iedit flycheck exec-path-from-shell evil-surround evil-numbers evil-magit evil-commentary evil-collection esup elpy dracula-theme diminish dashboard cython-mode csv-mode counsel conda company-reftex company-auctex auto-package-update auctex-latexmk))
 '(pdf-tools-handle-upgrades nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "black" :foreground "white"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "darkgray" :foreground "white"))))
 '(persp-selected-face ((t (:foreground "#FD7CC5")))))