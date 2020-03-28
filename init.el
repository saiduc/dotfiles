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

(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold 16777216
		     gc-cons-percentage 0.1)))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Documents/workstuff/University/Year 3/Semester 2/RevisionSchedule.org")))
 '(package-selected-packages
   (quote
    (dracula-theme autopair org yasnippet-snippets which-key vterm vimrc-mode use-package spaceline smex projectile pdf-tools org-bullets markdown-mode jupyter iedit flycheck exec-path-from-shell evil-surround evil-magit evil-commentary evil-collection esup elpy doom-themes diminish dashboard cython-mode csv-mode counsel conda company-reftex company-irony company-auctex beacon auto-package-update auctex-latexmk)))
 '(pdf-tools-handle-upgrades nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:background "#FF0000"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.2)))))
