;;   _____       _ _____             
;;  / ____|     (_)  __ \            
;; | (___   __ _ _| |  | |_   _  ___ 
;;  \___ \ / _` | | |  | | | | |/ __|
;;  ____) | (_| | | |__| | |_| | (__       Sai Pandian
;; |_____/ \__,_|_|_____/ \__,_|\___|      github.com/saiduc
;; ------------------------------------------------------------------------

;; LOAD ORG FILE
;; --------------------------------------
(package-initialize)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(org-agenda-files (quote ("~/.emacs.d/init/init-org.org")))
 '(package-selected-packages
   (quote
    (company-reftex elpy diminish org-bullets auctex-latexmk company-auctex ein conda company-anaconda anaconda-mode pdf-tools smex exec-path-from-shell which-key dashboard company iedit evil-commentary evil-collection autopair spaceline dracula-theme auctex auctex-lua)))
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
 '(company-tooltip-selection ((t (:background "darkgray" :foreground "white")))))
