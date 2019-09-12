;;   _____       _ _____             
;;  / ____|     (_)  __ \            
;; | (___   __ _ _| |  | |_   _  ___ 
;;  \___ \ / _` | | |  | | | | |/ __|
;;  ____) | (_| | | |__| | |_| | (__       Sai Pandian
;; |_____/ \__,_|_|_____/ \__,_|\___|      github.com/saiduc
;; ------------------------------------------------------------------------

;; SETUP PACKAGE INSTALL
;; --------------------------------------
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; LOAD ORG FILE
;; --------------------------------------
(setq gc-cons-threshold 1000000000)
(set-frame-font "Monaco 14" nil t)

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
 '(org-agenda-files (quote ("~/.emacs.d/init/init-org.org")))
 '(package-selected-packages
   (quote
    (elpy diminish org-bullets auctex-latexmk company-auctex ein conda company-anaconda anaconda-mode pdf-tools smex exec-path-from-shell which-key all-the-icons neotree dashboard company iedit evil-commentary evil-collection autopair spaceline dracula-theme auctex auctex-lua)))
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
