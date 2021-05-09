;; disable package.el
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)

;; set gc threshold during normal operation
(defvar default-gc-cons-threshold 1000000)
                                  
;; make garbage collector high at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.9)

;; restore gc threshold after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold default-gc-cons-threshold
			   gc-cons-percentage 0.1)))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set font
(setq frame-inhibit-implied-resize t)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono 14"))

;; Optimisations
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)) 100)
