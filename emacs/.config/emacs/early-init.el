;; disable package.el
(setq package-enable-at-startup nil)

;; set gc threshold during normal operation
(defvar default-gc-cons-threshold 100000000)

;; make garbage collector high at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; set gc cons lower again after startup
(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            (setq gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)
            (makunbound 'default-file-name-handler-alist)))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set font
(setq frame-inhibit-implied-resize t) 
(add-to-list 'default-frame-alist '(font . "Roboto Mono 14"))

;; Some boilerplate init-time optimisation
(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)
