;; disable package.el
(setq package-enable-at-startup nil)

;; set gc threshold
(setq gc-cons-threshold 402653184)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-to-list 'default-frame-alist '(font . "Roboto Mono 14"))

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)
