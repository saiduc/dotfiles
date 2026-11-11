;;; ani-cli.el --- Emacs interface for ani-cli -*- lexical-binding: t; -*-

;;; Sai Pandian
;;; saipandian97@gmail.com

;; An Emacs frontend for ani-cli, providing a follow list,
;; watch history integration, and terminal playback through Ghostel.

(require 'cl-lib)
(require 'tabulated-list)
(require 'transient)
(require 'powerline)
(require 'ghostel)

;; --- Customization ---

(defgroup ani-cli nil
  "Emacs frontend for ani-cli."
  :group 'external)

(defcustom ani-cli-command "ani-cli"
  "Path to the ani-cli executable."
  :type 'string :group 'ani-cli)

(defcustom ani-cli-default-quality "best"
  "Default video quality."
  :type '(choice (const "best") (const "worst")
                 (const "360") (const "480") (const "720") (const "1080"))
  :group 'ani-cli)

(defcustom ani-cli-follows-file
  (expand-file-name "ani-cli-follows.el" user-emacs-directory)
  "File to persist followed anime."
  :type 'file :group 'ani-cli)

(defcustom ani-cli-history-file
  (expand-file-name "ani-cli/ani-hsts"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name ".local/state" "~")))
  "Path to ani-cli's history file."
  :type 'file :group 'ani-cli)

(defcustom ani-cli-use-vlc nil
  "Whether to use VLC for video playback."
  :type 'boolean
  :group 'ani-cli)

;; --- Persistence ---

(defvar ani-cli--follows nil
  "Alist of followed anime. Each entry: (name . plist)
where plist has :mode and :quality.")

(defun ani-cli--save-follows ()
  "Save followed anime to disk."
  (with-temp-file ani-cli-follows-file
    (let ((print-level nil) (print-length nil))
      (prin1 ani-cli--follows (current-buffer)))))

(defun ani-cli--load-follows ()
  "Load followed anime from disk."
  (setq ani-cli--follows
        (when (file-exists-p ani-cli-follows-file)
          (with-temp-buffer
            (insert-file-contents ani-cli-follows-file)
            (read (current-buffer))))))

;; --- History (read from ani-cli's own file) ---

(defun ani-cli--read-history ()
  "Read ani-cli history. Returns alist of (title . last-ep)."
  (when (file-exists-p ani-cli-history-file)
    (with-temp-buffer
      (insert-file-contents ani-cli-history-file)
      (cl-loop for line in (split-string (buffer-string) "\n" t)
               for fields = (split-string line "\t")
               when (>= (length fields) 3)
               collect (cons (nth 2 fields) (nth 0 fields))))))

(defun ani-cli--history-ep (name history)
  "Find last watched episode for NAME in HISTORY.
Does fuzzy matching on the anime name portion."
  (let ((name-down (downcase name)))
    (cl-loop for (title . ep) in history
             when (string-match-p (regexp-quote name-down)
                                  (downcase title))
             return ep)))

;; --- Launching ani-cli ---
(defun ani-cli--run (args)
  "Run ani-cli with ARGS in a Ghostel buffer.
The buffer is killed automatically when ani-cli exits."
  (let ((buf-name "*ani-cli-play*"))
    (when-let ((buf (get-buffer buf-name)))
      (kill-buffer buf))
    (let* ((buf (get-buffer-create buf-name))
           (win (display-buffer-in-side-window
                 buf
                 '((side . bottom)
                   (window-height . 0.2)))))
      ;; Launch ani-cli directly inside Ghostel.
      (ghostel-exec buf ani-cli-command
		    (if ani-cli-use-vlc
			(cons "-v" args)
                      args))

      ;; Preserve the original vterm behaviour.
      (with-current-buffer buf
        (setq-local ghostel-kill-buffer-on-exit t))

      (select-window win))))

(defun ani-cli--parse-name (name)
  "Parse NAME, returning (clean-name . episode-count-or-nil).
Strips trailing \"(13 episodes)\" suffix and extracts the count."
  (if (string-match "\\(.*?\\)\\s-*(\\([0-9]+\\) episodes?)\\s-*$" name)
      (cons (match-string 1 name) (match-string 2 name))
    (cons name nil)))

(defun ani-cli--launch-for-entry (entry)
  "Launch ani-cli for a follow list ENTRY (name . plist)."
  (let* ((name (car (ani-cli--parse-name (car entry))))
         (plist (cdr entry))
         (mode (or (plist-get plist :mode) "sub"))
         (quality (or (plist-get plist :quality) ani-cli-default-quality))
         (args (list "-q" quality "--no-detach" name)))
    (when (string= mode "dub") (push "--dub" args))
    (ani-cli--run args)))

;; --- Faces ---

(defface ani-cli-title-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for anime titles." :group 'ani-cli)

(defface ani-cli-mode-sub-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for SUB entries." :group 'ani-cli)

(defface ani-cli-mode-dub-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for DUB entries." :group 'ani-cli)


;; --- List Mode ---

(defvar ani-cli-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ani-cli-list-open)
    (define-key map (kbd "s")   #'ani-cli-list-search)
    (define-key map (kbd "c")   #'ani-cli-list-continue)
    (define-key map (kbd "f")   #'ani-cli-list-follow)
    (define-key map (kbd "d")   #'ani-cli-list-toggle-dub)
    (define-key map (kbd "Q")   #'ani-cli-list-set-quality)
    (define-key map (kbd "g")   #'ani-cli-list-refresh)
    (define-key map (kbd "x")   #'ani-cli-list-remove)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'ani-cli-dispatch)
    map))

;; --- Table rendering ---

(defun ani-cli--set-header ()
  "Install the custom Powerline header in the current buffer."
  (setq-local header-line-format '(:eval (ani-cli--header-draw)))
  (force-mode-line-update t))

(defun ani-cli--update-header ()
  "Recalculate column widths and initialise the custom header."
  (let* ((fixed-cols (+ 9 10 9 9 10))
         (padding (* 6 (or tabulated-list-padding 2)))
         (title-width (max 20 (- (window-width) fixed-cols padding 2))))
    (setq tabulated-list-format
          (vector (list "Title" title-width t)
                  '("Started" 9 t)
                  '("Episodes" 10 t)
                  '("Watched" 9 t)
                  '("Quality" 9 t)
                  '("Language" 10 t))))
  ;; This creates Tabulated List's normal clickable/sortable header.
  (tabulated-list-init-header)
  ;; Replace it with our Powerline version afterwards.
  (ani-cli--set-header))

(defun ani-cli--header-draw ()
  "Draw a Powerline header, using Elfeed Goodies' separator setting when available."
  (let* ((separator
          (or (and (boundp 'elfeed-goodies/powerline-default-separator)
                   elfeed-goodies/powerline-default-separator)
              (powerline-current-separator)
              'arrow-fade))
         (separator-left
          (intern (format "powerline-%s-%s"
                          separator
                          (car powerline-default-separator-dir))))
	 (faces [powerline-active1 mode-line])
         (pad (or tabulated-list-padding 0))
         (ncols (length tabulated-list-format))
         (segments
          (cl-loop for i from 0 below ncols
                   for col across tabulated-list-format
                   for label = (car col)
                   for width = (cadr col)
                   for face = (aref faces (mod i 2))
                   for previous-face = (aref faces (mod (1- i) 2))
                   for adj-width = (max 1 (1- width))
                   for prefix = (if (= i 0) (make-string pad ?\s) "")
                   for padded = (concat prefix
                                        (format (format "%%-%ds" adj-width) label))
                   append (if (= i 0)
                              (list (powerline-raw padded face 'l))
                            (list (funcall separator-left previous-face face)
                                  (powerline-raw padded face 'l)))))
         (last-face (aref faces (mod (1- ncols) 2))))
    (concat (powerline-render segments)
            (powerline-fill last-face 0))))

(define-derived-mode ani-cli-list-mode tabulated-list-mode "Ani-CLI"
  "Major mode for browsing followed anime."
  (setq tabulated-list-padding 2)
  (ani-cli--update-header))

(defun ani-cli--restore-powerline-header (&rest _args)
  "Keep the custom header when Tabulated List rebuilds its header."
  (when (derived-mode-p 'ani-cli-list-mode)
    (ani-cli--set-header)))

(advice-add 'tabulated-list-init-header :after
            #'ani-cli--restore-powerline-header)

(defun ani-cli--list-entries ()
  "Build tabulated-list entries from follows + history."
  (let ((history (ani-cli--read-history)))
    (mapcar
     (lambda (entry)
       (let* ((name (car entry))
              (plist (cdr entry))
              (parsed (ani-cli--parse-name name))
	      (last-ep (or (ani-cli--history-ep (car parsed) history) "-"))
              (watched (not (string= last-ep "-")))
              (lang (upcase (or (plist-get plist :mode) "sub"))))
         (list name (vector
                     (propertize (car parsed) 'face 'ani-cli-title-face)
                     (propertize (if watched "Yes" "No") 'face
                                 (if watched 'font-lock-constant-face 'font-lock-comment-face))
                     (propertize (or (cdr parsed) "-") 'face 'font-lock-constant-face)
                     (propertize (if watched last-ep "-") 'face 'font-lock-constant-face)
                     (propertize (or (plist-get plist :quality) ani-cli-default-quality)
                                 'face 'font-lock-type-face)
                     (propertize lang 'face
                                 (if (string= lang "DUB") 'ani-cli-mode-dub-face
                                   'ani-cli-mode-sub-face))))))
     ani-cli--follows)))

(defun ani-cli--entry-at-point ()
  "Get the follow entry (name . plist) at point."
  (when-let ((id (tabulated-list-get-id)))
    (assoc id ani-cli--follows)))

(defun ani-cli-list-open ()
  "Launch ani-cli for the anime at point."
  (interactive)
  (if-let ((entry (ani-cli--entry-at-point)))
      (ani-cli--launch-for-entry entry)
    (user-error "No anime at point")))

(defun ani-cli-list-search ()
  "Search for anime via ani-cli in vterm."
  (interactive)
  (ani-cli--run (list "--no-detach" (read-string "Search anime: "))))

(defun ani-cli-list-continue ()
  "Continue watching from ani-cli history."
  (interactive)
  (ani-cli--run (list "--continue" "--no-detach")))

(defun ani-cli-list-follow ()
  "Add an anime to the follow list.
Offers completion from ani-cli's history, or type a new name."
  (interactive)
  (let* ((history (ani-cli--read-history))
         (history-names (mapcar #'car history))
         (existing-names (mapcar #'car ani-cli--follows))
         (candidates (cl-remove-if (lambda (n) (member n existing-names))
                                   history-names))
         (name (completing-read "Follow anime (or type a name): " candidates nil nil))
         (mode (completing-read "Mode [sub]: " '("sub" "dub") nil t nil nil "sub"))
         (quality (completing-read
                   (format "Quality [%s]: " ani-cli-default-quality)
                   '("best" "worst" "360" "480" "720" "1080") nil t nil nil
                   ani-cli-default-quality)))
    (if (assoc name ani-cli--follows)
        (user-error "'%s' is already followed" name)
      (push (cons name (list :mode mode :quality quality)) ani-cli--follows)
      (ani-cli--save-follows)
      (ani-cli-list-refresh)
      (message "Followed: %s" name))))

(defun ani-cli-list-toggle-dub ()
  "Toggle sub/dub for the anime at point."
  (interactive)
  (if-let ((entry (ani-cli--entry-at-point)))
      (let* ((plist (cdr entry))
             (cur (or (plist-get plist :mode) "sub"))
             (new (if (string= cur "sub") "dub" "sub")))
        (plist-put plist :mode new)
        (ani-cli--save-follows)
        (ani-cli-list-refresh)
        (message "%s -> %s" (car entry) new))
    (user-error "No anime at point")))

(defun ani-cli-list-set-quality ()
  "Set quality for the anime at point."
  (interactive)
  (if-let ((entry (ani-cli--entry-at-point)))
      (let* ((plist (cdr entry))
             (new (completing-read "Quality: "
                                   '("best" "worst" "360" "480" "720" "1080")
                                   nil t)))
        (plist-put plist :quality new)
        (ani-cli--save-follows)
        (ani-cli-list-refresh)
        (message "%s -> %s" (car entry) new))
    (user-error "No anime at point")))

(defun ani-cli-list-remove ()
  "Remove the anime at point from follows."
  (interactive)
  (if-let ((entry (ani-cli--entry-at-point)))
      (when (y-or-n-p (format "Unfollow \"%s\"? " (car entry)))
        (setq ani-cli--follows (assoc-delete-all (car entry) ani-cli--follows))
        (ani-cli--save-follows)
        (ani-cli-list-refresh)
        (message "Unfollowed: %s" (car entry)))
    (user-error "No anime at point")))

(defun ani-cli-list-refresh ()
  "Refresh the follow list."
  (interactive)
  (ani-cli--load-follows)
  (ani-cli--update-header)
  (setq tabulated-list-entries (ani-cli--list-entries))
  (tabulated-list-print t)
  (ani-cli--set-header))

;; --- Transient menu ---

(transient-define-prefix ani-cli-dispatch ()
  "Ani-CLI actions."
  [["Play"
    ("RET" "Open at point"     ani-cli-list-open)
    ("s"   "Search"            ani-cli-list-search)
    ("c"   "Continue history"  ani-cli-list-continue)]
   ["Manage"
    ("f"   "Follow anime"      ani-cli-list-follow)
    ("x"   "Unfollow at point" ani-cli-list-remove)
    ("d"   "Toggle sub/dub"    ani-cli-list-toggle-dub)
    ("Q"   "Set quality"       ani-cli-list-set-quality)]
   ["View"
    ("g"   "Refresh"           ani-cli-list-refresh)
    ("q"   "Quit"              quit-window)]])

;; --- Evil support ---

(with-eval-after-load 'evil
  (evil-set-initial-state 'ani-cli-list-mode 'normal)
  (evil-define-key 'normal ani-cli-list-mode-map
    (kbd "RET") #'ani-cli-list-open
    "s"  #'ani-cli-list-search
    "c"  #'ani-cli-list-continue
    "f"  #'ani-cli-list-follow
    "d"  #'ani-cli-list-toggle-dub
    "Q"  #'ani-cli-list-set-quality
    "gr" #'ani-cli-list-refresh
    "x"  #'ani-cli-list-remove
    "q"  #'quit-window
    "?"  #'ani-cli-dispatch))

;; --- Entry Point ---

;;;###autoload
(defun ani-cli ()
  "Open the ani-cli follow list."
  (interactive)
  (ani-cli--load-follows)
  (let ((buf (get-buffer-create "*ani-cli*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ani-cli-list-mode)
        (ani-cli-list-mode))
      (setq tabulated-list-entries (ani-cli--list-entries))
      (tabulated-list-print t)
      (ani-cli--set-header)
      (goto-char (point-min))
      (when (null ani-cli--follows)
        (let ((inhibit-read-only t))
          (insert (propertize "\n  No followed anime yet.\n\n  [f] follow  [s] search  [?] all actions\n"
                              'face 'font-lock-comment-face)))))
    (switch-to-buffer buf)))

(provide 'ani-cli)

;;; ani-cli.el ends here
