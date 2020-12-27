(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Theme
(use-package doom-themes
	     :config
	     (setq custom-safe-themes '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570"))
	     (load-theme 'doom-one))

;; Evil bindings
(use-package evil
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global (kbd "C-w 1") 'delete-other-windows)
  (evil-define-key 'insert 'global (kbd "C-e") 'end-of-line)
  (evil-define-key 'insert 'global (kbd "C-a") 'beginning-of-line)
  (evil-define-key 'normal 'global (kbd "<tab>") 'indent-for-tab-command)
  (evil-add-hjkl-bindings help-mode-map completion-list-mode-map
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous))

(use-package evil-magit)

;; EMMS
(use-package emms
  :config
  (if (eq system-type 'gnu/linux)
      (progn (require 'emms-setup)
	     (emms-all)
	     (setq emms-source-file-default-directory "~/msc/collection/")
	     (define-emms-simple-player mine '(file) (emms-player-simple-regexp "mp3" "flac")
	       "mplayer"  "-slave" "-quiet" "-really-quiet" "-novideo")
	     (emms-player-set emms-player-mine 'pause 'emms-player-mine-pause)
	     (emms-player-set emms-player-mine 'resume 'emms-player-mine-pause)
	     (defun emms-player-mine-pause ()
	       (process-send-string
		emms-player-simple-process-name "pause\n"))
	     (setq emms-player-list '(emms-player-mine))
	     (global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
	     (global-set-key (kbd "<XF86AudioStop>") 'emms-browser))))

;; VTerm
(use-package vterm
  :config
  ;; Alt-[0-9] to shells
  (defun open-shell-as-root ()
    (interactive)
    (let ((default-directory "/sudo::/"))
      (shell "s9")))

  (defun my-term (name)
    (interactive)
    (if (get-buffer name)
	(switch-to-buffer name)
      (vterm name)))

  ;; Fix C-p being broken in vterm because evil tries to use it for completion
  (evil-define-key 'insert vterm-mode-map (kbd "C-p") 'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n") 'vterm--self-insert)

  (setq elp-function-list '(ansi-term-in-default-dir))

  (global-set-key (kbd "C-0") (lambda () (interactive) (my-term "s0")))
  (global-set-key (kbd "C-1") (lambda () (interactive) (my-term "s1")))
  (global-set-key (kbd "C-2") (lambda () (interactive) (my-term "s2")))
  (global-set-key (kbd "C-3") (lambda () (interactive) (my-term "s3")))
  (global-set-key (kbd "C-4") (lambda () (interactive) (my-term "s4")))
  (global-set-key (kbd "C-5") (lambda () (interactive) (my-term "s5")))
  (global-set-key (kbd "C-6") (lambda () (interactive) (my-term "s6")))
  (global-set-key (kbd "C-7") (lambda () (interactive) (my-term "s7")))
  (global-set-key (kbd "C-8") (lambda () (interactive) (my-term "s8")))
  (global-set-key (kbd "C-9") 'open-shell-as-root)

  ;; Meta doesn't work on mac for whatever reason
  (if (eq system-type 'darwin)
      (progn (global-set-key (kbd "s-0") (lambda () (interactive) (my-term "s0")))
	     (global-set-key (kbd "s-1") (lambda () (interactive) (my-term "s1")))
	     (global-set-key (kbd "s-2") (lambda () (interactive) (my-term "s2")))
	     (global-set-key (kbd "s-3") (lambda () (interactive) (my-term "s3")))
	     (global-set-key (kbd "s-4") (lambda () (interactive) (my-term "s4")))
	     (global-set-key (kbd "s-5") (lambda () (interactive) (my-term "s5")))
	     (global-set-key (kbd "s-6") (lambda () (interactive) (my-term "s6")))
	     (global-set-key (kbd "s-7") (lambda () (interactive) (my-term "s7")))
	     (global-set-key (kbd "s-8") (lambda () (interactive) (my-term "s8")))
	     (global-set-key (kbd "s-9") 'open-shell-as-root)))

  (setq same-window-regexps '("s[[:digit:]]")))

(use-package magit)

;; Shell-related stuff
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")

;; Wrapping
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'c-mode-common-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)

;; C setup
(setq c-default-style "linux")
(defun my-c-mode-hook ()
  (column-number-mode))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Pipes region through perl -wle <minibuffer input>
(defun perl-pipe-selection (command arg)
  (interactive (list (read-from-minibuffer "perl -wle: " nil nil nil 'shell-command-history)
		     current-prefix-arg))
  (let ((real-command (concat "perl -wle '" command "'")))
    (message real-command)
    (let ((p (if mark-active (region-beginning) 0))
	  (m (if mark-active (region-end) 0)))
      (if (= p m)
	  ;; No active region
	  ;;(message "No active region")
	  ()
	;; Active region
	(if (eq arg nil)
	    (shell-command-on-region p m real-command t t)
	  (shell-command-on-region p m real-command))))))

;; No GUI crap
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Cache TRAMP passwords forever
(setq password-cache-expiry nil)

;; Better buffer switching
(setq pop-up-frames 'graphic-only)
(setq ido-default-buffer-method 'selected-window)

(use-package general :ensure t
	     :config
	     (general-define-key
	      :keymaps '(normal visual emacs)
	      :prefix "SPC"
	      :non-normal-prefix "M-m"
	      ;; Project
	      "p m" '(magit-status :which-key "magit")
	      ;; Buffer
	      "b b" '(switch-to-buffer :which-key "switch")
	      ;; Execute
	      "x e" '(eval-buffer :which-key "eval buffer")))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))
