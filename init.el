;; MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Uses /bin/sh otherwise
(setq explicit-shell-file-name "/bin/bash")

;; Global agenda
(setq org-agenda-files (list "~/org/school.org"
			     "~/org/me.org"))

;; Map C-x C-a to open agenda
(global-set-key (kbd "C-x C-a") 'org-agenda-list)
;; Map C-x t to open todo list
(global-set-key (kbd "C-x t") 'org-todo-list)

;; Add CLOSED timestamp each time a TODO is completed
(setq org-log-done 'time)
;; Save clock history across sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Always show 7 days into the future
(setq org-agenda-start-on-weekday nil)

;; Capture
(setq org-default-notes-file "~/org/notes.org")

(defun org-capture-todo () (interactive) (org-capture nil "t"))
(defun org-capture-todo-exclusive () (interactive) (org-capture-todo) (delete-other-windows))
(global-set-key (kbd "M-g M-r") 'org-capture-todo)

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

;; M-g M-p pipes region through perl -wle <minibuffer input>
(setq shell-file-name "/bin/bash")

(defun my-pipe-selection (command arg)
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

(global-set-key (kbd "M-g M-p") 'my-pipe-selection)

;; Map M-g M-e to eval-buffer
(global-set-key (kbd "M-g M-e") 'eval-buffer)

;; M-g M-i to imenu
(global-set-key (kbd "M-g M-i") 'imenu)
(put 'downcase-region 'disabled nil)

;; Reopen as root
(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x C-r") #'er-sudo-edit)
(put 'narrow-to-region 'disabled nil)

;; Zap until
(global-set-key (kbd "C-z") 'zap-up-to-char)

;; Yank line
(defun yank-line ()
  "Yanks current line in its entirety."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (let ((beg (point)))
      (forward-line 1)
      (copy-region-as-kill beg (point)))))

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (save-excursion
    (yank-line)
    (move-beginning-of-line 1)
    (yank)))

;; Yank paragraph
(defun yank-paragraph ()
  "Yanks current paragraph in its entirety."
  (interactive)
  (save-excursion
    (backward-paragraph)
    (let ((beg (point)))
      (forward-paragraph)
      (copy-region-as-kill beg (point)))))

;; Format function definition/call
(defun format-fun ()
  "Format function definition/call."
  (interactive)
  (save-excursion
    (down-list)
    (merge-lines)
    (backward-up-list)
    (forward-list)
    (previous-line 1)
    (move-end-of-line 1)
    (merge-lines)
    (backward-up-list)
    (let ((beg (point)))
      (forward-list)
      (indent-region beg (point)))))

;; Merges all [ \t\n] after cursor
(defun merge-lines ()
  (let ((beg (point)))
    (skip-chars-forward " \t\n")
    (delete-region beg (point))))

;; [C] Yank statement
(defun yank-statement ()
  "[C] Yank current statement in its entirety."
  (interactive)
  (save-excursion
    (c-beginning-of-statement-1)
    (let ((beg (point)))
      (c-end-of-statement)
      (copy-region-as-kill beg (point)))))

;; No GUI crap
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Alt-[0-9] to shells
(defun open-shell-as-root ()
  (interactive)
  (let ((default-directory "/sudo::/"))
    (shell "s9")))

(defun my-term ()
  (interactive)
  (if (get-buffer "s1")
      (switch-to-buffer "s1")
      ((set-buffer (make-term "new-terminal" "/bin/bash"))
       (term-mode)
       (term-line-mode)
       (switch-to-buffer "*new-terminal*")
       (rename-buffer "s1"))))

(setq elp-function-list '(ansi-term-in-default-dir))

(global-set-key (kbd "M-0") (lambda () (interactive) (shell "s0")))
(global-set-key (kbd "M-1") (lambda () (interactive) (shell "s1")))
(global-set-key (kbd "M-2") (lambda () (interactive) (shell "s2")))
(global-set-key (kbd "M-3") (lambda () (interactive) (shell "s3")))
(global-set-key (kbd "M-4") (lambda () (interactive) (shell "s4")))
(global-set-key (kbd "M-5") (lambda () (interactive) (shell "s5")))
(global-set-key (kbd "M-6") (lambda () (interactive) (shell "s6")))
(global-set-key (kbd "M-7") (lambda () (interactive) (shell "s7")))
(global-set-key (kbd "M-8") (lambda () (interactive) (shell "s8")))
(global-set-key (kbd "M-9") 'open-shell-as-root)

(setq same-window-regexps '("s[[:digit:]]"))

;; Cache TRAMP passwords forever
(setq password-cache-expiry nil)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 256)
(setq recentf-max-saved-items 256)
(global-set-key (kbd "C-x C-y") 'recentf-open-files)
;; Save every 5 mins
(run-at-time nil (* 5 60) 'recentf-save-list)
;; Map to C-x f
(global-set-key (kbd "C-x f") 'recentf-open-files)

;; Backups go in ~/.emacs.d/backups/
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("7451f243a18b4b37cabfec57facc01bd1fe28b00e101e488c61e1eed913d9db9" default))
 '(package-selected-packages '(dracula-theme magit hydra emms)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; EMMS
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/msc/collection/")
(global-set-key (kbd "M-SPC") 'emms-pause)
(emms-add-directory-tree emms-source-file-default-directory)

(global-set-key (kbd "<XF86AudioStop>") 'emms)

;; Ido
(ido-mode 1)

;; Hydras
(global-set-key
 (kbd "M-m")
 (defhydra hydra-main (:exit t)
   "My stuff"
   ("y" yank-line "Yank line")
   (")" blink-matching-open "Show matching (")
   ("p" yank-paragraph "Yank paragraph")
   ("s" yank-statement "[C] Yank statement")
   ("f" format-fun "Format function definition/call")
   ("j" join-line "Join line")
   ("d" duplicate-line "Duplicate line")
   ("M-m" back-to-indentation "Back to indentation")))
(put 'set-goal-column 'disabled nil)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
