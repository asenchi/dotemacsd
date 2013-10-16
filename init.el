;; test
(mapcar (lambda (path)
          (add-to-list 'exec-path path))
        (split-string (getenv "PATH") ":"))

(require 'cl)

;(setenv "ESHELL" (expand-file-name "~/Developer/bin/eshell"))
(setq system-uses-terminfo nil)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages 
  '(color-theme
    color-theme-molokai
    egg
    full-ack
    gist
    go-mode
    magit
    markdown-mode
    marmalade
    melpa
    org
    shell-here
    starter-kit
    starter-kit-bindings
    starter-kit-ruby
    textmate
    tramp
    yaml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'magit)
(require 'ido)
(require 'textmate)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)

(require 'tramp)
(setq tramp-default-method "scp")

(require 'gist)
(setq gist-view-gist 1)

(setq ispell-program-name "~/Developer/bin/aspell")

(require 'shell-here)
(define-key (current-global-map) "\C-c!" 'shell-here)

;; (require 'color-theme)
;; (color-theme-molokai)
(set-default-font "Droid Sans Mono-14")

(setq default-frame-alist
      (append
       (list '(width . 120) '(height . 42))
       default-frame-alist))

;; org
(require 'org-install)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)))
(org-remember-insinuate)
(setq org-directory "~/Projects/orgfiles")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-remember-templates
      '(("TODO" ?t "* TODO %?\n  %i\n  %a" "~/Projects/orgfiles/todos.org" "Tasks")))
(setq org-agenda-files '("~/Projects/orgfiles"))

;; rbenv
(setq exec-path (cons "~/.rbenv/bin" exec-path))
(setenv "PATH" (concat "~/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons "~/.rbenv/shims" exec-path))
(setenv "PATH" (concat "~/.rbenv/shims:" (getenv "PATH")))

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Use zsh for shell
(setq shell-file-name "/bin/zsh")

(setq inhibit-startup-message t)
(menu-bar-mode 1)
(display-time)
(if window-system
    (tool-bar-mode -1))
(show-paren-mode t)
(column-number-mode t)
(setq show-trailing-whitespace t)
(setq insert-time-string-default-format "iso-8601-date")
(setq-default truncate-lines t)
(setq echo-keystrokes 0.1)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq x-select-enable-clipboard t)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file
       (ido-completing-read
        "Find recent file: "
        (mapcar (lambda (path)
                  (replace-regexp-in-string (getenv "HOME") "~" path))
                recentf-list)))
      (message "Opening file...")
    (message "Aborting")))

(setq c-default-style "bsd"
      c-basic-offset 8
      indent-tabs-mode t)

(setq ruby-deep-indent-paren nil)
(defun ruby-end-of-block-or-parens ()
  (interactive)
  (if (looking-at "\[({[\]")
      (forward-list)
    (ruby-end-of-block)))

(defun ruby-beginning-of-block-or-parens ()
  (interactive)
  (let ((char (buffer-substring (1- (point)) (point))))
    (if (looking-at "\[)}]\]")
        (backward-list)
      (ruby-beginning-of-block))))

(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-M-=") 'align-equal-signs)
(global-set-key [C-M-return] 'ns-toggle-fullscreen)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-pull)
(global-set-key (kbd "C-c C-g") 'magit-pull)
(global-set-key (kbd "C-c C-p") 'magit-push)
(global-set-key (kbd "C-c C-r") 'org-remember)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-M-n") 'ruby-end-of-block-or-parens)
(global-set-key (kbd "C-M-p") 'ruby-beginning-of-block-or-parens)

(server-start)
