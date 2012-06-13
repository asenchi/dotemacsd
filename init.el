(mapcar (lambda (path)
          (add-to-list 'exec-path path))
        (split-string (getenv "PATH") ":"))

(require 'cl)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      color-theme
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

(require 'color-theme)
(color-theme-molokai)
(set-default-font "Droid Sans Mono-14")

(setq default-frame-alist
      (append
       (list '(width . 120) '(height . 60))
       default-frame-alist))

;; org
(require 'org-install)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "ISSUE(i)" "|" "FIXED(f)")
        (sequence "FEATURE(f)" "TESTS(T)" "DOCS(D)" "|" "COMPLETE(c)")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("ISSUE" . (:foreground "white" :weight bold))
        ("FEATURE" . (:foreground "yellow" :weight bold))))
(org-remember-insinuate)
(setq org-directory "~/Projects/orgfiles")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-remember-templates
      '(("TODO" ?t "* TODO %?\n  %i\n  %a" "~/Projects/orgfiles/todos.org" "Tasks")
        ("NOTES" ?n "* %U %?\n\n  %i\n %a" "~/Projects/orgfiles/notes.org" "Notes")))
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

(setq ruby-deep-indent-paren nil)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-M-=") 'align-equal-signs)
(global-set-key [C-M-return] 'ns-toggle-fullscreen)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-pull)
(global-set-key (kbd "C-c C-p") 'magit-push)
(global-set-key (kbd "C-c C-r") 'org-remember)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(server-start)
