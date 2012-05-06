(mapcar (lambda (path)
          (add-to-list 'exec-path path))
        (split-string (getenv "PATH") ":"))

(require 'cl)

;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      starter-kit
                      starter-kit-bindings
                      starter-kit-ruby
                      markdown-mode
                      gist
                      go-mode
                      textmate
                      tramp
                      marmalade))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'ido)
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)

(require 'tramp)
(setq tramp-default-method "scp")

(setq gist-view-gist 1)

(require 'textmate)

(setq ispell-program-name "aspell")

(load-theme 'adwaita)
(set-default-font "Droid Sans Mono-14")

;; rbenv
(setq exec-path (cons "~/.rbenv/bin" exec-path))
(setenv "PATH" (concat "~/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons "~/.rbenv/shims" exec-path))
(setenv "PATH" (concat "~/.rbenv/shims:" (getenv "PATH")))

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

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-M-=") 'align-equal-signs)
(global-set-key [C-M-return] 'ns-toggle-fullscreen)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-g") 'gist-buffer)
(global-set-key (kbd "C-c C-p") 'gist-buffer-private)

(server-start)
