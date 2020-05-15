;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")
(message "Loading Emacs...done (%.3fs)"
         (float-time (time-subtract before-user-init-time
                                    before-init-time)))
(message "Loading %s..." user-init-file)

(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")
(setq load-prefer-newer t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require  'borg)
(borg-initialize)

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(setq auto-compile-display-buffer               nil)
(setq auto-compile-mode-line-counter            t)
(setq auto-compile-source-recreate-deletes-dest t)
(setq auto-compile-toggle-deletes-nonlib-dest   t)
(setq auto-compile-update-autoloads             t)

(require 'epkg)
(setq epkg-repository
      (expand-file-name "var/epkgs/" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)
(or (server-running-p) (server-mode))

(require 'dash)
(dash-enable-font-lock)

(require 'diff-hl)
(setq diff-hl-draw-borders nil)
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)

(require 'diff-mode)
(when (>= emacs-major-version 27)
  (set-face-attribute 'diff-refine-changed nil :extend t)
  (set-face-attribute 'diff-refine-removed nil :extend t)
  (set-face-attribute 'diff-refine-added   nil :extend t))

(require 'eldoc)
(global-eldoc-mode)

(require 'dired)
(setq dired-listing-switches "-alh")

(require 'help)
(temp-buffer-resize-mode)

(require 'lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'reveal-mode)
(defun indent-spaces-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode)

(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-x M-g") 'magit-dispatch)
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-modules
                        'magit-insert-stashes
                        'append)

(require 'man)
(setq Man-width 80)

(require 'paren)
(show-paren-mode)

(require 'prog-mode)
(global-prettify-symbols-mode)
(defun indicate-buffer-boundaries-left ()
  (setq indicate-buffer-boundaries 'left))
(add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left)

(require 'recentf)
(add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")

(require 'savehist)
(savehist-mode)

(require 'saveplace)
(save-place-mode)

(require 'simple)
(column-number-mode)

(require 'smerge-mode)
(when (>= emacs-major-version 27)
  (set-face-attribute 'smerge-refined-changed nil :extend t)
  (set-face-attribute 'smerge-refined-removed nil :extend t)
  (set-face-attribute 'smerge-refined-added   nil :extend t))

(add-hook 'text-mode-hook #'indicate-buffer-boundaries-left)

(require 'tramp)
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
(add-to-list 'tramp-default-proxies-alist
             (list (regexp-quote (system-name)) nil nil))
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defvar spc-map (make-sparse-keymap))

(add-to-list 'load-path (expand-file-name "lib/evil/lib" user-emacs-directory))
(setq evil-symbol-word-search t)
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map (kbd "SPC") spc-map)

(require 'evil-magit)

(require 'ivy)
(require 'counsel)
(require 'swiper)
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(define-key evil-motion-state-map (kbd "/") 'swiper)
(define-key spc-map (kbd "/") 'counsel-rg)
(define-key global-map (kbd "C-c C-r") 'ivy-resume)

(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

(require 'airline-themes)
(load-theme 'airline-doom-one t)

(message "Loading %s...done (%.3fs)" user-init-file
         (float-time (time-subtract (current-time)
                                    before-user-init-time)))
(add-hook 'after-init-hook
          (lambda ()
            (message
             "Loading %s...done (%.3fs) [after-init]" user-init-file
             (float-time (time-subtract (current-time)
                                        before-user-init-time))))
          t)

(let ((file (expand-file-name (concat (user-real-login-name) ".el")
                              user-emacs-directory)))
(when (file-exists-p file)
  (load file)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
