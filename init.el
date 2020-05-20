;; -*- lexical-binding: t -*-

;;; startup

(setq load-prefer-newer t)

(require 'xdg)
(defvar user-config-directory user-emacs-directory)
(defvar user-cache-directory (expand-file-name "emacs/" (xdg-cache-home)))
(defvar user-data-directory (expand-file-name "emacs/" (xdg-data-home)))

(defun in-config-directory (name)
  (expand-file-name name user-config-directory))
(defun in-cache-directory (name)
  (expand-file-name name user-cache-directory))
(defun in-data-directory (name)
  (expand-file-name name user-data-directory))

(dolist (directory (list user-data-directory user-cache-directory))
  (unless (file-exists-p directory)
    (make-directory directory)))

(add-to-list 'load-path (in-config-directory "lib/borg"))
(add-to-list 'load-path (in-config-directory "lib/evil/lib"))

(fset 'display-startup-echo-area-message 'ignore)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(set-face-font 'default "Noto Sans Mono-12")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq auto-save-list-file-prefix (in-data-directory "auto-save-list/"))
(setq custom-file (in-config-directory "custom.el"))
(setq enable-recursive-minibuffers t)
(setq make-backup-files nil)
(setq frame-title-format "%b")
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)

(when (file-exists-p custom-file)
  (load custom-file))

(require 'borg)
(borg-initialize)

;;; amx

(setq amx-save-file (in-data-directory "amx-items"))

;;; auto-compile

(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)
(setq auto-compile-source-recreate-deletes-dest t)
(setq auto-compile-toggle-deletes-nonlib-dest t)
(setq auto-compile-update-autoloads t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;;; cc-mode

(defconst qt-c-style
  '((c-basic-offset . 4)
    (c-offsets-alist . ((innamespace . 0)
                        (access-label . -)
                        (inline-open . 0)
                        (inlambda . 0)
                        (arglist-intro . ++)
                        (func-decl-cont . ++)
                        (statement-cont . ++)
                        (member-init-intro . +)))))

(defface qt-macro-face
  '((t :foreground "#61BD72"))
  "Face for Qt macros.")

(defconst qt-macro-names-with-semicolon
  '(
    "QT_BEGIN_NAMESPACE"
    "QT_END_NAMESPACE"
    "Q_CLASSINFO"
    "Q_DECLARE_FLAGS"
    "Q_DECLARE_LOGGING_CATEGORY"
    "Q_DECLARE_METATYPE"
    "Q_DECLARE_OPERATORS_FOR_FLAGS"
    "Q_DECLARE_PRIVATE"
    "Q_DECLARE_PRIVATE_D"
    "Q_DECLARE_PUBLIC"
    "Q_DECLARE_SHARED"
    "Q_DECLARE_SHARED_NOT_MOVABLE_UNTIL_QT6"
    "Q_DECLARE_TR_FUNCTIONS"
    "Q_DECLARE_TYPEINFO"
    "Q_DEPRECATED_VERSION_X"
    "Q_DEPRECATED_X"
    "Q_DISABLE_COPY"
    "Q_ENUM"
    "Q_ENUMS"
    "Q_ENUM_NS"
    "Q_FLAG"
    "Q_FLAGS"
    "Q_FLAG_NS"
    "Q_GADGET"
    "Q_INTERFACES"
    "Q_OBJECT"
    "Q_OVERRIDE"
    "Q_PLUGIN_METADATA"
    "Q_PRIVATE_PROPERTY"
    "Q_PRIVATE_SLOT"
    "Q_PROPERTY"
    "Q_SIGNALS"
    "Q_SLOTS"
    "signals"
    "slots"
    ))

(defconst qt-macro-names-without-semicolon
  '(
    "METHOD"
    "Q_EMIT"
    "Q_INVOKABLE"
    "Q_REVISION"
    "Q_SCRIPTABLE"
    "Q_SIGNAL"
    "Q_SLOT"
    "SIGNAL"
    "SLOT"
    "emit"
    ))

(defconst qt-macro-names (append qt-macro-names-with-semicolon
                                 qt-macro-names-without-semicolon))

(defconst qt-macro-regexp
  (rx-to-string `(: bow (or ,@qt-macro-names) eow)))

(defun qt-c++-mode-setup ()
  (setq c-macro-names-with-semicolon qt-macro-names-with-semicolon)
  (font-lock-add-keywords nil
                          `((,qt-macro-regexp . 'qt-macro-face)))
  (c-set-style "qt"))

(require 'cc-mode)
(c-add-style "qt" qt-c-style)
(add-hook 'c++-mode-hook 'qt-c++-mode-setup)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; company

(setq company-dabbrev-downcase nil)
(require 'company)
(global-company-mode 1)

;;; counsel

(require 'counsel)
(counsel-mode 1)

;;; dash

(require 'dash)
(dash-enable-font-lock)

;;; diff-mode

(require 'diff-mode)
(set-face-attribute 'diff-refine-added   nil :extend t)
(set-face-attribute 'diff-refine-changed nil :extend t)
(set-face-attribute 'diff-refine-removed nil :extend t)

;;; diff-mode diff-hl

(setq diff-hl-draw-borders nil)
(require 'diff-hl)
(global-diff-hl-mode)

;;; dired

(setq dired-listing-switches "-alh")

;;; doom-themes

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(require 'doom-themes)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

;;; doom-themes airline-themes

(require 'airline-themes)
(load-theme 'airline-doom-one t)

;;; eldoc

(require 'eldoc)
(global-eldoc-mode)

;;; epkg

(setq epkg-repository (in-data-directory "epkgs/"))

;;; evil

(setq evil-want-keybinding nil)
(setq evil-respect-visual-line-mode 't)
(require 'evil)
(evil-mode 1)
(evil-declare-change-repeat 'company-complete)

;; evil-collection

(require 'evil-collection)
(evil-collection-init)

;;; help

(require 'help)
(temp-buffer-resize-mode)

;;; ivy

(require 'ivy)
(ivy-configure 'counsel-imenu :update-fn 'auto)
(ivy-mode 1)

;;; ivy-xref

(require 'ivy-xref)
(setq xref-show-definitions-function #'ivy-xref-show-defs)

;;; magit

(require 'magit)
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-modules
                        'magit-insert-stashes
                        'append)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)

;;; magit evil-magit

(require 'evil-magit)

;;; man

(setq Man-width 80)

;;; man evil

(setq evil-lookup-func 'man)

;;; org

(setq org-adapt-indentation nil)

;;; paren

(require 'paren)
(show-paren-mode)

;;; prog-mode

(require 'prog-mode)
(global-prettify-symbols-mode)

;;; recentf

(setq recentf-save-file (in-data-directory "recentf"))
(setq recentf-max-saved-items 1024)
(require 'recentf)
(add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")

;;; savehist

(setq savehist-file (in-data-directory "savehist"))
(require 'savehist)
(savehist-mode)

;;; saveplace

(setq save-place-file (in-data-directory "saveplace"))
(require 'saveplace)
(save-place-mode)

;;; server

(require 'server)
(unless (server-running-p)
  (server-mode))

;;; simple

(require 'simple)
(column-number-mode)

;;; smerge

(require 'smerge-mode)
(set-face-attribute 'smerge-refined-added   nil :extend t)
(set-face-attribute 'smerge-refined-changed nil :extend t)
(set-face-attribute 'smerge-refined-removed nil :extend t)

;;; text-mode
(require 'text-mode)

;;; text-mode prog-mode

(defun indicate-buffer-boundaries-left ()
  (setq indicate-buffer-boundaries 'left))
(add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left)
(add-hook 'text-mode-hook #'indicate-buffer-boundaries-left)

;;; tramp

(require 'tramp)
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
(add-to-list 'tramp-default-proxies-alist
             (list (regexp-quote (system-name)) nil nil))

;;; transient

(setq transient-history-file (in-data-directory "transient/history.el"))
(setq transient-levels-file (in-data-directory "transient/levels.el"))
(setq transient-values-file (in-data-directory "transient/values.el"))

;;; undo-tree

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      `(("." . ,(in-data-directory "undo-tree/"))))

;;; vc

(require 'vc-hooks)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;; visual-fill-column
(require 'visual-fill-column)
(defun toggle-soft-wrap-on ()
  (interactive)
  (setq visual-fill-column-center-text t)
  (visual-line-mode 't)
  (visual-fill-column-mode 't))

;;; visual-fill-column text-mode
;(add-hook 'text-mode-hook 'toggle-soft-wrap-on)

;;; ws-butler
(require 'ws-butler)
(ws-butler-global-mode)

;;; xref

(setq xref-prompt-for-identifier nil)

;;; xref global

(defun global-run (&rest args)
  (condition-case nil
      (apply #'process-lines "global" args)
    (error nil)))

(defun global-toplevel ()
  (car (global-run "-p")))

(defun global-map-cxrefs (fn &rest args)
  (delq nil
        (mapcar (lambda (cxref)
                  (when (string-match (rx (group (+ (not space)))
                                          (+ space)
                                          (group (+ (not space)))
                                          (+ space)
                                          (group (+ (not space)))
                                          (+ space)
                                          (group (+ anything)))
                                      cxref)
                    (let* ((identifier (match-string 1 cxref))
                           (line-number (string-to-number (match-string 2 cxref)))
                           (path (match-string 3 cxref))
                           (line-contents (match-string 4 cxref))
                           (column-number (string-match (regexp-quote identifier) line-contents)))
                      (funcall fn identifier path line-number column-number line-contents))))
                (apply #'global-run "-x" "-a" args))))

(defun global-xrefs (&rest args)
  (apply #'global-map-cxrefs
         (lambda (_ path line-number column-number line-contents)
           (xref-make line-contents
                      (xref-make-file-location path
                                               line-number
                                               (or column-number 0))))
         args))

(defun global-tags (&rest args)
  (apply #'global-map-cxrefs
         (lambda (identifier &rest _) identifier)
         args))

(defun global-xref-backend-function ()
  (when (global-toplevel)
    'global))

(add-to-list 'xref-backend-functions 'global-xref-backend-function)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql global)))
  (find-tag-default))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql global)))
  (global-tags "-f" buffer-file-name))

(cl-defmethod xref-backend-definitions ((_backend (eql global)) identifier)
  (global-xrefs "-d" (shell-quote-argument identifier)))

(cl-defmethod xref-backend-apropos ((_backend (eql global)) identifier)
  (global-xrefs "-g" (shell-quote-argument identifier)))

(cl-defmethod xref-backend-references ((_backend (eql global)) identifier)
  (global-xrefs "-r" (shell-quote-argument identifier)))

;;; yasnippet

(setq yas-snippet-dirs (list (in-config-directory "snippets/")))
(require 'yasnippet)
(yas-global-mode 1)

;;; keybindings

(defhydra find (:color teal)
  "find"
  ("x" find/text/body "text")
  ("i" find/identifier/body "identifier")
  ("f" find/file/body "file")
  ("d" find/directory/body "directory")
  ("p" find/project/body "project"))
(defhydra find/text (:color teal)
  "find text"
  ("i" xref-find-references "for identifier")
  ("b" swiper "in buffer")
  ("B" swiper-all "in buffers")
  ("d" (counsel-rg nil default-directory) "in directory")
  ("p" (counsel-rg nil (magit-toplevel)) "in project"))
(defhydra find/identifier (:color teal)
  "find identifier"
  ("i" xref-find-definitions "at point")
  ("b" counsel-imenu "in buffer"))
(defhydra find/file (:color teal)
  "find file"
  ("f" ff-find-other-file "for file")
  ("d" (counsel-fzf nil default-directory) "in directory")
  ("p" (counsel-fzf nil (magit-toplevel)) "in project")
  ("r" counsel-recentf "in history"))
(defhydra find/directory (:color teal)
  "find directory"
  ("f" (dired default-directory) "for file"))
(defhydra find/project (:color teal)
  "find project"
  ("f" (magit-status-setup-buffer (magit-toplevel)) "for file"))

(defhydra paste ()
  "paste"
  ("p" evil-paste-after nil)
  ("P" evil-paste-before nil)
  ("C-j" (evil-paste-pop 1) "next")
  ("C-k" (evil-paste-pop -1) "prev"))

(evil-define-key 'normal 'dired-mode-map (kbd "SPC") 'find/body)

(evil-define-key 'insert 'global (kbd "C-SPC") 'company-complete)
(evil-define-key 'normal 'global (kbd "P") 'paste/evil-paste-before)
(evil-define-key 'normal 'global (kbd "SPC") 'find/body)
(evil-define-key 'normal 'global (kbd "p") 'paste/evil-paste-after)

(define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)

(define-key global-map (kbd "C-SPC") 'find/body)
(define-key global-map (kbd "C-l") 'ivy-resume)

;;; postlude

(let ((file (in-config-directory (concat (user-real-login-name) ".el"))))
  (when (file-exists-p file)
    (load file)))
