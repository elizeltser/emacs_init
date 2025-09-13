;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; --------- Basics ----------------
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      confirm-kill-process nil)
(delete-selection-mode 1)

;; minimal UI niceties
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(hl-line-mode 1)
(tooltip-mode -1)
(set-fringe-mode 10)
(column-number-mode)
(global-display-line-numbers-mode t)

(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Change backups to be stored in a directory
(custom-set-variables
 '(backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups")))))

;; more easily keep track and manage user keybindings
(require 'bind-key)

;; Code compl
(setq dabbrev-case-fold-search t   ; A=a matches
      dabbrev-case-replace nil)    ; keep the case you typed
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; which-key presents key-bindings in a more visual way
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Fix dired so that it will not create extra buffers when navigating
(setq dired-kill-when-opening-new-dired-buffer t)

;; ----- Theme --------------------
;; load a single theme cleanly on startup
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'wombat t)

;; ------ Fonts --------------------
;; Use consolas at ~14pt everywhere
(set-face-attribute 'default nil :family "Consolas" :height 140)
;; If Consolas not found, fall back to Lucida Console
(unless (member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :family "Lucida Console"))
(setq-default line-spacing 0.2)
(global-font-lock-mode 1)

;; ------ Icons ----------------
(use-package all-the-icons)

;; ------ Python specific ------
;; Set python interpreter explicitly
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  (setq python-shell-interpreter "C:\\Users\\eli.zeltser\\AppData\\Local\\Programs\\Python\\Python313\\python.exe")

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "Scripts/python.exe")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "C:\\Users\\eli.zeltser\\AppData\\Local\\Programs\\Python\\Python313\\python.exe")))))

;; (defun my-python-restart-and-run ()
;;   "Restart python REPL and run the current file."
;;   (interactive)
;;   (let ((filename (buffer-file-name)))
;;     (when (get-buffer "*Python*")
;;       (kill-buffer "*Python*"))
;;     (run-python (python-shell-parse-command) nil nil)
;;     (python-shell-send-file filename)))

;;(with-eval-after-load 'python
;;  (define-key python-mode-map (kbd "C-c C-r") 'my-python-restart-and-run))

(defun kill-python-buffer-no-confirm ()
  "Kill the *Python* buffer without confirmation if it exists."
  (interactive)
  (let ((buffer (get-buffer "*Python*")))
    (when buffer
      (let ((kill-buffer-query-functions nil)) ; Disable confirmation
        (kill-buffer buffer)))))

;; Optionally bind it to a key for convenience
(global-set-key (kbd "C-c k p") 'kill-python-buffer-no-confirm)

(defun my-run-python-in-project-root ()
  "Start Python interpreter in the project root using project.el."
  (interactive)
  (let* ((project (project-current))
         (default-directory (if project
                                (project-root project)
                              default-directory)))
    (run-python (python-shell-parse-command) nil nil)))
