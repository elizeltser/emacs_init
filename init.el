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
(column-number-mode 1)
(hl-line-mode 1)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

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

;; Code compl
(setq dabbrev-case-fold-search t   ; A=a matches
      dabbrev-case-replace nil)    ; keep the case you typed
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set python interpreter explicitly
(setq python-shell-interpreter "C:\\Users\\eli.zeltser\\AppData\\Local\\Programs\\Python\\Python313\\python.exe")
