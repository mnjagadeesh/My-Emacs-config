;; PREDEFINED STUFF

;; --- emacs theme ---
;; (edited with the built-in theme editor :-))

(custom-set-variables
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(canlock-password "de42867bdc0548f4d88062604f49c8368a2551d7")
 '(hl-paren-colors (quote ("orange" "yellow" "greenyellow" "green" "springgreen" "cyan" "slateblue" "magenta" "purple")))
 '(custom-enabled-themes (quote (deeper-blue))))


;; //////////////////////////////

;; OWN STUFF

;; Start a server so emacsclients can later just connect to this Emacs:
(server-start)

;; Goodbye, menubar! Goodbye, scrollbar! Goodbye, toolbar!
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Start Emacs maximized on Windows:
(if (eq system-type 'windows-nt)
  (w32-send-sys-command 61488)
  (set-face-attribute 'default nil :family "Consolas" :height 100) ; neat default font
)

;; Make Emacs follow sane UI conventions:
;(cua-mode t)             ; enable usual Ctrl+X/C/V/Z behavior
(delete-selection-mode 1) ; delete selected text when typing

;; Show line numbers only when jumping there:
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Use the OS clipboard:
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; "Yes or no"? "Y or n"!
(defalias 'yes-or-no-p 'y-or-n-p)

;; I prefer my backups sorted elsewhere
;; - taken from http://superuser.com/questions/236883/why-does-emacs-create-a-file-that-starts-with
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      5  ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; Remove that splash screen thingy:
(setq inhibit-splash-screen t)   ; no "this is Emacs, click here to read that again"
(setq inhibit-startup-message t) ; *sigh*

;; Store current point positions between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Highlight matching parens:
(show-paren-mode t)

;; Column numbers ftw!
(setq column-number-mode t)

;; Build an MRU list:
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Assume new files as modified:
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

;; Use C-l to jump to a line:
(global-set-key (kbd "C-l") 'goto-line)

;; Use M-j to join lines:
(global-set-key (kbd "M-j")
  (lambda ()
    (interactive)
    (join-line -1)))

;; Simulate Sublime Text's "Go to the beginning of the line" behavior:
(defun simulate-st-goto-home ()
  (interactive)
  (let ((col (current-column)))
    (back-to-indentation)
    (if (= col (current-column)) (move-beginning-of-line nil))))

(global-set-key (kbd "C-a") 'simulate-st-goto-home)
(global-set-key [home] 'simulate-st-goto-home)

;; Create (C)Tags (requires Exuberant CTags in <PATH>):
(defun make-ctags ()
  "compile ctags for the current project or file"
  (interactive)
  (compile "ctags -Re"))


;; //////////////////////////////

;; ADDITIONS FROM MELPA:

;; Add the MELPA repository:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(package-refresh-contents) ; always do that! :)


;; Add Twitter support (now that's critical!):
(require 'twittering-mode)              ; requires the twittering-mode package.
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)           ; Show icons
(setq twittering-url-show-status nil)   ; Keeps the echo area from showing all the http processes


;; Add sane (Vim) keybindings (disabled for now so I can actually use Emacs):
;(require 'evil)                         ; requires the evil package.
;(evil-mode t)


;; Add sane document switching (disabled for now due to UI horrors):
;(require 'tabbar)                       ; requires the tabbar package.
;(tabbar-mode t)
;
;(setq tabbar-ruler-global-tabbar t)     ; enable tabbar
;;(setq tabbar-ruler-popup-toolbar t)    ; enable popup-toolbar
;(require 'cl)                           ; required for tabbar-ruler
;(require 'tabbar-ruler)                 ; requires the tabbar-ruler and the tabbar packages.


;; Enable easier multi-line editing:
(require 'multiple-cursors)              ; requires multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)            ; C-S-c (twice) converts selected lines into cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)           ; C-> marks the next line accordingly
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)       ; C-< marks the previous line accordingly
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click) ; multi-line editing by C-S-click


;; Add a Sublime-Text-like GoTo mode (better than ido ;-)):
(require 'helm)                          ; requires the helm package.
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-mode 1)

(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))


;; Add a sane tab completion:
(require 'smart-tab)                     ; requires the smart-tab package.
(global-smart-tab-mode 1)


;; Add snippets:
(require 'yasnippet)                     ; requires the yasnippet package.
(yas-global-mode t)


;; //////////////////////////////

;; LASTLY, START STUFF:

;; Start Gnus with Emacs:
(gnus)