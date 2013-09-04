;; PREDEFINED STUFF

;; --- emacs theme ---
;; (edited with the built-in theme editor :-))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "a83d5f7c7c23b017f77078710aab86597acb752a")
 '(hl-paren-colors (quote ("orange" "yellow" "greenyellow" "green" "springgreen" "cyan" "slateblue" "magenta" "purple"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; //////////////////////////////

;; OWN STUFF

;; Start a server so emacsclients can later just connect to this Emacs:
(server-start)

;; Goodbye, menubar! Goodbye, scrollbar! Goodbye, toolbar!
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; OS-specific tweaks:
(if (eq system-type 'windows-nt)
  (progn
    (set-face-attribute 'default nil :family "Consolas" :height 100)
    (w32-send-sys-command 61488)) ; do this after setting the font (will resize the window!)
  ;; if not on Windows, use the OS clipboard (Windows-Emacs does that by default):
  ;(progn
  ;  (setq x-select-enable-clipboard t)
  ;  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))
)

;; Make Emacs follow sane UI conventions:
;(cua-mode t)             ; enable usual Ctrl+X/C/V/Z behavior ; nah, that's lame
(delete-selection-mode 1) ; delete selected text when typing

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
(setq show-paren-delay 0) ; no delay, damnit!

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

;; Use M-/ to (un)comment a line/region:
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)
(defmacro allow-line-as-region-for-function (orig-function)
  `(defun ,(intern (concat (symbol-name orig-function) "-or-line")) ()
     ,(format "Like `%s', but acts on the current line if mark is not active." orig-function)
     (interactive)
     (if mark-active
         (call-interactively (function ,orig-function))
       (save-excursion
         ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
         (beginning-of-line)
         (set-mark (point))
         (end-of-line)
         (call-interactively (function ,orig-function))))))
(unless (fboundp 'comment-or-uncomment-region-or-line)
  (allow-line-as-region-for-function comment-or-uncomment-region))

;; Use C-l to jump to a line:
(global-set-key (kbd "C-l") 'goto-line)

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

;; Remap M to C-x C-m so the left hand won't die after two days of real work:
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command) ; fallback for fast-typing.

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
(unless package-archive-contents (package-refresh-contents)) ; note to self: call that sometimes


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

(setq helm-display-function              ; Load helm where it fits better
      (lambda (buf)
        (split-window-horizontally)
        (other-window 1)
        (switch-to-buffer buf)))


;; Add a sane tab completion:
(require 'smart-tab)                     ; requires the smart-tab package.
(global-smart-tab-mode 1)


;; Add snippets:
(require 'yasnippet)                     ; requires the yasnippet package.
(yas-global-mode t)


;; Change colors:
(load-theme 'zenburn t)                  ; requires zenburn-theme.


;; //////////////////////////////

;; LASTLY, START STUFF:

;; Start Gnus with Emacs:
;(gnus)