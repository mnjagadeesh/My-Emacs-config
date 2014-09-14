;; PREDEFINED STUFF

;; --- emacs theme ---
;; (edited with the built-in theme editor :-))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-paren-colors (quote ("orange" "yellow" "greenyellow" "green" "springgreen" "cyan" "slateblue" "magenta" "purple"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 

;; //////////////////////////////

;; OWN STUFF


;; Goodbye, menubar! Goodbye, scrollbar! Goodbye, toolbar!
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))


;; OS-specific tweaks:
(if (eq system-type 'darwin)
   ;; On Mac OS, I prefer cmd key for meta so Alt+[num] stays intact
   (setq mac-command-modifier 'meta
         mac-option-modifier nil))

(if (eq system-type 'windows-nt)
  (progn
    ;; On Windows, I want a neater default font at least
    (set-face-attribute 'default nil :family "Consolas" :height 100)
    (w32-send-sys-command 61488))) ; do this after setting the font (will resize the window!)


;; Make Emacs follow sane UI conventions:
;(cua-mode t)             ; enable usual Ctrl+X/C/V/Z behavior ; nah, that's lame
(delete-selection-mode 1) ; delete selected text when typing


;; Make Emacs FUCKING USE SANE ENCODINGS:
(setq locale-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(unless (eq system-type 'windows-nt)
  ; on Win32, cooperation between Emacs and other Unicode applications is weird.
  ; let's avoid that.
  (set-selection-coding-system 'utf-8-unix))
(prefer-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)


;; "Yes or no"? "Y or n"!
(defalias 'yes-or-no-p 'y-or-n-p)


;; Drop the scratch info message
(setq initial-scratch-message nil)


;; Enable basic syntax highlighting for "everything":
(require 'generic-x)
(add-to-list 'auto-mode-alist '("\\.ini\\'" . conf-mode)) ; .ini files should have conf-mode
(add-to-list 'auto-mode-alist '("\\.iss\\'" . conf-mode)) ; .iss files should have conf-mode


;; I prefer my backups sorted elsewhere:
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      5  ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old


;; While I'd like Emacs to backup files I am working on, I would prefer them to be stored
;; outside the original directories so they won't pollute my file lists:
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; Open a new buffer on C-x C-n:
(defun add-new-empty-buffer ()
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(setq initial-major-mode (quote text-mode))
(global-set-key (kbd "C-x C-n") 'add-new-empty-buffer)


;; Indent b0rked buffers completely anew:
(defun reindent-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key [f12] 'reindent-buffer)


;; Remove that splash screen thingy:
(setq inhibit-splash-screen t)   ; no "this is Emacs, click here to read that again"
(setq inhibit-startup-message t) ; *sigh*


;; TRAMP mode is probably preferring SSH (for most of my servers):
(setq tramp-default-method "ssh")


;; Use spaces, not tabs for indentation:
(setq-default indent-tabs-mode nil)


;; Wrap words at their boundaries, not anywhere else:
(setq-default word-wrap t)


;; Treat display lines as actual lines:
;(global-visual-line-mode t)


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
; (global-set-key (kbd "C-x C-r") 'recentf-open-files)  ; Helm is used, see below


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


;; Remap M-x to C-x C-m so the left hand won't die after two days of real work:
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; fallback for fast-typing.


;; Create (C)Tags (requires Exuberant CTags in <PATH>):
(defun make-ctags ()
  "compile ctags for the current project or file"
  (interactive)
  (compile "ctags -Re"))


;; Byte-compilation of everything inside .emacs.d:
;; (Called from the package update function)
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))





;; //////////////////////////////

;; ADDITIONS FROM MELPA:

;; Add the MELPA repository:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))


;; Automatically install the packages I need:
(setq package-list '(
  ; configured below:
  twittering-mode   ; most important features first!
  ;evil             ; Vim emulation (disabled for now)
  ;tabbar           ; tab switching for Emacs (disabled for now)
  ;tabbar-ruler     ; tab switching for Emacs (disabled for now)
  multiple-cursors  ; multi-line editing made easier
  helm              ; auto-completing popup system
  smart-tab         ; tab completion and more
  dpaste_de         ; put the current buffer to the web
  ;yasnippet        ; easy snippet handling (disabled for now)
  sr-speedbar       ; sidebar as a buffer
  znc               ; ZNC for ERC
  todotxt           ; todo.txt support
  emmet-mode        ; Zen Coding
  browse-kill-ring  ; a more useful kill-ring experience
  web-mode          ; multiple modes
  ; flycheck        ; syntax checkers (disabled for now)

  ; color themes:
  zenburn-theme     ; most eye-pleasant coding theme available

  ; no need to configure here:
  markdown-mode     ; Markdown support
  php-mode          ; PHP support
  dash              ; required by helm (and probably others)
  erc-tweet         ; ERC add-in: show tweets
  erc-youtube       ; ERC add-in: show YT video details
  erc-image         ; ERC add-in: inline images
  erc-hl-nicks      ; ERC add-in: highlight nicknames
  org               ; org-mode

  ; Gnus extensions:
  bbdb              ; Big Brother database
  bbdb-ext          ; ... with extras
))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


;; Quick access to the package updater:
(defun update-packages ()
  (interactive)
  (package-refresh-contents)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute)
  (byte-compile-init-dir))


;; //////////////////////////////

;; CONFIGURING THE MELPA PACKAGES:

;; Add Twitter support (now that's critical! ;-)):
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
;(require 'cl)                           ; required for tabbar-ruler
;(require 'tabbar-ruler)                 ; requires the tabbar-ruler and the tabbar packages.


;; Enable easier multi-line editing:
(require 'multiple-cursors)              ; requires multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)            ; C-S-c (twice) converts selected lines into cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)           ; C-> marks the next line accordingly
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)       ; C-< marks the previous line accordingly
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click) ; multi-line editing by C-S-click


;; Add a Sublime-Text-like GoTo mode:
(require 'helm)                          ; requires the helm package.
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(helm-mode 1)


;; Load helm as a horizontal split buffer (always on the right side):
(setq helm-display-function
      (lambda (buf)
        (split-window-horizontally)
        (other-window 1)
        (switch-to-buffer buf)))


;; Add a sane tab completion:
(require 'smart-tab)                     ; requires the smart-tab package.
(global-smart-tab-mode 1)


;; Add dpaste support:
(require 'dpaste_de)                     ; requires the dpaste_de package.


;; Add todo.txt support:
(require 'todotxt)                       ; requires the todotxt package.
(add-to-list 'auto-mode-alist '("\\todo.txt\\'" . todotxt-mode)) ; auto-mode todo.txt files


;; Add snippets (disabled for now due to slow startup and not having used it much):
;(require 'yasnippet)                     ; requires the yasnippet package.
;(yas-global-mode t)


;; Add a neat sidebar for easier directory/project browsing:
(require 'sr-speedbar)                   ; requires the sr-speedbar package.
(global-set-key (kbd "M-s") 'sr-speedbar-toggle)   ; this shortcut was still free.


;; ERC config:
(load-file "~/.erc.userdata.el")         ; don't expose my ZNC accounts to github :)
(setq erc-interpret-mirc-color t)
(setq erc-server-coding-system '(utf-8 . utf-8))
(setq erc-kill-buffer-on-part t)         ; Kill buffers for channels after /part
(setq erc-kill-queries-on-quit t)        ; Kill buffers for private queries after quitting the server
(setq erc-kill-server-buffer-on-quit t)  ; Kill buffers for server messages after quitting the server
(setq erc-query-display 'buffer)         ; open query buffers in the current window

; fill buffer horizontally:
(add-hook 'window-configuration-change-hook 
 '(lambda ()
    (setq erc-fill-column (- (window-width) 2))))
    
(setq erc-fill-function 'erc-fill-static)  ; align nicknames to the right …
(setq erc-fill-static-center 24)           ; … with 24 characters
;;Settings for timestamp
(setq erc-timestamp-only-if-changed-flag nil
          erc-timestamp-format "%H:%M "
          erc-fill-prefix "      "
          erc-insert-timestamp-function 'erc-insert-timestamp-left)


;; Emmet:
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)        ; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode)        ; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t)     ; use it as "snippets"


;; browse-kill-ring:
(global-set-key (kbd "M-y") 'browse-kill-ring)     ; we misuse the "normal" paste keystroke here
(setq browse-kill-ring-highlight-current-entry t)  ; highlight entry to be inserted
(setq browse-kill-ring-highlight-inserted-item t)  ; highlight entry just inserted


;; web-mode:
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-ac-sources-alist
	'(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
	  ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
	  ("css" . (ac-source-css-property ac-source-emmet-css-snippets))
	)
)
(add-hook 'web-mode-before-auto-complete-hooks
	'(lambda ()
		(let ((web-mode-cur-language
			(web-mode-language-at-pos)))
			(if (string= web-mode-cur-language "php")
				(yas-activate-extra-mode 'php-mode)
				(yas-deactivate-extra-mode 'php-mode)
			)
			(if (string= web-mode-cur-language "css")
				(setq emmet-use-css-transform t)
				(setq emmet-use-css-transform nil)
			)
		)
	)
)

(defun my-web-mode-hook ()
	"Hooks for Web mode."
	(setq web-mode-markup-indent-offset 2)             ; HTML markup offset
	(setq web-mode-code-indent-offset 2)               ; JavaScript (etc.) code offset
	(setq web-mode-enable-current-element-highlight t) ; highlight current element
	(local-set-key (kbd "RET") 'newline-and-indent)    ; auto-indent
	(add-hook 'local-write-file-hooks
		(lambda ()
			(delete-trailing-whitespace)                   ; remove redundant closing whitespaces
			nil
		)
	)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)


;; flycheck config:
;(add-hook 'after-init-hook #'global-flycheck-mode)


;; Change colors:
(load-theme 'zenburn t)                  ; requires the zenburn-theme package.