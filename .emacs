;; PREDEFINED STUFF

;; --- emacs theme ---
;; (edited with the built-in theme editor :-))

(custom-set-variables
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(canlock-password "de42867bdc0548f4d88062604f49c8368a2551d7")
 '(custom-enabled-themes (quote (deeper-blue))))

(custom-set-faces
)


;; //////////////////////////////

;; OWN STUFF

;; Start Emacs maximized on Windows:
(if (eq system-type 'windows-nt)
  (w32-send-sys-command 61488)
  (set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-utf-8") ; neat default font
)

;; Make Emacs follow sane UI conventions:
;(cua-mode t)             ; enable usual Ctrl+X/C/V/Z behavior
(delete-selection-mode 1) ; delete selected text when typing
(global-linum-mode 1)     ; enable line numbers

;; Use the OS clipboard:
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Add the MELPA repository:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

;; I prefer my backups sorted elsewhere
;; - taken from http://superuser.com/questions/236883/why-does-emacs-create-a-file-that-starts-with
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      5  ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; Remove that splash screen thingy:
(setq inhibit-splash-screen t)

;; Start Gnus with Emacs:
(gnus)

;; Load interactively-do-things mode:
(require 'ido)
(ido-mode t)

;; now, with ido, we can have an interactive M-x mode :-)
(global-set-key "\M-x"
  (lambda ()
    (interactive) (call-interactively
      (intern
        (ido-completing-read "M-x "
          (all-completions "" obarray 'commandp))))))

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)


;; //////////////////////////////

;; ADDITIONS FROM MELPA:

;; Add Twitter support:
(require 'twittering-mode)              ; requires the twittering-mode package.
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)           ; Show icons
(setq twittering-url-show-status nil)   ; Keeps the echo area from showing all the http processes

;; Add sane keybindings:
(require 'evil)                         ; requires the evil package.
(evil-mode t)

;; Add sane document switching:
(require 'tabbar)                       ; requires the tabbar package.
(tabbar-mode t)

(setq tabbar-ruler-global-tabbar t)     ; enable tabbar
(setq tabbar-ruler-popup-toolbar t)     ; enable popup-toolbar
(require 'cl)                           ; required for tabbar-ruler
(require 'tabbar-ruler)                 ; requires the tabbar-ruler and the tabbar packages.