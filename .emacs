;; PREDEFINED STUFF

;; --- emacs theme ---
;; (edited with the built-in theme editor :-))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(canlock-password "de42867bdc0548f4d88062604f49c8368a2551d7")
 '(custom-enabled-themes (quote (deeper-blue))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; //////////////////////////////

;; OWN STUFF

;; Start Emacs maximized on Windows:
(if (eq system-type 'windows-nt)
  (w32-send-sys-command 61488)
)

;; I prefer my backups sorted elsewhere
;; - taken from http://superuser.com/questions/236883/why-does-emacs-create-a-file-that-starts-with
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; Remove that splash screen thingy:
(setq inhibit-splash-screen t)

;; Start Gnus with Emacs:
(gnus)