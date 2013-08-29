(load-file "~/.gnus.userdata.el") ; don't expose my mail address to github :)

(setq gnus-select-method '(nntp "news.piratenpartei.de" (nntp-connection-timeout 10))) ;; 10 seconds

(setq gnus-article-decode-mime-words t
      gnus-article-decode-charset 1
      gnus-mime-view-all-parts t	; View all the MIME parts in current

      gnus-always-read-dribble-file 1 ; alreays read auto-save file
      
					; article
      gnus-ignored-mime-types '("text/x-vcard")
      gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed")
      gnus-unbuttonized-mime-types '("text/plain")

      gnus-treat-buttonize t		   ; Add buttons
      gnus-treat-buttonize-head 'head	   ; Add buttons to the head
      gnus-treat-emphasize t		   ; Emphasize text
      gnus-treat-strip-cr 'last		   ; Remove carriage returns
      gnus-treat-hide-headers 'head	   ; Hide headers
      gnus-treat-hide-boring-headers 'head ; -Hide boring headers
      gnus-outgoing-message-group nil) ; Don't save outgoing messages to a separate group

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)


(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-date))) ; newest stuff on top
 
(setq gnus-fetch-old-headers 'some) ; prevent teared threads by loading older but read postings

; "<name> writes" is a bit boring.
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "%f schrob am %d. %b. %Y um %R Uhr dies:")

(gnus-demon-add-handler 'gnus-demon-scan-news 2 t) ; grab new news every 2 minutes

; don't keep Gnus alive on shutdown
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate) "Timeout for Gnus."(with-timeout (120 (message "Gnus timed out.")) ad-do-it))

; I'd prefer text/plain messages, HTML mails are for sissies.
(eval-after-load "mm-decode"
 '(progn 
      (add-to-list 'mm-discouraged-alternatives "text/html")
      (add-to-list 'mm-discouraged-alternatives "text/richtext")))

; also I'd prefer to have sane default headers
(setq gnus-visible-headers "^From:\\|^Subject:\\|To:\\|^Cc:\\|^Date:\\|^Newsgroups:\\|^X-Newsreader:\\|^X-Mailer:")