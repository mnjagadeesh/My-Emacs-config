(load-file "~/.gnus.userdata.el") ; don't expose my mail address to github :)

(setq gnus-article-decode-mime-words t
      gnus-article-decode-charset 1
      gnus-agent t

      ; don't spam my ~ with bogus folders!
      gnus-directory "~/.emacs.d/News/"
      message-directory "~/.emacs.d/Mail/"

      gnus-agent-directory        (concat gnus-directory "agent/")
      gnus-cache-directory        (concat gnus-directory "cache/")
      gnus-cache-active-file      (concat gnus-directory "cache/active") ; no slash! this is a file, not a directory!
      gnus-article-save-directory (concat gnus-directory "save/")
      gnus-kill-files-directory   (concat gnus-directory "killfiles/")

      nndraft-directory           (concat message-directory "drafts/")
      nnfolder-directory          (concat message-directory "archive/")

      gnus-mime-view-all-parts t    ; View all the MIME parts in current

      gnus-always-read-dribble-file 1 ; always read auto-save file

      gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-date)) ; inverted sorting: newest threads on top

      gnus-use-cache t

      gnus-treat-buttonize t           ; Add buttons
      gnus-treat-buttonize-head 'head  ; Add buttons to the head
      gnus-treat-emphasize t           ; Emphasize text
      gnus-treat-display-smileys t     ; Use Smilies
      gnus-treat-strip-cr 'last        ; Remove carriage returns
      gnus-treat-hide-headers 'head    ; Hide headers

      gnus-boring-article-headers '(empty followup-to newsgroups many-to reply-to)
      gnus-treat-hide-boring-headers 'head ; -Hide boring headers
      gnus-fetch-old-headers 'some         ; prevent teared threads by loading older but read postings

      gnutls-log-level 0 ; don't annoy me with GnuTLS messages. I don't need that for NNTP.

      ; better summary line in the group overview
      ; inspired by http://eschulte.github.io/emacs-starter-kit/starter-kit-gnus.html
      gnus-sum-thread-tree-single-indent "* "
      gnus-sum-thread-tree-single-leaf "+-> "
      gnus-summary-display-arrow t
      gnus-summary-line-format "%0{%U%R%z%}%3{│%} %1{%d%} %3{│%}  %4{%-20,20f%}  %3{│%} %1{%B%}%s\n"

      ; better group lines too (no news server display & stuff)
      ; inspired by http://www.sopos.org/olli/?gnus
      gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40G%)\n"

      gnus-message-archive-group  nil  ; Don't use archiving
      gnus-outgoing-message-group nil) ; Don't save outgoing messages

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)
 
; "<name> writes:" is a bit boring.
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "%N schrob am %d. %b. %Y um %R Uhr dies:\n") ; %N = (real name, else mail address)

(gnus-demon-add-handler 'gnus-demon-scan-news 2 t) ; grab new news every 2 minutes

(add-hook 'gnus-article-display-hook 'gnus-article-highlight-citation t) ; highlight quotes

; don't keep Gnus alive on shutdown
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate) "Timeout for Gnus." (with-timeout (120 (message "Gnus timed out.")) ad-do-it))

; I'd prefer text/plain messages (with inline attachments), HTML mails are for sissies.
(eval-after-load "mm-decode"
 '(progn
      (add-to-list 'mm-discouraged-alternatives "text/html")
      (add-to-list 'mm-discouraged-alternatives "text/richtext")))

; also I'd prefer to have sane default headers
(setq gnus-visible-headers '("^From:\\|^Subject:\\|To:\\|^Cc:\\|^Date:\\|^Newsgroups:\\|^X-Newsreader:\\|^X-Mailer:")
      gnus-sorted-header-list gnus-visible-headers)

; list all groups, not only the unread ones, in case I want to post something to them...
;(add-hook 'gnus-started-hook 'gnus-group-list-all-groups)
;(add-hook 'gnus-summary-exit-hook 'gnus-group-list-all-groups)
(setq gnus-permanently-visible-groups ".*") ; more elegant method