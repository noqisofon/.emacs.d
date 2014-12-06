;;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil; -*-

;;; Code:

(setq org-directory "~/var/memo")

(setq org-html-doctype "html5")

(setq org-export-default-language "ja")

(setq org-startup-truncated nil)
(setq org-return-follows-link t)
;;(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory "/agenda.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")))

(push '("\\.org$" . org-mode) auto-mode-alist)

(require-if-exists
 org

 (set-face-foreground 'org-level-1 "#7fb000")
 (set-face-foreground 'org-level-2 "#7f9142")
 (set-face-foreground 'org-level-3 "#00379c")
 (set-face-foreground 'org-level-4 "#212ca4")
 (set-face-foreground 'org-level-5 "#6a40ad")
 (set-face-foreground 'org-level-6 "#8c3e13")

 (set-face-foreground 'org-document-title "midnight blue")

 (set-face-attribute 'org-document-title nil
                     :height 1.0
                     :weight 'bold))
