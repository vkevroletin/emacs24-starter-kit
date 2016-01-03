;;; -*- lexical-binding: t -

(require 'f)
(require 'org-agenda)

(defun sk-goto-mail-content-type ()
  "Move point to end of CC field, creating it if necessary."
  (expand-abbrev)
  (or (mail-position-on-field "cc" t)
      (progn
        (mail-position-on-field "to")
        (insert "\nContent-Type: "))))

;; Initially based on http://www.randomsample.de/dru5/node/61
;; Set org-habit-show-habits nil if you done't want habbits
;; in agenda
(defun sk-send-agenda-to-email (dest-email)
  (let ((number-of-days 7))
    (unless (boundp 'org-agenda-buffer-name)
      (org-agenda-list nil nil number-of-days))

    (with-current-buffer org-agenda-buffer-name
      (unless (eobp)
        ;; htmlize-buffer fails for some reason so use temporary file
        ;; to get html using org-agenda-write function
        (let ((tmp-file (f-join temporary-file-directory "agenda.html")))
          (org-agenda-write tmp-file)
          (setq text (f-read-text tmp-file))
          (f-delete tmp-file))

        (mail)
        (mail-to) (insert dest-email)
        (mail-subject) (insert "Diary entries generated "
                               (calendar-date-string (calendar-current-date)))
        (sk-goto-mail-content-type) (insert  "text/html; charset=\"UTF-8\"")
        (mail-text) (insert text)
        (mail-send-and-exit nil)))))

(provide 'sk-email-agenda)
