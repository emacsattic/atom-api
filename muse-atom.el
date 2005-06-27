(provide 'muse-atom)
(require 'muse)
(require 'muse-html)

;;returns the current date &time in w3cdtf format
(defun muse-atom:w3cdtf () 
  (let ((tz-hours (/ (car (current-time-zone)) 3600)))
     (concat 
      (format-time-string "%Y-%m-%dT%T")
      (if (> tz-hours 0)
	  (format "+%02d:00" tz-hours)
	(format "%03d:00" tz-hours)))))

(unless (assoc "atom" muse-publishing-styles)
  (muse-derive-style "atom" "xhtml"
   :header 'muse-atom-header
   :footer 'muse-atom-footer))
;   :before 'muse-atom:before-hook
;   :after 'muse-atom:after-hook))


(defvar muse-atom-header
   "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>
 <entry xmlns=\"http://purl.org/atom/ns#\">
   <generator url=\"http://purl.org/net/emacs-atom-api/\">Elisp Atom API+Muse</generator>
   <author>
     <name><lisp>(or (muse-publishing-directive \"author\") \"\")</lisp></name>
   </author>
   <issued><lisp>(or (muse-publishing-directive \"issued\") (muse-atom:w3cdtf))</lisp></issued>
   <title mode=\"escaped\" type=\"text/html\">
     <lisp>(or (muse-publishing-directive \"title\") \"Untitled\")</lisp>
   </title>
   <content type=\"application/xhtml+xml\" xml:space=\"preserve\">
     <div xmlns=\"http://www.w3.org/1999/xhtml\">"
   "Header used for publishing atom entries.")

(defun muse-atom:before-hook ()
  (setq muse-atom:raw-content (buffer-substring-no-properties (point-min) (point-max))))

(defun muse-atom:after-hook ()
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region (point-min) (point-max))
    (insert
     (atom-api:xml/to-string
      `(entry ((xmlns . "http://purl.org/atom/ns#"))
	      (generator ((url . "http://purl.org/net/emacs-atom-api/"))
			 "Elisp Atom API+Muse")
	      (author nil
		      (name nil ,(muse-publishing-directive "author")))
	      (issued nil 
		      ,(muse-publishing-directive "issued"))
	      (title ((mode . "escaped") (type . "text/html"))
		     ,(muse-publishing-directive "issued"))
	      (content ((type . "application/xhtml+xml")
			(xml:space . "preserve"))
		       ,(atom-api:xml/parse-string content))
	      (muse-source nil
			   ,muse-atom:raw-content)))))
  (setq muse-atom:raw-content ""))


(defvar muse-atom-footer
  "</div>
   </content>
   <muse-source><lisp>muse-content</lisp></muse-source>
</entry>"
  "Footer used for publishing atom entries.")
