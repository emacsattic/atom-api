;;; muse-atom.el --- Turn Muse documents into atom entries.
;;; version 2005-06-27

;; Copyright (c) 2005 Erik Hetzner

;; Author: Erik Hetzner <ehetzner@gmail.com>

;; This file is NOT part of GNU Emacs.
;;
;; muse-atom.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; muse-atom.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. (Or get it from fsf.org.)

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

(defvar muse-atom-footer
  "</div>
   </content></entry>"
  "Footer used for publishing atom entries.")

;; (defun muse-atom:before-hook ()
;;   (setq muse-atom:raw-content (buffer-substring-no-properties (point-min) (point-max))))

;; (defun muse-atom:after-hook ()
;;   (let ((content (buffer-substring-no-properties (point-min) (point-max))))
;;     (delete-region (point-min) (point-max))
;;     (insert
;;      (atom-api:xml/to-string
;;       `(entry ((xmlns . "http://purl.org/atom/ns#"))
;; 	      (generator ((url . "http://purl.org/net/emacs-atom-api/"))
;; 			 "Elisp Atom API+Muse")
;; 	      (author nil
;; 		      (name nil ,(muse-publishing-directive "author")))
;; 	      (issued nil 
;; 		      ,(muse-publishing-directive "issued"))
;; 	      (title ((mode . "escaped") (type . "text/html"))
;; 		     ,(muse-publishing-directive "issued"))
;; 	      (content ((type . "application/xhtml+xml")
;; 			(xml:space . "preserve"))
;; 		       ,(atom-api:xml/parse-string content))
;; 	      (muse-source nil
;; 			   ,muse-atom:raw-content)))))
;;   (setq muse-atom:raw-content ""))

(provide 'muse-atom)
;;; muse-atom.el ends here.
