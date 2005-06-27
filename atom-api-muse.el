;;; atom-api-muse.el --- Muse (wiki publishing) extension for atom-api.el
;;; version 2005-06-26

;; Copyright (c) 2005 Erik Hetzner

;; Author: Erik Hetzner <ehetzner@gmail.com>

;; This file is NOT part of GNU Emacs.
;;
;; atom-api-muse.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; atom-api-muse.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. (Or get it from fsf.org.)

;;; Introduction

;; http://purl.org/net/emacs-atom-api/

;; This adds Muse <http://www.mwolson.org/projects/MuseMode.html>
;; support to atom-api.el. This means you can use wiki-like sytanx to
;; create entries. See the Muse manual for information on muse
;; syntax. Anything supported by muse-html should work.

;;; Features:

;; All the features of atom-api.el without staring at XML.

;;; Anti-features:

;; Probably doesn't work well.

;;; Installation
;;
;; Put the following in .emacs or wherever:
;; (autoload 'atom-api:entry/edit "atom-api-muse"
;;  "Prompts for an entry to edit, opens in new buffer." t)
;; (autoload 'atom-api:entry/new "atom-api-muse"
;;   "Create a new entry." t)
;; (autoload 'atom-api:entry/delete "atom-api-muse"
;;   "Prompts for an entry to delete." t)

;;; Code:

(provide 'atom-api-muse)
(require 'muse-atom)
(require 'atom-api)

(defun atom-api:entry/edit-thunk ()
  (if (save-excursion
	(goto-char (point-min))
	(looking-at "#"))
      (progn
	(re-search-forward "\n\n" nil t )
	(muse-mode))
    (nxml-mode)))

(defun atom-api:entry/to-editable (entry)
  "Turns an entry into muse form IF we can find a <muse-source> element or there is
no content yet (entry is new)."
  (if (or (string= "" (nth 2 (xml-get-child (xml-get-child entry 'content) 'div)))
	  (not (nth 2 (xml-get-child (xml-get-child entry 'content) 'div)))
	  (xml-get-child entry 'muse-source))  
      (let ((author (xml-node-text (xml-get-child (xml-get-child entry 'author) 'name)))
	    (title (xml-node-text (xml-get-child entry 'title)))
	    (issued (xml-node-text (xml-get-child entry 'issued)))
	    (content (xml-node-text (xml-get-child entry 'muse-source))))
	(concat
	 "#title " title "\n"
	 "#author " author "\n"
	 "#issued " issued "\n\n"
	 content))
    entry))

(defun atom-api:entry/from-editable (raw-content)
  "Turns entry into xml."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert raw-content)
    (muse-publish-markup-buffer "Untitled" "atom")
    (atom-api:xml/parse-buffer)))
