;;; atom-api.el --- Implementation of draft atom-api
;;; version 2006-01-07

;; Copyright (c) 2005-2006 Erik Hetzner

;; Author: Erik Hetzner <ehetzner@gmail.com>

;; This file is NOT part of GNU Emacs.
;;
;; atom-api.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; atom-api.el is distributed in the hope that it will be useful, but
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

;; This implements as nearly as I can tell the REST version of the
;; ATOM API in its latest draft.

;;; Sites known to work:

;; * blogger.com: no known issues (any more)

;;; Features:

;; * Internationalized; you should be able to post in any unicode
;; supported script.

;; * Supports WSSE for authentication, customize atom-api:use-wsse.


;; * Seems to work.

;;; Anti-features:

;; atom-api:prompt-for-defaults doesn't work. I can't remember how
;; this works with following links (ie, atom feeds which link to
;; others for older entries). There are surely bugs. Code needs clean
;; up.

;;; Installation
;;
;; Put the following in .emacs or wherever:
;; (autoload 'atom-api:entry/edit "atom-api"
;;  "Prompts for an entry to edit, opens in new buffer." t)
;; (autoload 'atom-api:entry/new "atom-api"
;;   "Create a new entry." t)
;; (autoload 'atom-api:entry/delete "atom-api"
;;   "Prompts for an entry to delete." t)

;;; Commentary:
;; 
;; It has been tested on emacs21 and 22 (cvs). The URL package may
;; cause you trouble; the latest version from Emacs CVS seems to
;; work. The one with emacs21 on debian unstable does not seem to
;; work. I am trying to keep a working copy at the projet web
;; site. Diffs for working on other emacs are welcomed.

;; You can customize atom-api:feed/seed-urls to point to atom apis.

;; This relies upon nxml-mode to edit atom entries. You may add some
;; filter functions to atom-api:filters/entry/pre-edit &
;; /post-edit. Filters are already there to fix blogger.com
;; peculiarities.

;; To begin a new entry, type M-x atom-api:entry/new. This will prompt
;; for a title and open a new buffer with your new entry.

;; To edit an entry, type M-x atom-api:entry/edit. You can then type
;; the title, date, or id of the entry to fetch & edit it.

;; After you edit a buffer run atom-api:entry/publish. This works for
;; new or edited buffers.

;; If you type return after running atom-api:entry/delete, it will try
;; to delete the currently edited entry.

;; To do:

;; Add support for 'magic' file extension; ie, be able to 'open' and
;; 'save' atom entries as files.

;; Clean up variable/function names.

;; Decrease deviation from spec; also, increase the conformity to the
;; spec.

;;; Code:

(require 'url)
(require 'url-auth)
(require 'url-cache)
(require 'xml)

;;debugging
(setq debug-on-error t)
(setq url-debug t)

(defgroup atom-api nil
  "Customization items for the Emacs Atom API."
  :group 'emacs)

(defcustom atom-api:author
  ""
  "Name to sign posts as."
  :group 'atom-api
  :type 'string)

(defcustom atom-api:feed/seed-urls
  '("http://www.blogger.com/atom/")
  "URLs to seed from."
  :group 'atom-api
  :type '(repeat string))

;prevents byte-compiler warnings
(defvar url-http-response-status)

(defvar atom-api:uri "http://purl.oclc.org/NET/emacs-atom-api/"
  "URI for emacs atom-api.")

(defvar atom-api:generator
  `(generator ((url . ,atom-api:uri)) "Atom API")
  "The generator of these entries.")

(defvar atom-api:link/posturl/prompt-history nil)

(defvar atom-api:link/posturl/lookup-table nil
  "Lookup of available posturis & info.")

(defvar atom-api:link/index-attribute-list '(title)
  "Lookup of available posturis & info.")

(defcustom atom-api:link/posturl/default
  nil
  "The default uri to post to."
  :group 'atom-api
  :type 'string)

(defvar atom-api:entry/lookup-table nil
  "Lookup mapping strings to info about an entry.")

(defvar atom-api:entry/element-index-list '(title id issued)
  "List of xml elements to index.")

(defvar atom-api:entry/prompt-history nil)

(defun atom-api:entry/edit-thunk ()
  "Function to start the mode used to edit entries."
    (nxml-mode)
    (goto-char (point-min))
    (re-search-forward "<content[^>]*>[ \t\n]*<div[^>]*>"))

;; Filters are functions which are called at certain places. Entries
;; are filtered in order according the the filter list. The filter
;; list consists of a list of lists of three elements. The first
;; element is a regexp to match against a url attached to the
;; item. The second is either 'data or 'tree; it describes whether the
;; filter should be passed an xml string or parse tree. Filter
;; functions should accept one argument, the entry.

;; Keep in mind that each 'data followed by 'tree, and vice-versa,
;; will require the entry to be transformed. This may be slow, though
;; probably shouldn't matter.

(defgroup atom-api:filters nil
  "Customization filters for the Emacs Atom API."
  :group 'atom-api)

(defvar atom-api:filters/customize-type
  '(alist
    :key-type string
    :value-type (list (restricted-sexp :match-alternatives ('data 'tree)) function))
    "The rather complicated type which describes filters for customize.")

(defcustom atom-api:filters/entry/pre-digest
  nil
  "These filters are called before each entry is digested."
  :group 'atom-api:filters
  :type atom-api:filters/customize-type)

(defcustom atom-api:filters/entry/pre-publish
  '(("blogger.com" 'data 
     atom-api:filter-data/entry/blogger-publish-workaround))
  "Filters which are called before an entry is published (ie, edited
OR new entries). This is applied AFTER post/put specific filters."
  :group 'atom-api:filters
  :type atom-api:filters/customize-type)

(defcustom atom-api:filters/entry/pre-put
  '(("blogger.com" 'data 
     (lambda (data) (atom-api:filter-data-util/delete-element "id" data)))
    ("blogger.com" 'data 
     (lambda (data)
       (atom-api:filter-data-util/insert-string
	(atom-api:xml/to-string-fragment atom-api:generator) "author" data))))
    "Filters which are called just before the entry is PUT (i.e., an
edit) to the server."
    :group 'atom-api:filters
    :type atom-api:filters/customize-type)

(defcustom atom-api:filters/entry/pre-post nil
  "Filters which are called on the entry before the data is POSTed
(i.e., a a new entry) to the server."
  :group 'atom-api:filters
  :type atom-api:filters/customize-type)

(defcustom atom-api:filters/entry/pre-edit
  '(("" 'data (lambda (data) (atom-api:filter-data-util/delete-element "modified" data)))
    ("" 'data (lambda (data) (atom-api:filter-data-util/delete-element "created" data)))
    ("" 'data (lambda (data) (atom-api:filter-data/remove-double-returns data))))    
  "Filters which are called before entry is edited."
  :group 'atom-api:filters
  :type atom-api:filters/customize-type)

(defcustom atom-api:filters/entry/post-edit
  nil
  "These filters are called after the entry has been edited, before it
is passed to the post/put filters."
  :group 'atom-api:filters
  :type atom-api:filters/customize-type)
  
(defcustom atom-api:prompt-for-defaults
  t
  "If yes, empty defaults are prompted for and customized."
  :group 'atom-api
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

;contr. by Masayuki Ataka
(defcustom atom-api:file-prefix nil
  "Your blog will be saved at `PREFIX'-`TIME'.xml.  If `nil', do not
create file but only prepare buffer."
  :group 'atom-api
  :type 'string)

(defcustom atom-api:url-request-function
  'atom-api:url-el/request
  "What to use for fetching urls."
  :group 'atom-api
  :type '(radio (const :tag "Curl program" atom-api:curl/request)
		(const :tag "Url package (elisp)" atom-api:url-el/request)))

(defcustom atom-api:use-wsse
  nil
  "If yes, use WSSE authentication."
  :group 'atom-api
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

;;some utility functions

;;it seems that newer versions of url use this; 'ignore' I think
;;should be is-case-relevant or some such; not guaranteed to work!
(if (not (functionp 'assoc-string))
    (defun assoc-string (key alist ignore)
      "Assoc using string-equal as equal function."
      (assoc-default key alist 'string-equal)))

(defun atom-api:util/process-filters (filter-list url entry)
  "Processes a list of filters against a node & url."
  (if (not filter-list)
      entry
    (if (not (string-match (car (car filter-list)) url))
	(atom-api:util/process-filters (cdr filter-list) url entry)
      (let* ((filter (car filter-list))
	     (mode (nth 1 filter))
	     (filter-func (nth 2 filter))
	     (new-entry
	      (funcall filter-func
		       (if (eq mode 'tree)
			   (atom-api:xml/parse-string entry)
			 (atom-api:xml/to-string entry)))))
	(atom-api:util/process-filters (cdr filter-list) url new-entry)))))

(defun atom-api:util/encode-string-to-utf (string)
;  (cond (= (string-to-multibyte
;  (encode-coding-string
;   (encode-coding-string string 'utf-8))); 'binary))
  (let ((temp-file-name (make-temp-file "atom-api"))
 	(coding-system-for-write 'utf-8)
 	(coding-system-for-read 'binary))
     (with-temp-file temp-file-name
       (insert string))
     (with-temp-buffer
       (insert-file-contents-literally temp-file-name)
       (buffer-string))))

(defun atom-api:util/decode-current-buffer-from-utf ()
  (let ((temp-file-name (make-temp-file "atom-api"))
	(coding-system-for-write 'binary)
	(coding-system-for-read 'utf-8))
    (write-region (point-min) (point-max) temp-file-name)
    (delete-region (point-min) (point-max))
    (set-buffer-multibyte t)
    (insert-file-contents temp-file-name nil nil nil t)))

;;returns the current date &time in w3cdtf format
(defun atom-api:util/w3cdtf () 
  (let ((tz-hours (/ (car (current-time-zone)) 3600)))
     (concat 
      (format-time-string "%Y-%m-%dT%T")
      (if (> tz-hours 0)
	  (format "+%02d:00" tz-hours)
	(format "%03d:00" tz-hours)))))

(defun xml-get-children (node child-name)
  "Return the children of NODE whose tag is CHILD-NAME.
CHILD-NAME should match the value returned by `xml-node-name'."
  (let ((match ()))
    (dolist (child (xml-node-children node))
      (if (and (listp child)
               (equal (xml-node-name child) child-name))
          (push child match)))
    (nreverse match)))

(defsubst xml-get-child (node element)
  "Returns the first node; useful for nodes with only one child element."
  (car (xml-get-children node element)))

;;fixed to work properly on string nodes
(defsubst xml-node-name (node)
  "Return the tag associated with NODE.
Without namespace-aware parsing, the tag is a symbol.

With namespace-aware parsing, the tag is a cons of a string
representing the uri of the namespace with the local name of the
tag.  For example,

    <foo>

would be represented by

    '(\"\" . \"foo\")."

  (if (listp node) (car node) nil))

(defsubst atom-api:xml/fix-parsed (node)
  "This silly function is necessary to fix the fact that xml.el
sometimes returns a node & sometimes a list of nodes; very confusing."
  (let (node-name (xml-node-name node))
    (if (not (and (symbolp node-name)
		  (stringp (car node-name))))
	(car node)
      node)))

(defsubst xml-node-text (node)
  "Returns the child text of the node."
  (cond ((stringp node)
	 node)
	((listp node)
	 (mapconcat 'xml-node-text (xml-node-children node) ""))))
    
;  (let* ((child-text
;	  (lambda (node)
;	(funcall child-text node)))

;;filtery; could easily fuck things up.
(defmacro let-func (old new  body)
  "Temporarily binds one function to another."
  `(prog2
       (progn 
	 (put ,new 'real-function (symbol-function ,old))
	 (fset ,old (symbol-function ,new)))
       ,body
     (fset ,old (get ,new 'real-function))
     (put ,new 'real-function nil)))

;;xml.el warns that this removes composition info but otherwise text
;;is screwy when output
(defun atom-api:xml/parse-buffer ()
  (let-func 'buffer-substring 'buffer-substring-no-properties
	    (let-func 'match-string 'match-string-no-properties
		      (atom-api:xml/fix-parsed
		       (xml-parse-region (point-min) (point-max))))))

(defun atom-api:xml/parse-string (string)
  "Parses the xml contained in string."
  (if (stringp string)
      (with-temp-buffer
	(insert string)
	(atom-api:xml/parse-buffer))
    string))

;;wrapper for xml-print which adds a few useful things.
(defun atom-api:xml/print (node)
  "Prints the xml-tree node in the current buffer."
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\n")
  (xml-debug-print (list node)))

(defun atom-api:xml/print-fragment (node)
  (xml-debug-print (list node)))

(defun atom-api:xml/to-string (node)
  "Returns the string of the xml-tree."
  (if (not (stringp node))
      (with-temp-buffer
	(atom-api:xml/print node)
	(buffer-string))
    node))

(defun atom-api:xml/to-string-fragment (node)
  "Returns the string of the xml-tree, without an <?xml ...> prolog."
  (with-temp-buffer
    (atom-api:xml/print-fragment node)
    (buffer-string)))

;;someday generic function for looking up info
(defun atom-api:util/lookup (lookup-table key)
  (assoc key lookup-table))

;;historical
(defmacro atom-api:util/add-to-alistq (alist key value)
  `(if (not (atom-api:util/assoc-string ,key ,alist))
       (setq ,alist (append ,alist (list (cons ,key ,value))))
     (cons ,key ,value)))

;(defun atom-api:util/add-to-alist (alist key value)
;  "Adds key and value to alist uniquely, like add-to-list."
;  (add-to-alist alist key value))
;  (if (not (assoc key alist))
;      (if alist
;	  (nconc alist (list (cons key value)))
;	(set alist (list (cons key value)))))
;  (cons key value))

(defun atom-api:util/add-to-lookup-table (key value lookup-table)
  (add-to-list lookup-table (cons key value)))

(defun atom-api:util/lookup-table-to-alist (lookup-table)
  lookup-table)

(defun atom-api:util/remove-from-alist (alist x &rest ignore)
  ;; Remove X from ALIST, return new alist
  ;; from w3-hot.el
  (if (eq (assoc x alist) (car alist)) (cdr alist)
    (delq (assoc x alist) alist)))
				      
; (set alist
;       (atom-api:util/mapcarshorten
;	(lambda (extant-key-val)
;	  (if (not (equal key (car extant-key-val)))
;	      extant-key-val))
;	(eval alist))))

(defun atom-api:util/remove-from-lookup-table (key value lookup-table)
  (atom-api:util/remove-from-alist lookup-table key))

(defun atom-api:util/narrow-http-to-body ()
  "Narrows the current buffer to body of HTTP message."
  (widen)
  (goto-char (point-min))
  (narrow-to-region
   (or (re-search-forward "\n\r?\n" nil t)
       (point-max))
   (point-max)))

(defun atom-api:util/prompt-for (prompt lookup-table history)
  (cdr (assoc
	(completing-read prompt 
			 (atom-api:util/lookup-table-to-alist lookup-table)
			 nil t nil history) lookup-table)))

(defun atom-api:util/get-longest-matching-substring 
  (&optional stringa stringb &rest strings)
  "Returns the longest substring which matches all the strings passed
to the function."
  (if (not stringb)
      stringa
    (let* ((compare-results
	    (compare-strings stringb 0 nil stringa 0 nil))
	   (substring-match
	    (substring stringa 0
		       (if (eq compare-results t)
			   ;; stringa & string are equal
			   (length stringa)
			 (- (abs compare-results) 1)))))
      (cond ((= 0 (length substring-match))
	     nil)
	    ((not strings)
	     substring-match)
	    (t (apply 'atom-api:util/get-longest-matching-substring 
		      substring-match strings))))))

(defun atom-api:util/mapcarshorten (func list)
  "Like mapcar, except that nil values are not added to the return
list."
  (if (not list) nil
    (let ((retval (funcall func (car list))))
      (if retval
	  (cons retval (atom-api:util/mapcarshorten func (cdr list)))
	(atom-api:util/mapcarshorten func (cdr list))))))

(defun atom-api:util/expanding-completion
 (alist expand-function string-fragment predicate ignore)
 "This function returns the longest common substring of all
possible completions of string in alist; if, however, there is no
match, it calls expand-function, which should expand the possible
completions. If expand-function returns nil, there are no more
completions; if t, there are more, which we should check against
again."
; (if (not lookup)
;     nil
   (let* ((string-fragment-regexp
	   (concat "^" (regexp-quote string-fragment)))
	  (possible-string-p
	   (lambda (possible-string)
	     (if (string-match string-fragment-regexp possible-string)
		 (if predicate
		     (funcall predicate possible-string)
		   possible-string))))
	  (possible-strings
	   (atom-api:util/mapcarshorten
	    (lambda (string-cons)
	      (funcall possible-string-p (car string-cons)))
	    alist)))
     (if (not (= (length possible-strings) 0))
	 ;;we have some values, so find the largest possible substring
	 (apply 'atom-api:util/get-longest-matching-substring possible-strings)
       ;;otherwise we expand
       (if expand-function
	   ;;if we have found no values & we have an expanding function
	   (progn
	     (message "Finding more values...")
	     (atom-api:util/expanding-completion
	      (funcall expand-function) nil string-fragment predicate ignore))
	 ;;if no expand-function, there are no completions
	 nil))))

(defun atom-api:util/foreach-child-element
  (node elements function &rest args)
  "Calls function with args for each of node's child element in
elements."
  (dolist (element elements)
    (dolist (child-node (xml-get-children node element))
      (apply function (xml-node-text child-node) node args))))
  
(defun atom-api:util/populate-lookup-table-with-elements
  (lookup-table elements node)
  "Adds elements of node to lookup-table."
  (atom-api:util/foreach-child-element
   node elements 'atom-api:util/add-to-lookup-table lookup-table))

(defun atom-api:util/depopulate-lookup-table-with-elements
  (lookup-table elements node)
  "Remove child elements of node from lookup-table."
  (atom-api:util/foreach-child-element 
   node elements
   'atom-api:util/remove-from-lookup-table lookup-table))

(defun atom-api:util/foreach-attribute
  (node attributes function &rest args)
  "Calls function with args for each attribute in attributes."
  (dolist (attribute attributes)
    (apply function (xml-get-attribute node attribute) node args)))

(defun atom-api:util/populate-lookup-table-with-attributes
  (lookup-table attributes node)
  (atom-api:util/foreach-attribute
   node attributes 'atom-api:util/add-to-lookup-table lookup-table))

(defun atom-api:util/depopulate-lookup-table-with-attributes
  (lookup-table attributes node)
  (atom-api:util/foreach-attribute
   node attributes 'atom-api:util/remove-from-lookup-table lookup-table))

(defun atom-api:util/sgml-quote (start end)
   "Quote SGML text in region START ... END.
 Only &, < and > are quoted, the rest is left untouched."
   (interactive "r")
   ;; This code was copied from the function `sgml-quote' in
   ;; `sgml-mode.el' (CVS Emacs, 2005-05-31) and adapted accordingly.
   ;; by Masayuki Ataka
   (save-restriction
     (narrow-to-region start end)
     (goto-char (point-min))
     (while (re-search-forward "[&<>]" nil t)
       (replace-match (cdr (assq (char-before) 
				 '((?& . "&amp;")
				   (?< . "&lt;")
				   (?> . "&gt;"))))
		      t t))))

;;functions & variables related to feed urls & trees
(defvar atom-api:feed/digested-urls '("")
  "List of feed urls which have already been digested.")

(defun atom-api:feed/ingest-url (feedurl &optional mode)
  "Retrieves the feed located at URI and calls
atom-api:feed/ingest-url-callback as a callback."
  (if (not (member feedurl atom-api:feed/digested-urls))
      (atom-api:url/request 
       "" "GET" feedurl mode
       'atom-api:feed/ingest-url-callback (list feedurl))))
    
(defun atom-api:feed/ingest-url-callback (feedurl)
  "Callback to be used to process a buffer containing a HTTP
response containing an atom feed."
  (save-excursion
    (atom-api:util/narrow-http-to-body)
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (atom-api:feed/digest (atom-api:xml/parse-buffer) feedurl)
    ;;otherwise we don't get populated list...doesn't make sense
    (sleep-for 1)))

(defun atom-api:feed/digest (node feedurl)
  (dolist (child-node (xml-node-children node))
    (let ((node-name (xml-node-name child-node)))
      (cond ((eq node-name 'entry)
	     (atom-api:entry/digest child-node feedurl))
	    ((eq node-name 'link)
	     (atom-api:link/digest child-node feedurl)))))
  (add-to-list 'atom-api:feed/digested-urls feedurl))

;;functions related to entries
(defun atom-api:entry/lookup (key)
  "Retuns the entry mapped to by key."
  (atom-api:util/lookup atom-api:entry/lookup-table key))

;;;###autoload
(defun atom-api:entry/new ()
  "Create a new entry."
  (interactive)
  (let* ((entry (atom-api:xml/parse-string
		 (atom-api:util/process-filters
		  atom-api:filters/entry/pre-edit ""
		  (atom-api:entry/generate-new))))
	 (buffer (atom-api:util/new-buffer entry)))
;	 (buffer (generate-ne
;		  (xml-node-text
;		   (xml-get-child entry 'title)))))
    (switch-to-buffer buffer)
    (insert (atom-api:xml/to-string (atom-api:entry/to-editable (atom-api:xml/parse-string entry))))
    (atom-api:entry/edit-thunk)))

(defun atom-api:util/new-buffer (entry)
  "Return your new blog's buffer name.  See also
atom-api:file-prefix."
  (if atom-api:file-prefix
      (find-file-noselect (format "%s-%s.xml"
				  atom-api:file-prefix
				  (xml-node-text
				   (xml-get-child entry 'issued))))
    (generate-new-buffer (xml-node-text
			  (xml-get-child entry 'title)))))

(defun atom-api:entry/to-editable (entry)
  "Returns user-editable entry."
;;  (message (atom-api:xml/to-string entry))
  (atom-api:xml/to-string entry))

(defun atom-api:entry/from-editable (text)
  "Turns used-edited entry into atom entry."
  (atom-api:xml/parse-string
   (atom-api:util/process-filters
    atom-api:filters/entry/post-edit "" text)))

(defun atom-api:entry/generate-new ()
  "Returns an xml-tree for a generic new entry."
  (let* ((title (read-string "Title: ")))
    `(entry ((xmlns . "http://purl.org/atom/ns#"))
	    (generator ((url . "http://purl.org/net/emacs-atom-api/"))
		       "Elisp Atom API")
	    (author nil
		    (name nil ,atom-api:author))
	    (issued nil ,(atom-api:util/w3cdtf))
	    (title ((mode . "escaped") (type . "text/html")) ,title)
	    (content ((type . "application/xhtml+xml")
		      (xml:space . "preserve"))
		     (div ((xmlns . "http://www.w3.org/1999/xhtml")) "")))))

(defun atom-api:entry/digest (node feedurl)
  "Builds lookup keys for the entry."
  (if (atom-api:entry/get-edit-link node)
      (atom-api:util/populate-lookup-table-with-elements
       'atom-api:entry/lookup-table
       atom-api:entry/element-index-list
       (atom-api:xml/parse-string
	(atom-api:util/process-filters
	 atom-api:filters/entry/pre-digest feedurl node)))))

(defun atom-api:entry/vomit (node)
  "Removes lookup keys for the entry."
  (if (atom-api:entry/get-edit-link node)
      (atom-api:util/depopulate-lookup-table-with-elements
       atom-api:entry/lookup-table
       atom-api:entry/element-index-list
       node)))

(defun atom-api:entry/prompt-for (prompt-string)
  "Prompt for an entry to use."
  (atom-api:util/prompt-for 
   prompt-string atom-api:entry/lookup-table
   atom-api:entry/prompt-history))

;;;###autoload
(defun atom-api:entry/edit ()
  "Prompts for an entry to edit, opens in new buffer"
  (interactive)
  (atom-api:init 'sync)
  (let ((entry (atom-api:entry/prompt-for "Entry: ")))
    (switch-to-buffer (xml-node-text (xml-get-child entry 'title)))
    (if (not (buffer-modified-p))
	(progn
	  (insert (atom-api:entry/to-editable 
		   (atom-api:util/process-filters
		    atom-api:filters/entry/pre-edit ""
		    (atom-api:xml/parse-string entry))))
	  (atom-api:entry/edit-thunk)))))

;;;###autoload
(defun atom-api:entry/delete ()
  "Prompts for an entry to delete, or deletes the current entry."
  (interactive)
  (atom-api:init 'sync)
  (let* ((entry 
	  (or (atom-api:entry/prompt-for 
	       "Entry to delete (or return for current): ")
	      (atom-api:entry/from-editable (buffer-string))))
	 (entry-url
	  (xml-get-attribute (atom-api:entry/get-edit-link entry) 'href)))
    (atom-api:url/request "" "DELETE" entry-url  'async
			   'atom-api:entry/delete-callback (list entry))))

(defun atom-api:entry/delete-callback (entry)
  (if (eq 200 url-http-response-status)
      (progn
	(atom-api:entry/vomit entry)
	(message (concat
		  (xml-node-text (xml-get-child entry 'title))
		  " deleted.")))
    (message (format "Error: %d" url-http-response-status))))

(defun atom-api:entry/get-edit-link (entry)
  (let ((link-walker nil))
    (setq link-walker
	 (lambda (links)
	   (cond ((null links) nil)
		 ((string-equal "service.edit" 
				(xml-get-attribute (car links) 'rel))
		  (car links))
		 (t (funcall link-walker (cdr links))))))
    (funcall link-walker (xml-get-children entry 'link))))

(defvar atom-api:wsse/user nil)
(defvar atom-api:wsse/password nil)

(defun atom-api:wsse/timestamp (&optional time)
  (format-time-string "%Y-%m-%eT%TZ" time t))

(defun atom-api:wsse/nonce ()
  (number-to-string (random t)))

(defun atom-api:wsse/digest (password &optional timestring nonce)
  (let ((timestring (or timestring (atom-api:wsse/timestamp)))
	(nonce (or nonce (atom-api:wsse/nonce))))
    (base64-encode-string (sha1-binary (concat nonce timestring password)))))

(defun atom-api:wsse/headers ()
  (progn
    (if (not atom-api:wsse/user)
	(setq atom-api:wsse/user (read-from-minibuffer "Username: ")))
    (if (not atom-api:wsse/password)
	(setq atom-api:wsse/password (read-from-minibuffer "Password: ")))
    (let* ((timestamp (atom-api:wsse/timestamp))
	   (nonce (atom-api:wsse/nonce))
	   (digest (atom-api:wsse/digest atom-api:wsse/password timestamp nonce))
	   (wsse
	    (format "Username=\"%s\", PasswordDigest=\"%s\", Created=\"%s\", Nonce=\"%s\""
		    atom-api:wsse/user digest timestamp (base64-encode-string nonce))))
      (list '("Authorization" . "WSSE profile=\"UsernameToken\"")
	    (cons "X-WSSE" (concat "UsernameToken " wsse))))))

(defvar atom-api:wsse/user nil)
(defvar atom-api:wsse/password nil)

(defun atom-api:wsse/timestamp (&optional time)
  (format-time-string "%Y-%m-%eT%TZ" time t))

(defun atom-api:wsse/nonce ()
  (number-to-string (random t)))

(defun atom-api:wsse/digest (password &optional timestring nonce)
  (let ((timestring (or timestring (atom-api:wsse/timestamp)))
	(nonce (or nonce (atom-api:wsse/nonce))))
    (base64-encode-string (sha1-binary (concat nonce timestring password)))))

(defun atom-api:wsse/headers ()
  (progn
    (if (not atom-api:wsse/user)
	(setq atom-api:wsse/user (read-from-minibuffer "Username: ")))
    (if (not atom-api:wsse/password)
	(setq atom-api:wsse/password (read-from-minibuffer "Password: ")))
    (let* ((timestamp (atom-api:wsse/timestamp))
	   (nonce (atom-api:wsse/nonce))
	   (digest (atom-api:wsse/digest atom-api:wsse/password timestamp nonce))
	   (wsse
	    (format "Username=\"%s\", PasswordDigest=\"%s\", Created=\"%s\", Nonce=\"%s\""
		    atom-api:wsse/user digest timestamp (base64-encode-string nonce))))
      (list '("Authorization" . "WSSE profile=\"UsernameToken\"")
	    (cons "X-WSSE" (concat "UsernameToken " wsse))))))

(defun atom-api:url-el/request (body method url mode callback &optional cbargs)
  "Request (ie, post, put, delete) body to the url."
  (let* ((url-request-method method)
	 (url-mime-accept-string "application/atom+xml")
	 (url-request-extra-headers
	  (list
	   (if (equal body "") nil
	     '("Content-type" . "application/atom+xml; charset=utf-8"))
	   '("Accept-charset: \"utf-8\"")))
	 (url-request-extra-headers
	  (if atom-api:use-wsse
	      (dolist (hdr (atom-api:wsse/headers) url-request-extra-headers)
		(setq url-request-extra-headers 
		      (cons hdr url-request-extra-headers)))))
  	 (url-request-data (if (equal body "") nil body)))
;    (if (eq mode 'sync)
   
    ;;the following is extraordinarily complicated but the only way I
    ;;can get encoding right.
    (save-excursion
      (set-buffer (url-retrieve-synchronously url))
      (let ((temp-file-name (make-temp-file "atom-api"))	
	;;    (original-buffer (current-buffer))
	    (coding-system-for-read 'utf-8)
	    (coding-system-for-write 'binary))
	(write-region (point-min) (point-max) temp-file-name)
	;;(set-buffer (generate-new-buffer "temp"))
	(erase-buffer)
	(insert-file-contents temp-file-name)
	(if callback (apply callback cbargs))))))
;      (url-retrieve url callback cbargs))))

(defvar atom-api:curl/user nil)
(defvar atom-api:curl/password nil)

(defun atom-api:curl/request
  (body method url mode callback &optional cbargs)
  "Request (ie, post, put, delete) data to the url (using curl)."
  (if (not atom-api:curl/user) 
      (setq atom-api:curl/user (read-from-minibuffer "Username: ")))
  (if (not atom-api:curl/password) 
      (setq atom-api:curl/password (read-passwd "Password: ")))
  (let* ((which-buffer (generate-new-buffer "atom-curl"))
	 (response-code-buffer (generate-new-buffer "atom-curl-code"))
	 (response-data-temp-file (make-temp-file "atom-api"))	
	 (request-body-temp-file (make-temp-file "atom-api-curl-data"))
	 (curl-args
	  `("--request" ,method
	    "--user"
	    ,(concat atom-api:curl/user ":" atom-api:curl/password)
	    ,@(if (and body (not (string= body "")))
		 (list "--data-binary"
		       (concat "@" request-body-temp-file)))
	    "-o" ,response-data-temp-file
	    "--header" 
	    "Content-type: application/atom+xml; charset=utf-8"
	    "--header" "Accept-charset: utf-8"
	    ,@(if atom-api:use-wsse
		  (mapcar (lambda (x) (list "--header" x))
			  (atom-api:wsse/headers)))
	    "-w" "%{http_code}"
	    "--include" "-s" "--location-trusted" ;; single args
	    ,url)))
    (save-excursion
      (if (and body (not (string= body "")))
	  (let ((coding-system-for-write 'binary))
	    (with-temp-file request-body-temp-file
	      (insert body))))
      (set-buffer response-code-buffer)
      (message "curl args: %s" curl-args) 
      (apply 'call-process "curl" nil t nil curl-args)
      (if callback 
	  (let ((url-http-response-status (string-to-number (buffer-string))))
	    (with-temp-buffer 
	      (insert-file-contents response-data-temp-file)
	      (apply callback cbargs)))))))

(defun atom-api:url/request (&rest args)
  (apply atom-api:url-request-function args))

(defun atom-api:entry/publish ()
  "Publish the current buffer."
  (interactive)
  (atom-api:init 'sync)
  (let* ((raw-entry 
	  (atom-api:entry/from-editable
	   (buffer-substring-no-properties (point-min) (point-max))))
	 (edit-link (atom-api:entry/get-edit-link raw-entry))
	 (url (xml-get-attribute
	       (or edit-link
		   atom-api:link/posturl/default
		   (atom-api:link/posturl/prompt-for "Publish to: "))
	       'href))
	 (entry (atom-api:util/process-filters
		 atom-api:filters/entry/post-edit url
		 raw-entry))
	 (method (if (not edit-link)
		     "POST"
		   "PUT"))
	 (dirty-entry
	  (atom-api:util/process-filters
	   atom-api:filters/entry/pre-publish url
	   (cond ((equal method "PUT")
		  (atom-api:util/process-filters
		   atom-api:filters/entry/pre-put url entry))
		 ((equal method "POST")
		  (atom-api:util/process-filters
		   atom-api:filters/entry/pre-post url entry)))))
    (old-entry (atom-api:entry/lookup
		     (xml-node-text (xml-get-child entry 'id)))))
    (atom-api:url/request
     (atom-api:util/encode-string-to-utf
      (atom-api:xml/to-string dirty-entry))
     method url 'sync 'atom-api:entry/publish-callback 
     (list entry old-entry method url))))

(defun atom-api:entry/publish-callback (entry old-entry method entry-url)
  "Function which is called when the item has been posted."
  (atom-api:util/narrow-http-to-body)
  (set-buffer-multibyte t)
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (cond ((eq 200 url-http-response-status)
	 (cond ((string= method "PUT")
		(atom-api:entry/vomit old-entry)
		(atom-api:entry/digest (atom-api:xml/parse-buffer) entry-url)
		(message (concat "Published edit successfully to " entry-url)))
	       ((string= method "POST")
		(atom-api:util/narrow-http-to-body)
					;(decode-coding-region (point-min) (point-max) 'utf-8)
		(atom-api:entry/digest (atom-api:xml/parse-buffer) entry-url)
		(message
		 (concat "Published new entry successfully to " entry-url)))))
	(t
	 (url-debug 'http (buffer-string))
	 (message (format "Error: %d" url-http-response-status)))))

;;functions & variables related to links
(defun atom-api:link/digest (node feedurl)
  (let ((rel (xml-get-attribute node 'rel)))
    (cond ((string= rel "service.post")
	   (atom-api:util/populate-lookup-table-with-attributes
	    'atom-api:link/posturl/lookup-table
	    atom-api:link/index-attribute-list
	    node))
	  ((and (string= rel "service.feed")
		(not (string= feedurl (xml-get-attribute node 'href)))) ;; don't follow to self
	   (atom-api:feed/ingest-url (xml-get-attribute node 'href) 'sync)))))

(defun atom-api:link/posturl/prompt-for (prompt-string)
  "Prompt for a posturi (or title) to use."
  (atom-api:util/prompt-for prompt-string atom-api:link/posturl/lookup-table atom-api:link/posturl/prompt-history))

(defun atom-api:init (&optional mode)
  (dolist (url atom-api:feed/seed-urls)
    (atom-api:feed/ingest-url url mode)))
  
(defun atom-api:reinit (&optional mode)
  (setq atom-api:link/posturl/lookup-table nil	
	atom-api:entry/lookup-table nil
	atom-api:link/posturl/lookup-table nil
	atom-api:feed/digested-urls '(""))
  (atom-api:init 'sync))

;;some example filters
(defun atom-api:util/encode-element (element encode-function &optional attribute attribute-value)
  "This filter takes the data contained in the element and encodes it
with encode function. If attribute and attribute-value are supplied,
makes sure that these are set in the element (e.g., attribute:
\"encoding\" and attribute-value: \"base-64\")."
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "<" element " "))
    (let ((start (point)))
      (search-forward ">")
      (cond ((and attribute attribute-value)
	     ;; should we add attribute?
	     (cond ((search-backward (concat attribute "=\"") start 'third)
		    ;;attribute already exists; replace it
		    (re-search-forward "\"[^\"]+\"")
		    (replace-match (concat "\"" attribute-value "\"")))
		   (t ;;attribute doesn't exist; add it.
		    (insert (concat " " attribute "=\"" attribute-value "\" "))))
	     ;;go back to where we started
	     (search-forward ">")))
      (let* ((text-start (point))
	     (raw-string (buffer-substring text-start (point)))
	     (element-end-tag (concat "</" element ">")))
	(search-forward element-end-tag)
	(goto-char (match-beginning 0))
;	(backward-char (length element-end-tag))
	(delete-region text-start (point))
	(insert (funcall encode-function raw-string))))))

(defun atom-api:filter-data/entry/base64-encode-body (data)
  "This base64 encodes the <content> element."
  (with-temp-buffer
    (insert data)
    (atom-api:util/encode-element "content" 'base64-encode-string "mode" "base64")))

(defun atom-api:filter-data/entry/hexify-title (data)
  "This hex-encodes the <title> element."
  (with-temp-buffer
    (insert data)
    (atom-api:util/encode-element "title" 'url-hexify-string "mode" "escaped")
    (buffer-string)))

(defun atom-api:filter-data/entry/hexify-content (data)
  "This hex-encodes the <content> element."
  (with-temp-buffer
    (insert data)
    (atom-api:util/encode-element "content" 'url-hexify-string "mode" "escaped")
    (buffer-string)))

(defun atom-api:filter-data-util/delete-element (element data)
  "This removes the first instance of the specified element from the data."
  (with-temp-buffer
    (insert data)
    (goto-char (point-min))
    (let ((element-start (concat "<" element)))
      (if (search-forward element-start nil t)
	  (let ((element-start-point (- (point) (length element-start))))
	    (search-forward (concat "</" element ">"))
	    (delete-region element-start-point (point)))))
    (buffer-string)))

(defun atom-api:filter-data-util/insert-string (insert-string after-element data)
  "Inserts the insert-string after after-element in data; returns new
data."
  (with-temp-buffer
    (insert data)
    (goto-char (point-min))
    (if (search-forward (concat "</" after-element ">") nil t)
	(insert insert-string))
      (buffer-string)))

(defun atom-api:filter-data/remove-double-returns (data)
  (with-temp-buffer
    (insert data)
    (goto-char (point-min))
    (while (search-forward "\n\n" nil t)
      (replace-match "\n"))
    (buffer-string)))

(defun atom-api:filter-data/entry/blogger-publish-workaround (data)
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert data)
    (goto-char (point-min))
    (re-search-forward "<content[^>]*>" nil t)
    (replace-match "<content mode=\"escaped\" type=\"text/html\">")
    (goto-char (point-min))
    (atom-api:util/sgml-quote (re-search-forward "<content[^>]*>")
			      (progn (re-search-forward "</content>")
				     (match-beginning 0)))
;    (re-search-forward "<content[^>]*>")
;    (narrow-to-region (point)
;		      (progn (re-search-forward "</content>")
;			     (match-beginning 0)))
;    (goto-char (point-min))
;    (while (search-forward "<" nil t)
;      (replace-match "&lt;"))
;    (goto-char (point-min))
;    (while (search-forward ">" nil t)
;      (replace-match "&gt;"))
;    (widen)
    (goto-char (point-max))
    (insert "\n\n")
    (buffer-string)))

(provide 'atom-api)

;;; atom-api.el ends here.