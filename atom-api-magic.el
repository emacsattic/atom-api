;; examples of magic names of atom resources:

;; canonical: atom:http://example.org/collection/entry
;; entry in collection: atom:Collection title:title:Entry title
;; might be good for specifying pics: atom:Collection title:/entry/uri
;; other metatdata about entries besides title: atom:Collection title:id:entry id
;; uses current collection, if such a beast: atom:title:some entry

;etc.

(require 'atompub)

(add-to-list
 'file-name-handler-alist
 (cons "^atom:" 'atompub:magic/handler))

(defun atompub:magic/handler (op resource &rest rest)
  (let ((function-name
	 (intern (concat "atompub:magic/" (symbol-name op)))))
    (if (functionp function-name)
	(apply function-name resource rest)
      (error "atompub: Doesn't support file operation %s" op)))
    
(defun atompub:entry/cache/exists (uri)
  "Returns boolean existence of uri entry in cache.")

(defun atompub:entry/exists (uri)
  "Returns boolean of existence of entry described by uri."
  (or (atompub:entry/cache/exists (uri))
      (atompub:entry/cache/add (uri))))

;; functions which implement atom forms of file handlers
(defun atompub:magic/access-file (uri error-string)
  (if (and (atompub:entry/exists uri)
	   (atompub:entry/writable uri))
      nil
    (error "Error opening atom doc %s for editing" error-string)))

(defun atompub:magic/add-name-to-file (resource newname &optional or-if-already-exists)
  (error "atompub: Can't add names to atom entries"))

(defun atompub:magic/byte-compiler-base-file-name (resource)
  (error "atompub: Can't byte-compile atom entries"))

(defun atompub:magic/copy-file (resource new-resource &optional ok-if-already-exists keep-time)
  (let* ((c-new-resource (atompub:resource/canonicalize new-resource))
	 (c-new-collection
	  (atompub:member/get-collection c-new-resource))
	 (c-new-member-local-name
	  (atompub:member/get-local-name c-new-resource))
	 (c-resource (atompub:member/canonicalize resource)))
    (if (and c-new-resource
	     c-resource)
	(let ((resource-data (atompub:member/retrieve c-resource)))
	  (atompub:url/request 
	   resource-data "POST" c-new-collection 'async 
	   'atompub:magic/copy-file-cb (list c-resource c-new-resource) 
	   '(("Name:" . c-new-member-local-name)))))))

(defun atompub:magic/copy-file-cb (resource new-resource)
  
(defun atompub:magic/delete-directory (directory)
  (error "atompub: Can't delete collections"))
  ;;(atompub:collection/delete uri))

(defun atompub:magic/delete-file (uri)
  (atompub:member/delete uri))

(defun atompub:magic/directory-file-name (collection)
  "Returns the uri of the collection named collection."
  (string-match "^(.*)/?$)" collection)
  (match-string 1))

(defun atompub:magic/directory-files (collection &optional full match nosort)
  "Returns the entries in the collection."
					;todo
  )

;(defun atompub:magic/dired-call-process (...))

;(defun atompub:magic/dired-compress-file (...))

;(defun atompub:magic/dired-uncache (...))

(defun atompub:magic/expand-file-name (resource &optional default-collection)
  "Returns the canonical representation of the resource (ie, a uri)."
  (atompub:resource/canonicalize resource)

;(defun atompub:magic/file-accessible-directory-p',

;(defun atompub:magic/file-attributes (...))

(defun atompub:magic/file-directory-p (resource)
  (atompub:collection/exists-p 
   (atompub:resource/canonicalize resource)))

(defun atompub:magic/file-executable-p (resource)
  "Returns t if resource is a known collection; members cannot be executable."
  (atompub:collection/exists-p (atompub:resource/canonicalize)))

(defun atompub:magic/file-exists-p (resource)
  (atompub:resource/exists-p resource))

(defun atompub:magic/file-local-copy (resource)
  "Returns a local copy of an atom entry; returns file name."
  (if (atompub:resource/exists-p)
      (let ((temp-file (make-temp-file "atompub")))
	(with-temp-file
	    (atompub:magic/insert-file-contents uri))
	temp-file)))
  
;; (defun atompub:magic/file-modes (...))

(defun atompub:magic/file-name-all-completions (file directory)
  
(defun atompub:magic/file-name-as-directory (resource)
  (concat resource "/"))

;; (defun atompub:magic/file-name-completion (...))
(defun atompub:magic/file-name-directory (resource)
  "Returns the collection part of the resource."
  (if (string-match "^(atom:[^:]+)" resource)
      (match-string 1)
    nil)

(defun atompub:magic/file-name-nondirectory (resource)
  (if (string-match "^atom:[^:]+:(.*)$")
      (concat "atom:" (match-string 1))
    nil))

(defun atompub:magic/file-name-sans-versions (resource &optional keep-backup-version)
  resource)

;; (defun atompub:magic/file-newer-than-file-p (...))
;; (defun atompub:magic/file-ownership-preserved-p (...))

(defun atompub:magic/file-readable-p (resource)
  "Returns t if the resource exists; all known resources are readable."
  (atompub:resource/exists-p resource))

(defun atompub:magic/file-regular-p (resource)
  "Returns t if the resource exists; all known resources are normal."
  (atompub:resource/exists-p resource))

(defun atompub:magic/file-symlink-p (resource)
  "Returns nil; no such beast known to man or the atom spec."
  nil)

(defun atompub:magic/file-truename (resource &optional counter prev-dirs)
  "Returns resource with "
  (atompub:magic/file-truename
  ;; on the other hand, it might be necessary  at some point to translate
  ;; stuff like atom:title:Some title to a uri with this function.
  uri)

;; (defun atompub:magic/file-writable-p (...))
;; (defun atompub:magic/find-backup-file-name (...))
;; (defun atompub:magic/get-file-buffer (...))
;; (defun atompub:magic/insert-directory (...))

(defun atompub:magic/insert-file-contents (uri &optional visit beg end replace)
  (if replace
      (erase-buffer)) ;; this is wrong
  ;;todo: implement beg & end & visit
  (with-temp-buffer
    (atompub:xml/to-string
     (atompub:entry/retrieve uri))))

(defun atompub:magic/load (file &optional noerror nomessage nosuffix must-suffix)
  (error "atompub: Can't load atom entries."))

(defun atompub:magic/make-directory (dir &optional parents)
  (error "atompub: Can't create atom collections."))

(defun atompub:magic/make-symbolic-link (&rest args)
  (error "atompub: Can't symlink atom entries."))

(defun atompub:magic/rename-file (&rest args)
  (error "atompub: It is not possible to 'move' atom entries; try
editing the entry & changing its title, etc."))

(defun atompub:magic/set-file-modes (&rest args)
  (error "atompub: Atom entires don't have modes."))

(defun atompub:magic/set-visited-file-modtime (&optional time-list)
  ;todo: update entry:modtime
  )

(defun atompub:magic/shell-command (&rest args)
  (error "atompub: You crazy.")

;(defun atompub:magic/unhandled-file-name-directory (...))

;(defun atompub:magic/vc-registered (...))
(defun atompub:magic/verify-visited-file-modtime (buf)
  ;todo
)

(defun atompub:magic/write-region (...))
