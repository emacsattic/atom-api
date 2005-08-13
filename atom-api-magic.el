;canonical: atom:http://example.org/uri/of/some/entry
;

(require 'atom-api)

(add-to-list
 'file-name-handler-alist
 (cons "^atom:" 'atom-api:magic/handler))
 
(add-to-list
 'file-name-handler-alist
 (cons "^atom:http://" 'atom-api:magic/uri-handler))

(defun atom-api:magic/handler (op atom-entry &rest rest)
  (let ((function-name
	 (intern (concat "atom-api:magic/" (symbol-name op))))
	(entry-uri atom-api:entry/description-to-uri atom-entry))
    (apply atom-api:magic/uri-handler op uri rest)))

(defun atom-api:magic/uri-handler (op uri &rest rest)
  (if (functionp function-name)
      (apply function-name uri rest)
    nil))

(defun atom-api:entry/cache/exists (uri)
  "Returns boolean existence of uri entry in cache.")

(defun atom-api:entry/exists (uri)
  "Returns boolean of existence of entry described by uri."
  (or (atom-api:entry/cache/exists (uri))
      (atom-api:entry/cache/add (uri))))

(defun atom-api:magic/access-file (uri error-string)
  (if (and (atom-api:entry/exists uri)
	   (atom-api:entry/writable uri))
      nil
    (error "Error opening atom doc %s for editing" error-string)))

(defun atom-api:magic/add-name-to-file (uri newname &optional of-if-already-exists)
  (error "atompub: Can't add names to atom entries"))


(defun atom-api:magic/byte-compiler-base-file-name (uri)
  (error "atompub: Can't byte-compile atom entries"))


(defun atom-api:magic/copy-file (uri newname)
  ;;todo
  (error "atompub: todo"))

(defun atom-api:magic/delete-directory (directory)
  (error "atompub: Can't delete collections"))

(defun atompub:magic/delete-file (uri)
  (atom-api:entry/delete uri))


(defun atom-api:magic/directory-file-name (collection)
  (error "atompub: Huh?"))

(defun atompub:magic/directory-files (collection &optional full match nosort)
  "Returns the entries in the collection."
  )


(defun atompub:magic/dired-call-process (...))

(defun atompub:magic/dired-compress-file (...))
(defun atompub:magic/dired-uncache (...))
(defun atompub:magic/expand-file-name (...))
(defun atompub:magic/file-accessible-directory-p',
(defun atompub:magic/file-attributes (...))
(defun atompub:magic/file-directory-p (...))
(defun atompub:magic/file-executable-p (uri)
  ;todo; return t for accessible collections
  nil)

(defun atom-api:magic/file-exists-p (uri)
  (atom-api:entry/exists entry-uri))

(defun atompub:magic/file-local-copy (uri)
  "Returns a local copy of an atom entry; returns file name."
  (let ((temp-file (make-temp-file "atompub")))
    (with-temp-file
     (atom-api:xml/to-string
      (atom-api:entry/retrieve uri)))
    temp-file))

(defun atompub:magic/file-modes (...))
(defun atompub:magic/file-name-all-completions',
(defun atompub:magic/file-name-as-directory (...))
(defun atompub:magic/file-name-completion (...))
(defun atompub:magic/file-name-directory (...)
(defun atompub:magic/file-name-nondirectory (...))
(defun atompub:magic/file-name-sans-versions (...)))
(defun atompub:magic/file-newer-than-file-p (...))
(defun atompub:magic/file-ownership-preserved-p (...))
(defun atompub:magic/file-readable-p (...))
(defun atompub:magic/file-regular-p (...))
(defun atompub:magic/file-symlink-p (...))
(defun atompub:magic/file-truename (...))
(defun atompub:magic/file-writable-p (...))
(defun atompub:magic/find-backup-file-name (...))
(defun atompub:magic/get-file-buffer (...))
(defun atompub:magic/insert-directory (...))

(defun atompub:magic/insert-file-contents (...))

(defun atompub:magic/load (file &optional noerror nomessage nosuffix must-suffix)
  (error "atompub: Can't load atom entries."))

(defun atompub:magic/make-directory (...))

(defun atompub:magic/make-symbolic-link (&rest args)
  (error "atompub: Can't symlink atom entries."))

(defun atompub:magic/rename-file (...))

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
