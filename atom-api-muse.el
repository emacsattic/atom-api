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
