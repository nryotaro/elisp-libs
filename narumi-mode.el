;;;
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Image-Descriptors.html
;;;
(defgroup narumi-mode nil
  "Yet another splash screen."
  :prefix "narumi-mode-")

(defcustom narumi-mode-buffer-name
  "*narumi*"
  "The buffer name of narumi-mode"
  :type 'string)

(defcustom narumi-mode-image-directory
  "*narumi*"
  "A directory where images s are."
  :type 'string)

(defun narumi-mode-display ()
  "Set the buffer of narumi-mode to the current window."
  (interactive)
  (let ((buffer-name narumi-mode-buffer-name))
    (get-buffer-create buffer-name)
    (with-current-buffer buffer-name
      (narumi-mode))
    (set-window-buffer (selected-window)
		       buffer-name)))

(defun narumi-calc-scale-window-height (height-pixel ratio)
  "Return a scale to make the size of an image is ratio * (frame-pixel-height).
height is the height in pixel of an image."
  (let* ((frame-height (frame-pixel-height))
	 (max-height (* frame-height ratio)))
    (if (<= height-pixel max-height)
	1.0
      (/ (* frame-height ratio) height-pixel))))

(defun narumi-update-image-scale (image new-scale)
  "Take an object returrned by create-image, returning the new object with new-scale attribute."
  (let* ((scale-tag-pos (seq-position image :scale)))
    (if scale-tag-pos
	(seq-map-indexed (lambda (item i)
		     (if (= i (+ 1 scale-tag-pos))
			 new-scale
		       item))
		   image)
      (append image (list :scale new-scale)))))

(defun narumi-create-center-image (file-path)
  "Create an image object that nser-image can accepts. The returned object can contains the margin attribute."
  (let* ((image (create-image (expand-file-name file-path)))
	 (original-height (cdr (image-size image t)))
	 (new-scale (narumi-calc-scale-window-height original-height 0.4))
	 (resized-image (narumi-update-image-scale image new-scale))
	 (resized-width (car (image-size resized-image t)))
	 (window-width (window-body-width nil t))
	 (width-margin (if (<= window-width resized-width)
			   0
			 (/ (- window-width resized-width) 2))))
    (append resized-image (list :margin (cons (- width-margin 25) 10)))))


(defun narumi-find-image-files ()
  ""
  (seq-filter (lambda (entry)		 
		(let ((len (length entry)))
		  (if (<= 4 len)
		      (let ((jpeg (substring entry -4))
			    (ext (substring entry -3)))
			(or (equal "jpeg" jpeg)
			    (equal "png" ext)
			    (equal "jpg" ext))))))
	      (directory-files narumi-mode-image-directory)))

(defun narumi-put-image ()
  ""
  (let* ((images (narumi-find-image-files))
	(image (expand-file-name (nth (random (length images)) images)
				 narumi-mode-image-directory)))
    (insert-image (narumi-create-center-image image))))

(defun narumi-jump-entry ()
  ""
  (interactive)
  (let ((text-properties (text-properties-at (point))))
	(if text-properties
	    (find-file (nth (+ 1 (seq-position text-properties 'entry-value))
			    text-properties)))))

(defun narumi-insert-entry (title path)
  ""
  (let ((map (make-sparse-keymap)))
    (bind-key "RET" 'narumi-jump-entry map)
    (insert "  ◯ ")
    (insert (propertize title 'face 'button 'entry-value path 'keymap map))
    (insert "\n") ;(newline) can append '_' to the paths.
    ))

(defun narumi-list-bookmarks ()
  "Return the paths of the bookmarks."
  (with-current-buffer (get-buffer-create bookmark-bmenu-buffer)
      (list-bookmarks))
  (mapcar (lambda (x)
	  (cdr (assoc 'filename (cdr x))))
	  bookmark-alist))

(defun narumi-insert-title (title)
  (add-text-properties 0
		       (length title)
		       '(face bookmark-menu-heading display (height 1.2))
		       title)
  (insert title))

(defun narumi-mode-refresh ()
  "Refresh the *narumi* buffer."
  (interactive)
  (recentf-mode t)  
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-line 0)
  (if (getenv "PRIVATE")
      (narumi-put-image))
  (newline)
  (narumi-insert-title "Bookmarks")
  (newline)
  (dolist (bookmark-path (narumi-list-bookmarks))
    (narumi-insert-entry bookmark-path bookmark-path))
  (newline)    
  (narumi-insert-title "Recent files")
  (newline)
  (dolist (recent-file recentf-list)
    (narumi-insert-entry recent-file recent-file))
  (goto-line 3)
  (setq buffer-read-only t))

(define-derived-mode narumi-mode
  special-mode "narumi"
  "Display the links to locations you may visit."
  (bind-key "r" 'narumi-mode-refresh narumi-mode-map)
  (narumi-mode-refresh))

(provide 'narumi-mode)
