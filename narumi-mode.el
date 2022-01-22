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




(defun narumi-mode-calc-scale
    (image-width image-height width height max-height-ratio)
  "Take width and height of an image,
width and height of area for narumi and max-height-ratio
to calc the scale that let image be in width height * max-height-ratio area.
the range of max-height-ratio is (0 1), the sizes are in pixel."
  (let ((height-bound (* height max-height-ratio))
	(width-bound (* 0.9 width)))
    (if (<= image-width width-bound)
	(if (<= image-height height-bound)
	    1.0
	  (/ height-bound image-height))
      (if (<= image-height height-bound)
	 ; width-bound < image-width
	(/ width-bound image-width)
	(min (/ height-bound image-height)
	     (/ width-bound image-width))))))

(defun narumi-mode-update-image-scale (image new-scale)
  "Take an object returned by create-image, returning the new object with new-scale attribute."
  (let* ((scale-tag-pos (seq-position image :scale)))
    (if scale-tag-pos
	(seq-map-indexed (lambda (item i)
		     (if (= i (+ 1 scale-tag-pos))
			 new-scale
		       item))
		   image)
      (append image (list :scale new-scale)))))

(defun narumi-mode-create-center-image (file-path)
  "Create an image object that insert-image can accept. The returned object can contain the margin attribute."
  (let* ((image (create-image (expand-file-name file-path)))
	 (image-width (car (image-size image t)))	 
	 (image-height (cdr (image-size image t)))
	 (new-scale (narumi-mode-calc-scale image-width
					    image-height
					    (window-pixel-width)
					    (frame-pixel-height)
					    0.4))
	 (resized-image (narumi-mode-update-image-scale image new-scale))
	 (resized-width (car (image-size resized-image t)))
	 (window-width (window-body-width nil t))
	 (width-margin (if (<= window-width resized-width)
			   0
			 (/ (- window-width resized-width) 2))))
    (append resized-image (list :margin
				(cons width-margin 10)))))


(defun narumi-mode-find-image-files ()
  "Return image jpeg or png image files in narumi-mode-image-directory."
  (seq-filter (lambda (entry)		 
		(let ((len (length entry)))
		  (if (<= 4 len)
		      (let ((jpeg (substring entry -4))
			    (ext (substring entry -3)))
			(or (equal "jpeg" jpeg)
			    (equal "png" ext)
			    (equal "jpg" ext))))))
	      (directory-files narumi-mode-image-directory)))

(defun narumi-mode-put-image ()
  "Insert an image into the buffer."
  (let* ((images (narumi-mode-find-image-files))
	 (image (expand-file-name (nth (random (length images)) images)
				  narumi-mode-image-directory)))
    (insert-image (narumi-mode-create-center-image image))))

(defun narumi-mode-jump-entry ()
  ""
  (interactive)
  (let ((text-properties (text-properties-at (point))))
	(if text-properties
	    (find-file (nth (+ 1 (seq-position text-properties 'entry-value))
			    text-properties)))))

(defun narumi-mode-insert-entry (title path)
  ""
  (let ((map (make-sparse-keymap)))
    (bind-key "RET" 'narumi-mode-jump-entry map)
    (insert "  ◯ ")
    (insert (propertize title 'face 'button 'entry-value path 'keymap map))
    (insert "\n") ;(newline) can append '_' to the paths.
    ))

(defun narumi-mode-list-bookmarks ()
  "Return the paths of the bookmarks."
  (with-current-buffer (get-buffer-create bookmark-bmenu-buffer)
      (list-bookmarks))
  (mapcar (lambda (x)
	  (cdr (assoc 'filename (cdr x))))
	  bookmark-alist))

(defun narumi-mode-insert-title (title)
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
      (narumi-mode-put-image))
  (newline)
  (narumi-mode-insert-title "Bookmarks")
  (newline)
  (dolist (bookmark-path (narumi-mode-list-bookmarks))
    (narumi-mode-insert-entry bookmark-path bookmark-path))
  (newline)    
  (narumi-mode-insert-title "Recent files")
  (newline)
  (dolist (recent-file recentf-list)
    (narumi-mode-insert-entry recent-file recent-file))
  (goto-line 3)
  (setq buffer-read-only t))

(define-derived-mode narumi-mode
  special-mode "narumi"
  "Display the links to locations you may visit."
  (bind-key "r" 'narumi-mode-refresh narumi-mode-map)
  (narumi-mode-refresh))

(provide 'narumi-mode)
