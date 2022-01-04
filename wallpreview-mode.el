(require 'image-dired)

(defvar wallpreview-wallpaper-directory "~/Pictures"
  "Wallpapers directory.")

(defun wallpreview-set-wallpaper (&optional arg)
  "Set a background image."
  (interactive "fBackground image: ")
  (let ((wallpaper-path (or arg (image-dired-original-file-name))))
    (call-process-shell-command
     ; shell-quote-argument replaces ~ with \~
     (concat "mywall " (shell-quote-argument wallpaper-path) "&")
     nil 0)))

(defun wallpreview-set-wallpaper-after (&rest arg)
  "Use walpreview-set-wallpaper. This function is an after adivce for image-dired-[forward, backward]-image, image-dired-[previous, next]-line."
  (wallpreview-set-wallpaper))

(defun wallpreview-image-dired-track-thumbnail ()
  "Sync the pointer in the image-dired-thumbnail buffer with "
  (interactive)
  (image-dired-track-thumbnail))

(defun wallpreview-enable ()
  (image-dired wallpreview-wallpaper-directory)
  (advice-add 'image-dired-forward-image :after #'wallpreview-set-wallpaper-after)
  (advice-add 'image-dired-backward-image :after #'wallpreview-set-wallpaper-after)
  (advice-add 'image-dired-previous-line :after #'wallpreview-set-wallpaper-after)
  (advice-add 'image-dired-next-line :after #'wallpreview-set-wallpaper-after))

(defun wallpreview-disable ()
  (advice-remove 'image-dired-forward-image #'wallpreview-set-wallpaper-after)
  (advice-remove 'image-dired-backward-image #'wallpreview-set-wallpaper-after)
  (advice-remove 'image-dired-previous-line #'wallpreview-set-wallpaper-after)
  (advice-remove 'image-dired-next-line #'wallpreview-set-wallpaper-after))

(define-minor-mode wallpreview-mode
  "Preview wallpapers in Image-dired mode."
  nil ; Initial value, nil for disabled
  :lighter "  wallpreview"
  (if wallpreview-mode
      (wallpreview-enable)
    (wallpreview-disable)))

(provide 'wallpreview-mode)
