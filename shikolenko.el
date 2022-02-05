(require 'mpc)

(transient-define-prefix transient-shikolenko-menu ()
  "shikolenko"
  [[("se" "select" (lambda () (interactive) (mpc-select)))
    ("l" "playlist" (lambda () (interactive) (mpc-playlist)))]
   [("a" "add" (lambda () (interactive) (mpc-playlist-add)))
    ("p" "play" (lambda () (interactive) (mpc-play)))]
   [("d" "delete" (lambda ()(interactive) (mpc-playlist-delete)))]])

(bind-key "m" 'transient-shikolenko-menu mpc-mode-map)

(provide 'shikolenko)

