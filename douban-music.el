;;;douban-music.el
;;;Author: Xiang Wang (wxjeacen@gmail.com)
;;
;;
;;code:

(eval-when-compile
  (require 'cl))
(require 'url-http)
(require 'json)

(defgroup douban-music nil
  "douban music interface"
  :group 'entertainment
  :prefix "douban-music-" )

(defvar local-music-store '()
  "Song information in local store.
This store is a containner which pulls data from remote server, and
feed data to music player")

(defcustom douban-music-server "http://douban.fm/j/mine/playlist?type=n&channel=2"
  "douban server url address"
  :group 'douban-music)

(defvar current-song '()
  "The current Song that music player is opened"
)

(defun douban-music-fetch-songs-from-server ()
  "Get next songs from douban server"
  (let ((url douban-music-server)
        (url-request-method "GET")
        (url-request-data nil)
        (url-request-extra-headers '(("Content-Length" . "0")))
        (url-mime-charset-string)
        (url-extensions-header)
        (url-show-status)
        json
        buffer)
    (setq buffer (url-retrieve-synchronously  url))
    (with-current-buffer buffer
      (goto-char (point-max))
      (setq json (cdr (assoc 'song
                             (json-read-from-string
                              (buffer-substring
                               (line-beginning-position)
                               (point-max))))))

      ( if (vectorp json)
          (copy-to-local-store json)
        (error "Invalid data format")
        )
      )
    (kill-buffer buffer)
    )
  )

(defun copy-to-local-store (data)
  ( if (vectorp data)
      (progn
        (if (vectorp local-music-store)
            (error "Should be list"))
        (dotimes (i (length data))
          ( let ((var (aref data i)))
            (setq local-music-store
                  (cons var local-music-store))
            )
          ))
    (error "Invalid data format")
    )
  )

(defun douban-music-pop-song-from-store ()
  "Pop up a muisc from local music store"
  (let ((song))
    ( if (eq nil local-music-store )
        (douban-music-fetch-songs-from-server)
      )
    (if (eq nil local-music-store)
        (error "Fail to fetch muiscs from douban music server"))
    (setq song (elt local-music-store 0))
    (setq local-music-store (cdr local-music-store))
    song
    )
  )

(defun douban-music-play-song ()
  (let ((song ))
    (setq current-song (douban-music-pop-song-from-store))
    (setq song current-song)
    (start-process "mpg321" nil "mpg321" (aget song 'url))
    )
  )

(defun douban-music-current-song-info ()
  (
   princ current-song   
   )
  )


(provide 'douban-music)
