;;; emms-player-spotify-counsel.el --- Integration of emms-player-spotify and counsel-spotify -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2025 by Sergey Trofimov
;; SPDX-License-Identifier: Unlicense

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;;
;; This file is part of emms-player-spotify.
;;
;;; Commentary:

;; This module hijacks the counsel-spotify-do-play function to enqueue the
;; selected item in the current EMMS playlist. For albums it also expands it in
;; individual tracks.

;;; Code:

(require 'counsel-spotify)

(defun counsel-spotify--utf8 (str)
  (when str
    (decode-coding-string (string-make-unibyte str) 'utf-8)))

(defun emms-player-spotify-expand-album (id insert-point callback)
  "Fetch tracks of album ID and write them to INSERT-POINT of the current
emms playlist. Run the CALLBACK afterwards."
  (counsel-spotify-with-auth-token (auth-token)
                                   (counsel-spotify-with-query-results
                                    (auth-token (concat counsel-spotify-spotify-api-url "/albums/" id "/tracks") results)

                                    (with-current-emms-playlist
                                      (goto-char insert-point)
                                      (ignore (mapc (lambda (el)
                                                      (let ((track (emms-track 'url (uri el))))
                                                        (emms-track-set track 'info-title
                                                                        (counsel-spotify--utf8 (name el)))
                                                        (emms-track-set track 'info-artist
                                                                        (counsel-spotify--utf8 (name (artist el))))
                                                        (emms-track-set track 'info-album
                                                                        (counsel-spotify--utf8 (name (album el))))
                                                        (emms-track-set track 'info-playing-time (round (* 1e-6 (duration-in-ms el))))
                                                        (emms-playlist-insert-track track)))
                                                    (counsel-spotify-parse-items (list (cons 'tracks results)) 'tracks)))
                                      (funcall callback)))))

(cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-linux-backend) (playable counsel-spotify-playable))
  "Replacement implementation that insert tracks to the current EMMS playlist."
  (with-current-emms-playlist
    (let* ((insert-point (goto-char (point-max)))
           (play-now (lambda ()
                       (goto-char insert-point)
                       (emms-playlist-mode-play-current-track))))

      (cond
       ((counsel-spotify-album-p playable)
        (emms-player-spotify-expand-album
         (nth 2 (string-split (uri playable) ":"))
         insert-point
         play-now))

       ((counsel-spotify-track-p playable)
        (let ((track (emms-track 'url (uri playable))))
          (emms-track-set track 'info-artist (counsel-spotify--utf8 (name (artist playable))))
          (emms-track-set track 'info-title (counsel-spotify--utf8 (name playable)))
          (emms-playlist-insert-track track)
          (funcall play-now)))

       (t
        (let ((track (emms-track 'url (uri playable))))
          (emms-track-set track 'info-artist "Spotify Playlist")
          (emms-track-set track 'info-title (counsel-spotify--utf8 (name playable)))
          (emms-playlist-insert-track track)
          (funcall play-now)))))))

(provide 'emms-player-spotify-counsel)
;;; emms-player-spotify-counsel.el ends here
