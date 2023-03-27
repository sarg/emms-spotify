;;; emms-player-spotify.el --- Spotify player for EMMS

;; Copyright (C) 2023 by Sergey Trofimov

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;; Version: 0.1
;; URL: https://github.com/sarg/emms-spotify.el
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package displays torrent files using tablist-mode.

;;; Code:
(require 's)
(require 'dbus)
(require 'url-parse)
(require 'emms)
(require 'emms-playing-time)
(require 'emms-source-file)
(require 'seq)

(defcustom emms-player-spotify
  (emms-player
   #'emms-player-spotify-start
   #'emms-player-spotify-stop
   #'emms-player-spotify-playable-p)
  "*Parameters for spotify player."
  :type '(cons symbol alist)
  :group 'emms-player-spotify)

(defun millis-to-seconds (ms)
  (round (* ms (expt 10 -6))))

(defun emms-player-spotify--update-metadata (metadata)
  "Update current EMMS track with METADATA."
  (let* ((album   (caadr  (assoc "xesam:album"  metadata)))
         (length  (caadr  (assoc "mpris:length" metadata)))
         (artist  (caaadr (assoc "xesam:artist" metadata)))
         (title   (caadr  (assoc "xesam:title"  metadata)))
         (url     (caadr  (assoc "xesam:url"    metadata)))
         (trackid (emms-player-spotify--transform-url url)))

    (when-let ((current-track (emms-playlist-current-selected-track))
               ((eq emms-player-playing-p emms-player-spotify))
               ((equal (emms-track-get current-track 'name) trackid)))
      (emms-track-set current-track 'info-artist artist)
      (emms-track-set current-track 'info-album album)
      (emms-track-set current-track 'info-title title)
      (emms-track-set current-track 'info-playing-time (millis-to-seconds length))
      (emms-track-updated current-track))))

(defun emms-player-spotify--seek-handler (pos)
  "Set current playing time to POS when Seeked event occurs."
  (emms-playing-time-set (millis-to-seconds pos)))

(defun emms-player-spotify--set-volume (val)
    (dbus-set-property
     :session
     "org.mpris.MediaPlayer2.spotify"
     "/org/mpris/MediaPlayer2"
     "org.mpris.MediaPlayer2.Player"
     "Volume"
     (float val)))

(defun emms-player-spotify--adblock-do (is-ad)
  (if is-ad (emms-player-spotify--set-volume 0)
    (run-with-timer 2 nil #'emms-player-spotify--set-volume 1)))

(define-minor-mode emms-player-spotify-adblock
  "Mutes spotify ads."
  :require 'emms-player-spotify
  :global t)

;;; Following mode
(defun emms-player-spotify-following-next ()
  (interactive)
  (emms-player-spotify--dbus-call "Next"))

(defun emms-player-spotify-following-previous ()
  (interactive)
  (emms-player-spotify--dbus-call "Previous"))

(define-minor-mode emms-player-spotify-following
  "When playing radios keep history in the same playlist."
  :global nil

  (defun emms-player-spotify-following--on-new-track (new-track)
    (save-excursion
      ;; create new entry with current track from the radio
      (goto-char emms-playlist-selected-marker)
      (emms-with-inhibit-read-only-t
       (if (s-prefix-p "spotify:track" (emms-track-name (emms-playlist-track-at)))
           (forward-line))

       (emms-insert-url new-track)
       (forward-line -1)
       (set-marker emms-playlist-selected-marker (point))
       (emms-player-started emms-player-spotify))))

  (cond
   (emms-player-spotify-following
    (advice-add 'emms-next :override #'emms-player-spotify-following-next)
    (advice-add 'emms-previous :override #'emms-player-spotify-following-previous)
    (when-let* ((current-track (emms-playlist-current-selected-track))
                ((not (emms-track-get current-track 'info-title))))
      ;; load playlist name and set it
      ;; (emms-track-set current-track 'info-title "Some radio")
      ;; (emms-track-updated current-track)
      ))
   (t
    (advice-remove 'emms-next #'emms-player-spotify-following-next)
    (advice-remove 'emms-previous #'emms-player-spotify-following-previous))))

;;; DBUS events handler
(defun emms-player-spotify--event-handler (_ properties &rest _)
  "Handles mpris dbus event.
Extracts playback status and track metadata from PROPERTIES."
  (let* ((metadata (caadr (assoc "Metadata" properties)))
         (playback-status
          (or (caadr (assoc "PlaybackStatus" properties))
              (and metadata "Playing"))))

    (pcase playback-status
      ;; play pressed outside of emms
      ((and "Playing" (guard emms-player-paused-p))
       (setq emms-player-paused-p nil)
       (run-hooks 'emms-player-paused-hook))

      ;; new track reported via mpris
      ("Playing"
       (let* ((metadata (or metadata (emms-player-spotify--get-mpris-metadata)))
              (url (caadr (assoc "xesam:url" metadata)))
              (new-track (emms-player-spotify--transform-url url))
              (is-ad (s-prefix-p "spotify:ad" new-track)))

         (when emms-player-spotify-adblock
           (emms-player-spotify--adblock-do is-ad))

         (if (emms-player-get emms-player-spotify 'playpause-requested)
             (emms-player-set emms-player-spotify 'playpause-requested nil)

           (with-current-emms-playlist
             (if is-ad
                 (unless emms-player-spotify-following (emms-player-stopped))

               (when emms-player-spotify-following
                 (emms-player-spotify-following--on-new-track new-track))

               (emms-player-spotify--update-metadata metadata))))))

      ((and "Paused" (guard (emms-player-get emms-player-spotify 'playpause-requested)))
       (emms-player-set emms-player-spotify 'playpause-requested nil)
       (ignore))

      ((and "Paused" (guard (emms-player-get emms-player-spotify 'stop-requested)))
       ;; special case, when user changes track in emms
       ;; emms-player-spotify-stop called
       ;; mpris Stop called
       ;; emms-player-spotify-start called
       ;; Paused mpris event comes
       (emms-player-set emms-player-spotify 'stop-requested nil)
       (ignore))

      ("Paused"
       ;; pause pressed in spotify or the song ended
       (let* ((track-len (emms-track-get
                          (emms-playlist-current-selected-track)
                          'info-playing-time))
              (song-ended (< (- track-len emms-playing-time) 2)))

         (cond
          (song-ended
           (emms-player-stopped))
          ('paused-externally
           (setq emms-player-paused-p t)
           (run-hooks 'emms-player-paused-hook))))))))

(defun emms-player-spotify-playable-p (track)
  (and (memq (emms-track-type track) '(url))
    (string-match-p (emms-player-get emms-player-spotify 'regex) (emms-track-name track))))

(defun emms-player-spotify--transform-url (url)
  (or (and (string-prefix-p "https" url)
        (concat "spotify"
          (replace-regexp-in-string
            "/" ":"
            (car (url-path-and-query (url-generic-parse-url url))))))
    url))

(defun emms-player-spotify--get-mpris-metadata ()
  (cdr (assoc "Metadata"
         (dbus-get-all-properties :session
           "org.mpris.MediaPlayer2.spotify"
           "/org/mpris/MediaPlayer2"
           "org.mpris.MediaPlayer2.Player"))))

(defmacro emms-player-spotify--dbus-call (method &rest args)
  `(dbus-call-method-asynchronously :session
     "org.mpris.MediaPlayer2.spotify"
     "/org/mpris/MediaPlayer2"
     "org.mpris.MediaPlayer2.Player"
     ,method
     nil
     ,@(if args args '())))

(defun emms-player--fetch-metadata (url)
  ;; (let* ((track-id (nth 2 (string-split url ":")))
  ;;         (query-url (concat counsel-spotify-spotify-api-url "/tracks/" track-id)))

  ;;   (counsel-spotify-with-auth-token (auth-token)
  ;;     (counsel-spotify-with-query-results (auth-token query-url results)
  ;;       (funcall cb (counsel-spotify-parse-spotify-object results 'tracks)))))
  )

(defun emms-player-spotify-start (track)
  (emms-player-spotify-enable-dbus-handler)
  (let ((url (emms-player-spotify--transform-url (emms-track-name track))))

    (unless (string= "track" (nth 1 (split-string url ":")))
      (emms-player-spotify-following t))

    (emms-player-spotify--dbus-call "OpenUri" url))
  (emms-player-started emms-player-spotify))

(defun emms-player-spotify-stop ()
  (emms-player-spotify-following -1)
  (emms-player-stopped)
  (emms-player-set emms-player-spotify 'stop-requested t)
  (emms-player-spotify--dbus-call "Stop"))

(defun emms-player-spotify-disable-dbus-handler ()
  (dbus-unregister-object (emms-player-get emms-player-spotify 'dbus-handler))
  (dbus-unregister-object (emms-player-get emms-player-spotify 'dbus-seek-handler)))

(defun emms-player-spotify-play ()
  "Start playing current track in spotify."
  (interactive)
  (emms-player-set emms-player-spotify 'playpause-requested t)
  (emms-player-spotify--dbus-call "Play"))

(defun emms-player-spotify-pause ()
  "Pause current track in spotify."
  (interactive)
  (emms-player-set emms-player-spotify 'playpause-requested t)
  (emms-player-spotify--dbus-call "Pause"))

(defun emms-player-spotify-enable-dbus-handler ()
  (unless (member "org.mpris.MediaPlayer2.spotify" (dbus-list-known-names :session))
   (error "Spotify App is not running"))

  (emms-player-set emms-player-spotify
    'dbus-seek-handler
    (dbus-register-signal :session
      "org.mpris.MediaPlayer2.spotify"
      "/org/mpris/MediaPlayer2"
      "org.mpris.MediaPlayer2.Player"
      "Seeked"
      #'emms-player-spotify--seek-handler))

  (emms-player-set emms-player-spotify
    'dbus-handler
    (dbus-register-signal :session
      "org.mpris.MediaPlayer2.spotify"
      "/org/mpris/MediaPlayer2"
      "org.freedesktop.DBus.Properties"
      "PropertiesChanged"
      (lambda (&rest args)
        (when (eq emms-player-playing-p emms-player-spotify)
          (apply #'emms-player-spotify--event-handler args))))))

(emms-player-set emms-player-spotify 'regex
                 (rx string-start (or "https://open.spotify.com" "spotify:")))
(emms-player-set emms-player-spotify 'pause #'emms-player-spotify-pause)
(emms-player-set emms-player-spotify 'resume #'emms-player-spotify-play)

(provide 'emms-player-spotify)
;;; emms-player-spotify.el ends here
