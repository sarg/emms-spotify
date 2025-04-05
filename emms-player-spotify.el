;;; emms-player-spotify.el --- Spotify player for EMMS  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 by Sergey Trofimov
;; SPDX-License-Identifier: Unlicense

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;; Version: 0.2.1
;; URL: https://github.com/sarg/emms-spotify
;; Package-Requires: ((emacs "26.1") (compat "29.1") (emms "18") (s "1.13.0"))

;;; Commentary:
;; This package provides an EMMS player wrapper for Spotify. It supports two
;; types of links: internal spotify ids in form of "spotify:<type>:<id>" and in
;; form of a "https://open.spotify.com/<type>/<id>" URLs. The package delegates
;; actual playback to the desktop app, which must be already running. For proper
;; work, please disable Autoplay feature in the desktop app, so that EMMS would
;; have full control over the playback queue. As the package uses DBUS MPRIS
;; interface to control the player, it will work only on platforms where dbus is
;; available.

;;; Code:
(require 'compat)
(require 's)
(require 'dbus)
(require 'url-parse)
(require 'emms)
(require 'emms-playing-time)
(require 'emms-playlist-mode)
(require 'emms-source-file)

(defcustom emms-player-spotify
  (emms-player
   #'emms-player-spotify-start
   #'emms-player-spotify-stop
   #'emms-player-spotify-playable-p)
  "*Parameters for spotify player."
  :type '(cons symbol alist)
  :group 'emms-player-spotify)

(defcustom emms-player-spotify-adblock nil
  "Mute ads automatically."
  :type '(boolean)
  :group 'emms-player-spotify)

(defcustom emms-player-spotify-launch-cmd nil
  "Command to start spotify desktop app."
  :type '(string)
  :group 'emms-player-spotify)

(defcustom emms-player-spotify-adblock-delay 1
  "Extend ad mute for this long."
  :type '(number)
  :group 'emms-player-spotify)

(defvar emms-player-spotify-following nil)
(defvar emms-player-spotify-debug nil)

;;; Utils

(defun emms-player-spotify-debug-msg (msg &rest args)
  "Log MSG formatted with ARGS to the debug buffer."
  (when emms-player-spotify-debug
    (with-current-buffer (get-buffer-create "*emms-player-spotify-debug*")
      (goto-char (point-max))
      (insert (apply #'format
                     (append (list (concat "%.1f [%s%s] " msg "\n")
                                   (float-time)
                                   (if (emms-player-get emms-player-spotify 'playpause-expected) "p" " ")
                                   (if (emms-player-get emms-player-spotify 'stop-expected) "s" " "))
                             args))))))

(defun emms-player-spotify--transform-url (url)
  "Convert URL from http scheme to spotify."
  (if (string-prefix-p "http" url)
      (thread-last
        url
        (url-generic-parse-url)
        (url-path-and-query)
        (car)
        (s-replace "/" ":")
        (concat "spotify"))
    url))

(defun emms-player-spotify--track-uri (track)
  "Return spotify uri for TRACK."
  (emms-player-spotify--transform-url (emms-track-name track)))

(defun emms-player-spotify--single-track-p (track)
  "Return t if TRACK is an individual `playable'."
  (let ((uri (emms-player-spotify--track-uri track)))
    (or (s-prefix-p "spotify:track:" uri)
        (s-prefix-p "spotify:episode:" uri)
        (s-prefix-p "spotify:ad:" uri))))

;;; adblock

(defun emms-player-spotify--set-volume (val)
  "Set spotify volume to VAL over MPRIS."
  (dbus-set-property
   :session
   "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player"
   "Volume"
   (float val)))

;;; DBUS events handler

(defun emms-player-spotify--update-metadata (metadata)
  "Update current EMMS track with METADATA."
  (let* ((album   (caadr  (assoc "xesam:album"  metadata)))
         (length  (caadr  (assoc "mpris:length" metadata)))
         (artist  (caaadr (assoc "xesam:artist" metadata)))
         (title   (caadr  (assoc "xesam:title"  metadata)))
         (url     (caadr  (assoc "xesam:url"    metadata)))
         (trackid (emms-player-spotify--transform-url url)))

    (when-let* (((eq emms-player-playing-p emms-player-spotify))
                (current-track (emms-playlist-current-selected-track))
                ((equal (emms-player-spotify--track-uri current-track) trackid)))
      (emms-track-set current-track 'info-artist artist)
      (emms-track-set current-track 'info-album album)
      (emms-track-set current-track 'info-title title)
      (emms-track-set current-track 'info-playing-time (round (* length 1e-6)))
      (emms-track-updated current-track))))

(defun emms-player-spotify--adblock-toggle (is-ad)
  "Toggle mute when IS-AD."
  (pcase is-ad
    ('t
     (emms-player-set emms-player-spotify 'saved-volume
                      (emms-player-spotify--get-property "Volume"))
     (emms-player-spotify--set-volume 0))

    (_
     (when-let ((saved-volume (emms-player-get emms-player-spotify 'saved-volume)))
       (run-with-timer emms-player-spotify-adblock-delay nil
                       #'emms-player-spotify--set-volume saved-volume)
       (emms-player-set emms-player-spotify 'saved-volume nil)))))

(defun emms-player-spotify--remove-ad-placeholder ()
  "Delete Advertisement track placeholder from current playlist."
  (with-current-emms-playlist
   (save-excursion
     (goto-char emms-playlist-selected-marker)
     (emms-playlist-delete-track)
     (delete-line)
     (emms-playlist-select-previous))))

(defun emms-player-spotify--event-handler (_ properties &rest _)
  "Handles mpris dbus event.
Extracts playback status and track metadata from PROPERTIES."
  (let* ((metadata (caadr (assoc "Metadata" properties)))
         (playback-status
          (or (caadr (assoc "PlaybackStatus" properties))
              (and metadata "Playing"))))

    (pcase playback-status
      ((and "Playing" (guard emms-player-paused-p))
       (emms-player-spotify-debug-msg "Play pressed in spotify directly")
       (setq emms-player-paused-p nil)
       (run-hooks 'emms-player-paused-hook))

      ;; resumed after emms-playpause request
      ;; ignore to avoid creating new tracks in following mode
      ((and "Playing" (guard (emms-player-get emms-player-spotify 'playpause-expected)))
       (emms-player-spotify-debug-msg "Resuming play after emms-pause")
       (emms-player-set emms-player-spotify 'playpause-expected nil)
       (ignore))

      ((and "Paused" (guard (emms-player-get emms-player-spotify 'playpause-expected)))
       (emms-player-spotify-debug-msg "Paused after emms-pause request")
       (emms-player-set emms-player-spotify 'playpause-expected nil)
       (ignore))

      ((and "Paused" (guard (emms-player-get emms-player-spotify 'stop-expected)))
       ;; special case, when user changes track in emms
       ;; emms-player-spotify-stop called
       ;; mpris Stop called
       ;; emms-player-spotify-start called
       ;; Paused mpris event comes
       (emms-player-spotify-debug-msg "Paused after emms-stop")
       (emms-player-set emms-player-spotify 'stop-expected nil))

      ("Playing" ;; new track reported via mpris
       (with-current-emms-playlist
        (let* ((metadata (or metadata (cdr (emms-player-spotify--get-property "Metadata"))))
               (url (caadr (assoc "xesam:url" metadata)))
               (new-track (emms-player-spotify--transform-url url))
               (new-is-ad (s-prefix-p "spotify:ad:" new-track))
               (cur-track (emms-playlist-selected-track))
               (cur-is-ad (s-prefix-p "spotify:ad:" (emms-player-spotify--track-uri cur-track))))

          (emms-player-spotify-debug-msg "New track playing: %s %s" new-track (caadr (assoc "xesam:title"  metadata)))
          (emms-player-set emms-player-spotify 'stop-expected nil)

          ;; override artist and title for ads
          (when new-is-ad
            (setf (alist-get "xesam:artist" metadata nil nil #'equal)
                  '((("Spotify")))

                  (alist-get "xesam:title" metadata nil nil #'equal)
                  '(("Ads"))))

          (cond
           ;; subsequent ad, ignore
           ((and new-is-ad cur-is-ad)
            (ignore))

           (new-is-ad
            (when emms-player-spotify-adblock
              (emms-player-spotify--adblock-toggle 't))

            ;; first ad track, add a placeholder
            (emms-player-spotify-following--on-new-track new-track))

           (cur-is-ad
            (emms-player-spotify--remove-ad-placeholder)

            (when emms-player-spotify-adblock
              (emms-player-spotify--adblock-toggle nil))

            (when emms-player-spotify-following
              (emms-player-spotify-following--on-new-track new-track)))

           ('just-a-new-track
            (when emms-player-spotify-following
              (emms-player-spotify-following--on-new-track new-track))))

          (emms-player-spotify--update-metadata metadata))))

      ("Paused"
       ;; pause pressed in spotify or the song ended
       (let* ((current-track (emms-playlist-current-selected-track))
              (song-ended (zerop (emms-player-spotify--get-property "Position")))
              (is-ad (s-prefix-p "spotify:ad:" (emms-player-spotify--track-uri current-track))))

         (emms-player-spotify-debug-msg
          "Paused without user request: %s ended: %s" (emms-player-spotify--track-uri current-track) song-ended)

         (cond
          (song-ended
           (when is-ad
             (emms-player-spotify--remove-ad-placeholder)

             (when emms-player-spotify-adblock
               (emms-player-spotify--adblock-toggle nil)))

           (emms-player-stopped))

          ('paused-externally
           (setq emms-player-paused-p t)
           (run-hooks 'emms-player-paused-hook)))))
      (_ (emms-player-spotify-debug-msg "Unprocessed event %s" properties)))))

(defun emms-player-spotify--get-property (name)
  "Query dbus property NAME from running spotify process."
  (dbus-get-property
   :session
   "org.mpris.MediaPlayer2.spotify"
   "/org/mpris/MediaPlayer2"
   "org.mpris.MediaPlayer2.Player"
   name))

(defmacro emms-player-spotify--dbus-call (method &rest args)
  "Call dbus METHOD with ARGS on spotify."
  `(progn
     (emms-player-spotify-debug-msg "DBUS: %s" ,method)
     (dbus-call-method :session
                       "org.mpris.MediaPlayer2.spotify"
                       "/org/mpris/MediaPlayer2"
                       "org.mpris.MediaPlayer2.Player"
                       ,method ,@(if args args '()))))

(defun emms-player-spotify-disable-dbus-handler ()
  "Unregister dbus handlers."
  (interactive)
  (dbus-unregister-object (emms-player-get emms-player-spotify 'dbus-handler))
  (dbus-unregister-object (emms-player-get emms-player-spotify 'dbus-seek-handler)))

(defun emms-player-spotify--seek-handler (pos)
  "Set current playing time to POS when Seeked event occurs."
  (emms-playing-time-set (round (* pos 1e-6))))

(defun emms-player-spotify-enable-dbus-handler ()
  "Register dbus signal handlers to receive spotify player events."
  (unless (member "org.mpris.MediaPlayer2.spotify" (dbus-list-known-names :session))
    (when emms-player-spotify-launch-cmd
      (call-process-shell-command emms-player-spotify-launch-cmd nil 0 nil))
    (error "Spotify App is not running. Starting"))

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

;;; Following mode

(defun emms-player-spotify-following-next ()
  "Call spotify next."
  (emms-player-spotify--dbus-call "Next"))

(defun emms-player-spotify-following-previous ()
  "Call spotify previous."
  (emms-player-spotify--dbus-call "Previous"))

(defun emms-player-spotify-following--on-new-track (new-track)
  "Insert NEW-TRACK before the `playlist'-track which is being followed."
  (save-excursion
    ;; create new entry with current track from the radio
    (goto-char emms-playlist-selected-marker)
    (emms-with-inhibit-read-only-t
     (when (emms-player-spotify--single-track-p (emms-playlist-track-at))
       (forward-line))

     (emms-insert-url new-track)
     (forward-line -1)
     (emms-playlist-select (point))
     (emms-player-started emms-player-spotify))))

(define-minor-mode emms-player-spotify-following
  "When playing radios keep history in the same playlist."
  :global nil

  (cond
   (emms-player-spotify-following
    (when emms-random-playlist (emms-toggle-random-playlist))
    (advice-add 'emms-next :override #'emms-player-spotify-following-next)
    (advice-add 'emms-previous :override #'emms-player-spotify-following-previous))
   (t
    (advice-remove 'emms-next #'emms-player-spotify-following-next)
    (advice-remove 'emms-previous #'emms-player-spotify-following-previous))))

;;; emms interface

(defun emms-player-spotify-start (track)
  "Start playing TRACK."
  (emms-player-spotify-enable-dbus-handler)
  (emms-player-spotify-debug-msg "Start requested %s" track)
  (emms-player-spotify--adblock-toggle nil)
  (when (not (emms-player-spotify--single-track-p track))
    (emms-player-spotify-following t))
  (emms-player-spotify--dbus-call "OpenUri" (emms-player-spotify--track-uri track))
  (emms-player-started emms-player-spotify))

(defun emms-player-spotify-stop ()
  "Stop playing."
  (emms-player-spotify-debug-msg "Stop requested")
  (emms-player-spotify-following -1)
  (when emms-player-playing-p
    (emms-player-set emms-player-spotify 'stop-expected t))
  (emms-player-spotify--dbus-call "Stop")
  (emms-player-stopped))

(defun emms-player-spotify-play ()
  "Start playing current track in spotify."
  (interactive)
  (emms-player-spotify-debug-msg "Play requested")
  (emms-player-set emms-player-spotify 'playpause-expected t)
  (emms-player-spotify--dbus-call "Play"))

(defun emms-player-spotify-seek (sec)
  "Seek to SEC relatively."
  (interactive)

  (emms-player-spotify--dbus-call "Seek" :int64 (* sec 100000)))

(defun emms-player-spotify-seek-to (sec)
  "Seek to absolute position SEC."
  (interactive)

  (with-current-emms-playlist
   (let* ((track (emms-playlist-current-selected-track))
          (uri (emms-player-spotify--track-uri track))
          (trackid (concat "/com/" (s-replace ":" "/" uri))))

     (emms-player-spotify--dbus-call
      "SetPosition" :object-path trackid :int64 (* sec 100000)))))

(defun emms-player-spotify-pause ()
  "Pause current track in spotify."
  (interactive)
  (emms-player-spotify-debug-msg "Pause requested")
  (emms-player-set emms-player-spotify 'playpause-expected t)
  (emms-player-spotify--dbus-call "Pause"))

(defun emms-player-spotify-playable-p (track)
  "Return t if TRACK is playable with spotify."
  (and (memq (emms-track-type track) '(url))
       (string-match-p (emms-player-get emms-player-spotify 'regex) (emms-track-name track))))

(emms-player-set emms-player-spotify 'regex
                 (rx string-start (or "https://open.spotify.com" "spotify:")))
(emms-player-set emms-player-spotify 'pause #'emms-player-spotify-pause)
(emms-player-set emms-player-spotify 'resume #'emms-player-spotify-play)
(emms-player-set emms-player-spotify 'seek #'emms-player-spotify-seek)
(emms-player-set emms-player-spotify 'seek-to #'emms-player-spotify-seek-to)

(provide 'emms-player-spotify)
;;; emms-player-spotify.el ends here
