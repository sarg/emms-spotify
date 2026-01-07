;;; consult-spotify-emms.el --- Integration of emms-player-spotify and consult -*- lexical-binding: t; -*-
;; Copyright (C) 2023-2025 by Sergey Trofimov
;; SPDX-License-Identifier: Unlicense

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (request "0.3.0") (consult "1.0") (emms "18"))
;; URL: https://github.com/sarg/emms-player-spotify
;; Keywords: completion, multimedia
;;
;; This file is part of emms-player-spotify.
;;
;;; Commentary:
;; The candidates are grouped by type (track, album, playlist) and consult's
;; narrowing is enabled. Albums and playlists are expanded when inserting to
;; EMMS playlist, however only first 100 tracks are fetched (paging not
;; implemented yet).

;;; Code:

(require 'request)
(require 'consult)
(require 'emms)

;;;; Custom
(defgroup consult-spotify-emms nil
  "Access to Spotify API and clients"
  :group 'multimedia)

(defcustom consult-spotify-emms-client-id nil
  "Spotify application client ID."
  :type '(string function))

(defcustom consult-spotify-emms-client-secret nil
  "Spotify application client secret."
  :type '(string function))

(defcustom consult-spotify-emms-max-name-length 40
  "Max name length to display in search results."
  :type 'number)

(defface consult-spotify-emms-album '((t :inherit underline))
  "Face for album name in search results.")

;;;; Spotify API
(defmacro consult-spotify-emms--secret-value (name)
  `(cl-typecase ,name
     (function (funcall ,name))
     (string consult-spotify-emms-client-id)
     (t (user-error ,(concat (symbol-name name) " is not set")))))

(defvar consult-spotify-emms--cached-token nil)
(defun consult-spotify-emms--refresh-token ()
  (message "Authorizing")
  (request "https://accounts.spotify.com/api/token"
    :headers `(("Content-Type" . "application/x-www-form-urlencoded")
               ("Authorization" .
                ,(concat "Basic "
                         (base64-encode-string
                          (concat
                           (consult-spotify-emms--secret-value consult-spotify-emms-client-id) ":"
                           (consult-spotify-emms--secret-value consult-spotify-emms-client-secret))
                          'no-line-break))))
    :type "POST"
    :params '((grant_type . client_credentials))
    :parser #'json-read 
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq consult-spotify-emms--cached-token
                      (alist-get 'access_token data))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (user-error "Failed to get access token: %s" error-thrown)))
    :sync t))

(defun consult-spotify-emms--api-request (path &rest args)
  (declare (indent 1))
  (let ((retries 0) result)
    (while (and (< retries 2) (not result))
      (apply #'request (concat "https://api.spotify.com/v1" path)
             :headers `(("Authorization" . ,(concat "Bearer " consult-spotify-emms--cached-token)))
             :sync t
             :parser (lambda () (let ((json-array-type 'list)) (json-read)))
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (setq result data)))
             :error (cl-function (lambda (&key response &allow-other-keys)
                                   (when (member (request-response-status-code response)
                                                 '(400 401))
                                     (setq result 'unauthorized))))
             args)

      (when (eq result 'unauthorized)
        (setq retries (+ retries 1) result nil)
        (consult-spotify-emms--refresh-token)))
    result))

;;;; Consult functions
(defun consult-spotify-emms--format (data)
  (let-alist data
    (concat
     (truncate-string-to-width .name consult-spotify-emms-max-name-length nil nil 'ellipsis)
     #(" " 0 1 (display (space :align-to 40)))
     
     (cl-case (string-to-char .type)
       (?t
        (concat
         " from " (propertize (alist-get 'name .album) 'face 'consult-spotify-emms-album)
         " by " (alist-get 'name (car .artists))))

       (?a (concat " by " (alist-get 'name (car .artists))))
       (?p (concat " by " (alist-get 'display_name .owner)))))))

(defun consult-spotify-emms--search (input &optional type)
  "Search albums, playlists and tracks given user INPUT."
  (declare (indent 1))
  (thread-last
    (consult-spotify-emms--api-request "/search"
      :params `((q . ,input)
                (type . ,(or type "album,playlist,track"))
                (limit . 10)))

    (mapcan (lambda (r) (delq nil (alist-get 'items r))))
    (mapcar (lambda (item)
              (let ((type (string-to-char (alist-get 'type item))))
                (propertize (consult--tofu-append (consult-spotify-emms--format item) type)
                            'consult--type type 
                            'consult--candidate item))))))

(defconst consult-spotify-emms--narrow
  '((?a . "album")
    (?t . "track")
    (?p . "playlist")))

;;;###autoload
(defun consult-spotify-emms ()
  "Search music on spotify to play in EMMS."
  (interactive)
  (consult--forbid-minibuffer)
  (consult-spotify-emms--add-to-playlist
   (consult--read
    (consult--dynamic-collection
        (lambda (input)
          (consult--slow-operation "Searching..."
            (consult-spotify-emms--search input
              (alist-get consult--narrow consult-spotify-emms--narrow))))
      :min-input 3
      :throttle 1
      :debounce 0.5)
    :sort nil
    :prompt "Search: "
    :require-match t
    :lookup #'consult--lookup-candidate
    :history 'consult-spotify-emms--history
    :category 'spotify
    :group (consult--type-group consult-spotify-emms--narrow)
    :narrow (consult--type-narrow consult-spotify-emms--narrow))))

;;;; EMMS functions 
(defun consult-spotify-emms--make-track (item)
  "Make emms-track out of spotify's json."
  (let-alist item
    (let ((track (emms-track 'url .uri)))
      (emms-track-set track 'info-title .name)
      (emms-track-set track 'info-artist (alist-get 'name (car .artists)))
      (emms-track-set track 'info-album (alist-get 'name .album))
      (emms-track-set track 'info-playing-time (/ 1000 .duration_ms))
      track)))

(defun consult-spotify-emms--add-to-playlist (item)
  "Insert selected ITEM in current emms playlist."
  (with-current-emms-playlist
    (goto-char (point-max))
    (let ((name (alist-get 'name item))
          (id (alist-get 'id item))
          (type (alist-get 'type item)))

      (cond
       ((string= "track" type)
        (emms-playlist-insert-track
         (consult-spotify-emms--make-track item)))

       (t
        (thread-last
          (concat "/" type "s/" id "/tracks")
          (consult-spotify-emms--api-request)
          (alist-get 'items)
          (mapcar (lambda (i) (or (alist-get 'track i) i)))
          (mapcar #'consult-spotify-emms--make-track)
          (mapc (lambda (track)
                  (when (string= type "album")
                    (emms-track-set track 'info-album name))
                  (emms-playlist-insert-track track)))))))))

(provide 'consult-spotify-emms)
;;; consult-spotify-emms.el ends here
