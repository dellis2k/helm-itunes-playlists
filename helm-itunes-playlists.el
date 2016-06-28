;;; helm-itunes-playlists.el --- Play local iTunes playlists
;; Copyright 2014 Adam Schwartz
;;
;; Author: Derek Ellis <derek.ellis@gmail.com>
;; URL: https://github.com/dellis2k/helm-itunes-playlist
;;
;; Created: 2016-05-30
;; Version: 0.0.1
;; Package-Requires: ((helm "1.6.1"))

;;; Commentary:
;;
;; A helm interface for iTunes playlists that sorts and uses regex to remove any unwanted playlist from helm results.
;;
;;
;; Bugs:
;;  - none that I know of
;; 
;; Currently only supports OS X.
;;
;; Inspired by helm-spotify: https://github.com/krisajenkins/helm-spotify
;; and helm-itunes: https://github.com/anschwa/helm-itunes
;;

;;; Code:
(require 'url)
(require 'helm)

;; Playlists to exclude from helm results.  Note special character for iTunesU and line break on Voice Memos.
(defvar helm-itunes-playlist-exclusion-list
  '("Music Videos" "Movies" "^AB" "Home Videos" "iTunes U" "Audiobooks" "Books" "PDFs" "Purchased on Professor" "TV Shows" "Podcasts" "Voice Memos\n" "^zz" "^Com"))

;; AppleScript that returns all playlists

(defun helm-itunes-playlist-applescript ()
  (format "
tell application \"iTunes\"
 return name of every user playlist
end tell"))
  
;; Return a list of playlists from the AppleScript.

(defun helm-itunes-playlist-get-playlists ()
  "Return a list of playlists in your iTunes library."
  (sort (split-string (shell-command-to-string
   (format "osascript -e %S" (helm-itunes-playlist-applescript)))
  "\\,\s") 'string-lessp)) 


(defun helm-itunes-playlist-grep-list (pattern input)
   "Grep for regexp PATTERN in INPUT.
 Each element of INPUT must be either a string or a list of string.
 grep-list will recurse through these lists."
   (let ((things input)
	 result thing)
     (while things
       (setq thing (car things)
	     things (cdr things))
       (cond ((listp thing)
	      (setq result (append (grep-list pattern thing) result)))
	     ((stringp thing)
	      (unless (string-match pattern thing)
		(setq result (append (list thing) result))))
	     (t (error "neither string nor list item"))))
     (nreverse result)))

;!
(defun helm-itunes-playlist-get-clean-playlists (pattern-list input)
  "grep over a list"
  (loop for regex in pattern-list do	
	(setf input (helm-itunes-playlist-grep-list regex input)))
  input)

;!
(defun helm-itunes-playlist-helm-search ()
  (helm-itunes-playlist-get-clean-playlists helm-itunes-playlist-exclusion-list (helm-itunes-playlist-get-playlists)))

;; play the playlist
;!
(defun helm-itunes-playlist-play (playlist)
  (shell-command (format "osascript -e 'tell application \"iTunes\" to play user playlist %S'"
                         playlist)))

;;---------- Helm Functions ----------;;

(defvar helm-source-itunes-playlist
  (helm-build-async-source "iTunes Playlist Search"
    ;;:candidates-process #'helm-itunes-playlist-get-playlists
    :candidates-process #'helm-itunes-playlist-helm-search
    :volatile t
    :multiline t
    :requires-pattern 0
    :action '(("Play Playlist" . helm-itunes-playlist-play))))

;;;###autoload
(defun helm-itunes-playlist ()
  "Bring up an iTunes playlist search interface in helm."
  (interactive)
  (if (eq system-type 'darwin)
      (helm :sources 'helm-source-itunes-playlist
            :buffer "*helm-itunes-playlist*")
    (message "Sorry, helm-itunes-playlist does not support %S" system-type)))


(provide 'helm-itunes-playlist)
;;; helm-itunes.el ends here
