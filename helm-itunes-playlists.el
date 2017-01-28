;;; helm-itunes-playlists.el --- Play local iTunes playlists
;; Copyright 2014 Adam Schwartz
;;
;; Author: Derek Ellis <derek.ellis@gmail.com>
;; URL: https://github.com/dellis2k/helm-itunes-playlists
;;
;; Created: 2016-05-30
;; Version: 0.0.2
;; Package-Requires: ((helm "1.6.1"))

;;; Commentary:
;;
;; A helm interface for iTunes playlists that sorts and uses regex to remove any unwanted playlists from helm results.
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
(defvar helm-itunes-playlists-exclusion-list
  '("Music Videos" "Movies" "^AB" "Home Videos" "iTunes U" "Audiobooks" "Books" "PDFs" "Purchased on Professor" "TV Shows" "Podcasts" "Voice Memos\n" "^zz" "^Com"))

;; AppleScript that returns all playlists

(defun helm-itunes-playlists-applescript ()
  (format "
tell application \"iTunes\"
 return name of every user playlist
end tell"))
  
;; Return a list of playlists from the AppleScript.

(defun helm-itunes-playlists-get-playlists ()
  "Return a list of playlists in your iTunes library."
  (sort (split-string (shell-command-to-string
   (format "osascript -e %S" (helm-itunes-playlists-applescript)))
  "\\,\s") 'string-lessp)) 


(defun helm-itunes-playlists-grep-list (pattern input)
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
(defun helm-itunes-playlists-get-clean-playlists (pattern-list input)
  "grep over a list"
  (loop for regex in pattern-list do	
	(setf input (helm-itunes-playlists-grep-list regex input)))
  input)

;!
(defun helm-itunes-playlists-helm-search ()
  (helm-itunes-playlists-get-clean-playlists helm-itunes-playlists-exclusion-list (helm-itunes-playlists-get-playlists)))

;; play the playlist
;!
(defun helm-itunes-playlists-play (playlist)
  (shell-command (format "osascript -e 'tell application \"iTunes\" to play user playlist %S'" playlist)))

;;(setq helm-input-idle-delay 0.1)

;;---------- Helm Functions ----------;;
;;(setq helm-exit-idle-delay 0)  ;; this no worky
;;(defun helm-source-itunes-playlist
;;  (helm-build-async-source "iTunes Playlist Search"
;;    :volatile t
;;    :candiates 'helm-itunes-playlist-helm-search
;;    :multiline t
;;    :requires-pattern 0
;;    :action '(("Play Playlist" . (helm-itunes-playlist-play candidate)))))


(setq helm-source-itunes-playlists
      '((name . "HELM iTunes Playlists")
        (candidates . helm-itunes-playlists-helm-search)
	(multiline . t)
	(volatile . t)
	(requires-pattern . 0)
	(prompt . "iTunes Playlist: ")
	(buffer . "*helm-itunes-playlists*")
        (action . (lambda (candidate)
                    ;;(message "You chose %s" candidate)
		    (helm-itunes-playlists-play candidate)))))



;;;###autoload
(defun helm-itunes-playlists ()
  "Bring up an iTunes playlist search interface in helm."
  (interactive)
  (if (eq system-type 'darwin)
      (helm :sources 'helm-source-itunes-playlists)
    (message "Sorry, helm-itunes-playlist does not support %S" system-type)))


(provide 'helm-itunes-playlists)
;;; helm-itunes-playlists.el ends here
