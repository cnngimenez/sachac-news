;;; sachac-news.el --- Read Sacha Chua's news! -*- lexical-binding: t; -*-

;; Copyright 2020 cnngimenez
;;
;; Author: Christian Gimenez <cnngimenez@disroot.org>
;; Maintainer: Christian Gimenez <cnngimenez@disroot.org>
;; Version: 0.1.0
;; Keywords: news
;; URL: https://git.sr.ht/~cngimenez/sachac-news
;; Package-Requires: ((emacs "27.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Check periodically for new commits on Sacha Chua's news repository.
;; Basically, by default the code does the following steps:
;;
;; 1. Clone or update the repository.
;; 2. Search the latest news in it.
;; 3. Notify if there is a new blog post (a new header in the index.org).
;; 4. Fold the categories specified at `sachac-news-fold-category-regexp-list'.
;; 5. Show the index.org buffer!
;;
;; Interesting functions:
;; - `sachac-news-show-last-new' :: Show only the latest news.
;; - `sachac-news-open-index-file ':: Open the complete emacs-news (the
;;    index.org file).  Try to not edit this file because it is used for
;;    searching new titles.
;; - `sachac-news-show-update-time' :: Display all the time information
;;    about the last updated time, the last timed update, etc.
;; - `sachac-news-update-git' :: If a the interval time has passed,
;;    update the git repository.  Force the update with C-u.
;; - `sachac-news-fold-categories' :: If the current buffer is an org
;;    file (the latest news or the index.org), fold the parent items
;;    (used as categories) that are listed in the
;;    `sachac-news-category-regexp-list' customization option.
;; - `sachac-news-activate-timer' :: Activate the timer.  If the timer
;;    is already activated, then reset it.  The timer is configure to
;;    update the repository (the index.org file) after the hours
;;    indicated at the customization option `sachac-news-update-hours-wait'.
;; - `sachac-news-deactivate-timer' :: Deactivate the timer.
;; - `sachac-news-timer-status' :: Show if the timer is activated or not.
;;
;; See README.org for more information about usage and configuration.

;;; Code:

(require 'org-element)
(require 'org-list)
(require 'cl-lib)

(defgroup sachac-news nil
  "Sacha Chua's Emacs news customizations."
  :group 'applications)

(defcustom sachac-news-git-command
  (eval-when-compile
    (require 'vc-git)
    vc-git-program)
  "Path or git command name.

Valid values are \"/usr/bin/git\" or \"git\" if it is in the current PATH."
  :type 'string) ;; defcustom

(defcustom sachac-news-fold-category-regexp-list '()
  "A list of regexp strings of the matching categories that should be folded.

The function `sachac-news-fold-categories' use this variable to find
categories that the user wants to hide."
  :type '(repeat regexp)) ;; defcustom

(defcustom sachac-news-alarm-sound-file
  "/usr/share/sounds/freedesktop/stereo/bell.oga"
  "The path to a sound file.
If the value is nil or the file does not exists, the `ding' function is used.

See `sachac-news-default-sound-alarm' function."
  :type 'file) ;; defcustom

(defcustom sachac-news-alarm-sound-programs
  '(("mpv" . "--really-quiet %s")
    ("mplayer" . "%s") ("ogg123" . "%s"))
  "The program name and arguments to execute the sound file.
The first %s would be expanded with the sound file name.  If none of these
programs is founded on the system, the `ding' function will be used.  The
first program founded is used.

This variable is used by `sachac-news-default-sound-alarm' function."
  :type '(alist :key-type string :value-type string)) ;; defcustom

(defcustom sachac-news-alarm-functions-hook
  '(sachac-news-default-notify-alarm
    sachac-news-default-sound-alarm)
  "The alarm functions.
These functions are called when there are new news."
  :type 'hook) ;; defcustom

(defconst sachac-news-title-regexp
  "^\\*\\*[[:space:]]+[[:digit:]]+-[[:digit:]]+-[[:digit:]]+[[:space:]]+Emacs news"
  "Regexp used to find news titles in the index.org file.") ;; defconst

(defvar sachac-news-timer-setted-time 0	;perhaps mark these as internal: sachac-news--...
  "At what time the timer has been setted?
See `sachac-news-set-timer'.")

(defvar sachac-news-last-saved-title nil
  "This is the last title saved on the data file.")

(defvar sachac-news-last-update nil
  "The last update date.")

(defvar sachac-news-data-loaded nil
  "Has been the data loaded?")

(defvar sachac-news--git-hook nil
  "Internal hook.  This is seeted by the git update functions.
This hook is called when the sentinel found a finished status on the async git
 process.  After hook call, it is setted to nil.

See `sachac-news--git-sentinel' and `sachac-news-update-git'.")

(defvar sachac-news--git-process nil
  "The git process.
If the git process is running, this variable contains the process object.
Else, this variable contains nil.")

(defvar sachac-news-timer nil
  "A timer object used to update the local git repository.")

(defun sachac-news-take-last-new (&optional use-index-org)
  "Take the portion of the org file with the last news in the current buffer.

If USE-INDEX-ORG is t, then load the index.org file.  Else, use the current
buffer as if it is the index.org."
  (if use-index-org
      (with-temp-buffer
	(insert-file-contents (sachac-news-git-index-org))
	(sachac-news-take-last-new nil))
    (progn
      (goto-char (point-min))
      (search-forward-regexp sachac-news-title-regexp)
      (let ((sachac-news-title (org-element-at-point)))
	(buffer-substring-no-properties
	 (org-element-property :begin sachac-news-title)
	 (org-element-property :end sachac-news-title))))))

(defcustom sachac-news-data-directory
  (locate-user-emacs-file "sachac")
  "Where is the data directory?"
  :type 'directory) ;; defcustom

(defcustom sachac-news-data-file "data.eld"
  "The configuration and data file.
This is where the last updated date and other data is stored."
  :type 'file) ;; defcustom

(defcustom sachac-news-git-dirname "git"
  "The directory where the git repository should be cloned."
  :type 'string)

;; She publishes the news every week around the beginning, why check
;; every day?
(defcustom sachac-news-update-hours-wait 24
  "The amount of hours when the git clone/pull must wait before be called.

Default is 24 hours.  Only positive values should be used."
  :type 'natnum
  :group 'sachac-news) ;; defcustom

(defun sachac-news-dir-git ()
  "Return the complete git path."
  (expand-file-name  sachac-news-git-dirname sachac-news-data-directory))

(defun sachac-news-dir-datafile ()
  "Return the complete data file path."
  (expand-file-name sachac-news-data-file sachac-news-data-directory))

(defun sachac-news-git-index-org ()
  "Return the index.org path on the git directory."
  (expand-file-name
   "index.org"
   (expand-file-name
    "emacs-news"
    (sachac-news-dir-git))))

(defun sachac-news--show-last-new-internal ()
  "Show the last news.
This is used after the update sentinel is executed.
See `sachac-news-show-last-new'."

  (let ((org-data-str (sachac-news-take-last-new t)))
    (with-current-buffer (get-buffer-create "*last-news*")
      (org-mode)

      (erase-buffer)
      (insert org-data-str)
      
      (goto-char (point-min))
	
      (save-excursion
	(sachac-news-run-alarm-if-needed)
	  
	(sachac-news-update-last-saved-title)
	(sachac-news-fold-categories))
	
      (display-buffer (current-buffer)))))

(defun sachac-news-show-last-new-if-new ()
  "Show the last new if there is a new title.

No update is performed.  This function is supposed to be used as a callback for
`sachac-news-update-git'."
  (when (sachac-news-is-there-new-title-p)
    (sachac-news--show-last-new-internal)))

(defun sachac-news-show-last-new ()
  "Create a new buffer with the last new.

Also update the last title displayed to the user (see
`sachac-news-last-saved-title' variable.

Update the local git repository if the waiting time has passed (it is not
forced).  See `sachac-news-update-git' function.  To change the waiting time
see `sachac-news-update-hours-wait' variable."
  (interactive)
  (sachac-news-update-git nil
			  #'sachac-news--show-last-new-internal
			  #'sachac-news--show-last-new-internal))

;;; Last saved title

(defun sachac-news-update-last-saved-title ()
  "Save the last title into the data file."
  (setq sachac-news-last-saved-title (sachac-news-get-last-title))
  (sachac-news-save-data))

(defun sachac-news-get-last-title (&optional use-current-buffer)
  "Get the first title founded in the current buffer.

If USE-CURRENT-BUFFER is nil, then load the index.org file and use it to get
the last title.  Else, if t, use the current buffer, but remember to call
`sachac-news-take-last-new' first."
  (if use-current-buffer
      (org-element-map (org-element-parse-buffer) 'headline
	(lambda (element) (org-element-property :raw-value element))
	nil t)
    (with-temp-buffer
      (insert (sachac-news-take-last-new t))
      (sachac-news-get-last-title t))))
 
(defun sachac-news-is-there-new-title-p (&optional use-current-buffer)
  "According to the last save, return t when a new post is found.

Check against the last title parsed from the last news displayed to the user (
`sachac-news-last-saved-title').  If a different title has been found at the
begining of the document, then a new post is found.

If USE-CURRENT-BUFFER is t, then the current buffer is considered to be the
last news buffer.  Else, open the index.org and retrieve the last news."

  (sachac-news-load-data-if-needed)

  (let ((last-title (if use-current-buffer
			(sachac-news-get-last-title t)
		      (sachac-news-get-last-title))))
	     
    (or (null sachac-news-last-saved-title)
	(not (string-equal last-title
			   sachac-news-last-saved-title)))))

;;; Data or config. load/save

(defun sachac-news-load-data ()
  "Update variables which values are in the configuration file.
Evaluate the `sachac-news-data-file' file and use the result to fill some
important variables."
  (when (file-exists-p (sachac-news-dir-datafile))
    (with-temp-buffer
      (insert-file-contents (sachac-news-dir-datafile))
      (let ((expr (read (buffer-string))))
	;; set the important variables
	(setq sachac-news-last-update
	      (alist-get 'last-update expr))
	(setq sachac-news-last-saved-title
	      (alist-get 'last-saved-title expr))
	;; Return the expression loaded
	expr))))

(defun sachac-news-save-data ()
  "Save some important variables into the data file.
These variables can be loaded again with `sachac-news-load-data'."
  (with-temp-buffer
    (let ((data (list (cons 'last-update sachac-news-last-update)
		      (cons 'last-saved-title sachac-news-last-saved-title))))
      (prin1 data (current-buffer))
      (write-region nil nil (sachac-news-dir-datafile) nil 'silent)
      data)))

(defun sachac-news-load-data-if-needed ()
  "If the data has not been loaded yet, load it."
  (unless sachac-news-data-loaded
    (sachac-news-load-data)
    (setq sachac-news-data-loaded t)))

;;; Git clone/update

(defun sachac-news-update-last-update ()
  "Update the `sachac-news-last-update' date with the current date."
  (setq sachac-news-last-update (time-convert (current-time) 'integer))
  (sachac-news-save-data))

(defun sachac-news-update-time-str ()
  "Return a string with the last time and the amount of time left."
  ;; Perhaps format this in a temporary buffer, then return the buffer string?
  (format "Waiting time: %s hours
-- Update --
Last time updated: %s
Time elapsed since update: %s %s
Time left to enable update (unless forced): %s %s
-- Timer --
Setted at: %s
Time left for automatic forced update: %s %s"

	  (number-to-string sachac-news-update-hours-wait)

	  ;; update
	  (if sachac-news-last-update
	      (format-time-string "%D %T" sachac-news-last-update)
	    "No last update")
	  (number-to-string (/ (sachac-news-get-update-time-elapsed) 60))
	  "minutes"
	  (number-to-string (/ (sachac-news-get-update-enable-time-left) 60))
	  "minutes"

	  ;; timer
	  (if sachac-news-timer-setted-time
	      (format-time-string "%D %T" (+ sachac-news-timer-setted-time
					     (* sachac-news-update-hours-wait 60 60)))
	    "No timer setted")
	  (number-to-string (/ (sachac-news-get-update-time-left) 60))
	  "minutes"))

(defun sachac-news-get-update-wait-seconds ()
  "Get the `sachac-news-update-hours-wait' in seconds."
  (* sachac-news-update-hours-wait 60 60))

(defun sachac-news-show-update-time ()
  "Display the time left for the next update."
  (interactive)
  (sachac-news-load-data-if-needed)
  (if sachac-news-last-update
      (message "%s" (sachac-news-update-time-str))
    (message "Git has not been called before.")))

(defun sachac-news-get-update-time-left ()
  "Return the seconds left for the next scheduled update.
This is according to the timer and if the timer is setted.

Return 0 if `sachac-news-timer-setted-time' is nil (no timer has
been setted)."
  (if sachac-news-timer-setted-time
      (- (+ sachac-news-timer-setted-time (sachac-news-get-update-wait-seconds))
	 (time-convert (current-time) 'integer))
    0))

(defun sachac-news-get-update-enable-time-left ()
  "Return the seconds left for the next enabled update.
This has no relation with the timer.

When the returned value is zero or negative, calling `sachac-news-update-git'
will actually call git clone/update, unless forced.

Return 0 if `sachac-news-last-update' is nil (no last update time has been
loaded)."
  (if sachac-news-last-update
      (- (+ sachac-news-last-update (sachac-news-get-update-wait-seconds))
	 (time-convert (current-time) 'integer))
    0))

(defun sachac-news-get-update-time-elapsed ()
  "Return the seconds elapsed since the last update.

Return the numbre of seconds after the maximum wait + 1 if
`sachac-news-last-update' is nil (no last update time has been loaded)."
  (if sachac-news-last-update
      (- (time-convert (current-time) 'integer)
	 sachac-news-last-update)
    (+ (sachac-news-get-update-wait-seconds) 1)))

(defun sachac-news-is-time-for-update-p ()
  "Check if a day has passed since the last update."
  (if sachac-news-last-update
      (>= (sachac-news-get-update-time-elapsed)
	 (sachac-news-get-update-wait-seconds))
    t))

(defun sachac-news-create-dirs ()
  "Create the needed directories to save data and the repository."
  (make-directory sachac-news-data-directory t)
  (make-directory (sachac-news-dir-git) t))

(defun sachac-news--git-sentinel (_process event)
  "Git sentinel.
If the git process endend correctly, update last updated date and  call
functions from `sachac-news--git-hook'.
If the git process sent another signal or endend unexpectedly, just report it.

When the sentinel ends, reset all variables refering to the git process and
hooks.

EVENT is the signal received from the process."
  (cond
   ((string-equal event "finished\n")
    (sachac-news-update-last-update)
    (run-hooks 'sachac-news--git-hook))
   (t (message "SachaC-news's git sentinel: Something wrong happened. Receive \"%s\" event from git async process."
	       event)))

  ;; Reset variables
  (setq sachac-news--git-hook nil
	sachac-news--git-process nil))

(defun sachac-news--git-update (git-program &optional func-call-after)
  "Do the git pull.
GIT-PROGRAM is the git PATH.

FUNC-CALL-AFTER is a function called after the git process endend successfully."

  (unless sachac-news--git-process
    ;; No process exists, then its fine to continue.
    (when func-call-after
      (add-hook 'sachac-news--git-hook func-call-after))
    (setq sachac-news--git-process
	  (let ((default-directory (expand-file-name "emacs-news" (sachac-news-dir-git))))
	    ;; I am not sure what the point is there, but I suspect
	    ;; there should be a better way to do this using timers
	    ;; and vc-git.
	    (if (file-exists-p (sachac-news-git-index-org))
		(start-process-shell-command "sachac-news-git-pull"
					     "*sachac-news-git*"
					     (concat "sleep 60 ; " git-program " pull"))
	      (start-process-shell-command "sachac-news-git-clone"
					   "*sachac-news-git*"
					   (concat "sleep 60 ; " git-program " clone \
https://github.com/sachac/emacs-news.git")))))
    (set-process-sentinel sachac-news--git-process #'sachac-news--git-sentinel)))


(defun sachac-news-update-git (&optional force-update
				     callback-after-update
				     callback-if-no-update)
  "Call git asyncronously whenever a day has passed since the last update.
To avoid checking every time `sachac-news-is-time-for-update-p' is used to
check if enough time has passed.

If FORCE-UPDATE is t (or C-u is used interactively), then do not check if it
 passed a day.

CALLBACK-AFTER-UPDATE is called as soon as the git process is finished.
CALLBACK-IF-NO-UPDATE is called when there's no need for use the git
pull/clone."
  (interactive "P")

  (sachac-news-create-dirs)
  (sachac-news-load-data-if-needed)

  (let ((git-program (executable-find sachac-news-git-command)))
    (if git-program
	;; Git program founded
	(if (or force-update (sachac-news-is-time-for-update-p))
	    (progn
	      (message "Updating Sacha's news!")
	      (sachac-news--git-update git-program callback-after-update))
	  (progn
	    (message "%s\n%s"
		     "Not enough time passed to update and not forced."
		     "To force update, use C-u M-x sachac-news-update-git.")
	    (when callback-if-no-update
	      (funcall callback-if-no-update))))
      ;; Git program not founded
      (message (substitute-command-keys
		"The Git program has not been founded! \
SachaC-news cannot download news without it!
Please install it in our system or customize the variable: )
\\[customize-option] sachac-news-git-command")))))

(defun sachac-news-open-index-file ()
  "Open the index.org file from the local repository.

Update if needed and then open the index.org file.

See `sachac-news-update-git' and `sachac-news-is-time-for-update-p' to learn
how the update is done."
  (interactive)
  
  (sachac-news-update-git)
  (if (file-exists-p (sachac-news-git-index-org))
      (find-file (sachac-news-git-index-org))
    (message "Index file not found! Did something wrong happen?
See `sachac-news-update-git'.")))

;;; Folding categories

(defun sachac-news-find-all-categories (category-regexps &optional org-element)
  "Match paragraph with the CATEGORY-REGEXPS regexp.
The parameter ORG-ELEMENT is the returned element from
`org-element-parse-buffer' or `org-element-at-point'.

Returns a list of org-element of type \\'item found in the index.org."
  (unless org-element
    (setq org-element (org-element-parse-buffer)))
  
  (org-element-map org-element 'paragraph
    (lambda (paragraph)
      "Return the PARAGRAPH parent only when:
- its parent is an item.
- its contents is a string and it matches the category regexp."
      (let ((element (car (org-element-contents paragraph)))
	    (parent (org-element-property :parent paragraph)))
	(when (and
	       ;; The parent is an item.
	       (eql (org-element-type parent) 'item)
	       ;; The contents match one category.
	       (stringp element)
	       (cl-some (lambda (category)
			  (string-match-p category element))
			category-regexps))

	    parent)))))


(defun sachac-news-fold-all-items (item-list)
  "Fold all items from ITEM-LIST.

The ITEM-LIST parameter is a list of org element.
`org-element-type' should return \\'item when called for each item in ITEM-LIST."

  (cl-map 'list
	  (lambda (item)
	    (org-list-set-item-visibility
	     (org-element-property :begin item)
	     (org-element-property :structure item)
	     'folded))
	  item-list))

(defun sachac-news-fold-categories (&optional category-regexp-list)
  "Fold all items that match the category regexps.

Category regexps are taken from `sachac-news-category-regexp-list' or from the
optional parameter CATEGORY-REGEXP-LIST if given.

This function works on any Org file, even at the Emacs news' index.org."
  (interactive)
  (let ((category-list (if category-regexp-list category-regexp-list
			 sachac-news-fold-category-regexp-list)))
    (sachac-news-fold-all-items
     (sachac-news-find-all-categories category-list))))

;;; Alarm

(defun sachac-news-default-notify-alarm ()
  "The default alarm.
Use the notify-send to send the alarm."
  (let ((program (executable-find "notify-send")))
    (when program
      (shell-command (concat program
			     " --app-name=\"Emacs: SachaC-news\""
			     " \"Check the News!\"")))))

(defun sachac-news-default-sound-alarm ()
  "The default sound alarm.
If the `sachac-news-alarm-sound-file' and any of the
`sachac-news-alarm-sound-program' exists, then play it, else use a beep sound
as an alarm.

The first program founded on your system is used.  `ding' function will be used
as fallback."
  (let ((program-data
	 (cl-some (lambda (program)
		    (let ((program-path (executable-find (car program))))
		      (when program-path
			(list program-path (cdr program)))))
		  sachac-news-alarm-sound-programs)))
    (if (and program-data
	     (file-exists-p sachac-news-alarm-sound-file))
	(apply #'start-process
	       "sachac-news-sound-alarm" nil
	       (car program-data)
	       (split-string
		(format (cadr program-data) sachac-news-alarm-sound-file)))
      (ding t))))

(defun sachac-news-run-alarm-if-needed ()
  "Run the alarm hook functions if there is a new post ."
  (when (sachac-news-is-there-new-title-p)
    (run-hooks 'sachac-news-alarm-functions-hook)))

;;; Timer

(defun sachac-news-timer-function ()
  "The function used by the timer."
  (message "SachaC-news: Timer call for update news!")

  (sachac-news-update-git t #'sachac-news-show-last-new-if-new)
  (sachac-news-run-alarm-if-needed)

  (sachac-news-activate-timer))


(defun sachac-news-activate-timer ()
  "Set the timer to download the git repository.

Set the timer for executing on `sachac-news-update-hours-wait' hours."
  (interactive)
  (sachac-news-deactivate-timer) ;; just in case there's a timer running
  (setq sachac-news-timer-setted-time (time-convert (current-time) 'integer))
  (setq sachac-news-timer
	(run-at-time
	 (format "%d hours" sachac-news-update-hours-wait)
	 nil
	 #'sachac-news-timer-function)))

(defun sachac-news-deactivate-timer ()
  "Stop and cancel the timer."
  (interactive)
  (when (timerp sachac-news-timer)
    (cancel-timer sachac-news-timer)
    (setq sachac-news-timer nil))
  (setq sachac-news-timer-setted-time nil))

(defun sachac-news-timer-status ()
  "Is the timer setted or not?
Report the user about the timer status."
  (interactive)
  (if (timerp sachac-news-timer)
      (message "Timer is setted and running.")
    (message "Timer is deactivated")))

;; Don't activate side effects while loading your package!  Instruct
;; the users to add this to their init.el, so that one knows what is
;; going on.

;; (sachac-news-activate-timer)

(provide 'sachac-news)
;;; sachac-news.el ends here
