;;; emacs-news.el --- 

;; Copyright 2020 cnngimenez
;;
;; Author: cnngimenez
;; Version:
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'emacs-news)

;;; Code:

(provide 'emacs-news)
(require 'org-element)


(defun emacs-news-take-last-new ()
  "Take the portion of the org file with the last news."
  (goto-char (point-min))
  (search-forward-regexp "^\\*\\* .* Emacs news")
  (let ((emacs-news-title (org-element-at-point)))
    (buffer-substring-no-properties (org-element-property :begin emacs-news-title)
				    (org-element-property :end emacs-news-title))
    )
  ) ;; defun

(defcustom emacs-news-data-directory "~/.emacs.d/sachac/"
  "Where is the data directory?"
  :group 'emacs-news) ;; defcustom

(defcustom emacs-news-data-file "data.el"
  "The configuration and data file.
This is where the last updated date and other data is stored."
  :group 'emacs-news) ;; defcustom

(defcustom emacs-news-git-dirname "git"
  "The directory where the git repository should be cloned."
  :group 'emacs-news)

(defun emacs-news-dir-git ()
  "Return the complete git path."
  (concat emacs-news-data-directory "/" emacs-news-git-dirname) ) ;; defun

(defun emacs-news-dir-datafile ()
  "Return the complete data file path."
  (concat emacs-news-data-directory "/" emacs-news-data-file) ) ;; defun


(defun emacs-news-git-index-org ()
  "Return the index.org path on the git directory."
  (concat (emacs-news-dir-git) "/emacs-news/index.org") ) ;; defun


(defun emacs-news-show-last-new ()
  "Create a new buffer with the last new."
  (interactive)
  (emacs-news-update-git)
  (with-current-buffer (find-file (emacs-news-git-index-org))
    (let ((str (emacs-news-take-last-new)))
      (with-current-buffer (get-buffer-create "*last-news*")
	(org-mode)

	(delete-region (point-min) (point-max))
	(insert str)
	
	(goto-char (point-min))
	(display-buffer (current-buffer))
	))
    (kill-buffer)) ) ;; defun

(defvar emacs-news-last-update nil
  "The last update date.")

(defun emacs-news-load-data ()
  "Update variables which values are in the configuration file.
Evaluate the `emacs-news-data-file' file and use the result to fill some
important variables."
  (when (file-exists-p (emacs-news-dir-datafile))
    (with-temp-buffer
      (insert-file-contents (emacs-news-dir-datafile))
      (let ((expr (read (buffer-string))))
	(setq emacs-news-last-update (alist-get 'last-update expr))))) ) ;; defun

(defun emacs-news-save-data ()
  "Save some important variables into the data file.
These variables can be loaded again with `emacs-news-load-data'."
  (with-temp-buffer
    (insert (prin1-to-string
	     (list (cons 'last-update emacs-news-last-update))))
    (write-file (emacs-news-dir-datafile))) ) ;; defun

(defvar emacs-news-data-loaded nil
  "Has been the data loaded?") ;; defun

(defun emacs-news-load-data-if-needed ()
  "If the data has not been loaded yet, load it."
  (when (not emacs-news-data-loaded)
    (emacs-news-load-data)
    (setq emacs-news-data-loaded t)) ) ;; defun


(defun emacs-news-update-last-update ()
  "Update the `emacs-news-last-update' date with the current date."
  (setq emacs-news-last-update (calendar-current-date))
  (emacs-news-save-data)) ;; defun

(defun emacs-news-is-time-for-update-p ()
  "Check if a day has passed since the last update."
  (if (not emacs-news-last-update)
      t
    (>= (- (calendar-absolute-from-gregorian (calendar-current-date))
	  (calendar-absolute-from-gregorian emacs-news-last-update))
       1)) ) ;; defun


(defun emacs-news-create-dirs ()
  "Create the needed directories to save data and the repository."
  (make-directory emacs-news-data-directory t)
  (make-directory (emacs-news-dir-git) t) ) ;; defun


(defun emacs-news-update-git (&optional force-update)
  "Call git whenever a day has passed since the las update.
If FORCE-UPDATE is t, then do not check if it passe a day."
  (emacs-news-create-dirs)
  (emacs-news-load-data-if-needed)
  (when (or force-update (emacs-news-is-time-for-update-p))
    (if (file-exists-p (emacs-news-git-index-org))
	(shell-command (concat
			"cd " (emacs-news-dir-git) "; "
			"git update"))
      (shell-command (concat
		      "cd " (emacs-news-dir-git) "; "
		      "git clone https://github.com/emacs-news/emacs-news.git")))
    (emacs-news-update-last-update)) ) ;; defun

  
;;; emacs-news.el ends here
