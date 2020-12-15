;;; sachac-news.el --- Read Sacha Chua's news! -*- lexical-binding: t; -*-

;; Copyright 2020 cnngimenez
;;
;; Author: cnngimenez
;; Maintainer: cnngimenez
;; Version: 0.1.0
;; Keywords: news
;; URL: https://github.com/cnngimenez/sachac-news
;; Package-Requires: ((emacs"25.1"))

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'sachac-news)

;;; Code:

(provide 'sachac-news)
(require 'org-element)
(require 'org-list)
(require 'cl-extra)

(defgroup sachac-news nil
  "Sacha Chua's Emacs news customizations")

(defun sachac-news-take-last-new ()
  "Take the portion of the org file with the last news."
  (goto-char (point-min))
  (search-forward-regexp "^\\*\\* .* Emacs news")
  (let ((sachac-news-title (org-element-at-point)))
    (buffer-substring-no-properties (org-element-property :begin sachac-news-title)
				    (org-element-property :end sachac-news-title))) ) ;; defun

(defcustom sachac-news-data-directory (concat user-emacs-directory
					     "sachac/")
  "Where is the data directory?"
  :type 'directory
  :group 'sachac-news) ;; defcustom

(defcustom sachac-news-data-file "data.el"
  "The configuration and data file.
This is where the last updated date and other data is stored."
  :type 'file
  :group 'sachac-news) ;; defcustom

(defcustom sachac-news-git-dirname "git"
  "The directory where the git repository should be cloned."
  :type 'string
  :group 'sachac-news)

(defun sachac-news-dir-git ()
  "Return the complete git path."
  (concat sachac-news-data-directory "/" sachac-news-git-dirname) ) ;; defun

(defun sachac-news-dir-datafile ()
  "Return the complete data file path."
  (concat sachac-news-data-directory "/" sachac-news-data-file) ) ;; defun


(defun sachac-news-git-index-org ()
  "Return the index.org path on the git directory."
  (concat (sachac-news-dir-git) "/emacs-news/index.org") ) ;; defun


(defun sachac-news-show-last-new ()
  "Create a new buffer with the last new."
  (interactive)
  (sachac-news-update-git)
  (with-current-buffer (find-file (sachac-news-git-index-org))
    (let ((str (sachac-news-take-last-new)))
      (with-current-buffer (get-buffer-create "*last-news*")
	(org-mode)

	(delete-region (point-min) (point-max))
	(insert str)
	
	(goto-char (point-min))
	(display-buffer (current-buffer))))
    (kill-buffer)) ) ;; defun

(defvar sachac-news-last-update nil
  "The last update date.")

(defun sachac-news-load-data ()
  "Update variables which values are in the configuration file.
Evaluate the `sachac-news-data-file' file and use the result to fill some
important variables."
  (when (file-exists-p (sachac-news-dir-datafile))
    (with-temp-buffer
      (insert-file-contents (sachac-news-dir-datafile))
      (let ((expr (read (buffer-string))))
	(setq sachac-news-last-update (alist-get 'last-update expr))))) ) ;; defun

(defun sachac-news-save-data ()
  "Save some important variables into the data file.
These variables can be loaded again with `sachac-news-load-data'."
  (with-temp-buffer
    (insert (prin1-to-string
	     (list (cons 'last-update sachac-news-last-update))))
    (write-file (sachac-news-dir-datafile))) ) ;; defun

(defvar sachac-news-data-loaded nil
  "Has been the data loaded?") ;; defun

(defun sachac-news-load-data-if-needed ()
  "If the data has not been loaded yet, load it."
  (unless sachac-news-data-loaded
    (sachac-news-load-data)
    (setq sachac-news-data-loaded t)) ) ;; defun


(defun sachac-news-update-last-update ()
  "Update the `sachac-news-last-update' date with the current date."
  (setq sachac-news-last-update (calendar-current-date))
  (sachac-news-save-data)) ;; defun

(defun sachac-news-is-time-for-update-p ()
  "Check if a day has passed since the last update."
  (if (not sachac-news-last-update)
      t
    (>= (- (calendar-absolute-from-gregorian (calendar-current-date))
	  (calendar-absolute-from-gregorian sachac-news-last-update))
       1)) ) ;; defun


(defun sachac-news-create-dirs ()
  "Create the needed directories to save data and the repository."
  (make-directory sachac-news-data-directory t)
  (make-directory (sachac-news-dir-git) t) ) ;; defun


(defun sachac-news-update-git (&optional force-update)
  "Call git whenever a day has passed since the las update.
If FORCE-UPDATE is t, then do not check if it passe a day."
  (sachac-news-create-dirs)
  (sachac-news-load-data-if-needed)
  (when (or force-update (sachac-news-is-time-for-update-p))
    (message "Updating Sacha's news!")
    (if (file-exists-p (sachac-news-git-index-org))
	(shell-command (concat
			"cd " (sachac-news-dir-git) "; "
			"git update"))
      (shell-command (concat
		      "cd " (sachac-news-dir-git) "; "
		      "git clone https://github.com/sachac/emacs-news.git")))
    (sachac-news-update-last-update)) ) ;; defun


(defun sachac-news-find-category (category-regexp org-element)
  "Find the paragraph that matches with the CATEGORY-REGEXP regexp.
The parameter ORG-ELEMENT is the returned element from 
`org-element-parse-buffer' or `org-element-at-point'.

Returns the first org-element of type 'paragraph founded."
  (org-element-map org-element 'paragraph
    (lambda (paragraph)
      "Return the PARAGRAPH only when its contents is a string and it matches
the category regexp."
      (let ((element (car (org-element-contents paragraph))))
	(when (and (stringp element)
		   (string-match-p category-regexp element))
	  paragraph))) nil t) ) ;; defun


(defun sachac-news-find-all-categories (category-regexp-list)
  "Find all the items that matches the category regexps.
The CATEGORY-REGEXP-LIST is a list of regexp strings.
Returns a list of org element of type 'item."  

  (let ((org-elts (org-element-parse-buffer)))
    (cl-map 'list
	    (lambda (cat)
	      "Return the parent of the first paragraph founded.
If the parent is not of 'item type, return nil."
	      (let ((maybe-item-elt (org-element-property
				     :parent
				     (sachac-news-find-category cat org-elts))))
		(when (eql (org-element-type maybe-item-elt) 'item)
		  maybe-item-elt)))
	    category-regexp-list)) ) ;; defun

(defun sachac-news-fold-all-items (item-list)
  "Fold all items from ITEM-LIST.

The ITEM-LIST parameter is a list of org element. 
`org-element-type' should return 'item when called for each item in ITEM-LIST."

  (cl-map 'list
	  (lambda (item)
	    (org-list-set-item-visibility
	     (org-element-property :begin item)
	     (org-element-property :structure item)
	     'folded))
	  item-list)) ;; defun

(defcustom sachac-news-fold-category-regexp-list '()
  "A list of regexp strings of the matching categories that should be folded.

The function `sachac-news-fold-categories' use this variable to find
categories that the user wants to hide."
  :type '(repeat regexp)
  :group 'sachac-news) ;; defcustom

(defun sachac-news-fold-categories (&optional category-regexp-list)
  "Fold all items that match the category regexps.

Category regexps are taken from `sachac-news-category-regexp-list' or from the
optional parameter CATEGORY-REGEXP-LIST if given."
  (interactive)
  (let ((category-list (if category-regexp-list category-regexp-list
			 sachac-news-fold-category-regexp-list)))
    (sachac-news-fold-all-items
     (sachac-news-find-all-categories category-list))) ) ;; defun


;;; sachac-news.el ends here
