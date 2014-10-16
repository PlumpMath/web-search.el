;;; web-search.el --- Search for text on the Internet

;; Copyright (C) 2013-2014 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 25 Jan 2013
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://gitorious.org/alezost-emacs/web-search
;; URL: https://github.com/alezost/web-search.el
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
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

;; This file provides functions for searching selected/prompted text on
;; the Internet using different engines defined in `web-search-engines'.

;; To install the package, add the following to your emacs init file:
;;
;;   (add-to-list 'load-path "/path/to/web-search")
;;   (autoload 'web-search "web-search" nil t)

;; Use "M-x web-search" to perform a search.  Also you can use
;; `web-search-<engine>' commands which are generated after the file is
;; loaded.

;; To define a new search engine or replace the existing one, use
;; `web-search-add-engine' function, for example:
;;
;;   (web-search-add-engine 'google-groups "Google Groups"
;;                          "http://groups.google.com/groups?q=%s")
;;   (web-search-add-engine 'wikipedia-en "Wikipedia (english)"
;;                          "http://en.wikipedia.org/w/index.php?search=%s")
;;
;; After evaluating the above code, you can use the "google-groups"
;; engine with "M-x web-search-google-groups" or "M-x web-search".  Also
;; a new URL will be used for the "wikipedia-en" engine.

;;; Code:

(require 'cl-lib)

(cl-defstruct (web-search-engine
               (:constructor nil)       ; no default constructor
               (:constructor web-search-create-engine
                             (name title url &optional filter))
               (:copier nil))
  name title url filter)

(defvar web-search-engines nil
  "List of search engines.
Each engine is a structure that consists of the following
elements:

NAME      Internal name (symbol), used for generating search commands.
TITLE     String, used in prompts.
URL       Searching URL, where \"%s\" is replaced by a searching text.
FILTER    (optional) If non-nil, a searching text is passed through
          this filter function before browsing the final URL.

To add an engine to this list, use `web-search-add-engine'.")

;;;###autoload
(defconst web-search-default-engines
  '((duckduckgo "DuckDuckGo"
                "https://duckduckgo.com/?q=%s")
    (google "Google"
            "http://www.google.com/search?q=%s")
    (yahoo "Yahoo"
           "http://search.yahoo.com/search?p=%s")
    (github "Github"
            "http://github.com/search?q=%s&type=Everything")
    (emacswiki "EmacsWiki"
               "http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&q=%s&sa=Search&siteurl=emacswiki.org/")
    (archwiki "ArchWiki"
              "https://wiki.archlinux.org/index.php?search=%s")
    (debbugs "GNU Bug Tracker"
             "http://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s")
    (wikipedia-en "Wikipedia (english)"
                  "http://en.wikipedia.org/wiki/%s")
    (wiktionary-en "Wiktionary (english)"
                   "http://en.wiktionary.org/wiki/%s")
    (tfd "The Free Dictionary"
         "http://www.thefreedictionary.com/%s")
    (ip "IP address"
        "http://www.ip-address.org/lookup/ip-locator.php?track=%s"
        web-search-clean-ip))
  "List of default engines.")

(defmacro web-search-define-get-engine-function (slot)
  "Define function for getting engine structure by SLOT.
Name of the defined function is `web-search-get-engine-by-SLOT'.
SLOT is a name (symbol) of one of the slots of an engine
structure (see `web-search-engines' for details)."
  (let* ((slot-str (symbol-name slot))
         (slot-getter (intern (concat "web-search-engine-" slot-str)))
         (fun-name (intern (concat "web-search-get-engine-by-" slot-str)))
         (fun-desc (concat "Return engine from `web-search-engines' by its "
                           (upcase slot-str) ".\n"
                           "Return nil, if there is no such engine.")))
    `(defun ,fun-name (,slot)
       ,fun-desc
       (cl-loop for engine in web-search-engines
                if (equal ,slot (,slot-getter engine))
                return engine))))

(web-search-define-get-engine-function name)
(web-search-define-get-engine-function title)

(defun web-search-delete-engine (name)
  "Delete engine with NAME from `web-search-engines'."
  (setq web-search-engines
        (cl-delete-if (lambda (engine)
                        (eq name (web-search-engine-name engine)))
                      web-search-engines)))

(defun web-search-add-engine (name title url &optional filter)
  "Add new web-search engine.

The added engine will be available in `web-search' command.  Also
a new function `web-search-NAME' will be generated.  If there is
an engine with such NAME, it will be replaced with the new one.

For the meaning of NAME, TITLE, URL and FILTER, see
`web-search-engines'."
  (web-search-delete-engine name)
  (push (web-search-create-engine name title url filter)
        web-search-engines)
  (eval `(web-search-define-search-engine-function ,name)))

(defun web-search-clean-ip (str)
  "Return IP address by substituting '-' with '.' in STR."
  ;; Just in case if IP looks like this: 123-45-678-90
  (replace-regexp-in-string "-" "." str))

(defun web-search-prompt-for-string (&optional prompt)
  "Prompt for and return a search string.
Use PROMPT if it is specified.
If there is a selected region, it is used as a default value."
  (read-string (or prompt "Search for: ")
               (and (use-region-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))))

(defun web-search-prompt-for-engine (&optional prompt)
  "Prompt for and return a name of the search engine.
Use PROMPT if it is specified."
  (web-search-engine-name
   (web-search-get-engine-by-title
    (completing-read (or prompt "Search engine: ")
                     (sort (mapcar #'web-search-engine-title
                                   web-search-engines)
                           #'string<)
                     nil t))))

;;;###autoload
(defun web-search (string engine-name)
  "Search for STRING on the Internet.
ENGINE-NAME is a name (symbol) of an ENGINE from
`web-search-engines'.
Interactively, prompt for STRING (use selected region as a
default value) and engine."
  (interactive
   (list (web-search-prompt-for-string)
         (web-search-prompt-for-engine)))
  (let ((engine (web-search-get-engine-by-name engine-name)))
    (or engine
        (error "Search engine '%S' does not exist" engine-name))
    (let* ((engine-url (web-search-engine-url engine))
           (engine-fun (web-search-engine-filter engine))
           (search-str (url-hexify-string
                        (if engine-fun
                            (funcall engine-fun string)
                          string))))
      (browse-url (format engine-url search-str)))))

(defmacro web-search-define-search-engine-function (engine-name)
  "Define function for searching text with a particular engine.
Name of the defined function is `web-search-ENGINE-NAME'.
ENGINE-NAME is a name (symbol) of an engine from `web-search-engines'."
  (let* ((engine (web-search-get-engine-by-name engine-name))
         (engine-title (web-search-engine-title engine))
         (fun-name (intern (concat "web-search-" (symbol-name engine-name))))
         (fun-desc (concat "Search for STRING with '" engine-title "'.\n"
                           "Interactively, prompt for STRING (use selected region as a\n"
                           "default value).")))
    `(defun ,fun-name (string)
       ,fun-desc
       (interactive
        (list (web-search-prompt-for-string
               (format "Search %s: " ,engine-title))))
       (web-search string ',engine-name))))

(mapc (lambda (args)
        (apply #'web-search-add-engine args))
      web-search-default-engines)

;; Autoload `web-search-<engine>' commands:
;;;###autoload (mapc (lambda (engine) (autoload (intern (concat "web-search-" (symbol-name (car engine)))) "web-search" nil t)) web-search-default-engines)

(provide 'web-search)

;;; web-search.el ends here
