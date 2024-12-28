;;; isgd.el --- Shorten URLs using the isgd.com shortener service


;; Copyright (C) 2015-2024  Chmouel Boudjnah <chmouel@chmouel.com>

;; Version: 0.2
;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; URL: https://github.com/chmouel/isgd.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple mode to shorten URLs from Emacs.

;; Adapted from bitly.el from Jorgen Schaefer <forcer@forcix.cx>
;; available here https://github.com/forcix/bitly.el

;; Use (isgd-shorten URL) from an Emacs Lisp program, or
;; M-x isgd-copy-url-at-point to copy the shortened URL at point (or the region)
;; to the kill ring, or M-x isgd-replace-url-at-point to replace the URL at point
;; (or the region) with a shortened version.

;;; Code:
(require 'thingatpt)
(require 'url-util)

;; User variables
(defvar isgd-base-url "https://is.gd/api.php")

;; Functions
(defun isgd-shorten (long-url)
  (with-current-buffer (url-retrieve-synchronously
                        (format "%s?longurl=%s"
                                isgd-base-url (url-hexify-string long-url)))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (buffer-substring-no-properties (point) (point-at-eol))))

(defun isgd--shorten-url-at-point ()
  "Return the shortened URL at point or in the region."
  (let* ((url (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'url)))
         (shorten (and url (string-match-p "\\`https?://" url) (isgd-shorten url))))
    (unless url
      (error "No URL at point"))
    (if (string= shorten url)
        (message "URL is already shortened")
      shorten)))

;;;###autoload
(defun isgd-copy-url-at-point()
  "Shorten the URL at point and copy it to the kill ring."
  (interactive)
  (let ((shorten (isgd--shorten-url-at-point)))
    (when shorten
      (kill-new shorten)
      (message "Shortened URL: %s" shorten))))

;;;###autoload
(defun isgd-replace-url-at-point()
  "Shorten the URL at point and replace it."
  (interactive)
  (let ((shorten (isgd--shorten-url-at-point)))
    (when shorten
      (if (use-region-p)
          (delete-region (region-beginning) (region-end))
        (let ((bounds (bounds-of-thing-at-point 'url)))
          (delete-region (car bounds) (cdr bounds))))
      (insert shorten)
      (message "Shortened URL: %s" shorten))))
;;; End isgd.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isgd)

;;; isgd.el ends here
