;;; compilation-alert.el --- Alert when a long-running compilation finishes -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Callum J. Cameron

;; Author: Callum J. Cameron <cjcameron7@gmail.com>
;; Version: 1.0
;; URL: https://github.com/CallumCameron/compilation-alert
;; Keywords: notifications processes
;; Package-Requires: ((emacs "24") (alert "1.1"))

;; This file is not part of GNU Emacs.

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

;; Get an alert when a long-running compilation (using `compile')
;; completes.
;;
;; 'Long-running' is by default 30 seconds, but this can be customised
;; using the variable `compilation-alert-threshold'.
;;
;;
;; Usage:
;;
;; Just compile things as normal using `compile'.
;;
;;
;; Installation:
;;
;;   1. Install the dependency: 'alert'
;;      (https://github.com/jwiegley/alert)
;;   2. Make sure this file is on your load path
;;   3. (require 'compilation-alert)

;;; Code:

(require 'alert)
(require 'compile)

(defvar compilation-alert-start-time nil
  "When the last compilation started.")

(defvar compilation-alert-threshold 30.0
  "How long a compilation must take before an alert is displayed when it finishes.")

;;;###autoload
(progn
  (add-hook 'compilation-start-hook
            (lambda (_process)
              (setq compilation-alert-start-time (current-time))))
  (add-to-list 'compilation-finish-functions
               (lambda (_buffer s)
                 (when (time-less-p
                        (time-add compilation-alert-start-time (seconds-to-time compilation-alert-threshold))
                        (current-time))
                   (alert (format "Compilation %s" s) :title "Emacs")))))

(provide 'compilation-alert)

;;; compilation-alert.el ends here
