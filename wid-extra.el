;;; wid-extra.el --- Extra widgets and enhancers -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/wid-extra
;; Version: 0.1.0
;; Keywords: extensions
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extra widgets and enhancers

;;; Code:




(require 'wid-edit)
(require 'facemenu)
(declare-function color-rgb-to-hex "shr-color")

(defun wid-extra-color-match (_widget value)
  "Non-nil if VALUE is a defined color or an RGB hex string."
  (and (stringp value)
       (color-defined-p value)))

(defun wid-extra-color-validate (widget)
  "Return nil if WIDGET's value is a valid color value."
  (let ((value  (widget-value widget)))
    (unless (wid-extra-color-match widget value)
      (widget-put widget :error (format "Invalid color: `%S'" value))
      widget)))



(defun wid-extra-color-notify (widget child &optional event)
  "Update the color of a WIDGETs overlay in response to an EVENT.

Argument WIDGET is a WIDGET object that is being manipulated.

Argument CHILD is a CHILD WIDGET of the main WIDGET.

Optional argument EVENT is an EVENT that triggers the function, with no default
value."
  (let* ((ovly       (widget-get widget :sample-overlay))
         (new-color  (widget-apply widget :sample-face-get)))
    (overlay-put ovly 'face new-color))
  (widget-default-notify widget child event))


(defun wid-extra-read-color (&optional prefix)
  "Prompt user to select a color, displaying color names and their hex values.

Optional argument PREFIX: This is a string type argument.
If not provided, its default value is nil.
It is used to filter the list of colors based on whether they start with a
specific PREFIX."
  (interactive)
  (let* ((colors
          (if (and prefix (string-prefix-p "#" prefix))
              (delete nil
                      (mapcar (lambda (cell)
                                (let* ((name (car cell))
                                       (dups (cdr cell))
                                       (hex (wid-extra-color--name-to-hex
                                             name)))
                                  (when hex
                                    (propertize hex 'hex hex 'dups dups 'name
                                                name))))
                              (list-colors-duplicates)))
            (delete nil
                    (mapcar (lambda (cell)
                              (let* ((name (car cell))
                                     (dups (cdr cell))
                                     (hex (wid-extra-color--name-to-hex name)))
                                (when hex
                                  (propertize name 'hex hex 'dups dups))))
                            (list-colors-duplicates)))))
         (len (apply #'max 1 (mapcar #'string-width colors)))
         (blank-back (make-string 10 ?\s))
         (formatter
          (lambda (color)
            (let ((fg (list :foreground color))
                  (blank (make-string (- len (length color)) ?\s)))
              (concat " " blank (concat ;
                                 (propertize blank-back 'face (list :background
                                                                    color))
                                 " "
                                 (propertize
                                  (or (get-text-property 0 'name
                                                         color)
                                      (get-text-property 0 'hex color))
                                  'face
                                  fg)
                                 (propertize (mapconcat (lambda (dup)
                                                          (concat " " dup))
                                                        (get-text-property 0
                                                                           'dups
                                                                           color)
                                                        ",")
                                             'face fg)))))))
    (completing-read "Candidates: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,formatter))
                         (complete-with-action action colors str pred)))
                     nil nil prefix)))

(defun wid-extra-color--name-to-hex (name)
  "Return hexadecimal RGB value of color with NAME.

Return nil if NAME does not designate a valid color."
  (require 'shr-color)
  (let ((rgb (color-name-to-rgb name)))
    (when rgb
      (apply #'color-rgb-to-hex rgb))))

(defun wid-extra-color-complete (widget)
  "Complete the color value in `color' widget WIDGET."
  (let* ((prefix      (buffer-substring-no-properties
                       (widget-field-start widget)
                       (point)))
         (completion (wid-extra-read-color prefix)))
    (cond ((string= completion prefix)
           (message "Sole completion")
           nil)
          ((not (string-equal prefix completion))
           (insert-and-inherit (substring completion (length prefix)))))))

(defun wid-extra-override-color-widget ()
  "Define a color widget with editable field and additional functionalities."
  (put 'color 'widget-type
       (cons 'editable-field
             (list
              :format
              "%{%t%}: %v (%{sample%})\n"
              :size
              (1+
               (apply #'max
                      13
                      (mapcar
                       #'length
                       (defined-colors))))
              :tag      "Color"
              :match
              'wid-extra-color-match
              :validate
              'wid-extra-color-validate
              :value    ""
              :complete
              'wid-extra-color-complete
              :sample-face-get
              'widget-color-sample-face-get
              :notify
              'wid-extra-color-notify
              :action   'widget-color-action)))
  (put 'color 'widget-documentation (purecopy "A wid-extra color widget.")))


 

(provide 'wid-extra)
;;; wid-extra.el ends here