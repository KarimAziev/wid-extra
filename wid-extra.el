;;; wid-extra.el --- Extra widgets and enhancers -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/wid-extra
;; Version: 0.1.0
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
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


(defun wid-extra-read-color (prompt &optional predicate require-match
                                    initial-input hist def inherit-input-method)
  "PROMPT user to select a color, displaying color names and hex values.

Argument PROMPT is a string that is displayed to the user as a PROMPT.

Optional argument PREDICATE is a function that filters the list of possible
completions.
It defaults to nil.

Optional argument REQUIRE-MATCH is a boolean that determines whether the user is
required to choose a valid completion.
It defaults to nil.

Optional argument INITIAL-INPUT is a string that is pre-filled in the input
field.
It defaults to nil.

Optional argument HIST is a symbol representing the history list to use for the
input field.
It defaults to nil.

Optional argument DEF is a string that is used as the default value if the user
enters nothing.
It defaults to nil.

Optional argument INHERIT-INPUT-METHOD is a boolean that determines whether to
inherit the current input method for the input field.
It defaults to nil."
  (interactive)
  (let* ((colors
          (if (and initial-input (string-prefix-p "#" initial-input))
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
              (concat " " blank (concat
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
    (completing-read prompt
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,formatter))
                         (complete-with-action action colors str pred)))
                     predicate require-match initial-input hist def
                     inherit-input-method)))

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
         (completion (wid-extra-read-color "Color: " nil nil prefix)))
    (cond ((string= completion prefix)
           (message "Sole completion")
           nil)
          ((not (string-equal prefix completion))
           (insert-and-inherit (substring completion (length prefix)))))))

(defun wid-extra-plist-remove (keys plist)
  "Remove KEYS and values from PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and (not (memq key keys))
                       (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun wid-extra-add-color-completions ()
  "Add annotated completions to color widget."
  (let* ((defaults (get 'color 'widget-type))
         (type (car defaults))
         (pl (cdr defaults)))
    (setq pl (plist-put pl :complete 'wid-extra-color-complete))
    (setq pl (wid-extra-plist-remove '(:completions)
                                     pl))
    (put 'color 'widget-type (cons type pl))))


(defun wid-extra-toggle-hide-all-widgets (&rest _args)
  "Toggle the visibility of all custom widgets in the buffer.
This function is supposed to use as advice:

\\=(advice-add \\='custom-buffer-create-internal
              :after #\\='wid-extra-toggle-hide-all-widgets)."
  (when (fboundp 'custom-toggle-hide-all-widgets)
    (let ((ov (seq-find (lambda (o)
                          (eq 'custom-visibility
                              (car (overlay-get o 'button))))
                        (overlays-in (point-min)
                                     (point-max)))))
      (when ov
        (goto-char (overlay-start ov))
        (custom-toggle-hide-all-widgets)))))


(provide 'wid-extra)
;;; wid-extra.el ends here