;;; live-lossage.el --- pop up a frame with live lossage display
;;; -*- lexical-binding:t; coding: utf-8 -*-
;;; Version: 0.0.1

;; Copyright (C) 2024 UwUnyaa
;; Author: UwUnyaa (https://github.com/UwUnyaa)

;; Package-Requires: (dash)

;;; license: GPLv3 or newer

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides a way of having a separate Emacs frame showing real time
;; lossage display, meant for livestreaming and screencasts.

(require 'dframe)
(require 'dash)

;;; Customization

;;; Code:

(defgroup live-lossage nil
  "Show live lossage in a separate frame."
  :prefix "live-lossage-"
  :group 'streaming)

(defcustom live-lossage-width
  80
  "Width of the input display frame."
  :group 'live-lossage
  :type 'integer)

(defcustom live-lossage-font-size
  250
  "Size of the font in the input display frame."
  :group 'live-lossage
  :type 'integer)

(defcustom live-lossage-formatting-alist
  '(("<return>"        . "RET")
    ("<backspace>"     . "DEL")
    ("C-S-<backspace>" . "C-S-DEL")
    ("<tab>"           . "TAB")
    ("S-<iso-lefttab>" . "S-TAB")
    ("<escape>"        . "ESC")
    ("M-<return>"      . "M-RET")
    ("M-S-<return>"    . "M-S-RET"))
  "Replacements for keys."
  :group 'live-lossage
  :type
  '(repeat
    (cons string string)))

(defcustom live-lossage-ignored-keys
  '("<down-mouse-1>"
    "<mouse-1>"
    "<drag-mouse-1>"
    "<down-mouse-2>"
    "<mouse-2>"
    "<down-mouse-3>"
    "<mouse-3>"
    "<help-echo>"
    "<mouse-movement>"
    "<wheel-down>"
    "<wheel-up>"
    "<double-wheel-up>"
    "<triple-wheel-up>"
    "<switch-frame>")
  "Keys to not display."
  :group 'live-lossage
  :type
  '(list string))

(defcustom live-lossage-frame-parameters
  `((minibuffer     . nil)
    (width          . ,live-lossage-width)
    (height         . 2)
    (border-width   . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable   . t)
    (left-fringe    . 0)
    (right-fringe   . 0)
    (title          . "EMACS INPUT DISPLAY"))
  "Parameters to use when creating the input display frame.
Any parameter supported by a frame may be added."
  :group 'live-lossage
  :type
  '(repeat
    (cons :format "%v"
          (symbol :tag "Parameter")
          (sexp :tag "Value"))))

;;; Variables
(defvar live-lossage--buffer nil
  "The buffer used for showing the lossage.")

(defvar live-lossage--frame nil
  "The frame used for showing the lossage.")

(defvar live-lossage--cached-frame nil
  "The frame that was last created, then removed from the display.")

;;; Functions
(defun live-lossage--setup-buffer ()
  "Set up a buffer for showing lossage.

This function does not check whether the buffer is set up, so
checking that is the responsibility of the caller."
  (with-current-buffer
      (setq live-lossage--buffer
            (get-buffer-create " EMACS INPUT DISPLAY"))
    (live-lossage-mode)))

(defun live-lossage--setup-frame ()
  "Set up a frame for showing lossage.

This function does not check whether the frame is set up, so
checking that is the responsibility of the caller."
  (set-face-attribute 'default live-lossage--frame
                      :height live-lossage-font-size)
  (setq mode-line-format nil)
  (read-only-mode -1)
  (delete-region (point-min) (point-max))
  ;; set up the newline to keep a margin from the top of the window
  (newline)
  (read-only-mode +1))

(define-derived-mode live-lossage-mode fundamental-mode "Input display"
  "Major mode for showing live lossage.

This mode shouldn't be used manually."
  (save-excursion
    (setq font-lock-keywords nil
          truncate-lines t
          case-fold-search nil
	  buffer-read-only t)
    (setq-local frame-title-format "Input display")))

(defun live-lossage--get-current-line-length ()
  "Get the length of the current line in a buffer."
  (- (line-end-position) (line-beginning-position)))

(defun live-lossage--get-last-key ()
  "Get the last pressed key."
  (let ((keys (recent-keys)))
    (aref keys (1- (length keys)))))

(defun live-lossage--pretty-print-key
    (key)
  "Pretty print a single KEY in a nice, terse format."
  (let ((pretty (single-key-description key)))
    (unless
        (member pretty live-lossage-ignored-keys)
      (or (cdr (assoc-string pretty live-lossage-formatting-alist))
          pretty))))

(defun live-lossage--delete-to-string (string)
  "Remove characters from point up to STRING."
  (delete-region (point) (search-forward string)))

(defun live-lossage--command-hook ()
  "Handler for the `post-command-hook'."
  (with-current-buffer live-lossage--buffer
    (read-only-mode -1)
    (goto-char (point-min))
    (forward-line 1)
    (beginning-of-line)
    (let* ((new-key (live-lossage--pretty-print-key
                     (live-lossage--get-last-key)))
           (key-length (length new-key)))
    (when new-key
      (while (>= (live-lossage--get-current-line-length)
                 (- live-lossage-width key-length 1))
        (live-lossage--delete-to-string " "))
      (end-of-line)
      (unless (zerop (live-lossage--get-current-line-length))
        (insert " "))
      (insert new-key)))
    (read-only-mode +1)))

(defun live-lossage--cleanup-hook ()
  "Clean up after `live-lossage'."
  (remove-hook 'post-command-hook #'live-lossage--command-hook))

;;;###autoload
(defun live-lossage (&optional arg)
  "Open a window with a live lossage display.

If called again, the window will be closed and cleaned up.

A nil ARG means toggle."
  (interactive)
  (unless live-lossage--buffer
    (live-lossage--setup-buffer))
  (dframe-frame-mode arg
                     'live-lossage--frame
                     'live-lossage--cached-frame
                     'live-lossage--buffer
                     "EMACS INPUT DISPLAY"
                     #'live-lossage-mode
                     live-lossage-frame-parameters
                     #'live-lossage--cleanup-hook)
  (add-hook 'post-command-hook #'live-lossage--command-hook)
  (when live-lossage--frame
    (live-lossage--setup-frame)
    (lower-frame live-lossage--frame)))

(provide 'live-lossage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; live-lossage.el ends here
