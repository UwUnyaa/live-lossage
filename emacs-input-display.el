;;; emacs-input-display.el --- pop up a frame with live lossage display
;;; -*- lexical-binding:t; coding: utf-8 -*-
;;; Version: 0.0.1

;; Copyright (C) 2024 UwUnyaa
;; Author: UwUnyaa (https://github.com/UwUnyaa)

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
;; This file provides a way of having a separate emacs frame showing real time
;; lossage display, meant for livestreaming and screencasts.

(require 'dframe)

;;; Customization
(defgroup emacs-input-display nil
  "Show live lossage in a separate frame."
  :prefix "emacs-input-display-"
  :group 'streaming)

(defcustom emacs-input-display-width
  80
  "Width of the input display frame."
  :group 'emacs-input-display
  :type 'integer)

(defcustom emacs-input-display-frame-parameters
  `((minibuffer     . nil)
    (width          . emacs-input-display-width)
    (height         . 3)
    (border-width   . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable   . t)
    (left-fringe    . 0)
    (title          . "EMACS INPUT DISPLAY"))
  "Parameters to use when creating the input display frame.
Any parameter supported by a frame may be added."
  :group 'emacs-input-display
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value"))))

;;; Variables
(defvar emacs-input-display--buffer nil
  "The buffer used for showing the lossage.")

(defvar emacs-input-display--frame nil
  "The frame used for showing the lossage.")

(defvar emacs-input-display--cached-frame nil
  "The frame that was last created, then removed from the display.")

(defun emacs-input-display (&optional arg)
  (interactive)
  (unless emacs-input-display--buffer
    (emacs-input-display--setup-buffer))
  (dframe-frame-mode arg
                     'emacs-input-display--frame
                     'emacs-input-display--cached-frame
                     'emacs-input-display--buffer
                     "EMACS INPUT DISPLAY"
                     #'emacs-input-display-mode
                     emacs-input-display-frame-parameters
                     #'emacs-input-display--cleanup-hook)
  (add-hook 'post-command-hook #'emacs-input-display--command-hook))

(defun emacs-input-display--setup-buffer ()
  "Set up a buffer for showing lossage.

This function does not check whether the buffer is set up, so
checking that is the responsibility of the caller."
  (with-current-buffer
          (setq emacs-input-display--buffer (get-buffer-create " EMACS INPUT DISPLAY"))
    (emacs-input-display-mode)))

(define-derived-mode emacs-input-display-mode fundamental-mode "Input display"
  "Major mode for showing live lossage.

This mode shouldn't be used manually."
  (save-excursion
    (setq font-lock-keywords nil
          truncate-lines t
          case-fold-search nil
	  buffer-read-only t)
    (setq-local frame-title-format "Input display")))

(defun emacs-input-display--pretty-print-key (key)
  "Pretty print a single KEY in a nice, terse format."
  ;; TODO: use a lookup to actually do translations like <return> -> RET
  ;; TODO: maybe add (xN) in some cases when certain keys (like DEL) are pressed multiple times?
  (single-key-description key t))

(defun emacs-input-display--get-formatted-losage ()
  "Get losage formatted for display in the buffer."
  (mapconcat #'emacs-input-display--pretty-print-key
             (recent-keys)
             " "))

(defun emacs-input-display--command-hook ()
  "Handler for the `post-command-hook'."
  (with-current-buffer emacs-input-display--buffer
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert (substring (emacs-input-display--get-formatted-losage) (- 0 60)))
    (read-only-mode +1)))

(defun emacs-input-display--cleanup-hook ()
  "Clean up after `emacs-input-display'."
  (remove-hook 'post-command-hook #'emacs-input-display--command-hook))

(provide 'emacs-input-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-input-display.el ends here
