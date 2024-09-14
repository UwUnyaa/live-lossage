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

(defgroup emacs-input-display nil
  "Show live lossage in a separate frame."
  :prefix "emacs-input-display-"
  :group 'streaming)

(defun emacs-input-display ()
  (interactive)
  (message "Not implemented."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-input-display.el ends here
