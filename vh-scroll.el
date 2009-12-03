;;; vh-scroll.el --- vert. and horiz. scrolling that preserves point visible.

;; Copyright (C) 1993 Anders Holst

;; Author: Anders Holst <aho@sans.kth.se>
;; Maintainer: Noah Friedman <friedman@splode.com>

;; $Id: vh-scroll.el,v 1.4 2001/12/08 13:55:09 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your copy of Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contains changes by Noah Friedman.
;; Based on original author's version 1.0 of 1993-11-18

;;  This file, when loaded, affects mainly three things:
;;
;;  * If lines are truncated, the window is automatically scrolled
;;    horizontally when the point has moved out to the left or right.
;;    The already existing variable `hscroll-step' is used to control
;;    how big steps the window are scrolled. If this variable is set
;;    to nil, auto-scrolling is disabled. The variable
;;    `vh-max-empty-visible' controls how much space may be scrolled
;;    in after the end of the current line.
;;
;;  * The horizontal scrolling commands `vh-scroll-right' and
;;    `vh-scroll-left' drags the point after them.
;;
;;  * The vertical scrolling commands `vh-scroll-down' and
;;    `vh-scroll-up' tries to keep the point in the same column, using
;;    `goal-column' and `temporary-goal-column' as appropriate. They
;;    can be intermixed with `next-line' and `previous-line' without
;;    loosing the temporary goal column. If `vh-track-goal-column' is
;;    non-nil, then auto-scrolling tries to keep the (temporary) goal
;;    column in sight rather than the point.
;;
;;  Also defined is the function `vh-scroll-to-point' which tries to
;;  recenter the point horizontally, and the function
;;  `toggle-truncate-lines' which does what it sounds like.
;;
;;  NOTE: At the end of this file are some variable settings and key
;;  bindings which you might rather want to move out to your .emacs
;;  and change to something else.
;;
;;  NOTE AGAIN: Not to loose the temporary goal column when scrolling with
;;  `vh-scroll-down' and `vh-scroll-up' is intermixed with `next-line' and
;;  `previous-line', this file advises the function `line-move'.

;;; Code:

;; Standard in Emacs 19, but not defined in XEmacs as of 19.12.
(defvar hscroll-step 0
  "*The number of columns to try scrolling a window by when point moves out.
If that fails to bring point back on frame, point is centered instead.
If this is zero, point is always centered after it moves off frame.")

(defvar vh-track-goal-column t
  "*If non-nil, `vh-scroll-point-visible' follows temporary goal column.")

(defvar vh-max-empty-visible 5
  "*Maximum desired empty visible positions after end of line.")

(defun vh-max-scroll ()
  (if (not (integerp vh-max-empty-visible))
      9999                    ; means approximately "no limit"
    (max (+ (save-excursion (end-of-line) (current-column))
            (- (window-width))
            vh-max-empty-visible 1)
         0)))

(defun vh-recenter-scroll (here)
  (let ((col (min (vh-max-scroll)
                  (- here (/ (window-width) 2)))))
    (if (<= col 1) 0 col)))

(defun vh-truncated-lines ()
  (or truncate-lines
      (not (zerop (window-hscroll)))
      (and truncate-partial-width-windows
           (< (window-width) (screen-width)))))

(defvar vh-temporary-goal-column-commands
  '(next-line previous-line vh-scroll-down vh-scroll-up)
  "A list of commands that uses same `temporary-goal-column'.")

(defun vh-update-temporary-goal-column ()
  (if (not (memq last-command vh-temporary-goal-column-commands))
      (setq temporary-goal-column
	    (if (and track-eol (eolp)
		     ;; Don't count beg of empty line as end of line
		     ;; unless we just did explicit end-of-line.
		     (or (not (bolp)) (eq last-command 'end-of-line)))
		9999
	      (current-column)))))

(defadvice line-move (around vh-scroll activate)
  "Do not update temporary goal column when vh-scroll commands are called."
  (vh-update-temporary-goal-column)
  (let ((temporary-goal-column temporary-goal-column))
    ad-do-it))

;; This is just a slight modification of hscroll-point-visible from
;; simple.el .
(defun vh-scroll-point-visible ()
  "Scrolls the window horizontally to make point visible."
  (let* ((here (if (and vh-track-goal-column
                        (memq this-command vh-temporary-goal-column-commands))
                   (or goal-column temporary-goal-column)
                 (current-column)))
	 (left (window-hscroll))
	 (right (if (= left 0)
                    (- (window-width) 2)
                  (+ left (window-width) -3)))
         (right (if (eolp) (+ 1 right) right)))
    (cond
     ((or (not (vh-truncated-lines))
          (not (integerp hscroll-step)))
      ())
     ;; Should we recenter?
     ((or (< here (- left  hscroll-step))
	  (> here (+ right hscroll-step)))
      (set-window-hscroll
       (selected-window)
       (vh-recenter-scroll here)))
     ;; Should we scroll left?
     ((> here right)
      (scroll-left (if (= left 0)
                       (+ 1 hscroll-step)
                     hscroll-step)))
     ;; Or right?
     ((< here left)
      (scroll-right (if (= left (+ 1 hscroll-step))
                       (+ 1 hscroll-step)
                     hscroll-step))))))

(defun vh-scroll-down (&optional n)
  "Scroll window text downward ARG lines, or near full screen, or to beginning.
Without an argument, scrolls `next-screen-context-lines' less than a
full screen.  With argument 0, scrolls to beginning of buffer."
  (interactive "P")
  (vh-update-temporary-goal-column)
  (if (equal n 0)
      (beginning-of-buffer)
    (scroll-down n))
  (move-to-column (or goal-column temporary-goal-column))
  nil)

(defun vh-scroll-up (&optional n)
  "Scroll window text upward ARG lines, or near full screen, or to end.
Without an argument, scrolls `next-screen-context-lines' less than a
full screen.  With argument 0, scrolls to end of buffer."
  (interactive "P")
  (vh-update-temporary-goal-column)
  (if (equal n 0)
      (end-of-buffer)
    (scroll-up n))
  (move-to-column (or goal-column temporary-goal-column))
  nil)

(defun vh-scroll-right (&optional num)
  "Scroll display ARG columns right, or one screen, or to beginning
Without an argument, scrolls one screen right.  With argument 0,
scrolls to beginning of line."
  (interactive "P")
  (cond ((equal num 0)
	 (set-window-hscroll (selected-window) 0)
	 (beginning-of-line))
	((null num)
	 (scroll-right (if (= (window-hscroll) (- (window-width) 2))
			   (- (window-width) 2)
			   (- (window-width) 3)))
         (if (= (window-hscroll) 1)
             (set-window-hscroll (selected-window) 0)))
	(t
	 (scroll-right num)))
  (if (> (current-column) (+ (window-hscroll) (window-width) -2))
      (move-to-column (+ (max 1 (window-hscroll)) (window-width) -3))))

(defun vh-scroll-left (&optional num)
  "Scroll display ARG columns left, or one screen, or to end.
Without an argument, scrolls one screen left.  With argument 0,
scrolls to end of line."
  (interactive "P")
  (cond ((equal num 0)
	 (end-of-line)
	 (set-window-hscroll
          (selected-window)
          (vh-recenter-scroll (current-column))))
	((null num)
	 (scroll-left (if (> (window-hscroll) 0)
			  (- (window-width) 3)
			  (- (window-width) 2)))
         (if (> (window-hscroll) (vh-max-scroll))
             (set-window-hscroll (selected-window) (vh-max-scroll)))
         (if (= (window-hscroll) 1)
             (set-window-hscroll (selected-window) 0)))
	(t
	 (scroll-left num)))
  (if (< (current-column) (window-hscroll))
      (move-to-column (window-hscroll))))

(defun vh-scroll-to-point ()
  "Scroll display horizontally to try to center point on the line."
  (interactive)
  (set-window-hscroll
   (selected-window)
   (vh-recenter-scroll (current-column))))

(defun toggle-truncate-lines (arg)
  "Toggles between truncated lines and continuation lines in buffer.
With ARG, truncate lines iff ARG is positive."
  (interactive "P")
  (let ((hs (window-hscroll)))
    (if (if arg
            (> (prefix-numeric-value arg) 0)
          (and (= hs 0)
               (not truncate-lines)))
	(progn
	  (setq truncate-lines t)
	  (if (> (current-column) (- (window-width) 2))
	      (setq hs (vh-recenter-scroll (current-column)))))
	(progn
	  (setq hs 0)
	  (setq truncate-lines ())))
    (set-window-hscroll (selected-window) 10)   ; needed for updating screen
    (set-window-hscroll (selected-window) hs)))

;; Start the auto-scrolling
(add-hook 'post-command-hook 'vh-scroll-point-visible)

;; Here follows some variable settings and key bindings that I find
;; convenient, but that perhaps you may want to remove, put in your
;; .emacs, and change to what you like.

;(setq truncate-partial-width-windows nil)
;(setq default-truncate-lines t)

;(define-key global-map "\C-v" 'vh-scroll-up)
;(define-key esc-map "v" 'vh-scroll-down)
;(define-key ctl-x-map "<" 'vh-scroll-left)
;(define-key ctl-x-map ">" 'vh-scroll-right)
;(define-key ctl-x-map "z" 'vh-scroll-to-point)
;(define-key ctl-x-map "t" 'toggle-truncate-lines)

;;; vh-scroll.el end here
