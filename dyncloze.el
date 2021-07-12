;;; dyncloze.el --- Language alternatives self-testing -*- lexical-binding:t -*-

;; Copyright (C) 2021 Andrew Hyatt

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/emacs-dyncloze
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (dash "2.18"))

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
;; This package is primarily designed for language learning.
;; To use, run the command `dyncloze' with the alternatives you want to choose
;; from. The program will test you on each occurence (if any), and mark each
;; answer in green or red depending on whether you got it correct. To clear the
;; buffer of these marks, run `dyncloze-erase'.
;;
;; The `dyncloze' command can also be used as a function for other more advanced
;; forms of clozes, since it can take in more complicated regexes.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'rx)
(require 'subr-x)

(defun dyncloze-label (target)
  "Return the appropriate label to display from TARGET."
  (if (consp target) (cdr target) target))

(defun dyncloze-regex (target)
  "Return the appropriate regex to display from TARGET."
  (if (consp target)
      (rx space (regexp (car target))  (or space line-end))
    (rx space (literal target) (or space line-end))))

(defun dyncloze-prompt (targets)
  "Create prompt for answer based on TARGETS."
  (concat (string-join
           (mapcar (lambda (i)
                     (format "[%d] %s" (+ i 1)
                             (dyncloze-label (nth i targets))))
                   (number-sequence 0 (- (length targets) 1)))
           " ") " [q] quit:"))

(defun dyncloze-get-answer (targets)
  "Read regex corresponding to answer chosen from TARGETS.
Returns the matching target string, or nil for quit."
  (let ((num 0)
        (quit nil))
    (while (and (not quit)
                (or (= num 0)
                    (> num (length targets))))
      (let ((response (read-key-sequence
                       (dyncloze-prompt targets))))
        (if (equal response "q")
            (setq quit t)
          (setq num (string-to-number
                 response)))))
    (unless quit (dyncloze-regex (nth (- num 1) targets)))))

(defun dyncloze-hide ()
  "Hides the word under point."
  (let* ((start (point))
         (end (save-excursion (forward-word) (point)))
         (o (make-overlay start end)))
    (overlay-put o 'dyncloze t)
    (overlay-put o 'invisible t)
    (overlay-put o 'display "[ ]")))

(defun dyncloze-mark (overlay correct)
  "If CORRECT, then mark the cloze OVERLAY as correctly answered.
If not, mark the cloze as incorrect. This is done by changing
colors.  ALso remove the cloze display."
  (overlay-put overlay 'dyncloze-correct correct)
  (overlay-put overlay 'dyncloze-answered t)
  (overlay-put overlay 'face
               `(background-color . ,(if correct "green" "red")))
  (overlay-put overlay 'display (buffer-substring (overlay-start overlay)
                                                  (overlay-end overlay))))

(defun dyncloze-overlays ()
  "Return all overlays in the current buffer."
  (-sort
   (lambda (a b) (< (overlay-start a) (overlay-start b)))
   (seq-filter (lambda (o) (overlay-get o 'dyncloze))
                    (overlays-in (point-min) (point-max)))))

(defun dyncloze-erase ()
  "Remove all cloze overlays in the buffer."
  (interactive)
  (dolist (o (dyncloze-overlays))
    (delete-overlay o)))

(defun dyncloze (targets)
  "Run a testing session with TARGETS on the current buffer.
Targets can be a list of strings or conses with regex and display
string in them. The targets do not all have to be the same type,
it can be a mix of strings and conses."
  (interactive "sTargets: ")
  (save-excursion
    (let ((orig-blink-mode (member 'blink-cursor-mode minor-mode-list))
          (blink-cursor-blinks -1))
      (blink-cursor-mode 1)
      (dyncloze-erase)
      (unwind-protect
          (cl-block nil
            (let ((targets (if (listp targets) targets
                             (split-string targets))))
              (dolist (target targets)
                (goto-char (point-min))
                (let ((regexp (dyncloze-regex target)))
                  (while (re-search-forward regexp (point-max) t)
                    (backward-word)
                    (dyncloze-hide)
                    (forward-word))))
              (goto-char (point-min))
              (dolist (o (dyncloze-overlays))
                (goto-char (overlay-start o))
                (let ((answer-regex (dyncloze-get-answer targets)))
                  (if answer-regex
                      (dyncloze-mark
                       o
                       (string-match answer-regex
                                     (downcase (buffer-substring (- (overlay-start o) 1)
                                                                 (+ (overlay-end o) 1)))))
                    (cl-return))
                  (redisplay)
                  (sleep-for 2)))))
        (let* ((overlays (dyncloze-overlays))
               (correct (-count (lambda (o) (overlay-get o 'dyncloze-correct)) overlays))
               (total (-count (lambda (o) (overlay-get o 'dyncloze-answered)) overlays)))
          (when (> total 0)
            (message "Score: %d / %d: %1.0f%%"
                     correct total
                     (* 100 (/ (float correct)
                               total))))
          (dolist (o (-filter (lambda (o) (not (overlay-get o 'dyncloze-answered))) overlays))
            (delete-overlay o)))
        (blink-cursor-mode (if orig-blink-mode 1 0))))))

(provide 'dyncloze)

;;; dyncloze.el ends here
