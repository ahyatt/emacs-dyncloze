;;; dyncloze.el --- Language alternatives self-testing -*- lexical-binding:t -*-

(require 'cl-macs)
(require 'dash)
(require 'rx)

;;; Commentary:
;; This package is primarily designed for language learning.
;; To use, run the command `dyncloze' with the alternatives you want to choose
;; from. The program will test you on each occurance (if any), and mark each
;; answer in green or red depending on whether you got it correct. To clear the
;; buffer of these marks, dun `dyncloze-erase'.

;;; Code:

(defun dyncloze-prompt (targets)
  "Create prompt for answer based on TARGETS."
  (concat (string-join
           (mapcar (lambda (i)
                     (format "[%d] %s" (+ i 1) (nth i targets)))
                   (number-sequence 0 (- (length targets) 1)))
           " ") " [q] quit:"))

(defun dyncloze-get-answer (targets)
  "Read a valid answer choosing from TARGETS.
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
    (unless quit (nth (- num 1) targets))))

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
  "Run a testing session with TARGETS on the current buffer."
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
                (let ((regexp (rx space (literal target) (or space line-end))))
                  (while (re-search-forward regexp (point-max) t)
                    (backward-word)
                    (dyncloze-hide))))
              (goto-char (point-min))
              (dolist (o (dyncloze-overlays))
                (goto-char (overlay-start o))
                (let ((answer (dyncloze-get-answer targets)))
                  (if answer
                      (dyncloze-mark
                       o
                       (string-equal answer
                                     (downcase (buffer-substring (overlay-start o)
                                                                 (overlay-end o)))))
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
