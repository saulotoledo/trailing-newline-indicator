;;; trailing-newline-indicator.el --- Show an indicator for the trailing newline -*- lexical-binding: t; -*-

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

;; Author: Saulo S. de Toledo <saulotoledo@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, display, editing
;; URL: https://github.com/saulotoledo/trailing-newline-indicator

;;; Commentary:

;; `trailing-newline-indicator' provides a minor mode that displays a visual
;; indicator in the left margin for the trailing newline at the end of a file.
;; This helps highlight the empty visual line that appears due to the final
;; newline character.
;;
;; Optionally, the indicator can also show the next line number (e.g., `⏎ 43'),
;; giving a clearer sense of where the file ends. The line number display is
;; controlled by the option:
;;
;;   M-x customize-variable RET trailing-newline-indicator-show-line-number RET
;;
;; Note that the line number is only visible when `display-line-numbers-mode'
;; is active. The `⏎' symbol itself, however, is always visible.
;;
;; Usage:
;;
;;   (require 'trailing-newline-indicator)
;;   (trailing-newline-indicator-mode 1)
;;
;; To enable globally for all buffers:
;;
;;   (global-trailing-newline-indicator-mode 1)
;;
;; This package is safe to load via package.el.

;;; Code:

;;; Customization:
(defgroup trailing-newline-indicator nil
  "Display an indicator for existing trailing newline in the file."
  :group 'convenience
  :prefix "trailing-newline-indicator-")

(defcustom trailing-newline-indicator-newline-symbol "⏎"
  "The icon to use for the newline symbol overlay."
  :type 'string
  :group 'trailing-newline-indicator)

(defcustom trailing-newline-indicator-show-line-number t
  "If non-nil, show the next line number next to the trailing newline symbol."
  :type 'boolean
  :group 'trailing-newline-indicator)

(defface trailing-newline-indicator-small-number
  '((t :height 0.7 :inherit line-number))
  "Face for the small trailing newline line number."
  :group 'trailing-newline-indicator)

;;; Internal Variables:
(defvar-local trailing-newline-indicator--overlay nil
  "Overlay used to display the trailing newline indicator in the margin.")

(put 'trailing-newline-indicator--overlay 'permanent-local t)

;;; Overlay Management:
(defun trailing-newline-indicator--delete-overlay ()
  "Delete the trailing newline indicator overlay in the current buffer."
  (when trailing-newline-indicator--overlay
    (progn
      (remove-overlays (point-max) (point-max)) ; Remove any overlays at the end of the buffer that could interfere
      (delete-overlay trailing-newline-indicator--overlay)
      (setq trailing-newline-indicator--overlay nil))))

(defun trailing-newline-indicator--update-indicator (&rest _)
  "Update the trailing newline indicator overlay in the current buffer.
Removes any existing overlay, and if the buffer ends with a newline,
adds an indicator in the left margin for the visual empty line."
  (trailing-newline-indicator--delete-overlay)
  (when (and (eq (char-before (point-max)) ?\n)
             (not (eq (point-min) (point-max))))
    (save-excursion
      (goto-char (1- (point-max)))
      (let* ((ov (make-overlay (point-max) (point-max)))
             (win (get-buffer-window (current-buffer)))
             (ln-width (if (and win (bound-and-true-p display-line-numbers))
                           (let ((w (or (window-parameter win 'line-number-width)
                                        (length (number-to-string (line-number-at-pos (point-max)))))))
                             (max 2 w))
                         3))

             (line (line-number-at-pos (1- (point-max))))
             (nl-symbol (propertize trailing-newline-indicator-newline-symbol 'face 'line-number))
             (indicator-text
              (if (and trailing-newline-indicator-show-line-number
                       (bound-and-true-p display-line-numbers))
                  (let ((small-num
                         (propertize (format "%d" (1+ line))
                                     'face 'trailing-newline-indicator-small-number)))
                    (concat nl-symbol " " small-num))
                nl-symbol)))
        ;; Ensure margin is wide enough for line numbers
        (when win
          (set-window-margins win ln-width (cdr (window-margins win))))
        (overlay-put ov 'after-string
                     (propertize "\u200b"
                                 'display `(margin left-margin ,indicator-text)))
        (setq trailing-newline-indicator--overlay ov)))))

;;; Control Hooks Setup:
(defun trailing-newline-indicator--hook-list ()
  "Return the list of hooks used by trailing-newline-indicator."
  (append
   ;; Use focus-in-hook if available and not obsolete, else use after-focus-change-function (Emacs 27.1+)
   (if (boundp 'after-focus-change-function)
       '(after-focus-change-function)
     '(focus-in-hook))
   '(after-change-functions
     after-save-hook
     after-revert-hook
     kill-buffer-hook
     post-command-hook)))

(defun trailing-newline-indicator--setup-hooks ()
  "Setup necessary hooks for trailing newline indicator."
  (let ((update-fn #'trailing-newline-indicator--update-indicator))
    (dolist (hook (trailing-newline-indicator--hook-list))
      (add-hook hook update-fn nil t))))

(defun trailing-newline-indicator--cleanup-hooks ()
  "Remove hooks used by trailing-newline-indicator."
  (let ((update-fn #'trailing-newline-indicator--update-indicator))
    (dolist (hook (trailing-newline-indicator--hook-list))
      (remove-hook hook update-fn t))))

;;; Minor Mode Definition:
;;;###autoload
(define-minor-mode trailing-newline-indicator-mode
  "Minor mode to show a special indicator for trailing newlines.
When enabled, displays a symbol (and optionally a small line number)
in the left margin for the visual empty line created by a trailing
newline."
  :lighter " TNLI"
  :group 'trailing-newline-indicator
  (if (or (eq trailing-newline-indicator-mode t)
          (and (numberp trailing-newline-indicator-mode)
              (> trailing-newline-indicator-mode 0)))

          (unless trailing-newline-indicator--overlay
            (trailing-newline-indicator--setup-hooks))

    (trailing-newline-indicator--delete-overlay)
    (trailing-newline-indicator--cleanup-hooks)))

;;;###autoload
(define-globalized-minor-mode global-trailing-newline-indicator-mode
  trailing-newline-indicator-mode
  (lambda ()
    (unless (or (minibufferp)
                (derived-mode-p 'special-mode)
                (not buffer-file-name)
                trailing-newline-indicator-mode)

      (trailing-newline-indicator-mode 1)))
  :group 'trailing-newline-indicator)

(put 'trailing-newline-indicator-mode 'permanent-local t) ; Make mode state permanent across major mode changes

;;; Provide Feature:
(provide 'trailing-newline-indicator)
;;; trailing-newline-indicator.el ends here
