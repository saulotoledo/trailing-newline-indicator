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
;; Version: 0.3.6
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
(defvar trailing-newline-indicator--active-count 0
  "Number of buffers with `trailing-newline-indicator-mode' enabled.")

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
             (line (line-number-at-pos (1- (point-max))))
             (nl-symbol (propertize trailing-newline-indicator-newline-symbol 'face 'line-number))
             (indicator-text
              (if (and trailing-newline-indicator-show-line-number
                       (bound-and-true-p display-line-numbers))
                  (let ((small-num
                         (propertize (format " %d" (1+ line))
                                     'face 'trailing-newline-indicator-small-number)))
                    (concat nl-symbol small-num))
                nl-symbol)))
        (overlay-put ov 'after-string
                     (propertize "\u200b"
                                 'display `(margin left-margin ,indicator-text)))
        (setq trailing-newline-indicator--overlay ov)))))

;;; Control Hooks Setup:
(defun trailing-newline-indicator--hook-list ()
  "Return the list of hooks used by trailing-newline-indicator."
  '(after-change-functions
    after-save-hook
    after-revert-hook
    post-command-hook))

(defun trailing-newline-indicator--on-kill-buffer ()
  "Disable the trailing newline indicator mode when the buffer is killed.
This runs the cleanup logic of `trailing-newline-indicator-mode'."
  (when (bound-and-true-p trailing-newline-indicator-mode)
    (trailing-newline-indicator-mode -1)))

(defun trailing-newline-indicator--restore-hooks ()
  "Restore hooks if the mode is active but hooks were cleared.
In Emacs 31+, major-mode transitions are more aggressive in clearing
buffer-local variables. Since `trailing-newline-indicator-mode' is
`permanent-local', it remains enabled during a mode change, but the
underlying `after-change-functions' and other hooks are wiped. This
function detects that orphaned state and re-attaches the hooks."
  (when (and (bound-and-true-p trailing-newline-indicator-mode)
             (not (memq #'trailing-newline-indicator--update-indicator after-change-functions)))
    (trailing-newline-indicator--setup-hooks)))

(defun trailing-newline-indicator--setup-hooks ()
  "Setup necessary hooks for trailing newline indicator."
  (unless (memq #'trailing-newline-indicator--restore-hooks
                (default-value 'after-change-major-mode-hook))
    (add-hook 'after-change-major-mode-hook #'trailing-newline-indicator--restore-hooks))
  (add-hook 'kill-buffer-hook #'trailing-newline-indicator--on-kill-buffer nil t)
  (let ((update-fn #'trailing-newline-indicator--update-indicator))
    (dolist (hook (trailing-newline-indicator--hook-list))
      (add-hook hook update-fn nil t))))

(defun trailing-newline-indicator--cleanup-hooks ()
  "Remove hooks used by trailing-newline-indicator."
  (remove-hook 'kill-buffer-hook #'trailing-newline-indicator--on-kill-buffer t)
  (let ((update-fn #'trailing-newline-indicator--update-indicator))
    (dolist (hook (trailing-newline-indicator--hook-list))
      (remove-hook hook update-fn t)))
  (when (= trailing-newline-indicator--active-count 0)
    (remove-hook 'after-change-major-mode-hook #'trailing-newline-indicator--restore-hooks)))

(defun trailing-newline-indicator--increment-count ()
  "Increment the global counter of buffers with mode enabled."
  (setq trailing-newline-indicator--active-count (1+ trailing-newline-indicator--active-count)))

(defun trailing-newline-indicator--decrement-count ()
  "Decrement the global counter of buffers with mode enabled."
  (setq trailing-newline-indicator--active-count (max 0 (1- trailing-newline-indicator--active-count))))

;;; Minor Mode Definition and Activation:
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
        (trailing-newline-indicator--increment-count)
        (trailing-newline-indicator--setup-hooks)
        (trailing-newline-indicator--update-indicator))

    (when (or trailing-newline-indicator--overlay
              (memq #'trailing-newline-indicator--on-kill-buffer kill-buffer-hook))
      (trailing-newline-indicator--delete-overlay)
      (trailing-newline-indicator--decrement-count)
      (trailing-newline-indicator--cleanup-hooks))))

(defun trailing-newline-indicator--is-buffer-suitable-for-indicator-p ()
  "Return non-nil if the current buffer is suitable for the indicator.
That means that the buffer is not a minibuffer, not a special mode
buffer, and does not have the indicator already enabled."
  (and (not (minibufferp))
       (not (derived-mode-p 'special-mode))
       buffer-file-name
       (not trailing-newline-indicator-mode)))

;;;###autoload
(define-globalized-minor-mode global-trailing-newline-indicator-mode
  trailing-newline-indicator-mode
  (lambda ()
    (when (trailing-newline-indicator--is-buffer-suitable-for-indicator-p)
      (trailing-newline-indicator-mode 1)))
  :group 'trailing-newline-indicator)

(put 'trailing-newline-indicator-mode 'permanent-local t) ; Make mode state permanent across major mode changes

;;; Provide Feature:
(provide 'trailing-newline-indicator)
;;; trailing-newline-indicator.el ends here
