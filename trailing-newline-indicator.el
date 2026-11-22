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
;; Version: 0.3.8-snapshot
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, display, editing
;; URL: https://github.com/saulotoledo/trailing-newline-indicator

;;; Commentary:

;; `trailing-newline-indicator' provides a minor mode that displays inline
;; ghost text at the end of a file for the trailing newline.
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

(defvar-local trailing-newline-indicator--active nil
  "Buffer object when counted in `trailing-newline-indicator--active-count'.")

(defvar-local trailing-newline-indicator--overlay nil
  "Overlay used to display the trailing newline ghost-text indicator.")

(defvar-local trailing-newline-indicator--last-state nil
  "Cached buffer state from the last indicator update.")

(put 'trailing-newline-indicator--active 'permanent-local t)
(put 'trailing-newline-indicator--overlay 'permanent-local t)

;;; Overlay Management:
(defun trailing-newline-indicator--delete-overlay ()
  "Delete the trailing newline indicator overlay in the current buffer."
  (setq trailing-newline-indicator--last-state nil)
  (when (and (overlayp trailing-newline-indicator--overlay)
             (eq (overlay-buffer trailing-newline-indicator--overlay)
                 (current-buffer)))
    (delete-overlay trailing-newline-indicator--overlay))
  (setq trailing-newline-indicator--overlay nil)
  (save-restriction
    (widen)
    (remove-overlays (max (point-min) (1- (point-max)))
                     (point-max)
                     'trailing-newline-indicator
                     t)))

(defun trailing-newline-indicator--line-number (visible-max)
  "Return the trailing newline line number at VISIBLE-MAX, or nil.
Respects `display-line-numbers' (absolute and `relative'),
`display-line-numbers-widen', `display-line-numbers-offset', and
`display-line-numbers-current-absolute'.  Returns nil for `visual'
because visual line counts depend on window width."
  (let ((style (bound-and-true-p display-line-numbers))
        (widen (bound-and-true-p display-line-numbers-widen)))
    (cond
     ;; Disabled or visual (window-width dependent): no line number.
     ((or (null style) (eq style 'visual))
      nil)
     ;; Relative mode with point on the trailing line itself.
     ((and (eq style 'relative) (= (point) visible-max))
      (if (bound-and-true-p display-line-numbers-current-absolute)
          (1+ (line-number-at-pos (1- visible-max) widen))
        0))
     ;; Relative mode with point on an earlier line.
     ((eq style 'relative)
      (let ((eof-line (1+ (line-number-at-pos (1- visible-max))))
            (cur-line (line-number-at-pos (point))))
        (- eof-line cur-line)))
     ;; Absolute mode (any other non-nil value of `display-line-numbers').
     (t
      (let* ((raw-offset (bound-and-true-p display-line-numbers-offset))
             (offset (if (integerp raw-offset) raw-offset 0))
             (effective-widen (or widen (not (zerop offset)))))
        (abs (+ (1+ (line-number-at-pos (1- visible-max) effective-widen))
                offset)))))))

(defun trailing-newline-indicator--current-state (visible-max style)
  "Return a state key capturing all inputs affecting the indicator.
VISIBLE-MAX is `(point-max)' and STYLE is `display-line-numbers'."
  (list (buffer-chars-modified-tick)
        (point-min)
        visible-max
        trailing-newline-indicator-newline-symbol
        trailing-newline-indicator-show-line-number
        style
        (bound-and-true-p display-line-numbers-widen)
        (bound-and-true-p display-line-numbers-offset)
        (bound-and-true-p display-line-numbers-current-absolute)
        (when (and trailing-newline-indicator-show-line-number
                   (eq style 'relative)
                   (eq (char-before visible-max) ?\n))
          (line-beginning-position))))

(defun trailing-newline-indicator--build-text (visible-max style)
  "Build the propertized indicator string at VISIBLE-MAX for STYLE, or nil."
  (let ((show-indicator (> (length trailing-newline-indicator-newline-symbol) 0))
        (show-line-num (and trailing-newline-indicator-show-line-number
                            style
                            (not (eq style 'visual)))))
    (when (and (or show-indicator show-line-num)
               (eq (char-before visible-max) ?\n)
               (not (eq (point-min) visible-max))
               (= visible-max (save-restriction (widen) (point-max))))
      (let* ((nl-symbol (propertize trailing-newline-indicator-newline-symbol
                                    'face 'line-number))
             (text (if show-line-num
                       (concat nl-symbol
                               (propertize
                                (format (if show-indicator " %d" "%d")
                                        (trailing-newline-indicator--line-number visible-max))
                                'face 'trailing-newline-indicator-small-number))
                     nl-symbol)))
        ;; `cursor t' on the first character makes the block cursor appear ON
        ;; the symbol when point is at (point-max), matching inline ghost text.
        (add-text-properties 0 1 '(cursor t) text)
        text))))

(defun trailing-newline-indicator--ensure-overlay (pos text)
  "Place or reuse the indicator overlay at POS displaying TEXT."
  (let ((ov trailing-newline-indicator--overlay))
    (if (and (overlayp ov) (eq (overlay-buffer ov) (current-buffer)))
        (unless (and (= (overlay-start ov) pos) (= (overlay-end ov) pos))
          (move-overlay ov pos pos))
      (remove-overlays (max (point-min) (1- pos)) pos 'trailing-newline-indicator t)
      (setq ov (make-overlay pos pos nil t t))
      (overlay-put ov 'trailing-newline-indicator t)
      (overlay-put ov 'priority 100)
      (setq trailing-newline-indicator--overlay ov))
    (overlay-put ov 'before-string text)))

(defun trailing-newline-indicator--update-indicator (&rest _)
  "Update the trailing newline indicator overlay in the current buffer.
Skips work when buffer state is unchanged, and reuses the existing
overlay in place when updating."
  (let* ((visible-max (point-max))
         (style (bound-and-true-p display-line-numbers))
         (state (trailing-newline-indicator--current-state visible-max style))
         (ov trailing-newline-indicator--overlay))
    (unless (and (equal-including-properties state trailing-newline-indicator--last-state)
                 (or (null ov)
                     (and (overlayp ov)
                          (eq (overlay-buffer ov) (current-buffer))
                          (= (overlay-start ov) visible-max)
                          (= (overlay-end ov) visible-max))))
      (let ((text (trailing-newline-indicator--build-text visible-max style)))
        (if text
            (trailing-newline-indicator--ensure-overlay visible-max text)
          (trailing-newline-indicator--delete-overlay)))
      (setcar (nthcdr 3 state) (copy-sequence trailing-newline-indicator-newline-symbol))
      (setq trailing-newline-indicator--last-state state))))

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

(defun trailing-newline-indicator--recount-active ()
  "Recompute `trailing-newline-indicator--active-count' from live buffers."
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (eq (buffer-local-value 'trailing-newline-indicator--active buf) buf)
        (setq count (1+ count))))
    (setq trailing-newline-indicator--active-count count)))

(defun trailing-newline-indicator--on-clone-buffer ()
  "Isolate overlay and active-count state when a buffer is cloned."
  (setq trailing-newline-indicator--overlay nil
        trailing-newline-indicator--last-state nil)
  (if (bound-and-true-p trailing-newline-indicator-mode)
      (progn
        (setq trailing-newline-indicator--active (current-buffer))
        (trailing-newline-indicator--setup-hooks)
        (trailing-newline-indicator--update-indicator)
        (trailing-newline-indicator--recount-active))
    (setq trailing-newline-indicator--active nil)
    (trailing-newline-indicator--delete-overlay)
    (trailing-newline-indicator--recount-active)
    (trailing-newline-indicator--cleanup-hooks)))

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
  (unless (memq #'trailing-newline-indicator--on-clone-buffer
                (default-value 'clone-buffer-hook))
    (add-hook 'clone-buffer-hook #'trailing-newline-indicator--on-clone-buffer))
  (unless (memq #'trailing-newline-indicator--on-clone-buffer
                (default-value 'clone-indirect-buffer-hook))
    (add-hook 'clone-indirect-buffer-hook #'trailing-newline-indicator--on-clone-buffer))
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
    (remove-hook 'after-change-major-mode-hook #'trailing-newline-indicator--restore-hooks)
    (remove-hook 'clone-buffer-hook #'trailing-newline-indicator--on-clone-buffer)
    (remove-hook 'clone-indirect-buffer-hook #'trailing-newline-indicator--on-clone-buffer)))

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
When enabled, displays an inline ghost-text symbol (and optionally a
small line number) at the end of the buffer for the visual empty line
created by a trailing newline."
  :lighter " TNLI"
  :group 'trailing-newline-indicator
  (if (or (eq trailing-newline-indicator-mode t)
          (and (numberp trailing-newline-indicator-mode)
               (> trailing-newline-indicator-mode 0)))
      (progn
        (unless (eq trailing-newline-indicator--active (current-buffer))
          (setq trailing-newline-indicator--active (current-buffer))
          (trailing-newline-indicator--increment-count))
        (trailing-newline-indicator--setup-hooks)
        (trailing-newline-indicator--update-indicator))
    (trailing-newline-indicator--delete-overlay)
    (when (eq trailing-newline-indicator--active (current-buffer))
      (setq trailing-newline-indicator--active nil)
      (trailing-newline-indicator--decrement-count))
    (trailing-newline-indicator--cleanup-hooks)))

(defun trailing-newline-indicator--is-buffer-suitable-for-indicator-p ()
  "Return non-nil if the current buffer is suitable for the indicator.
That means the buffer visits a file (`buffer-file-name'), is not a
minibuffer, is not a special mode buffer, and does not have the
indicator already enabled."
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
