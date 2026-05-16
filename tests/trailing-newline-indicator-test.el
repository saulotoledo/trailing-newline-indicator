;;; trailing-newline-indicator-test.el --- ERT tests -*- lexical-binding: t; -*-

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

;; ERT tests for the trailing-newline-indicator package.
;;
;; Run locally with:
;;
;;   make test
;;
;; or directly:
;;
;;   emacs --batch -Q -L . \
;;     -l trailing-newline-indicator.el \
;;     -l tests/trailing-newline-indicator-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'display-line-numbers) ; ensure display-line-numbers is defvar'd as special
(require 'trailing-newline-indicator)

;;; Helpers

(defmacro tnli-test--with-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer pre-filled with CONTENT.
Always disables the mode in the unwind handler, even on test failure."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (unwind-protect
         (progn ,@body)
       (when (bound-and-true-p trailing-newline-indicator-mode)
         (trailing-newline-indicator-mode -1)))))

(defun tnli-test--before-string ()
  "Return the before-string of the active indicator overlay, or nil."
  (and trailing-newline-indicator--overlay
       (overlay-get trailing-newline-indicator--overlay 'before-string)))

;;; Overlay presence

(ert-deftest tnli/overlay-created-with-trailing-newline ()
  "An overlay is created when the buffer ends with a newline."
  (tnli-test--with-buffer "line1\nline2\n"
    (trailing-newline-indicator-mode 1)
    (should (overlayp trailing-newline-indicator--overlay))))

(ert-deftest tnli/no-overlay-without-trailing-newline ()
  "No overlay is created when the buffer does not end with a newline."
  (tnli-test--with-buffer "line1\nno-trailing-newline"
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/no-overlay-in-empty-buffer ()
  "No overlay is created for an empty buffer."
  (tnli-test--with-buffer ""
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/overlay-removed-on-mode-disable ()
  "The overlay is deleted when the mode is turned off."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should trailing-newline-indicator--overlay)
    (trailing-newline-indicator-mode -1)
    (should-not trailing-newline-indicator--overlay)))

;;; Overlay position

(ert-deftest tnli/overlay-spans-point-max ()
  "The overlay is a zero-width overlay anchored at (point-max)."
  (tnli-test--with-buffer "line1\nline2\n"
    (trailing-newline-indicator-mode 1)
    (should (= (overlay-start trailing-newline-indicator--overlay) (point-max)))
    (should (= (overlay-end   trailing-newline-indicator--overlay) (point-max)))))

;;; Ghost-text overlay properties

(ert-deftest tnli/overlay-uses-before-string-not-after-string ()
  "The overlay uses `before-string' (ghost text), never `after-string'."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should     (overlay-get trailing-newline-indicator--overlay 'before-string))
    (should-not (overlay-get trailing-newline-indicator--overlay 'after-string))))

(ert-deftest tnli/before-string-first-char-has-cursor-property ()
  "The first character of before-string has `cursor t' for ghost-text placement."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((str (tnli-test--before-string)))
      (should (and str (> (length str) 0)))
      (should (get-text-property 0 'cursor str)))))

;;; Symbol content

(ert-deftest tnli/before-string-starts-with-default-symbol ()
  "The before-string starts with the default ⏎ symbol."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "⏎")
          (trailing-newline-indicator-show-line-number nil))
      (trailing-newline-indicator-mode 1)
      (should (string-prefix-p "⏎" (substring-no-properties
                                    (tnli-test--before-string)))))))

(ert-deftest tnli/custom-symbol-reflected-in-before-string ()
  "A custom newline symbol is used when changed."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-newline-symbol "↵")
          (trailing-newline-indicator-show-line-number nil))
      (trailing-newline-indicator-mode 1)
      (should (string-prefix-p "↵" (substring-no-properties
                                    (tnli-test--before-string)))))))

(ert-deftest tnli/symbol-carries-line-number-face ()
  "The newline symbol is propertized with the `line-number' face."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-show-line-number nil))
      (trailing-newline-indicator-mode 1)
      (should (eq 'line-number
                  (get-text-property 0 'face (tnli-test--before-string)))))))

;;; Line number content

(ert-deftest tnli/line-number-shown-when-display-line-numbers-active ()
  "A line number is appended when `display-line-numbers' is non-nil."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (should (string-match-p "[[:space:]][[:digit:]]+"
                              (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/no-line-number-when-display-line-numbers-inactive ()
  "No line number is appended when `display-line-numbers' is nil."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (trailing-newline-indicator-newline-symbol "⏎")
          (display-line-numbers nil))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/no-line-number-when-option-disabled ()
  "No line number when `trailing-newline-indicator-show-line-number' is nil.
This happens regardless of `display-line-numbers'."
  (tnli-test--with-buffer "line1\nline2\n"
    (let ((trailing-newline-indicator-show-line-number nil)
          (trailing-newline-indicator-newline-symbol "⏎")
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (should (equal "⏎" (substring-no-properties (tnli-test--before-string)))))))

(ert-deftest tnli/line-number-value-is-one-plus-last-line ()
  "The displayed line number equals one past the last line."
  (tnli-test--with-buffer "a\nb\nc\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let ((expected (1+ (line-number-at-pos (1- (point-max)))))
            (str      (substring-no-properties (tnli-test--before-string))))
        (should (string-match-p (number-to-string expected) str))))))

(ert-deftest tnli/line-number-carries-small-number-face ()
  "The line-number portion uses `trailing-newline-indicator-small-number' face."
  (tnli-test--with-buffer "line1\n"
    (let ((trailing-newline-indicator-show-line-number t)
          (display-line-numbers t))
      (trailing-newline-indicator-mode 1)
      (let* ((str     (tnli-test--before-string))
             ;; The symbol occupies positions 0..(sym-len - 1); line number starts at sym-len.
             (sym-len (length trailing-newline-indicator-newline-symbol)))
        (should (eq 'trailing-newline-indicator-small-number
                    (get-text-property sym-len 'face str)))))))

;;; Hooks

(ert-deftest tnli/hooks-installed-on-enable ()
  "All expected buffer-local hooks are added when the mode is enabled."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should (memq #'trailing-newline-indicator--update-indicator after-change-functions))
    (should (memq #'trailing-newline-indicator--update-indicator after-save-hook))
    (should (memq #'trailing-newline-indicator--update-indicator after-revert-hook))
    (should (memq #'trailing-newline-indicator--update-indicator post-command-hook))
    (should (memq #'trailing-newline-indicator--on-kill-buffer   kill-buffer-hook))))

(ert-deftest tnli/hooks-removed-on-disable ()
  "All buffer-local hooks are removed when the mode is disabled."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (trailing-newline-indicator-mode -1)
    (should-not (memq #'trailing-newline-indicator--update-indicator after-change-functions))
    (should-not (memq #'trailing-newline-indicator--update-indicator after-save-hook))
    (should-not (memq #'trailing-newline-indicator--update-indicator after-revert-hook))
    (should-not (memq #'trailing-newline-indicator--update-indicator post-command-hook))
    (should-not (memq #'trailing-newline-indicator--on-kill-buffer   kill-buffer-hook))))

(ert-deftest tnli/hook-list-covers-all-expected-hooks ()
  "The hook list returned by `trailing-newline-indicator--hook-list' is complete."
  (let ((hooks (trailing-newline-indicator--hook-list)))
    (should (memq 'after-change-functions hooks))
    (should (memq 'after-save-hook        hooks))
    (should (memq 'after-revert-hook      hooks))
    (should (memq 'post-command-hook      hooks))))

;;; Active count

(ert-deftest tnli/active-count-increments-on-enable ()
  "The global active-count increases by one when a buffer enables the mode."
  (tnli-test--with-buffer "line1\n"
    (let ((before trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode 1)
      (should (= (1+ before) trailing-newline-indicator--active-count)))))

(ert-deftest tnli/active-count-decrements-on-disable ()
  "The global active-count decreases by one when a buffer disables the mode."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((after-enable trailing-newline-indicator--active-count))
      (trailing-newline-indicator-mode -1)
      (should (= (1- after-enable) trailing-newline-indicator--active-count)))))

;;; Overlay management helpers

(ert-deftest tnli/delete-overlay-nils-variable ()
  "`trailing-newline-indicator--delete-overlay' sets the overlay variable to nil."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should trailing-newline-indicator--overlay)
    (trailing-newline-indicator--delete-overlay)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/enable-is-idempotent ()
  "Enabling an already-enabled mode does not create a second overlay.
It also does not increment the active-count."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (let ((count-1st trailing-newline-indicator--active-count)
          (ov-1st    trailing-newline-indicator--overlay))
      (trailing-newline-indicator-mode 1)
      (should (=  count-1st trailing-newline-indicator--active-count))
      (should (eq ov-1st    trailing-newline-indicator--overlay)))))

;;; Live overlay updates

(ert-deftest tnli/overlay-cleared-after-trailing-newline-removed ()
  "The overlay disappears when the trailing newline is deleted from the buffer."
  (tnli-test--with-buffer "line1\n"
    (trailing-newline-indicator-mode 1)
    (should trailing-newline-indicator--overlay)
    (goto-char (point-max))
    (delete-char -1)
    (trailing-newline-indicator--update-indicator)
    (should-not trailing-newline-indicator--overlay)))

(ert-deftest tnli/overlay-appears-after-trailing-newline-inserted ()
  "The overlay is created when a trailing newline is added to the buffer."
  (tnli-test--with-buffer "no-trailing-newline"
    (trailing-newline-indicator-mode 1)
    (should-not trailing-newline-indicator--overlay)
    (goto-char (point-max))
    (insert "\n")
    (trailing-newline-indicator--update-indicator)
    (should (overlayp trailing-newline-indicator--overlay))))

;;; Buffer suitability

(ert-deftest tnli/buffer-without-file-is-not-suitable ()
  "A buffer with no associated file is excluded from the global mode."
  (with-temp-buffer
    (should-not (trailing-newline-indicator--is-buffer-suitable-for-indicator-p))))

(ert-deftest tnli/buffer-with-mode-already-on-is-not-suitable ()
  "A buffer where the mode is already active is not re-enabled by the global mode."
  (tnli-test--with-buffer "line1\n"
    (setq-local trailing-newline-indicator-mode t)
    (should-not (trailing-newline-indicator--is-buffer-suitable-for-indicator-p))))

(provide 'trailing-newline-indicator-test)
;;; trailing-newline-indicator-test.el ends here
