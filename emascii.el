;;; emascii.el --- Free-form ASCII drawing canvas -*- lexical-binding: t; -*-

;; Author: Rayan Raddatz
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: wp, convenience

;;; Commentary:

;; `emascii-mode' turns a buffer into an unbounded character grid:
;;
;; - The cursor moves freely; spaces and newlines are created on demand.
;; - Typing overwrites the cell under the cursor and never shifts text.
;; - Selections are sets of cells, built from shapes (rectangle,
;;   ellipse, lasso) dragged with mouse or keyboard, plus single cells.
;; - Cut/copied/pasted pieces float over the canvas, move with arrows or
;;   mouse, and are dropped with RET.  Spaces of a floating piece are
;;   transparent.
;; - Copying for export crops the drawing, pads every line to the same
;;   width and, on macOS, also puts HTML/RTF with a monospace font on
;;   the clipboard so word processors keep the alignment.
;;
;; The gallery of reusable drawings lives in emascii-gallery.el.
;;
;; Assumption: every character is one column wide (no tabs, no wide
;; CJK or emoji characters).  Tabs are converted when the mode starts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emascii nil
  "Free-form ASCII drawing canvas."
  :group 'wp
  :prefix "emascii-")

(defcustom emascii-start-in-overwrite t
  "When non-nil, canvases start in overwrite mode.
In overwrite mode typing replaces the cell under the cursor and
backspace leaves a blank.  In insert mode typing pushes the rest of
the line right and backspace pulls it left.  Toggle with
`emascii-toggle-overwrite'."
  :type 'boolean)

(defcustom emascii-big-step 8
  "Cells moved by shifted arrows while a piece is floating."
  :type 'integer)

(defcustom emascii-rich-clipboard t
  "When non-nil, also put HTML and RTF on the macOS clipboard on export.
The rich versions use `emascii-export-font' so that word processors
keep the drawing aligned."
  :type 'boolean)

(defcustom emascii-export-font "Menlo"
  "Monospace font name used in the rich clipboard versions."
  :type 'string)

(defcustom emascii-export-font-size 10
  "Font size in points used in the rich clipboard versions."
  :type 'integer)

(defface emascii-selection
  '((t :inherit region))
  "Face for selected cells.")

(defface emascii-float
  '((((background dark)) :background "#5a4a00" :foreground "#ffe680")
    (t :background "#fff0a0" :foreground "#5a4a00"))
  "Face for the cells of a floating piece.")

;;;; Buffer-local state

(defvar-local emascii-shape 'rectangle
  "Shape used by drag selection: `rectangle', `ellipse' or `lasso'.")

(defvar-local emascii--cells nil
  "Hash table of committed selected cells, keys are (ROW . COL).")

(defvar-local emascii--shape-p nil
  "Non-nil when the active region defines a live selection shape.")

(defvar-local emascii--path nil
  "Lasso path, most recent cell first.")

(defvar-local emascii--overlays nil)
(defvar-local emascii--pad-undo nil
  "Value of `buffer-undo-list' right after the last padding change.")
(defvar-local emascii--type-col nil
  "Column where the current typing run started.")
(defvar-local emascii--last-row nil)
(defvar-local emascii--float nil
  "Current floating piece, an `emascii--fl' struct, or nil.")
(defvar-local emascii-overwrite t
  "Non-nil when typing overwrites cells instead of pushing text.")
(defvar-local emascii--override nil
  "Non-nil in `emascii-mode' buffers; enables `emascii-override-map'.")

(defvar emascii--internal-mark nil
  "Non-nil while emascii itself activates the mark.")

(defvar emascii-clipboard nil
  "Lines of the last piece copied or cut with emascii.")

(cl-defstruct (emascii--fl (:constructor emascii--fl-make))
  lines row col saved group opaque)

;;;; Grid primitives

(defun emascii--row ()
  "Return the 0-based row of point."
  (1- (line-number-at-pos nil t)))

(defun emascii--cell ()
  "Return the (ROW . COL) cell of point."
  (cons (emascii--row) (current-column)))

(defun emascii--last-row ()
  "Return the row of the line that contains `point-max'."
  (save-excursion (goto-char (point-max)) (emascii--row)))

(defun emascii--merge-pad-undo ()
  "Drop undo boundaries added since the last padding change.
This keeps cursor-motion padding out of the undo history: it gets
merged into the next change instead of being a step of its own."
  (when (and emascii--pad-undo (not emascii--float)
             (consp buffer-undo-list))
    (let ((l buffer-undo-list))
      (while (and (consp l) (null (car l)))
        (setq l (cdr l)))
      (when (eq l emascii--pad-undo)
        (setq buffer-undo-list l))))
  (setq emascii--pad-undo nil))

(defun emascii--pad-op (fn)
  "Call FN, which only adds or removes blank space."
  (let ((deactivate-mark nil)
        (tick (buffer-chars-modified-tick)))
    (emascii--merge-pad-undo)
    (funcall fn)
    (setq emascii--pad-undo
          (if (= tick (buffer-chars-modified-tick)) nil buffer-undo-list))))

(defun emascii--goto-cell (row col &optional nopad)
  "Move point to ROW, COL.
Create the missing lines and spaces unless NOPAD is non-nil.
Return non-nil when point ends exactly on the cell."
  (goto-char (point-min))
  (let* ((short (forward-line row))
         (need (+ short (if (bolp) 0 1))))
    (when (> need 0)
      (unless nopad
        (emascii--pad-op
         (lambda ()
           (goto-char (point-max))
           (insert (make-string need ?\n))))))
    (if (and (> need 0) nopad)
        nil
      (if (>= (move-to-column col) col)
          (= (current-column) col)
        (unless nopad
          (emascii--pad-op (lambda () (move-to-column col t)))
          t)))))

(defun emascii--line-string (row)
  "Return the text of ROW without properties, or \"\" past the end."
  (save-excursion
    (if (or (> row (emascii--last-row))
            (progn (goto-char (point-min)) (/= 0 (forward-line row))))
        ""
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun emascii--char-at (row col)
  "Return the character at ROW, COL; a space for empty cells."
  (let ((s (emascii--line-string row)))
    (if (< col (length s)) (aref s col) ?\s)))

(defun emascii--put (row col char)
  "Write CHAR at ROW, COL.  Point ends right after the cell."
  (emascii--goto-cell row col)
  (let ((deactivate-mark nil))
    (emascii--merge-pad-undo)
    (if (eq (char-after) char)
        (forward-char 1)
      (unless (eolp) (delete-char 1))
      (insert char))))

(defun emascii--trim-row (row)
  "Remove trailing spaces of ROW and blank lines at the end of buffer.
Blank lines at or above the cursor row are kept."
  (save-excursion
    (emascii--pad-op
     (lambda ()
       (when (and (<= row (emascii--last-row))
                  (progn (goto-char (point-min)) (= 0 (forward-line row))))
         (goto-char (line-end-position))
         (let ((end (point)))
           (skip-chars-backward " " (line-beginning-position))
           (when (< (point) end)
             (delete-region (point) end))))))))

(defun emascii--trim-eob (keep-row)
  "Delete blank lines at the end of buffer below KEEP-ROW."
  (save-excursion
    (emascii--pad-op
     (lambda ()
       (goto-char (point-min))
       (when (and (= 0 (forward-line (1+ keep-row)))
                  (bolp)
                  (> (point) (point-min))
                  (string-match-p "\\`[ \n]*\\'"
                                  (buffer-substring-no-properties
                                   (point) (point-max))))
         (delete-region (1- (point)) (point-max)))))))

;;;; Text helpers

(defun emascii--crop (lines)
  "Crop LINES to the smallest box holding non-space characters.
Return (DROW DCOL CROPPED), where DROW and DCOL are the number of
rows and columns removed from the top and the left."
  (let* ((lines (mapcar #'string-trim-right lines))
         (top 0) (n (length lines)))
    (while (and (< top n) (string-empty-p (nth top lines)))
      (cl-incf top))
    (if (= top n)
        (list 0 0 nil)
      (let* ((body (nthcdr top lines))
             (body (reverse (seq-drop-while #'string-empty-p (reverse body))))
             (left (apply #'min
                          (mapcar (lambda (l)
                                    (if (string-empty-p l) most-positive-fixnum
                                      (- (length l)
                                         (length (string-trim-left l " +")))))
                                  body))))
        (list top left
              (mapcar (lambda (l) (if (> (length l) left) (substring l left) ""))
                      body))))))

(defun emascii--pad-lines (lines)
  "Return LINES padded with spaces to the same width."
  (let ((w (apply #'max 0 (mapcar #'length lines))))
    (mapcar (lambda (l) (concat l (make-string (- w (length l)) ?\s))) lines)))

(defun emascii--normalize-text (text)
  "Split TEXT into lines with tabs expanded and CRs removed."
  (with-temp-buffer
    (insert (replace-regexp-in-string "\r" "" text))
    (untabify (point-min) (point-max))
    (split-string (buffer-string) "\n")))

(defun emascii-export-string (lines)
  "Return LINES as text ready to paste anywhere: cropped and padded."
  (string-join (emascii--pad-lines (nth 2 (emascii--crop lines))) "\n"))

;;;; Shapes

(defun emascii--rect-cells (a b)
  "Cells of the rectangle with corners A and B."
  (let (cells)
    (cl-loop for r from (min (car a) (car b)) to (max (car a) (car b)) do
             (cl-loop for c from (min (cdr a) (cdr b)) to (max (cdr a) (cdr b))
                      do (push (cons r c) cells)))
    cells))

(defun emascii--ellipse-cells (a b)
  "Cells of the ellipse inscribed in the rectangle with corners A and B."
  (let* ((r0 (min (car a) (car b))) (r1 (max (car a) (car b)))
         (c0 (min (cdr a) (cdr b))) (c1 (max (cdr a) (cdr b)))
         (cy (/ (+ r0 r1) 2.0)) (cx (/ (+ c0 c1) 2.0))
         (ry (+ 0.5 (/ (- r1 r0) 2.0))) (rx (+ 0.5 (/ (- c1 c0) 2.0)))
         cells)
    (cl-loop for r from r0 to r1 do
             (cl-loop for c from c0 to c1
                      when (<= (+ (expt (/ (- c cx) rx) 2)
                                  (expt (/ (- r cy) ry) 2))
                               1.0)
                      do (push (cons r c) cells)))
    cells))

(defun emascii--line-cells (a b)
  "Cells on the straight line from A to B (Bresenham)."
  (let* ((r (car a)) (c (cdr a)) (r1 (car b)) (c1 (cdr b))
         (dr (abs (- r1 r))) (dc (abs (- c1 c)))
         (sr (if (< r r1) 1 -1)) (sc (if (< c c1) 1 -1))
         (err (- dc dr)) cells)
    (while (progn (push (cons r c) cells)
                  (not (and (= r r1) (= c c1))))
      (let ((e2 (* 2 err)))
        (when (> e2 (- dr)) (cl-decf err dr) (cl-incf c sc))
        (when (< e2 dc) (cl-incf err dc) (cl-incf r sr))))
    cells))

(defun emascii--inside-polygon-p (r c poly)
  "Non-nil if the point R, C lies inside polygon POLY (list of cells)."
  (let ((y (+ r 0.001)) (x (+ c 0.001)) inside
        (prev (car (last poly))))
    (dolist (v poly)
      (let ((yi (float (car v))) (xi (float (cdr v)))
            (yj (float (car prev))) (xj (float (cdr prev))))
        (when (and (not (eq (> yi y) (> yj y)))
                   (< x (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi)))))
          (setq inside (not inside))))
      (setq prev v))
    inside))

(defun emascii--lasso-cells (path)
  "Cells enclosed by the closed free-hand PATH (list of cells)."
  (when path
    (let ((table (make-hash-table :test #'equal))
          (closed (append path (list (car path)))))
      (cl-loop for (a b) on closed while b do
               (dolist (cell (emascii--line-cells a b))
                 (puthash cell t table)))
      (when (cddr path)
        (let ((r0 (apply #'min (mapcar #'car path)))
              (r1 (apply #'max (mapcar #'car path)))
              (c0 (apply #'min (mapcar #'cdr path)))
              (c1 (apply #'max (mapcar #'cdr path))))
          (cl-loop for r from r0 to r1 do
                   (cl-loop for c from c0 to c1
                            when (emascii--inside-polygon-p r c path)
                            do (puthash (cons r c) t table)))))
      (hash-table-keys table))))

;;;; Selection

(defun emascii--mark-cell ()
  (save-excursion (goto-char (mark t)) (emascii--cell)))

(defun emascii--live-cells ()
  "Cells of the shape between mark and point, if one is live."
  (when (and emascii--shape-p (region-active-p))
    (let ((a (emascii--mark-cell)) (b (emascii--cell)))
      (pcase emascii-shape
        ('ellipse (emascii--ellipse-cells a b))
        ('lasso (emascii--lasso-cells (reverse (if (equal b (car emascii--path))
                                                   emascii--path
                                                 (cons b emascii--path)))))
        (_ (emascii--rect-cells a b))))))

(defun emascii--selection ()
  "Return the selected cells as a list."
  (let ((table (copy-hash-table emascii--cells)))
    (dolist (c (emascii--live-cells)) (puthash c t table))
    (hash-table-keys table)))

(defun emascii--selection-p ()
  (or (> (hash-table-count emascii--cells) 0)
      (and emascii--shape-p (region-active-p))))

(defun emascii--commit-live ()
  "Turn the live shape into committed cells."
  (dolist (c (emascii--live-cells)) (puthash c t emascii--cells))
  (setq emascii--shape-p nil emascii--path nil))

(defun emascii--start-shape (cell)
  "Start a live shape anchored at CELL, keeping committed cells."
  (emascii--commit-live)
  (save-excursion
    (emascii--goto-cell (car cell) (cdr cell))
    (let ((emascii--internal-mark t)) (set-mark (point))))
  (setq emascii--shape-p t emascii--path (list cell)))

(defun emascii-clear-selection ()
  "Forget the whole selection."
  (interactive)
  (clrhash emascii--cells)
  (setq emascii--shape-p nil emascii--path nil)
  (let ((emascii--internal-mark t)) (deactivate-mark))
  (emascii--refresh))

(defun emascii--on-activate-mark ()
  (unless emascii--internal-mark
    (setq emascii--shape-p t
          emascii--path (list (emascii--mark-cell)))))

(defun emascii--on-deactivate-mark ()
  (unless emascii--internal-mark
    (setq emascii--shape-p nil emascii--path nil)))

(defun emascii--selection-lines ()
  "Return (ROW COL LINES) for the selection, cropped, or nil."
  (let ((cells (emascii--selection)))
    (when cells
      (let* ((r0 (apply #'min (mapcar #'car cells)))
             (r1 (apply #'max (mapcar #'car cells)))
             (c0 (apply #'min (mapcar #'cdr cells)))
             (c1 (apply #'max (mapcar #'cdr cells)))
             (table (make-hash-table :test #'equal))
             lines)
        (dolist (c cells) (puthash c t table))
        (cl-loop for r from r0 to r1 do
                 (let ((src (emascii--line-string r))
                       (dst (make-string (1+ (- c1 c0)) ?\s)))
                   (cl-loop for c from c0 to c1
                            when (and (gethash (cons r c) table)
                                      (< c (length src)))
                            do (aset dst (- c c0) (aref src c)))
                   (push dst lines)))
        (pcase-let ((`(,dr ,dc ,cropped) (emascii--crop (nreverse lines))))
          (when cropped
            (list (+ r0 dr) (+ c0 dc) cropped)))))))

(defun emascii--blank-cells (cells)
  "Replace every cell in CELLS with a space."
  (save-excursion
    (dolist (c cells)
      (when (< (cdr c) (length (emascii--line-string (car c))))
        (emascii--put (car c) (cdr c) ?\s)))))

;;;; Rendering

(defun emascii--clear-overlays ()
  (mapc #'delete-overlay emascii--overlays)
  (setq emascii--overlays nil))

(defun emascii--render (cells face)
  "Highlight CELLS with FACE, including cells past line ends."
  (let ((rows (make-hash-table)))
    (dolist (c cells) (push (cdr c) (gethash (car c) rows)))
    (let* ((last-row (emascii--last-row))
           (virtual last-row)
           (eob-string "")
           (sorted (sort (hash-table-keys rows) #'<)))
      (save-excursion
        (dolist (r sorted)
          (let* ((cols (sort (gethash r rows) #'<))
                 (exists (<= r last-row))
                 (bol (and exists
                           (progn (goto-char (point-min)) (forward-line r)
                                  (point))))
                 (len (if exists (- (line-end-position) bol) 0))
                 (tail ""))
            (dolist (c cols)
              (if (< c len)
                  (let ((ov (make-overlay (+ bol c) (+ bol c 1))))
                    (overlay-put ov 'face face)
                    (overlay-put ov 'priority 100)
                    (overlay-put ov 'emascii t)
                    (push ov emascii--overlays))
                (setq tail (concat tail
                                   (make-string (- c len (length tail)) ?\s)
                                   (propertize " " 'face face)))))
            (cond
             ((not exists)
              (setq eob-string (concat eob-string
                                       (make-string (- r virtual) ?\n)
                                       tail))
              (setq virtual r))
             ((= r last-row)
              (setq eob-string (concat tail eob-string)))
             ((not (string-empty-p tail))
              (let ((ov (make-overlay (+ bol len) (+ bol len))))
                (overlay-put ov 'after-string tail)
                (overlay-put ov 'emascii t)
                (push ov emascii--overlays)))))))
      (unless (string-empty-p eob-string)
        (let ((ov (make-overlay (point-max) (point-max))))
          (overlay-put ov 'after-string eob-string)
          (overlay-put ov 'emascii t)
          (push ov emascii--overlays))))))

(defun emascii--refresh ()
  "Redraw selection or floating piece highlights."
  (emascii--clear-overlays)
  (cond
   (emascii--float
    (emascii--render (mapcar #'car (emascii--fl-saved emascii--float))
                     'emascii-float))
   ((emascii--selection-p)
    (emascii--render (emascii--selection) 'emascii-selection))))

;;;; Command hooks

(defun emascii--post-command ()
  (unless emascii--float
    (when (and emascii--shape-p (region-active-p) (eq emascii-shape 'lasso))
      (let ((c (emascii--cell)))
        (unless (equal c (car emascii--path)) (push c emascii--path))))
    ;; Keep the mark active while only committed cells exist, so that
    ;; cua-mode and friends treat C-c / C-x as copy / cut.
    (when (and (not (region-active-p))
               (> (hash-table-count emascii--cells) 0))
      (let ((emascii--internal-mark t)) (set-mark (point)))
      (setq emascii--shape-p nil))
    (let ((row (emascii--row)))
      (when (and emascii--last-row (/= row emascii--last-row)
                 (not (region-active-p)))
        (emascii--trim-row emascii--last-row)
        (emascii--trim-eob row))
      (setq emascii--last-row row)))
  (emascii--refresh))

(defconst emascii--float-commands
  '(emascii-float-left emascii-float-right emascii-float-up emascii-float-down
    emascii-float-left-big emascii-float-right-big emascii-float-up-big
    emascii-float-down-big emascii-float-mouse-drag emascii-float-commit
    emascii-float-cancel emascii-float-toggle-opaque emascii-quit
    mwheel-scroll pixel-scroll-precision scroll-up-command scroll-down-command
    recenter-top-bottom handle-switch-frame ignore nil)
  "Commands that keep a floating piece floating.")

(defun emascii--pre-command ()
  (when (and emascii--float (not (memq this-command emascii--float-commands)))
    (emascii-float-commit)))

;;;; Motion and typing

(defun emascii--move-to (row col)
  (emascii--goto-cell (max 0 row) (max 0 col)))

(defun emascii-forward (&optional n)
  "Move N cells right, creating space as needed."
  (interactive "^p")
  (let ((c (emascii--cell))) (emascii--move-to (car c) (+ (cdr c) (or n 1)))))

(defun emascii-backward (&optional n)
  "Move N cells left."
  (interactive "^p")
  (let ((c (emascii--cell))) (emascii--move-to (car c) (- (cdr c) (or n 1)))))

(defun emascii-down (&optional n)
  "Move N cells down, keeping the column."
  (interactive "^p")
  (let ((c (emascii--cell))) (emascii--move-to (+ (car c) (or n 1)) (cdr c))))

(defun emascii-up (&optional n)
  "Move N cells up, keeping the column."
  (interactive "^p")
  (let ((c (emascii--cell))) (emascii--move-to (- (car c) (or n 1)) (cdr c))))

(defun emascii-tab ()
  "Move right to the next tab stop."
  (interactive "^")
  (let ((c (emascii--cell)))
    (emascii--move-to (car c) (* tab-width (1+ (/ (cdr c) tab-width))))))

(defun emascii-newline ()
  "Move to the next row, under the column where typing started."
  (interactive)
  (let ((c (emascii--cell)))
    (emascii--move-to (1+ (car c)) (or emascii--type-col (cdr c)))))

(defun emascii-self-insert (n)
  "Type the character N times, overwriting or pushing text.
See `emascii-overwrite'."
  (interactive "p")
  (when (emascii--selection-p) (emascii-clear-selection))
  (unless (memq last-command '(emascii-self-insert emascii-newline
                               emascii-delete-backward))
    (setq emascii--type-col (current-column)))
  (undo-auto-amalgamate)
  (let ((ch last-command-event))
    (when (characterp ch)
      (dotimes (_ n)
        (let ((c (emascii--cell)))
          (if emascii-overwrite
              (emascii--put (car c) (cdr c) ch)
            (emascii--goto-cell (car c) (cdr c))
            (let ((deactivate-mark nil))
              (emascii--merge-pad-undo)
              (insert ch))))))))

(defun emascii-delete-backward (n)
  "Delete the selection, or N cells left of the cursor.
In overwrite mode the cells become blank; in insert mode the rest of
the line is pulled left."
  (interactive "p")
  (cond
   ((emascii--selection-p) (emascii-clear))
   ((not emascii-overwrite) (emascii-delete-backward-shift n))
   (t (emascii--blank-backward n))))

(defun emascii--blank-backward (n)
  (dotimes (_ n)
      (let ((c (emascii--cell)))
        (when (> (cdr c) 0)
          (emascii--put (car c) (1- (cdr c)) ?\s)
          (emascii--goto-cell (car c) (1- (cdr c)))))))

(defun emascii--blank-forward (n)
  (let ((c (emascii--cell)))
    (save-excursion
      (dotimes (i n) (emascii--put (car c) (+ (cdr c) i) ?\s)))))

(defun emascii-delete-forward (n)
  "Delete the selection, or N cells at the cursor.
In overwrite mode the cells become blank; in insert mode the rest of
the line is pulled left."
  (interactive "p")
  (cond
   ((emascii--selection-p) (emascii-clear))
   ((not emascii-overwrite) (emascii-delete-forward-shift n))
   (t (emascii--blank-forward n))))

(defun emascii-delete-backward-other (n)
  "Delete N cells left of the cursor the way the other typing mode does."
  (interactive "p")
  (if emascii-overwrite (emascii-delete-backward-shift n) (emascii--blank-backward n)))

(defun emascii-delete-forward-other (n)
  "Delete N cells at the cursor the way the other typing mode does."
  (interactive "p")
  (if emascii-overwrite (emascii-delete-forward-shift n) (emascii--blank-forward n)))

(defun emascii-toggle-overwrite ()
  "Toggle between overwrite and insert typing."
  (interactive)
  (setq emascii-overwrite (not emascii-overwrite))
  (force-mode-line-update)
  (message "Typing: %s" (if emascii-overwrite "overwrite" "insert (pushes text)")))

(defun emascii-delete-backward-shift (n)
  "Delete N characters left of the cursor and pull the rest of the line left."
  (interactive "p")
  (let ((deactivate-mark nil)
        (k (min n (- (point) (line-beginning-position)))))
    (when (> k 0)
      (emascii--merge-pad-undo)
      (delete-char (- k)))))

(defun emascii-delete-forward-shift (n)
  "Delete N characters at the cursor and pull the rest of the line left."
  (interactive "p")
  (let ((deactivate-mark nil)
        (k (min n (- (line-end-position) (point)))))
    (when (> k 0)
      (emascii--merge-pad-undo)
      (delete-char k))))

(defun emascii-kill-line ()
  "Blank from the cursor to the end of the line, without joining lines."
  (interactive)
  (let ((deactivate-mark nil))
    (delete-region (point) (line-end-position))))

;;;; Mouse

(defun emascii--event-cell (event)
  "Return the (ROW . COL) cell under mouse EVENT, also past line ends."
  (let* ((posn (event-end event))
         (win (posn-window posn))
         (pos (posn-point posn)))
    (when (and (windowp win) (eq (window-buffer win) (current-buffer))
               (integerp pos) (null (posn-area posn)))
      (save-excursion
        (goto-char pos)
        (let ((row (emascii--row)) (col (current-column)))
          (when (eolp)
            (let ((here (posn-at-point (point) win))
                  (xy (posn-x-y posn)))
              (when (and here xy)
                (let ((cw (window-font-width win))
                      (lh (with-selected-window win (default-line-height)))
                      (ex (car (posn-x-y here)))
                      (ey (cdr (posn-x-y here))))
                  (setq col (+ col (max 0 (floor (- (car xy) ex) cw))))
                  (when (eobp)
                    (setq row (+ row (max 0 (floor (- (cdr xy) ey) lh)))))))))
          (cons row col))))))

(defun emascii--track-drag (start on-move)
  "Track the mouse from cell START, calling ON-MOVE with each new cell.
Return non-nil if the mouse moved to another cell."
  (let ((mouse-fine-grained-tracking t)
        (last start) moved ev)
    (track-mouse
      (while (progn (setq ev (read-event))
                    (mouse-movement-p ev))
        (let ((cell (emascii--event-cell ev)))
          (when (and cell (not (equal cell last)))
            (setq last cell moved t)
            (funcall on-move cell)
            (emascii--refresh)
            (redisplay)))))
    (unless (and (consp ev) (memq 'click (event-modifiers ev)))
      (unless (and (consp ev) (memq 'drag (event-modifiers ev)))
        (push ev unread-command-events)))
    moved))

(defun emascii--mouse-select (event add)
  (let ((win (posn-window (event-start event))))
    (when (windowp win) (select-window win)))
  (let ((start (emascii--event-cell event)))
    (when start
      (unless add (emascii-clear-selection))
      (emascii--goto-cell (car start) (cdr start))
      (let ((started nil))
        (emascii--track-drag
         start
         (lambda (cell)
           (unless started
             (setq started t)
             (emascii--start-shape start))
           (emascii--goto-cell (car cell) (cdr cell))
           (unless (equal cell (car emascii--path))
             (push cell emascii--path)))))
      (emascii--refresh))))

(defun emascii-mouse-down (event)
  "Click to place the cursor; drag to select with the current shape."
  (interactive "e")
  (emascii--mouse-select event nil))

(defun emascii-mouse-add (event)
  "Drag to add a shape to the selection."
  (interactive "e")
  (emascii--mouse-select event t))

(defun emascii-mouse-toggle-cell (event)
  "Toggle the cell under the mouse in the selection."
  (interactive "e")
  (let ((cell (emascii--event-cell event)))
    (when cell
      (emascii--goto-cell (car cell) (cdr cell))
      (emascii-toggle-cell))))

;;;; Selection commands

(defun emascii-toggle-cell ()
  "Add or remove the cell at point from the selection."
  (interactive)
  (emascii--commit-live)
  (let ((c (emascii--cell)))
    (if (gethash c emascii--cells)
        (remhash c emascii--cells)
      (puthash c t emascii--cells)))
  (if (> (hash-table-count emascii--cells) 0)
      (let ((emascii--internal-mark t)) (set-mark (point)))
    (let ((emascii--internal-mark t)) (deactivate-mark)))
  (setq emascii--shape-p nil)
  (emascii--refresh))

(defun emascii-set-anchor ()
  "Start a shape at point, or fix the current shape into the selection."
  (interactive)
  (if (and emascii--shape-p (region-active-p))
      (progn
        (emascii--commit-live)
        (let ((emascii--internal-mark t)) (set-mark (point)))
        (message "Shape added; C-SPC starts another one"))
    (emascii--start-shape (emascii--cell))
    (message "Shape %s started; move to resize" emascii-shape))
  (emascii--refresh))

(defun emascii-cycle-shape ()
  "Cycle the selection shape between rectangle, ellipse and lasso."
  (interactive)
  (setq emascii-shape (pcase emascii-shape
                        ('rectangle 'ellipse)
                        ('ellipse 'lasso)
                        (_ 'rectangle)))
  (when (and emascii--shape-p (region-active-p) (eq emascii-shape 'lasso))
    (setq emascii--path (list (emascii--mark-cell))))
  (force-mode-line-update)
  (message "Selection shape: %s" emascii-shape))

(defun emascii-clear ()
  "Blank the selected cells."
  (interactive)
  (let ((cells (emascii--selection)))
    (if (null cells)
        (message "Nothing selected")
      (emascii-clear-selection)
      (emascii--blank-cells cells))))

(defun emascii-quit ()
  "Cancel the floating piece, or clear the selection, or quit."
  (interactive)
  (cond (emascii--float (emascii-float-cancel))
        ((emascii--selection-p) (emascii-clear-selection))
        (t (keyboard-quit))))

;;;; Clipboard and export

(defun emascii--html-escape (s)
  (replace-regexp-in-string
   "[&<>\"]" (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;")
                           (_ "&quot;")))
   s t t))

(defun emascii--rtf (text)
  "Return TEXT as an RTF document using `emascii-export-font'."
  (concat
   "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0\\fmodern " emascii-export-font ";}}"
   (format "\\f0\\fs%d " (* 2 emascii-export-font-size))
   (mapconcat
    (lambda (ch)
      (cond ((memq ch '(?\\ ?{ ?})) (string ?\\ ch))
            ((eq ch ?\n) "\\line\n")
            ((< ch 128) (string ch))
            (t (format "\\u%d?" (if (> ch 32767) (- ch 65536) ch)))))
    text "")
   "}"))

(defconst emascii--pasteboard-script
  "ObjC.import('AppKit');
function run(argv) {
  function rd(p) {
    return $.NSString.stringWithContentsOfFileEncodingError(p, $.NSUTF8StringEncoding, null);
  }
  var pb = $.NSPasteboard.generalPasteboard;
  pb.clearContents;
  pb.setStringForType(rd(argv[0]), 'public.utf8-plain-text');
  pb.setStringForType(rd(argv[1]), 'public.html');
  pb.setStringForType(rd(argv[2]), 'public.rtf');
  return 'ok';
}"
  "JXA script that puts plain text, HTML and RTF on the clipboard.")

(defun emascii--set-rich-clipboard (text)
  "Put TEXT on the macOS clipboard as plain text, HTML and RTF.
Return non-nil on success."
  (when (and emascii-rich-clipboard (eq system-type 'darwin)
             (executable-find "osascript"))
    (let ((files (list (make-temp-file "emascii" nil ".txt")
                       (make-temp-file "emascii" nil ".html")
                       (make-temp-file "emascii" nil ".rtf")))
          (html (format (concat "<html><head><meta charset=\"utf-8\"></head><body>"
                                "<pre style=\"font-family: '%s', 'Courier New', "
                                "monospace; font-size: %dpt;\">%s</pre>"
                                "</body></html>")
                        emascii-export-font emascii-export-font-size
                        (emascii--html-escape text))))
      (unwind-protect
          (let ((coding-system-for-write 'utf-8-unix))
            (cl-mapc (lambda (f s) (write-region s nil f nil 'silent))
                     files (list text html (emascii--rtf text)))
            (with-temp-buffer
              (and (eq 0 (apply #'call-process "osascript" nil t nil
                                "-l" "JavaScript" "-e" emascii--pasteboard-script
                                files))
                   (string-match-p "ok" (buffer-string)))))
        (mapc #'delete-file files)))))

(defun emascii--canvas-lines ()
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(defun emascii--export (lines)
  "Copy LINES to the kill ring and clipboard, ready to paste anywhere."
  (let ((text (emascii-export-string lines)))
    (kill-new text)
    (let ((rich (emascii--set-rich-clipboard text)))
      (message "Copied %d lines%s" (length (split-string text "\n"))
               (if rich " (plain + rich text)" "")))
    text))

(defun emascii-copy ()
  "Copy the selection, or the whole canvas, ready to paste anywhere."
  (interactive)
  (let ((sel (emascii--selection-lines)))
    (if sel
        (progn (setq emascii-clipboard (nth 2 sel))
               (emascii--export (nth 2 sel))
               (emascii-clear-selection))
      (emascii--export (emascii--canvas-lines)))))

(defun emascii-cut ()
  "Copy the selection and blank its cells."
  (interactive)
  (let ((cells (emascii--selection))
        (sel (emascii--selection-lines)))
    (if (not sel)
        (message "Nothing selected")
      (setq emascii-clipboard (nth 2 sel))
      (emascii--export (nth 2 sel))
      (emascii-clear-selection)
      (emascii--blank-cells cells))))

(defun emascii-paste ()
  "Float the last copied text at point; drop it with RET."
  (interactive)
  (let ((text (ignore-errors (current-kill 0))))
    (if (or (null text) (string-blank-p text))
        (message "Clipboard is empty")
      (let ((lines (nth 2 (emascii--crop (emascii--normalize-text text)))))
        (when (emascii--selection-p) (emascii-clear-selection))
        (let ((c (emascii--cell)))
          (emascii-float-start lines (car c) (cdr c)))))))

(defun emascii-move ()
  "Lift the selection off the canvas so it can be moved."
  (interactive)
  (let ((cells (emascii--selection))
        (sel (emascii--selection-lines)))
    (if (not sel)
        (message "Nothing selected")
      (emascii-clear-selection)
      (setq emascii--pad-undo nil)
      (let ((group (prepare-change-group)))
        (activate-change-group group)
        (emascii--blank-cells cells)
        (emascii-float-start (nth 2 sel) (nth 0 sel) (nth 1 sel) group)))))

;;;; Floating pieces

(defun emascii--float-stamp (fl)
  (let ((saved nil) (row (emascii--fl-row fl)) (col (emascii--fl-col fl)))
    (save-excursion
      (cl-loop for line in (emascii--fl-lines fl) for i from 0 do
               (cl-loop for ch across line for j from 0
                        unless (and (eq ch ?\s) (not (emascii--fl-opaque fl)))
                        do (let ((r (+ row i)) (c (+ col j)))
                             (push (cons (cons r c) (emascii--char-at r c)) saved)
                             (emascii--put r c ch)))))
    (setf (emascii--fl-saved fl) saved)))

(defun emascii--float-unstamp (fl)
  (save-excursion
    (dolist (s (emascii--fl-saved fl))
      (emascii--put (caar s) (cdar s) (cdr s))))
  (setf (emascii--fl-saved fl) nil))

(defun emascii-float-start (lines row col &optional group)
  "Float LINES with top-left corner at ROW, COL.
GROUP is an already active change group to extend."
  (when emascii--float (emascii-float-commit))
  (setq emascii--pad-undo nil)
  (let ((group (or group
                   (let ((g (prepare-change-group)))
                     (activate-change-group g)
                     g))))
    (setq emascii--float (emascii--fl-make :lines lines :row row :col col
                                           :group group))
    (emascii--float-stamp emascii--float)
    (emascii--goto-cell row col)
    (emascii--refresh)
    (message "Floating: arrows/mouse move, S-arrows move %d, t transparency, RET drop, C-g cancel"
             emascii-big-step)))

(defun emascii-float-move-to (row col)
  "Move the floating piece so that its top-left corner is at ROW, COL."
  (let ((fl emascii--float))
    (when fl
      (emascii--float-unstamp fl)
      (setf (emascii--fl-row fl) (max 0 row)
            (emascii--fl-col fl) (max 0 col))
      (emascii--float-stamp fl)
      (emascii--goto-cell (emascii--fl-row fl) (emascii--fl-col fl))
      (emascii--refresh))))

(defun emascii--float-shift (dr dc)
  (let ((fl emascii--float))
    (when fl
      (emascii-float-move-to (+ (emascii--fl-row fl) dr)
                             (+ (emascii--fl-col fl) dc)))))

(defun emascii-float-left () "Move the piece left." (interactive) (emascii--float-shift 0 -1))
(defun emascii-float-right () "Move the piece right." (interactive) (emascii--float-shift 0 1))
(defun emascii-float-up () "Move the piece up." (interactive) (emascii--float-shift -1 0))
(defun emascii-float-down () "Move the piece down." (interactive) (emascii--float-shift 1 0))
(defun emascii-float-left-big () "Move the piece far left." (interactive)
       (emascii--float-shift 0 (- emascii-big-step)))
(defun emascii-float-right-big () "Move the piece far right." (interactive)
       (emascii--float-shift 0 emascii-big-step))
(defun emascii-float-up-big () "Move the piece far up." (interactive)
       (emascii--float-shift (- emascii-big-step) 0))
(defun emascii-float-down-big () "Move the piece far down." (interactive)
       (emascii--float-shift emascii-big-step 0))

(defun emascii-float-toggle-opaque ()
  "Toggle whether spaces of the floating piece hide what is below."
  (interactive)
  (let ((fl emascii--float))
    (when fl
      (emascii--float-unstamp fl)
      (setf (emascii--fl-opaque fl) (not (emascii--fl-opaque fl)))
      (emascii--float-stamp fl)
      (emascii--refresh)
      (message "Spaces are %s" (if (emascii--fl-opaque fl) "opaque" "transparent")))))

(defun emascii-float-mouse-drag (event)
  "Drag the floating piece with the mouse."
  (interactive "e")
  (let* ((fl emascii--float)
         (start (and fl (emascii--event-cell event))))
    (when start
      (let* ((r0 (emascii--fl-row fl)) (c0 (emascii--fl-col fl))
             (h (length (emascii--fl-lines fl)))
             (w (apply #'max 1 (mapcar #'length (emascii--fl-lines fl))))
             (inside (and (<= r0 (car start) (+ r0 h -1))
                          (<= c0 (cdr start) (+ c0 w -1))))
             (dr (if inside (- (car start) r0) 0))
             (dc (if inside (- (cdr start) c0) 0)))
        (unless inside (emascii-float-move-to (car start) (cdr start)))
        (emascii--track-drag
         start
         (lambda (cell)
           (emascii-float-move-to (- (car cell) dr) (- (cdr cell) dc))))))))

(defun emascii-float-commit ()
  "Drop the floating piece where it is."
  (interactive)
  (let ((fl emascii--float))
    (when fl
      (setq emascii--float nil emascii--pad-undo nil)
      (accept-change-group (emascii--fl-group fl))
      (undo-amalgamate-change-group (emascii--fl-group fl))
      (emascii--refresh))))

(defun emascii-float-cancel ()
  "Discard the floating piece and restore the canvas."
  (interactive)
  (let ((fl emascii--float))
    (when fl
      (setq emascii--float nil emascii--pad-undo nil)
      (let ((pt (point)))
        (cancel-change-group (emascii--fl-group fl))
        (goto-char (min pt (point-max))))
      (emascii--refresh)
      (message "Canceled"))))

;;;; Saving

(defun emascii--before-save ()
  "Remove trailing spaces and blank lines before writing."
  (save-excursion
    (let ((deactivate-mark nil))
      (delete-trailing-whitespace))))

;;;; Keymaps and mode

(defvar emascii-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap self-insert-command] #'emascii-self-insert)
    (define-key m [remap forward-char] #'emascii-forward)
    (define-key m [remap right-char] #'emascii-forward)
    (define-key m [remap backward-char] #'emascii-backward)
    (define-key m [remap left-char] #'emascii-backward)
    (define-key m [remap next-line] #'emascii-down)
    (define-key m [remap previous-line] #'emascii-up)
    (define-key m [remap kill-line] #'emascii-kill-line)
    (define-key m [remap delete-backward-char] #'emascii-delete-backward)
    (define-key m [remap backward-delete-char-untabify] #'emascii-delete-backward)
    (define-key m [remap delete-forward-char] #'emascii-delete-forward)
    (define-key m [remap delete-char] #'emascii-delete-forward)
    (define-key m (kbd "DEL") #'emascii-delete-backward)
    (define-key m (kbd "<deletechar>") #'emascii-delete-forward)
    (define-key m (kbd "S-DEL") #'emascii-delete-backward-other)
    (define-key m (kbd "S-<backspace>") #'emascii-delete-backward-other)
    (define-key m (kbd "S-<delete>") #'emascii-delete-forward-other)
    (define-key m (kbd "S-<deletechar>") #'emascii-delete-forward-other)
    (define-key m (kbd "<insert>") #'emascii-toggle-overwrite)
    (define-key m (kbd "<insertchar>") #'emascii-toggle-overwrite)
    (define-key m [remap overwrite-mode] #'emascii-toggle-overwrite)
    (define-key m (kbd "RET") #'emascii-newline)
    (define-key m (kbd "TAB") #'emascii-tab)
    (define-key m (kbd "C-c C-c") #'emascii-copy)
    (define-key m (kbd "C-c C-m") #'emascii-move)
    (define-key m (kbd "C-c C-k") #'emascii-clear)
    (define-key m (kbd "C-c C-t") #'emascii-toggle-cell)
    (define-key m (kbd "C-c C-o") #'emascii-cycle-shape)
    (define-key m (kbd "C-c C-SPC") #'emascii-set-anchor)
    (define-key m (kbd "C-c C-s") #'emascii-gallery-save)
    (define-key m (kbd "C-c C-i") #'emascii-gallery-insert)
    (define-key m (kbd "C-c C-g") #'emascii-gallery)
    (define-key m [down-mouse-1] #'emascii-mouse-down)
    (define-key m [S-down-mouse-1] #'emascii-mouse-add)
    (define-key m [C-down-mouse-1] #'emascii-mouse-toggle-cell)
    (define-key m [M-down-mouse-1] #'emascii-mouse-toggle-cell)
    (dolist (k '([mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
                 [double-down-mouse-1] [triple-down-mouse-1]
                 [S-mouse-1] [S-drag-mouse-1] [C-mouse-1] [M-mouse-1]
                 [M-drag-mouse-1] [C-drag-mouse-1]))
      (define-key m k #'ignore))
    m)
  "Keymap for `emascii-mode'.")

(defvar emascii-override-map
  (let ((m (make-sparse-keymap)))
    (dolist (c '(kill-ring-save copy-region-as-kill clipboard-kill-ring-save
                 cua-copy-region))
      (define-key m (vector 'remap c) #'emascii-copy))
    (dolist (c '(kill-region clipboard-kill-region cua-cut-region))
      (define-key m (vector 'remap c) #'emascii-cut))
    (dolist (c '(yank cua-paste clipboard-yank x-clipboard-yank))
      (define-key m (vector 'remap c) #'emascii-paste))
    (dolist (c '(set-mark-command cua-set-mark))
      (define-key m (vector 'remap c) #'emascii-set-anchor))
    (dolist (c '(keyboard-quit cua-cancel))
      (define-key m (vector 'remap c) #'emascii-quit))
    m)
  "Keymap that takes precedence over cua-mode in `emascii-mode'.")

(defvar emascii-float-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<left>") #'emascii-float-left)
    (define-key m (kbd "<right>") #'emascii-float-right)
    (define-key m (kbd "<up>") #'emascii-float-up)
    (define-key m (kbd "<down>") #'emascii-float-down)
    (define-key m (kbd "S-<left>") #'emascii-float-left-big)
    (define-key m (kbd "S-<right>") #'emascii-float-right-big)
    (define-key m (kbd "S-<up>") #'emascii-float-up-big)
    (define-key m (kbd "S-<down>") #'emascii-float-down-big)
    (define-key m (kbd "C-b") #'emascii-float-left)
    (define-key m (kbd "C-f") #'emascii-float-right)
    (define-key m (kbd "C-p") #'emascii-float-up)
    (define-key m (kbd "C-n") #'emascii-float-down)
    (define-key m (kbd "RET") #'emascii-float-commit)
    (define-key m (kbd "<return>") #'emascii-float-commit)
    (define-key m (kbd "t") #'emascii-float-toggle-opaque)
    (define-key m (kbd "C-g") #'emascii-float-cancel)
    (define-key m (kbd "<escape>") #'emascii-float-cancel)
    (define-key m [down-mouse-1] #'emascii-float-mouse-drag)
    (define-key m [mouse-1] #'ignore)
    (define-key m [drag-mouse-1] #'ignore)
    (define-key m [double-mouse-1] #'emascii-float-commit)
    m)
  "Keymap active while a piece is floating.")

(defvar emascii--emulation-alist
  `((emascii--float . ,emascii-float-map)
    (emascii--override . ,emascii-override-map)))

;; Order 100 puts these maps before cua-mode (order 400).
(add-to-ordered-list 'emulation-mode-map-alists 'emascii--emulation-alist 100)

(defun emascii--mode-line ()
  (let ((c (emascii--cell)))
    (format " [%s %s%s] %d:%d"
            (if emascii-overwrite "OVR" "INS")
            emascii-shape
            (if emascii--float " floating" "")
            (car c) (cdr c))))

(declare-function emascii-gallery-save "emascii-gallery")
(declare-function emascii-gallery-insert "emascii-gallery")
(declare-function emascii-gallery "emascii-gallery")

;;;###autoload
(define-derived-mode emascii-mode text-mode "Emascii"
  "Major mode for drawing ASCII art on an unbounded canvas.

\\{emascii-mode-map}"
  (require 'emascii-gallery)
  (setq-local indent-tabs-mode nil)
  (setq-local truncate-lines t)
  (setq-local show-trailing-whitespace nil)
  (setq-local require-final-newline nil)
  (setq-local mode-line-process '(:eval (emascii--mode-line)))
  (setq emascii--cells (make-hash-table :test #'equal)
        emascii--override t
        emascii-overwrite emascii-start-in-overwrite
        emascii--last-row nil)
  (when (fboundp 'electric-indent-local-mode) (electric-indent-local-mode -1))
  (auto-fill-mode -1)
  (face-remap-add-relative 'region 'default)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (point-min) (point-max))))
  (add-hook 'post-command-hook #'emascii--post-command -50 t)
  (add-hook 'pre-command-hook #'emascii--pre-command nil t)
  (add-hook 'activate-mark-hook #'emascii--on-activate-mark nil t)
  (add-hook 'deactivate-mark-hook #'emascii--on-deactivate-mark nil t)
  (add-hook 'before-save-hook #'emascii--before-save nil t)
  (add-hook 'change-major-mode-hook #'emascii--teardown nil t))

(defun emascii--teardown ()
  (when emascii--float (emascii-float-commit))
  (emascii--clear-overlays)
  (setq emascii--override nil))

;;;###autoload
(defun emascii-new ()
  "Open a new empty canvas."
  (interactive)
  (let ((buf (generate-new-buffer "*emascii*")))
    (switch-to-buffer buf)
    (emascii-mode)))

(provide 'emascii)
;;; emascii.el ends here
