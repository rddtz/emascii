;;; emascii-gallery.el --- Gallery of reusable ASCII drawings -*- lexical-binding: t; -*-

;; Author: Rayan Raddatz
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Drawings are plain .txt files under `emascii-gallery-directory'.
;; Sub-directories are folders (people, animals, ...).  An item is named
;; by its path relative to the gallery root, without extension, for
;; example "animals/cat".
;;
;; `emascii-gallery' opens a browser with thumbnails, grouped by folder,
;; with search by name.  `emascii-gallery-insert' picks an item by name
;; with completion.  Both drop the item as a floating piece on the
;; canvas, to be positioned and dropped with RET.

;;; Code:

(require 'emascii)

(defcustom emascii-gallery-directory (locate-user-emacs-file "emascii-gallery/")
  "Directory that holds the gallery drawings."
  :type 'directory
  :group 'emascii)

(defcustom emascii-gallery-thumb-height 8
  "Maximum number of rows shown in a gallery thumbnail."
  :type 'integer
  :group 'emascii)

(defcustom emascii-gallery-thumb-width 28
  "Maximum number of columns shown in a gallery thumbnail."
  :type 'integer
  :group 'emascii)

(defface emascii-gallery-folder '((t :inherit bold :height 1.1))
  "Face for folder headers in the gallery.")

(defface emascii-gallery-name '((t :inherit link))
  "Face for item names in the gallery.")

;;;; Storage

(defun emascii-gallery--root ()
  (file-name-as-directory (expand-file-name emascii-gallery-directory)))

(defun emascii-gallery--file (id)
  (expand-file-name (concat id ".txt") (emascii-gallery--root)))

(defun emascii-gallery--items ()
  "Return all item ids, sorted."
  (let ((root (emascii-gallery--root)))
    (when (file-directory-p root)
      (sort (mapcar (lambda (f) (file-name-sans-extension
                                 (file-relative-name f root)))
                    (directory-files-recursively root "\\.txt\\'"))
            #'string<))))

(defun emascii-gallery--folders ()
  "Return all folder names relative to the gallery root."
  (let ((root (emascii-gallery--root)))
    (when (file-directory-p root)
      (sort (mapcar (lambda (d) (directory-file-name (file-relative-name d root)))
                    (seq-filter #'file-directory-p
                                (directory-files-recursively root "" t)))
            #'string<))))

(defun emascii-gallery--read (id)
  "Return the lines of item ID."
  (with-temp-buffer
    (insert-file-contents (emascii-gallery--file id))
    (nth 2 (emascii--crop (emascii--normalize-text (buffer-string))))))

(defun emascii-gallery--check-id (id)
  (when (or (string-empty-p id)
            (string-match-p "\\(\\`\\|/\\)\\.\\.?\\(/\\|\\'\\)" id)
            (string-prefix-p "/" id)
            (string-suffix-p "/" id))
    (user-error "Invalid name: %S" id))
  id)

(defun emascii-gallery--write (id lines)
  (let ((file (emascii-gallery--file (emascii-gallery--check-id id))))
    (when (and (file-exists-p file)
               (not (y-or-n-p (format "Replace %s? " id))))
      (user-error "Not saved"))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (concat (string-join lines "\n") "\n") nil file nil 'silent))
    (emascii-gallery--refresh-buffers)
    file))

(defun emascii-gallery--read-folder ()
  (let ((f (completing-read "Folder (empty for root, new names allowed): "
                            (emascii-gallery--folders) nil nil)))
    (string-trim (string-trim f) "/+" "/+")))

;;;; Commands used from the canvas

;;;###autoload
(defun emascii-gallery-save ()
  "Save the selection, or the whole canvas, into the gallery."
  (interactive)
  (let* ((sel (and (derived-mode-p 'emascii-mode) (emascii--selection-lines)))
         (lines (if sel (nth 2 sel)
                  (nth 2 (emascii--crop
                          (split-string (buffer-substring-no-properties
                                         (point-min) (point-max))
                                        "\n"))))))
    (unless lines (user-error "Nothing to save"))
    (let* ((folder (emascii-gallery--read-folder))
           (name (string-trim (read-string "Name: ")))
           (id (if (string-empty-p folder) name (concat folder "/" name))))
      (emascii-gallery--write id lines)
      (when sel (emascii-clear-selection))
      (message "Saved %s (%d lines)" id (length lines)))))

(defun emascii-gallery--size (id)
  (let ((lines (ignore-errors (emascii-gallery--read id))))
    (if lines
        (format "%dx%d" (apply #'max 0 (mapcar #'length lines)) (length lines))
      "")))

(defun emascii-gallery--float-in (buffer id)
  "Float item ID at point in canvas BUFFER."
  (let ((lines (emascii-gallery--read id)))
    (unless lines (user-error "Item %s is empty" id))
    (pop-to-buffer buffer)
    (unless (derived-mode-p 'emascii-mode) (emascii-mode))
    (let ((c (emascii--cell)))
      (emascii-float-start lines (car c) (cdr c)))))

(defun emascii-gallery--target ()
  "Return the canvas buffer to insert into."
  (or (seq-find (lambda (b) (with-current-buffer b (derived-mode-p 'emascii-mode)))
                (buffer-list))
      (progn (emascii-new) (current-buffer))))

;;;###autoload
(defun emascii-gallery-insert (id)
  "Float gallery item ID on the canvas, chosen by name."
  (interactive
   (let ((items (or (emascii-gallery--items) (user-error "The gallery is empty"))))
     (list (completing-read
            "Insert drawing: "
            (lambda (str pred action)
              (if (eq action 'metadata)
                  `(metadata (category . emascii-drawing)
                             (annotation-function
                              . ,(lambda (id) (concat "  " (emascii-gallery--size id)))))
                (complete-with-action action items str pred)))
            nil t))))
  (emascii-gallery--float-in
   (if (derived-mode-p 'emascii-mode) (current-buffer) (emascii-gallery--target))
   id))

;;;; Browser

(defvar-local emascii-gallery--filter nil)
(defvar-local emascii-gallery--canvas nil)
(defvar-local emascii-gallery--positions nil)

(defun emascii-gallery--thumb (id)
  "Return thumbnail lines of ID, truncated to the thumbnail size."
  (let* ((lines (or (ignore-errors (emascii-gallery--read id)) '("(empty)")))
         (h emascii-gallery-thumb-height) (w emascii-gallery-thumb-width)
         (cut-rows (> (length lines) h))
         (shown (mapcar (lambda (l) (if (> (length l) w)
                                        (concat (substring l 0 (1- w)) "~")
                                      l))
                        (seq-take lines h))))
    (if cut-rows (append (butlast shown) (list "...")) shown)))

(defun emascii-gallery--tile (id)
  "Return (WIDTH . LINES) of the tile for ID; first line is the name."
  (let* ((name (file-name-nondirectory id))
         (thumb (emascii-gallery--thumb id))
         (w (apply #'max (length name) (mapcar #'length thumb))))
    (cons w (cons name thumb))))

(defun emascii-gallery--insert-row (tiles)
  "Insert TILES side by side.  Each tile is (ID WIDTH . LINES)."
  (let ((height (apply #'max (mapcar (lambda (tl) (length (cddr tl))) tiles))))
    (dotimes (k height)
      (dolist (tl tiles)
        (let* ((id (car tl)) (w (cadr tl))
               (line (or (nth k (cddr tl)) ""))
               (start (point)))
          (when (= k 0) (push (cons start id) emascii-gallery--positions))
          (insert (if (= k 0) (propertize line 'face 'emascii-gallery-name) line)
                  (make-string (- w (length line)) ?\s))
          (put-text-property start (point) 'emascii-gallery-id id)
          (insert "    ")))
      (insert "\n"))
    (insert "\n")))

(defun emascii-gallery--render ()
  (let* ((inhibit-read-only t)
         (id-at (get-text-property (point) 'emascii-gallery-id))
         (width (max 40 (- (window-body-width (get-buffer-window) t) 2)))
         (items (seq-filter (lambda (id)
                              (or (null emascii-gallery--filter)
                                  (string-match-p (regexp-quote emascii-gallery--filter)
                                                  id)))
                            (emascii-gallery--items)))
         (groups (seq-group-by (lambda (id) (or (file-name-directory id) "")) items)))
    (erase-buffer)
    (setq emascii-gallery--positions nil)
    (insert (propertize "Emascii gallery" 'face 'bold) "  "
            (abbreviate-file-name (emascii-gallery--root))
            (if emascii-gallery--filter (format "   search: %s" emascii-gallery--filter) "")
            "\n"
            (propertize "RET/click insert  / search  n p move  e edit  r rename  d delete  + folder  g refresh  q quit"
                        'face 'shadow)
            "\n\n")
    (when (null items)
      (insert (if emascii-gallery--filter "No drawing matches the search.\n"
                "The gallery is empty.  Save drawings with C-c C-s in a canvas.\n")))
    (dolist (group (sort groups (lambda (a b) (string< (car a) (car b)))))
      (insert (propertize (if (string-empty-p (car group)) "/"
                            (directory-file-name (car group)))
                          'face 'emascii-gallery-folder)
              "\n\n")
      (let ((row nil) (used 0))
        (dolist (id (cdr group))
          (let* ((tile (emascii-gallery--tile id))
                 (w (+ 4 (car tile))))
            (when (and row (> (+ used w) width))
              (emascii-gallery--insert-row (nreverse row))
              (setq row nil used 0))
            (push (cons id tile) row)
            (cl-incf used w)))
        (when row (emascii-gallery--insert-row (nreverse row)))))
    (setq emascii-gallery--positions (nreverse emascii-gallery--positions))
    (goto-char (or (car (rassoc id-at emascii-gallery--positions))
                   (caar emascii-gallery--positions)
                   (point-min)))))

(defun emascii-gallery--refresh-buffers ()
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (derived-mode-p 'emascii-gallery-mode)
        (emascii-gallery--render)))))

(defun emascii-gallery--id-at-point ()
  (or (get-text-property (point) 'emascii-gallery-id)
      (user-error "No drawing at point")))

(defun emascii-gallery-insert-at-point ()
  "Float the drawing at point on the canvas."
  (interactive)
  (let ((id (emascii-gallery--id-at-point))
        (canvas (if (buffer-live-p emascii-gallery--canvas)
                    emascii-gallery--canvas
                  (emascii-gallery--target))))
    (emascii-gallery--float-in canvas id)))

(defun emascii-gallery-mouse-insert (event)
  "Float the clicked drawing on the canvas."
  (interactive "e")
  (mouse-set-point event)
  (emascii-gallery-insert-at-point))

(defun emascii-gallery-next (n)
  "Move to the next N drawings."
  (interactive "p")
  (let* ((all emascii-gallery--positions)
         (id (get-text-property (point) 'emascii-gallery-id))
         (cur (or (and id (cl-position id all :key #'cdr :test #'equal))
                  (1- (or (cl-position-if (lambda (p) (> (car p) (point))) all)
                          (length all))))))
    (when all
      (goto-char (car (nth (max 0 (min (1- (length all)) (+ cur n))) all))))))

(defun emascii-gallery-previous (n)
  "Move to the previous N drawings."
  (interactive "p")
  (emascii-gallery-next (- n)))

(defun emascii-gallery-search (text)
  "Show only drawings whose name contains TEXT; empty shows all."
  (interactive (list (read-string "Search name (empty for all): "
                                  emascii-gallery--filter)))
  (setq emascii-gallery--filter (if (string-blank-p text) nil text))
  (emascii-gallery--render))

(defun emascii-gallery-delete ()
  "Delete the drawing at point."
  (interactive)
  (let ((id (emascii-gallery--id-at-point)))
    (when (yes-or-no-p (format "Delete %s? " id))
      (delete-file (emascii-gallery--file id))
      (emascii-gallery--render))))

(defun emascii-gallery-rename (new)
  "Rename or move the drawing at point to NEW (\"folder/name\")."
  (interactive (list (read-string "New name (folder/name): "
                                  (emascii-gallery--id-at-point))))
  (let* ((id (emascii-gallery--id-at-point))
         (to (emascii-gallery--file (emascii-gallery--check-id (string-trim new)))))
    (make-directory (file-name-directory to) t)
    (rename-file (emascii-gallery--file id) to)
    (emascii-gallery--render)))

(defun emascii-gallery-edit ()
  "Open the drawing at point in a canvas."
  (interactive)
  (find-file-other-window (emascii-gallery--file (emascii-gallery--id-at-point)))
  (emascii-mode))

(defun emascii-gallery-new-folder (name)
  "Create folder NAME in the gallery."
  (interactive "sNew folder: ")
  (make-directory (expand-file-name (emascii-gallery--check-id (string-trim name "/+" "/+"))
                                    (emascii-gallery--root))
                  t)
  (message "Folder %s created; it shows once it has a drawing" name))

(defun emascii-gallery-refresh ()
  "Redraw the gallery."
  (interactive)
  (emascii-gallery--render))

(defvar emascii-gallery-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'emascii-gallery-insert-at-point)
    (define-key m [mouse-1] #'emascii-gallery-mouse-insert)
    (define-key m (kbd "n") #'emascii-gallery-next)
    (define-key m (kbd "p") #'emascii-gallery-previous)
    (define-key m (kbd "TAB") #'emascii-gallery-next)
    (define-key m (kbd "<backtab>") #'emascii-gallery-previous)
    (define-key m (kbd "/") #'emascii-gallery-search)
    (define-key m (kbd "s") #'emascii-gallery-search)
    (define-key m (kbd "d") #'emascii-gallery-delete)
    (define-key m (kbd "r") #'emascii-gallery-rename)
    (define-key m (kbd "e") #'emascii-gallery-edit)
    (define-key m (kbd "+") #'emascii-gallery-new-folder)
    (define-key m (kbd "g") #'emascii-gallery-refresh)
    m)
  "Keymap for `emascii-gallery-mode'.")

(define-derived-mode emascii-gallery-mode special-mode "Emascii-Gallery"
  "Browse the emascii gallery.

\\{emascii-gallery-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (emascii-gallery--render))))

;;;###autoload
(defun emascii-gallery ()
  "Open the gallery browser for the current canvas."
  (interactive)
  (let ((canvas (and (derived-mode-p 'emascii-mode) (current-buffer)))
        (buf (get-buffer-create "*emascii gallery*")))
    (pop-to-buffer buf)
    (unless (derived-mode-p 'emascii-gallery-mode) (emascii-gallery-mode))
    (when canvas (setq emascii-gallery--canvas canvas))
    (emascii-gallery--render)))

(provide 'emascii-gallery)
;;; emascii-gallery.el ends here
