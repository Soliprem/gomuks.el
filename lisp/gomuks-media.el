;;; gomuks-media.el --- Media transfers and playback -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Own download files, preview limits and playback controls.

;;; Code:

(require 'gomuks-backend)
(require 'gomuks-content)
(declare-function gomuks--update-room-views "gomuks-view" (id &optional incoming))
(declare-function gomuks--event-at-point "gomuks-render")
(declare-function gomuks--refresh-sticker-pickers "gomuks-stickers" (&optional id fetch))
(declare-function empv-play "empv" (uri))
(declare-function empv-toggle "empv" ())
(declare-function empv-seek "empv" (target &optional type))
(declare-function emms-play-file "emms-source-file" (file))
(declare-function emms-pause "emms" ())
(declare-function emms-seek "emms" (duration))

(defun gomuks--attachment (content &optional event)
  "Return attachment details from message CONTENT and optional EVENT."
  (let* ((file (gomuks--alist 'file content))
         (mxc (or (gomuks--alist 'url file)
                  (gomuks--alist 'url content)))
         (info (gomuks--alist 'info content))
         (name (or (gomuks--alist 'filename content)
                   (gomuks--alist 'body content))))
    (when (and (stringp mxc) (string-prefix-p "mxc://" mxc))
      (list :mxc mxc :encrypted (and file t)
            :name (if (and (stringp name) (not (string-empty-p name)))
                      name "attachment")
            :mime (gomuks--alist 'mimetype info)
            :size (gomuks--alist 'size info)
            :kind (if (equal (or (gomuks--alist 'decrypted_type event)
                                 (gomuks--alist 'type event)) "m.sticker")
                      "m.sticker"
                    (gomuks--alist 'msgtype content))))))

(defun gomuks--audio-control (&optional seconds)
  "Procedure: toggle playback, or seek SECONDS, through the selected backend."
  (pcase gomuks-audio-backend
    ('emms
     (unless (require 'emms nil t)
       (user-error "EMMS is required for audio controls"))
     (if seconds (emms-seek seconds) (emms-pause)))
    ('empv
     (if (require 'empv nil t)
         (if seconds (empv-seek (number-to-string seconds)) (empv-toggle))
       (user-error "EMPV is required for audio controls")))))

(defun gomuks-audio-toggle ()
  "Pause or resume audio playing through the configured backend."
  (interactive)
  (gomuks--audio-control))

(defun gomuks-audio-backward ()
  "Seek five seconds backward in audio playing through the configured backend."
  (interactive)
  (gomuks--audio-control -5))

(defun gomuks-audio-forward ()
  "Seek five seconds forward in audio playing through the configured backend."
  (interactive)
  (gomuks--audio-control 5))

(defun gomuks--media-url (attachment)
  "Return the backend download URL for ATTACHMENT."
  (let ((mxc (plist-get attachment :mxc)))
    (unless (string-match "\\`mxc://\\([^/]+\\)/\\(.+\\)\\'" mxc)
      (user-error "Invalid attachment URL"))
    (gomuks--url
     (format "media/%s/%s?encrypted=%s"
             (url-hexify-string (match-string 1 mxc))
             (url-hexify-string (match-string 2 mxc))
             (if (plist-get attachment :encrypted) "true" "false")))))

(defun gomuks--media-key (attachment)
  "Return the cache key for ATTACHMENT."
  (list gomuks-backend-url gomuks--user-id
        (plist-get attachment :mxc)
        (plist-get attachment :encrypted)))

(defun gomuks--cached-media (attachment)
  "Return ATTACHMENT's cached local path if the file still exists."
  (let* ((key (gomuks--media-key attachment))
         (path (gethash key gomuks--media-cache)))
    (and path (file-exists-p path) path)))

(defun gomuks-save-attachment (&optional destination event)
  "Save the attachment in EVENT, or at point, to DESTINATION."
  (interactive)
  (let* ((event (or event (gomuks--event-at-point)))
         (attachment (gomuks--attachment (gomuks--effective-content event) event)))
    (unless attachment (user-error "No downloadable attachment on this line"))
    (setq destination
          (or destination
              (read-file-name "Save attachment as: " nil nil nil
                              (plist-get attachment :name))))
    (setq destination (expand-file-name destination))
    (when (and (file-exists-p destination)
               (not (yes-or-no-p (format "Overwrite %s? " destination))))
      (user-error "Download cancelled"))
    (gomuks--fetch-media
     attachment
     (lambda (path)
       (when path
         (condition-case err
             (progn
               (copy-file path destination t)
               (message "gomuks: saved %s" destination))
           (error (message "gomuks: save failed: %s" err))))))))

(defun gomuks-open-attachment (&optional event)
  "Load the attachment in EVENT, or at point, and open it."
  (interactive)
  (let* ((event (or event (gomuks--event-at-point)))
         (attachment (gomuks--attachment (gomuks--effective-content event) event))
         (mime (and attachment (plist-get attachment :mime)))
         (kind (and attachment (plist-get attachment :kind))))
    (unless attachment (user-error "No downloadable attachment on this line"))
    (gomuks--fetch-media
     attachment
     (lambda (path)
       (when path
         (cond
          ((or (string-equal (downcase (or (file-name-extension path) "")) "ogg")
               (equal kind "m.audio")
               (and mime (string-prefix-p "audio/" mime)))
           (pcase gomuks-audio-backend
             ('emms
              (unless (require 'emms-source-file nil t)
                (user-error "EMMS is required to play audio attachments"))
              (emms-play-file path))
             ('empv
              (if (require 'empv nil t)
                  (empv-play path)
                (user-error "Install EMPV for playback and controls, or select EMMS")))))
          ((or (equal kind "m.video")
               (and mime (string-prefix-p "video/" mime)))
           (browse-url-default-browser (browse-url-file-url path)))
          (t (pop-to-buffer (find-file-noselect path)))))))))


(defun gomuks--upload-file (file room-id callback)
  "Upload FILE using ROOM-ID's known encryption state, then call CALLBACK."
  (let ((meta (gethash room-id gomuks--rooms)))
    (unless meta (user-error "Room encryption state is unknown; reload the room first"))
    (gomuks--curl-file
     (gomuks--url (format "upload?filename=%s&encrypt=%s"
                          (url-hexify-string (file-name-nondirectory file))
                          (if (gomuks--alist 'encryption_event meta) "true" "false")))
     (list "--request" "POST" "--header" "Content-Type: application/octet-stream"
           "--data-binary" (concat "@" (expand-file-name file)))
     (lambda (failure path)
       (let (content)
         (when path
           (unwind-protect
               (condition-case err
                   (setq content (json-parse-string
                                  (with-temp-buffer
                                    (insert-file-contents path) (buffer-string))
                                  :object-type 'hash-table :array-type 'array
                                  :false-object nil :null-object nil))
                 (error (setq failure (format "invalid upload response: %s" err))))
             (delete-file path)))
         (funcall callback failure content)))
     (* 1024 1024) (gomuks--authorization))))

(defvar gomuks--preview-queue nil "Automatic previews waiting for a download slot.")
(defvar gomuks--preview-active 0 "Automatic downloads currently running.")
(defvar gomuks--media-uncached nil "Explicit oversized downloads owned until cleanup.")

(defun gomuks--clear-media-cache ()
  "Delete owned media files and discard queued automatic previews."
  (setq gomuks--preview-queue nil)
  (maphash (lambda (_key path) (ignore-errors (delete-file path))) gomuks--media-cache)
  (dolist (path gomuks--media-uncached) (ignore-errors (delete-file path)))
  (setq gomuks--media-uncached nil)
  (clrhash gomuks--media-cache)
  (clrhash gomuks--media-failures))

(defun gomuks--preview-file-p (path)
  "Query whether PATH's actual file size permits an automatic preview."
  (when-let* ((attributes (file-attributes path)))
    (<= (file-attribute-size attributes) gomuks-inline-image-max-bytes)))

(defun gomuks--cache-media (key path)
  "Own PATH and cache it under KEY, evicting oldest files to respect the quota."
  (when-let* ((previous (gethash key gomuks--media-cache)))
    (unless (equal previous path) (ignore-errors (delete-file previous)))
    (remhash key gomuks--media-cache))
  (let ((size (file-attribute-size (file-attributes path))) entries (total 0))
    (maphash (lambda (cache-key file)
               (when-let* ((attr (file-attributes file)))
                 (cl-incf total (file-attribute-size attr))
                 (push (list cache-key file (file-attribute-modification-time attr)
                             (file-attribute-size attr)) entries))) gomuks--media-cache)
    (if (> size gomuks-media-cache-max-bytes)
        (push path gomuks--media-uncached)
      (dolist (entry (sort entries (lambda (a b) (time-less-p (nth 2 a) (nth 2 b)))))
        (when (> (+ total size) gomuks-media-cache-max-bytes)
          (remhash (car entry) gomuks--media-cache)
          (ignore-errors (delete-file (cadr entry)))
          (cl-decf total (nth 3 entry))))
      (puthash key path gomuks--media-cache))))

(defun gomuks--fetch-media (attachment callback &optional limit)
  "Fetch ATTACHMENT, calling CALLBACK with path/nil. LIMIT bounds actual bytes.
Coalesce consumers with the same transfer policy; isolate consumer failures."
  (let* ((key (gomuks--media-key attachment))
         (pending-key (list key limit))
         (cached (gomuks--cached-media attachment))
         (pending (gethash pending-key gomuks--media-pending)))
    (cond
     ((and cached (or (not limit)
                      (<= (file-attribute-size (file-attributes cached)) limit)))
      (funcall callback cached))
     (pending (puthash pending-key (cons callback pending) gomuks--media-pending))
     (t
      (puthash pending-key (list callback) gomuks--media-pending)
      (remhash key gomuks--media-failures)
      (let ((complete
             (lambda (failure path)
               (let ((callbacks (gethash pending-key gomuks--media-pending)))
		 (remhash pending-key gomuks--media-pending)
		 (when path
		   (let ((extension (file-name-extension (plist-get attachment :name))))
		     (when (and extension (string-match-p "\\`[[:alnum:]]\\{1,12\\}\\'" extension))
                       (condition-case err
			   (let ((named (concat path "." extension)))
			     (rename-file path named) (setq path named))
			 (error
			  (ignore-errors (delete-file path))
			  (setq path nil failure (error-message-string err)))))))
		 (if failure (puthash key t gomuks--media-failures)
		   (gomuks--cache-media key path))
		 (dolist (fn (reverse callbacks))
		   (condition-case err (funcall fn path)
		     (error (message "gomuks: media callback failed: %s"
				     (error-message-string err)))))))))
        (condition-case err
            (gomuks--curl-file (gomuks--media-url attachment) nil complete
                               limit (gomuks--authorization))
          (error
           (if (gethash pending-key gomuks--media-pending)
               (funcall complete (error-message-string err) nil)
             (signal (car err) (cdr err))))))))))

(defun gomuks--drain-previews ()
  "Procedure: start valid queued previews while download slots remain."
  (while (and gomuks--preview-queue
              (< gomuks--preview-active (max 1 gomuks-preview-concurrency)))
    (pcase-let ((`(,attachment ,id ,token) (pop gomuks--preview-queue)))
      (when (and gomuks-inline-images (gomuks--cache-current-p id token)
                 (not (gomuks--cached-media attachment)))
        (cl-incf gomuks--preview-active)
        (gomuks--fetch-media
         attachment
         (lambda (path)
           (cl-decf gomuks--preview-active)
           (unwind-protect
               (when (and path (gomuks--cache-current-p id token))
                 (gomuks--update-room-views id)
                 (when (fboundp 'gomuks--refresh-sticker-pickers)
                   (gomuks--refresh-sticker-pickers id)))
             (gomuks--drain-previews)))
         gomuks-inline-image-max-bytes)))))

(defun gomuks--maybe-preview-image (attachment id)
  "Procedure: queue an eligible automatic ATTACHMENT preview for room ID."
  (let ((key (gomuks--media-key attachment)) (size (plist-get attachment :size)))
    (when (and gomuks-inline-images (display-images-p)
               (member (plist-get attachment :kind) '("m.image" "m.sticker"))
               (or (not (numberp size)) (<= size gomuks-inline-image-max-bytes))
               (not (gomuks--cached-media attachment))
               (not (gethash (list key gomuks-inline-image-max-bytes) gomuks--media-pending))
               (not (gethash key gomuks--media-failures))
               (not (cl-find attachment gomuks--preview-queue :key #'car :test #'equal)))
      (setq gomuks--preview-queue
            (append gomuks--preview-queue
                    (list (list attachment id (gomuks--cache-token id)))))
      (gomuks--drain-previews))))

(provide 'gomuks-media)
;;; gomuks-media.el ends here
