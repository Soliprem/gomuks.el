;;; gomuks-media.el --- Media handling for Gomuks -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-
;;
;; Copyright (C) 2026 Francesco Prem Solidoro
;;
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Maintainer: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Created: settembre 30, 2026
;; Modified: settembre 30, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/Soliprem/gomuks-media
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'gomuks-backend)
(declare-function gomuks--render-room "gomuks-view" (id))
(declare-function gomuks--event-at-point "gomuks-view")
(declare-function empv-play "empv" (uri))
(declare-function empv-toggle "empv" ())
(declare-function empv-seek "empv" (target &optional type))
(declare-function emms-play-file "emms-source-file" (file))
(declare-function emms-pause "emms" ())
(declare-function emms-seek "emms" (duration))

(defun gomuks--attachment (content)
  "Return attachment details from message CONTENT, if present."
  (let* ((file (gomuks--alist 'file content))
         (mxc (or (gomuks--alist 'url file)
                  (gomuks--alist 'url content)))
         (info (gomuks--alist 'info content)))
    (when (and (stringp mxc) (string-prefix-p "mxc://" mxc))
      (list :mxc mxc :encrypted (and file t)
            :name (or (gomuks--alist 'filename content)
                      (gomuks--alist 'body content) "attachment")
            :mime (gomuks--alist 'mimetype info)
            :size (gomuks--alist 'size info)
            :kind (gomuks--alist 'msgtype content)))))

(defun gomuks--upload-file (file room-id callback)
  "Upload FILE for ROOM-ID, then call CALLBACK with error and content."
  (unless (executable-find "curl")
    (user-error "Curl is required to upload attachments"))
  (let* ((filename (file-name-nondirectory file))
         (encrypted (gomuks--alist 'encryption_event
                                   (gethash room-id gomuks--rooms)))
         (url (gomuks--url
               (format "upload?filename=%s&encrypt=%s"
                       (url-hexify-string filename)
                       (if encrypted "true" "false"))))
         (response-file (make-temp-file "gomuks-upload-response-"))
         (auth (gomuks--authorization))
         (process
          (make-process
           :name "gomuks-upload" :buffer nil :connection-type 'pipe
           :command (list "curl" "--silent" "--show-error" "--fail"
                          "--request" "POST"
                          "--header" "Content-Type: application/octet-stream"
                          "--data-binary" (concat "@" (expand-file-name file))
                          "--output" response-file "--config" "-" url)
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((failure (unless (= (process-exit-status proc) 0)
                                "upload request failed"))
                     (content nil))
                 (unless failure
                   (condition-case err
                       (setq content
                             (json-parse-string
                              (with-temp-buffer
                                (insert-file-contents response-file)
                                (buffer-string))
                              :object-type 'hash-table :array-type 'array
                              :false-object nil :null-object nil))
                     (error (setq failure (format "invalid upload response: %s" err)))))
                 (delete-file response-file)
                 (funcall callback failure content)))))))
    (process-send-string process
                         (if auth
                             (format "header = \"Authorization: %s\"\n" auth)
                           ""))
    (process-send-eof process)))

(defun gomuks--clear-media-cache ()
  "Delete downloaded media files and clear their cache entries."
  (maphash (lambda (_key path)
             (when (file-exists-p path)
               (delete-file path)))
           gomuks--media-cache)
  (clrhash gomuks--media-cache))

(defun gomuks-audio-toggle ()
  "Pause or resume audio playing through the configured backend."
  (interactive)
  (pcase gomuks-audio-backend
    ('emms
     (unless (require 'emms nil t)
       (user-error "EMMS is required for audio controls"))
     (emms-pause))
    ('empv
     (if (require 'empv nil t)
         (empv-toggle)
       (user-error "EMPV is required for audio controls")))))

(defun gomuks-audio-backward ()
  "Seek five seconds backward in audio playing through the configured backend."
  (interactive)
  (pcase gomuks-audio-backend
    ('emms
     (unless (require 'emms nil t)
       (user-error "EMMS is required for audio controls"))
     (emms-seek -5))
    ('empv
     (if (require 'empv nil t)
         (empv-seek "-5")
       (user-error "EMPV is required for audio controls")))))

(defun gomuks-audio-forward ()
  "Seek five seconds forward in audio playing through the configured backend."
  (interactive)
  (pcase gomuks-audio-backend
    ('emms
     (unless (require 'emms nil t)
       (user-error "EMMS is required for audio controls"))
     (emms-seek 5))
    ('empv
     (if (require 'empv nil t)
         (empv-seek "5")
       (user-error "EMPV is required for audio controls")))))

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

(defun gomuks--fetch-media (attachment callback)
  "Fetch ATTACHMENT and call CALLBACK with a local path or nil."
  (let* ((key (gomuks--media-key attachment))
         (cached (gomuks--cached-media attachment))
         (pending (gethash key gomuks--media-pending)))
    (cond
     (cached (funcall callback cached))
     (pending
      (puthash key (cons callback pending) gomuks--media-pending))
     (t
      (unless (executable-find "curl")
        (user-error "Curl is required to load attachments"))
      (remhash key gomuks--media-failures)
      (let* ((extension (file-name-extension
                         (plist-get attachment :name)))
             (path (make-temp-file "gomuks-media-" nil
                                   (if extension (concat "." extension) ".bin")))
             (auth (gomuks--authorization))
             (url (gomuks--media-url attachment)))
        (puthash key (list callback) gomuks--media-pending)
        (let ((process
               (make-process
                :name "gomuks-media" :buffer nil :connection-type 'pipe
                :command (list "curl" "--silent" "--show-error" "--fail"
                               "--location" "--output" path "--config" "-" url)
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal))
                    (let ((callbacks (gethash key gomuks--media-pending))
                          (ok (= (process-exit-status proc) 0)))
                      (remhash key gomuks--media-pending)
                      (if ok
                          (puthash key path gomuks--media-cache)
                        (delete-file path)
                        (puthash key t gomuks--media-failures)
                        (message "gomuks: attachment download failed"))
                      (dolist (fn callbacks)
                        (funcall fn (when ok path)))))))))
          ;; The auth token stays off the process command line.
          (process-send-string process
                               (if auth
                                   (format "header = \"Authorization: %s\"\n" auth)
                                 ""))
          (process-send-eof process)))))))

(defun gomuks--media-key (attachment)
  "Return the cache key for ATTACHMENT."
  (list gomuks-backend-url
        (plist-get attachment :mxc)
        (plist-get attachment :encrypted)))

(defun gomuks--cached-media (attachment)
  "Return ATTACHMENT's cached local path if the file still exists."
  (let* ((key (gomuks--media-key attachment))
         (path (gethash key gomuks--media-cache)))
    (if (and path (file-exists-p path))
        path
      (remhash key gomuks--media-cache)
      nil)))

(defun gomuks--maybe-preview-image (attachment id)
  "Fetch a small image ATTACHMENT and redraw room ID when ready."
  (let ((key (gomuks--media-key attachment))
        (size (plist-get attachment :size)))
    (when (and gomuks-inline-images (display-images-p)
               (member (plist-get attachment :kind) '("m.image" "m.sticker"))
               (numberp size)
               (<= size gomuks-inline-image-max-bytes)
               (not (gomuks--cached-media attachment))
               (not (gethash key gomuks--media-pending))
               (not (gethash key gomuks--media-failures)))
      (gomuks--fetch-media
       attachment
       (lambda (path)
         (when path (gomuks--render-room id)))))))

(defun gomuks-save-attachment (&optional destination event)
  "Save the attachment in EVENT, or at point, to DESTINATION."
  (interactive)
  (let ((attachment (gomuks--attachment
                     (gomuks--event-content (or event (gomuks--event-at-point))))))
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
  (let* ((attachment (gomuks--attachment
                      (gomuks--event-content (or event (gomuks--event-at-point)))))
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
                (unless (executable-find "mpv")
                  (user-error "mpv is required to play audio attachments"))
                (start-process "gomuks-audio" nil "mpv" "--no-video" "--" path)))))
          ((or (equal kind "m.video")
               (and mime (string-prefix-p "video/" mime)))
           (browse-url-default-browser (browse-url-file-url path)))
          (t (pop-to-buffer (find-file-noselect path)))))))))


(provide 'gomuks-media)
;;; gomuks-media.el ends here
