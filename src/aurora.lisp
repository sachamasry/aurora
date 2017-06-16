(in-package :cl-user)
(defpackage aurora
  (:use :cl :aurora/filesystem-interface :aurora/string-manipulation)
  (:import-from :uiop
   :run-program)
  (:import-from :alexandria
   :when-let))
(in-package :aurora)

(defvar *ffmpeg-binary* ()
  "Store the absolute location of the ffmpeg binary for the duration of the session. If the binary doesn't exist, this will be set to NIL.")

#|
COMMAND EXECUTION

Build a call to a system binary, using the binary name
and any number of arguments passed in.
|#
(defun execute-command (program &rest arguments)
  "Execute binary specified by COMMAND, with any arguments passed
in as ARGUMENTS.

Return 3 values:
0: the result of the command's OUTPUT or NIL on non-zero exit status
1: the ERROR-OUTPUT string, or NIL
2: the actual EXIT-CODE of the process

Always directly execute the command, rather than calling a shell,
returning the entire output stream as a string stripped of any
newlines."
  (let ((command (or
                  (when arguments
                    (append (list program) arguments))
                  program)))
    (unless (zerop (length command))
      (multiple-value-bind (output error-output exit-status)
          (run-program command
                       :ignore-error-status t
                       :output '(:string :stripped t)
                       :error-output '(:string :stripped t)
                       :force-shell NIL)
        (values (or (and (zerop exit-status)
                         output)
                    NIL)
                error-output exit-status)))))

#|
Define a getter for the ffmpeg binary
|#

(defun find-ffmpeg ()
  "Set *FFMPEG-BINARY* to the location of the program binary,
or leave as NIL if not found."
  (unless *ffmpeg-binary*
    (setf *ffmpeg-binary* (execute-command "which" "ffmpeg")))
  *ffmpeg-binary*)

#|
METADATA CONSTRUCTION

Prepare all aspects of video conversion:
- metadata
- global options
- local, per-format options
- per-file options
|#
(defvar *metadata-key-list*
    '(title date copyright artist album-artist author
      composer publisher album comment synopsis
      description content-type
      genre make model location grouping show
      episode-id episode-sort season lyrics language
      compilation network media-type hd-video
      gapless-playback)
  "Define full list of accepted metatag KEYs.")

(defun get-metadata-key-list ()
  ""
  *metadata-key-list*)

(defmacro construct-metadata-plist-builder (metadata-key-list)
  "Construct metadata builder, from list of all accepted metatag
KEYs.

This macro defines a new function BUILD-METADATA-LIST, of the form:

(defun construct-metadata-list (&key title date ...)
  (mapcan #'(lambda (sublist) sublist)
    (list (when title (list :title title))
          (when date (list :date date))
          ...)))

which is a simple function prompting the user with all the legal values,
each of which, when supplied, is converted to a property list.
MAPCAN then takes each non-nil property list to create a broad, single-level
superlist."
  `(defun construct-metadata-plist ,(append '(&key) metadata-key-list) 
     (mapcan #'(lambda (sublist) sublist)
                (list
                 ,@(loop for i in metadata-key-list
                         collecting (list 'when i
                                          (list 'list
                                                (intern (string-upcase i) "KEYWORD")
                                                i)))))))

(defmacro construct-metadata-ffmpeg-args-builder (metadata-key-list)
  `(defun construct-metadata-ffmpeg-args-list ,(append '(&key) metadata-key-list)
     (remove-if 'null
                (list
                 ,@(loop for i in metadata-key-list
                         collecting (list 'when i 
                                          `(apply
                                            'concatenate
                                            (list 'string "-metadata "
                                                  ,(string->snake-case
                                                    (string-downcase
                                                     (symbol-name i)))
                                                  "="
                                                  (funcall 'string-escape ,i)))))))))

(construct-metadata-plist-builder (get-metadata-key-list))
(construct-metadata-ffmpeg-args-builder (get-metadata-key-list))

#|
FFMPEG PROFILES

Create a set of commonly used options, enabling quick generation of similarly-encoded
video files
|#
(defvar *global-ffmpeg-options*
  '("-y"                                ; Overwrite output without asking
    )
  "Set global ffmpeg options.")

(defvar *ffmpeg-web-video-sharing-hq-profile*
  '(:profile-name "Web video-sharing H264, high-quality"
    :profile-description "For use on video-sharing web-sites, H.264-encoded, full resolution, high quality (near-lossless)"
    :file-name-profile-suffix "VHQ"
    :file-name-extension "mp4"
    :profile-options
    (;; Use the libx264 codec
     "-c:v libx264"
     
     ;; Set output to 8 bits per pixel, 4:2:0 chroma subsampling
     "-pix_fmt yuv420p"                  

     ;; An optional setting which limits the output to a specific H.264 profile.
     ;; Current profiles include: baseline, main, high, high10, high422, high444
     "-profile:v high"

     ;; A baseline profile with a level of 3.0 disables some advanced features to
     ;; provide better device compatibility
     "-level 3.0"

     ;; Constant rate factor in a scale of 0-51, where 0 is lossless, 51 worst possible,
     ;; and the default is 23
     "-crf 17"

     ;; A preset is a collection of options providing a certain speed to compression
     ;; ratio. A slower preset will achieve better quality and comression. Presets in
     ;; descending order are:  ultrafast,superfast, veryfast, faster, fast, medium,
     ;; slow, slower, veryslow, placebo. Medium is default
     "-preset veryslow"

     ;; Use the native (not linked) AAC encoder for audio
     "-c:a aac"

     ;; Convert the audio stream to a bitrate of 192k
     "-b:a 320k"

     ;; When the output video is going to be viewed on a browser, this output option
     ;; moves some header information to the beginning of the file, allowing the
     ;; browser to start playing the file even before it's completely downloaded
     "-movflags +faststart"))
  "Define an ffmpeg conversion profile for video-sharing web sites, using the H.264 encoder.")

(defvar *ffmpeg-web-h264-profile*
  '(:profile-name "Web H264 1080p medium"
    :profile-description "For website display, H.264-encoded, 1080 wide, progressive medium quality"
    :file-name-profile-suffix "1080"
    :file-name-extension "mp4"
    :profile-options
    ("-vf scale=1080:-2"                 ; Scale to 1080 pixels horizontally

     ;; Use the libx264 codec
     "-c:v libx264"
     
     ;; Set output to 8 bits per pixel, 4:2:0 chroma subsampling
     "-pix_fmt yuv420p"                  

     ;; An optional setting which limits the output to a specific H.264 profile.
     ;; Current profiles include: baseline, main, high, high10, high422, high444
     "-profile:v baseline"

     ;; A baseline profile with a level of 3.0 disables some advanced features to
     ;; provide better device compatibility
     "-level 3.0"

     ;; Constant rate factor in a scale of 0-51, where 0 is lossless, 51 worst possible,
     ;; and the default is 23
     "-crf 23"

     ;; A preset is a collection of options providing a certain speed to compression
     ;; ratio. A slower preset will achieve better quality and comression. Presets in
     ;; descending order are:  ultrafast,superfast, veryfast, faster, fast, medium,
     ;; slow, slower, veryslow, placebo. Medium is default
     "-preset veryslow"

     ;; Use the native (not linked) AAC encoder for audio
     "-c:a aac"

     ;; Convert the audio stream to a bitrate of 192k
     "-b:a 192k"

     ;; When the output video is going to be viewed on a browser, this output option
     ;; moves some header information to the beginning of the file, allowing the
     ;; browser to start playing the file even before it's completely downloaded
     "-movflags +faststart"))
  "Define an ffmpeg conversion profile for on web-site use, using the H.264 encoder.")

(defvar *ffmpeg-web-vp8-webm-profile*
  '(:profile-name "Web WebM VP8 1080p medium"
    :profile-description "For website display, WebM-encoded (VP8), 1080 wide, progressive medium quality"
    :file-name-profile-suffix "1080"
    :file-name-extension "webm"
    :profile-options
    ("-vf scale=1080:-2"                 ; Scale to 1080 pixels horizontally

     ;; Use the libvpx codec
     "-c:v libvpx"
     
     ;; Set output to 8 bits per pixel, 4:2:0 chroma subsampling
     "-pix_fmt yuv420p"                  

     ;; To tweak the quality more finely, Q, or quantisation parameters can be
     ;; supplied in the form of -qmin and -qmax arguments, with a range of values
     ;; from 0-63, with 0 as best and 63 as worst
     "-qmin 0 -qmax 25"

     ;; Constant rate factor in a scale of 4-63, where 4 is best, 63 worst possible
     "-crf 4"

     ;; Supply an additional 'target' variable bit rate for the encoder to try
     ;; and reach (in Mbit/s)
     "-b:v 1M"

     ;; Use the libvorbis codec for audio
     "-c:a libvorbis"

     ;; Set the audio quality level in a range of 0-10 where 10 is highest,
     ;; and 0 is lowest. 3-6 is a good range, the default is 3
     "-q:a 7"))
  "Define an ffmpeg conversion profile for on web-site use, using the VP8 WebM encoder.")

(defvar *ffmpeg-web-theora-vorbis-profile*
  '(:profile-name "Web Theora-Vorbis 1080p medium"
    :profile-description "For website display, Theora/Vorbis-encoded, 1080 wide, progressive medium quality"
    :file-name-profile-suffix "1080"
    :file-name-extension "ogv"
    :profile-options
    ("-vf scale=1080:-2"                 ; Scale to 1080 pixels horizontally

     ;; Use the libtheora codec for video
     "-c:v libtheora"
     
     ;; Set output to 8 bits per pixel, 4:2:0 chroma subsampling
     "-pix_fmt yuv420p"                  

     ;; Video quality constant rate factor in a scale of 0-10, where 10 is highest
     ;; quality, 0 worst, with 5-7 a good range
     "-q:v 6"

     ;; A preset is a collection of options providing a certain speed to compression
     ;; ratio. A slower preset will achieve better quality and comression. Presets in
     ;; descending order are:  ultrafast,superfast, veryfast, faster, fast, medium,
     ;; slow, slower, veryslow, placebo. Medium is default
     "-preset veryslow"

     ;; Use the libvorbis codec for audio
     "-c:a libvorbis"

     ;; Audio quality constant rate factor in a range of 0-10, where 10 is highest
     ;; quality, 0 worst, with 3-6 a good range
     "-q:a 7"))
  "Define an ffmpeg conversion profile for on web-site use, using the Theora/Vorbis
encoders.")

(defvar *ffmpeg-web-profiles*
  '(*ffmpeg-web-h264-profile* *ffmpeg-web-vp8-webm-profile*
    *ffmpeg-web-theora-vorbis-profile*)
  "Define list of all profiles needed for a web export.")

#|
VIDEO ENCODING

Functions which actually do the work of compiling a string of binary, arguments and
other options, needed for the command-line
|#
(defun run-conversion (input-file
                       &key output-file-location output-file-name
                         output-file-prefix output-file-suffix
                         output-file-extension (separator "-")
                         profile metadata-list simulate-only)
  "Converts INPUT-FILE video to output."
  (when (and input-file (probe-file input-file))
    (let ((command-to-execute
            (join
             (append
              (list (find-ffmpeg))
              *global-ffmpeg-options*
              (list "-i" (string-escape
                          (namestring
                           (probe-file input-file))))
              (getf profile :profile-options)
              metadata-list
              (list
               (string-escape 
                (namestring
                 (construct-destination-path
                  :source-path input-file
                  :destination-directory output-file-location
                  :destination-file-name output-file-name
                  :destination-file-prefix output-file-prefix
                  :destination-file-suffix (or output-file-suffix
                                              (getf profile :file-name-profile-suffix))
                  :destination-file-extension (or output-file-extension
                                                  (getf profile :file-name-extension))
                  :separator separator)))))
             :separator " ")))
      (if simulate-only
          command-to-execute
          (execute-command command-to-execute)))))

(defun run-batch-conversion (input-file
                             &key output-file-location output-file-name
                               output-file-prefix output-file-suffix
                               (separator "-") profile-list
                               metadata-list simulate-only)
  "Batch convert INPUT-FILE to each output profile specified in
PROFILE-LIST"
  (mapcar #'(lambda (profile)
              (run-conversion input-file
                              :output-file-location output-file-location
                              :output-file-prefix output-file-prefix
                              :output-file-name output-file-name
                              :output-file-suffix output-file-suffix
                              :separator separator
                              :profile (eval profile)
                              :metadata-list metadata-list
                              :simulate-only simulate-only))
          profile-list))
