;; -*- lexical-binding: t; -*-

(defun run-whisper-stt ()
  "Record audio and transcribe it using Whisper, inserting text at cursor position."
  (interactive)
  (let* ((original-buf (current-buffer))
         (original-point (point-marker))  ; Marker tracks position even if buffer changes
         (wav-file "/tmp/whisper-recording.wav")
         (temp-buf (generate-new-buffer " *Whisper Temp*")))

    ;; Start recording audio
    (start-process "record-audio" nil "/bin/sh" "-c"
                   (format "sox -d -r 16000 -c 1 -b 16 %s --no-show-progress 2>/dev/null" wav-file))
    ;; Inform user recording has started
    (message "Recording started. Press C-g to stop.")
    ;; Wait for user to stop (C-g)
    (condition-case nil
        (while t (sit-for 1))
      (quit (interrupt-process "record-audio")))

    ;; Run Whisper STT
    (let ((proc (start-process "whisper-stt" temp-buf "/bin/sh" "-c"
                               (format "~/whisper.cpp/build/bin/whisper-cli -m ~/whisper.cpp/models/ggml-base.en.bin -f %s -nt -np 2>/dev/null"
                                       wav-file))))
      ;; Properly capture `temp-buf` using a lambda
      (set-process-sentinel
       proc
       `(lambda (proc event)
          (when (string= event "finished\n")
            (when (buffer-live-p ,temp-buf)
              (let* ((output (string-trim (with-current-buffer ,temp-buf (buffer-string))))) ;; Trim excess whitespace
                (when (buffer-live-p ,original-buf)
                  (with-current-buffer ,original-buf
                    (goto-char ,original-point)
                    (insert output " ")  ;; Insert text with a single space after
                    (goto-char (point))))) ;; Move cursor to end of inserted text
              ;; Clean up temporary buffer
              (kill-buffer ,temp-buf))))))))

(global-set-key (kbd "C-c v") 'run-whisper-stt)

(provide 'my-whisper)
