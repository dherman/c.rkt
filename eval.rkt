#lang racket/base

(require racket/file
         racket/system
         racket/match)

(provide current-gcc gcc (struct-out exn:fail:process))

;; (parameterof path)
(define current-gcc (make-parameter
                     (or (find-executable-path "gcc")
                         (find-executable-path "gcc.exe"))))

(define-struct (exn:fail:process exn:fail) (out error-out) #:transparent)

;; (-> void) -> (values input-port input-port)
(define (gcc print-source)
  (let ([gcc (current-gcc)])
    (unless (file-exists? gcc)
      (raise (exn:fail:filesystem (format "file not found: ~a" gcc)
                                  (current-continuation-marks))))
    (let ([ch (make-channel)])
      (let-values ([(stdout-read stdout-write) (make-pipe)]
                   [(stderr-read stderr-write) (make-pipe)])
        (thread
         (lambda ()
           (define executable #f)
           (dynamic-wind
            void
            (lambda ()
              (with-handlers ([exn? (lambda (exn) (channel-put ch exn))])
                (set! executable (make-temporary-file "mztmp~a.exe" #f (build-path 'same)))
                (match-let ([(list _ stdin pid _ control)
                             (process*/ports (current-output-port) #f stderr-write
                                             (current-gcc) "-x" "c" "-w" "-o" (path->string executable) "-")])
                  (parameterize ([current-output-port stdin])
                    (call-with-continuation-barrier print-source))
                  (flush-output stdin)
                  (close-output-port stdin)
                  (control 'wait)
                  (let ([exit-code (control 'exit-code)])
                    (unless (zero? exit-code)
                      (raise (exn:fail:process (format "process exited with code ~a" exit-code)
                                               (current-continuation-marks)
                                               stdout-read
                                               stderr-read)))))
                (match-let ([(list _ stdin pid _ control)
                             (process*/ports stdout-write #f stderr-write
                                             (path->string executable))])
                  (close-output-port stdin)
                  ;; Send go-ahead to parent thread.
                  (channel-put ch #t)
                  ;; Wait for program to run.
                  (control 'wait))))
            (lambda ()
              (with-handlers ([exn? void])
                (close-output-port stdout-write))
              (with-handlers ([exn? void])
                (close-output-port stderr-write))
              (when executable
                ;; Windows: wait for lock to free up.
                (sleep 3/10)
                (with-handlers ([exn? void])
                  (delete-file executable)))))))
        (let ([ack (channel-get ch)])
          (if (exn? ack)
              (raise ack)
              (values stdout-read stderr-read)))))))
