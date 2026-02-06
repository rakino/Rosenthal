;;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services networking)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (rosenthal packages networking)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (rosenthal utils predicates)
  #:export (sing-box-service-type
            sing-box-configuration

            tailscale-configuration
            tailscale-service-type))

;;;
;;; sing-box
;;;

(define-configuration/no-serialization sing-box-configuration
  (sing-box
   (file-like sing-box)
   "")
  (config-file
   file-object
   "")
  (data-directory
   (string "/var/lib/sing-box")
   "")
  ;; Account
  (group-id
   (user-and-group-id #f)
   "")
  ;; Shepherd
  (shepherd-provision
   (list-of-symbols '(sing-box))
   "")
  (shepherd-requirement
   (list-of-symbols '(networking))
   "")
  (log-file
   (string "/var/log/sing-box.log")
   "")
  (auto-start?
   (boolean #t)
   ""))

(define sing-box-account
  (match-record-lambda <sing-box-configuration>
      (group-id)
    (list (user-group
            (name "sing-box")
            (id group-id)
            (system? #t)))))

(define sing-box-activation
  (match-record-lambda <sing-box-configuration>
      (data-directory)
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$data-directory))))

(define sing-box-shepherd-service
  (match-record-lambda <sing-box-configuration>
      (sing-box data-directory config-file
       shepherd-provision shepherd-requirement log-file auto-start?)
    (list (shepherd-service
            (provision shepherd-provision)
            (requirement `(user-processes ,@shepherd-requirement))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append sing-box "/bin/sing-box")
                      "--config" #$config-file
                      "--directory" #$data-directory
                      "--disable-color"
                      "run")
                #:log-file #$log-file))
            (stop #~(make-kill-destructor))
            (auto-start? auto-start?)))))

(define sing-box-service-type
  (service-type
    (name 'sing-box)
    (extensions
     (list (service-extension account-service-type
                              sing-box-account)
           (service-extension activation-service-type
                              sing-box-activation)
           (service-extension shepherd-root-service-type
                              sing-box-shepherd-service)))
    (description "")))


;;;
;;; Tailscale
;;;


(define-configuration tailscale-configuration
  (tailscale
   (file-like tailscale)
   "The tailscale package to use.")

  (log-file
   (string "/var/log/tailscaled.log")
   "Path to log file.")

  (socket
   (string "/var/run/tailscale/tailscaled.sock")
   "Path of the service UNIX socket.")

  (state-directory
   (string "/var/lib/tailscale")
   "Path to directory for storage of config state, TLS certs, temporary incoming
Taildrop files, etc.  If empty, it's derived from @code{state-file} when
possible.")

  (upload-log?
   (boolean #f)
   "Whether to upload logs or not, technical support is also disabled when set
to #f.")

  (verbosity
   (integer 0)
   "Log verbosity level; 0 is default, 1 or higher are increasingly verbose.")

  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define tailscale-shepherd-service
  (match-record-lambda <tailscale-configuration>
      (tailscale log-file socket state-directory
                 upload-log? verbosity extra-options)
    (list (shepherd-service
            (documentation "Run tailscaled")
            (provision '(tailscaled))
            (requirement '(user-processes))
            (start
             #~(make-forkexec-constructor
                (list
                 #$(file-append tailscale "/bin/tailscaled")
                 #$@(if upload-log?
                        '()
                        '("-no-logs-no-support"))
                 "-socket" #$socket
                 "-statedir" #$state-directory
                 "-verbose" #$(number->string verbosity)
                 #$@extra-options)
                #:log-file #$log-file))
            (stop #~(make-kill-destructor))))))

(define tailscale-service-type
  (service-type
   (name 'tailscaled)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscale-shepherd-service)
          (service-extension profile-service-type
                             (compose list tailscale-configuration-tailscale))))
   (default-value (tailscale-configuration))
   (description "Run tailscaled.")))
