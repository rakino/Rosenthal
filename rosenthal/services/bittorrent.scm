;; SPDX-FileCopyrightText: 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services bittorrent)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:export (qbittorrent-configuration
            qbittorrent-service-type))

;;
;; qBittorrent
;;


(define-configuration qbittorrent-configuration
  (qbittorrent
   (file-like qbittorrent-nox)
   "The qBittorrent package to use, we need @command{qbittorrent-nox}.")
  (webui-port
   (integer 8080)
   "Change the Web UI port.")
  (profile-directory
   (string "/var/lib/qbittorrent")
   "Directory to store configuration files in.")
  (extra-options
   (list-of-strings '())
   "List of extra options.")
  (no-serialization))

(define %qbittorrent-accounts
  (list (user-group (name "qbittorrent") (system? #t))
        (user-account
         (name "qbittorrent")
         (group "qbittorrent")
         (system? #t)
         (comment "qBittorrent user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define qbittorrent-activation
  (match-record-lambda <qbittorrent-configuration>
      (qbittorrent webui-port profile-directory extra-options)
    #~(begin
        (use-modules (guix build utils))
        (let ((profile-directory #$profile-directory)
              (user (getpwnam "qbittorrent")))
          (mkdir-p profile-directory)
          (chown profile-directory (passwd:uid user) (passwd:gid user))))))

(define qbittorrent-shepherd-service
  (match-record-lambda <qbittorrent-configuration>
      (qbittorrent webui-port profile-directory extra-options)
    (list (shepherd-service
           (documentation "Run qbittorrent.")
           (provision '(qbittorrent))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append qbittorrent "/bin/qbittorrent-nox")
                           #$(string-append "--webui-port="
                                            (number->string webui-port))
                           #$(string-append "--profile=" profile-directory)
                           #$@extra-options)
                     #:user "qbittorrent"
                     #:group "qbittorrent"
                     #:resource-limits '((nofile 65536 65536))))
           (stop #~(make-kill-destructor #:grace-period 1800))))))

(define qbittorrent-service-type
  (service-type
   (name 'qbittorrent)
   (extensions
    (list (service-extension shepherd-root-service-type
                             qbittorrent-shepherd-service)
          (service-extension activation-service-type
                             qbittorrent-activation)
          (service-extension account-service-type
                             (const %qbittorrent-accounts))))
   (default-value (qbittorrent-configuration))
   (description "Run qBittorrent daemon.")))
