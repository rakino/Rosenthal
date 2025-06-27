;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal services desktop)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (rosenthal utils file)
  #:use-module (rosenthal utils packages)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm)
  #:use-module (gnu services xorg)
  #:use-module (rosenthal services base)

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)

  #:use-module (rosenthal packages xorg)

  #:export (home-blueman-applet-configuration
            home-blueman-applet-service-type

            home-fcitx5-configuration
            home-fcitx5-service-type

            home-mako-configuration
            home-mako-service-type

            home-network-manager-applet-configuration
            home-network-manager-applet-service-type

            home-niri-configuration
            home-niri-service-type

            home-rofi-configuration
            home-rofi-service-type

            home-swaybg-configuration
            home-swaybg-service-type

            home-theme-configuration
            home-theme-service-type

            home-waybar-configuration
            home-waybar-service-type

            %rosenthal-set-keymap-script
            %rosenthal-skeletons
            %rosenthal-desktop-services
            %rosenthal-desktop-home-services))


;;;
;;; Blueman
;;;

(define-configuration/no-serialization home-blueman-applet-configuration
  (blueman
   (file-like (spec->pkg+out "blueman"))
   ""))

(define %home-blueman-applet-shepherd
  (match-record-lambda <home-blueman-applet-configuration>
      (blueman)
    (list (shepherd-service
            (documentation "Start blueman applet.")
            (provision '(blueman-applet))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append blueman "/bin/blueman-applet"))))
            (stop #~(make-kill-destructor))))))

(define home-blueman-applet-service-type
  (service-type
    (name 'blueman-applet)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-blueman-applet-configuration-blueman))
           (service-extension home-shepherd-service-type
                              %home-blueman-applet-shepherd)))
    (default-value (home-blueman-applet-configuration))
    (description "Run blueman applet, a tray applet for managing bluetooth.")))


;;;
;;; Fcitx5
;;;

(define list-of-file-likes?
  (list-of file-like?))
(define-maybe file-like)

(define-configuration/no-serialization home-fcitx5-configuration
  (fcitx5
   (file-like (spec->pkg   "fcitx5"))
   "")
  (utilities
   (list-of-file-likes (specs->pkgs "fcitx5-configtool"))
   "")
  (themes
   (list-of-file-likes '())
   "")
  (input-method-editors
   (list-of-file-likes '())
   "")
  (gtk-im-module?
   (boolean #f)
   "")
  (qt-im-module?
   (boolean #f)
   "")
  (xim?
   (boolean #t)
   ""))

(define %home-fcitx5-environment-variables
  (match-record-lambda <home-fcitx5-configuration>
      (gtk-im-module? qt-im-module? xim?)
    `(,@(if gtk-im-module?
            '(("GTK_IM_MODULE" . "fcitx"))
            '())
      ,@(if qt-im-module?
            '(("QT_IM_MODULE" . "fcitx"))
            '())
      ,@(if xim?
            '(("XMODIFIERS" . "@im=fcitx"))
            '()))))

(define %home-fcitx5-profile
  (match-record-lambda <home-fcitx5-configuration>
      (fcitx5 utilities themes input-method-editors gtk-im-module? qt-im-module?)
    (append (list fcitx5)
            utilities
            themes
            input-method-editors
            (if gtk-im-module?
                (list (spec->pkg "fcitx5-gtk"))
                '())
            (if qt-im-module?
                (list (spec->pkg "fcitx5-qt"))
                '()))))

(define %home-fcitx5-shepherd
  (match-record-lambda <home-fcitx5-configuration>
      (fcitx5)
    (list (shepherd-service
            (documentation "Start fcitx5.")
            (provision '(fcitx5))
            (requirement '(dbus))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append fcitx5 "/bin/fcitx5"))))
            (stop #~(make-kill-destructor))))))

(define home-fcitx5-service-type
  (service-type
    (name 'fcitx5)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              %home-fcitx5-environment-variables)
           (service-extension home-profile-service-type
                              %home-fcitx5-profile)
           (service-extension home-shepherd-service-type
                              %home-fcitx5-shepherd)))
    (default-value (home-fcitx5-configuration))
    (description "Run fcitx5, an input method framework.")))


;;;
;;; mako
;;;

(define-configuration/no-serialization home-mako-configuration
  (mako
   (file-like (spec->pkg "mako"))
   "")
  (config
   maybe-file-like
   "")
  )

(define %home-mako-xdg-config
  (match-record-lambda <home-mako-configuration>
      (config)
    (if (maybe-value-set? config)
        `(("mako/config" ,config))
        '())))

(define %home-mako-shepherd
  (match-record-lambda <home-mako-configuration>
      (mako)
    (list (shepherd-service
            (documentation "Start mako.")
            (provision '(mako))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append mako "/bin/mako"))))
            (stop #~(make-kill-destructor))))))

(define home-mako-service-type
  (service-type
    (name 'mako)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-mako-xdg-config)
           (service-extension home-shepherd-service-type
                              %home-mako-shepherd)))
    (default-value (home-mako-configuration))
    (description "Run mako, a notification daemon.")))


;;;
;;; network-manager-applet
;;;

(define-configuration/no-serialization home-network-manager-applet-configuration
  (network-manager-applet
   (file-like (spec->pkg "network-manager-applet"))
   ""))

(define %home-network-manager-applet-shepherd
  (match-record-lambda <home-network-manager-applet-configuration>
      (network-manager-applet)
    (list (shepherd-service
            (documentation "Start network manager applet.")
            (provision '(network-manager-applet))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append network-manager-applet "/bin/nm-applet"))))
            (stop #~(make-kill-destructor))))))

(define home-network-manager-applet-service-type
  (service-type
    (name 'network-manager-applet)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-network-manager-applet-configuration-network-manager-applet))
           (service-extension home-shepherd-service-type
                              %home-network-manager-applet-shepherd)))
    (default-value (home-network-manager-applet-configuration))
    (description "Run nm-applet, a tray applet for managing networks.")))


;;;
;;; niri
;;;

(define-configuration/no-serialization home-niri-configuration
  (config
   maybe-file-like
   ""))

(define %home-niri-xdg-config
  (match-record-lambda <home-niri-configuration>
      (config)
    (if (maybe-value-set? config)
        `(("niri/config.kdl" ,config))
        '())))

(define home-niri-service-type
  (service-type
    (name 'niri)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-niri-xdg-config)))
    (default-value (home-niri-configuration))
    (description
     "Set up configuration file for niri, a scrollable-tiling Wayland
compositor.")))


;;;
;;; rofi
;;;

(define-configuration/no-serialization home-rofi-configuration
  (config
   maybe-file-like
   ""))

(define %home-rofi-xdg-config
  (match-record-lambda <home-rofi-configuration>
      (config)
    (if (maybe-value-set? config)
        `(("rofi/config.rasi" ,config))
        '())))

(define home-rofi-service-type
  (service-type
    (name 'rofi)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-rofi-xdg-config)))
    (default-value (home-rofi-configuration))
    (description
     "Set up configuration file for rofi, an application launcher.")))


;;;
;;; swaybg
;;;

(define-configuration/no-serialization home-swaybg-configuration
  (swaybg
   (file-like (spec->pkg "swaybg"))
   "")
  (background
   (file-like (local-file "../examples/wallpaper.jpg"))
   ""))

(define %home-swaybg-shepherd
  (match-record-lambda <home-swaybg-configuration>
      (swaybg background)
    (list (shepherd-service
            (documentation "Start swaybg.")
            (provision '(swaybg))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append swaybg "/bin/swaybg") "--mode" "fill"
                      "--image" #$background)))
            (stop #~(make-kill-destructor))))))

(define home-swaybg-service-type
  (service-type
    (name 'swaybg)
    (extensions
     (list (service-extension home-shepherd-service-type
                              %home-swaybg-shepherd)))
    (default-value (home-swaybg-configuration))
    (description
     "Run swaybg, a screen wallpaper utility for Wayland compositors.")))


;;;
;;; theme
;;;

(define-configuration/no-serialization home-theme-configuration
  (packages
   (list-of-file-likes '())
   "")
  (icon-theme
   (string "Adwaita")
   "")
  (cursor-theme
   (string "Adwaita")
   "")
  (cursor-size
   (number 24)
   "")
  (key-theme
   (string "Default")
   ""))

(define (%home-theme-environment-variables _)
  '(("QT_QPA_PLATFORMTHEME" . "gtk3")
    ("QT_WAYLAND_DECORATION" . "adwaita")))

(define %home-theme-profile
  (match-record-lambda <home-theme-configuration>
      (packages)
    (append (specs->pkgs "adwaita-icon-theme"
                         "hicolor-icon-theme"
                         "qtwayland")
            packages)))

(define %home-theme-files
  (match-record-lambda <home-theme-configuration>
      (icon-theme)
    `((".icons/default/index.theme"
       ,(plain-file "index.theme"
          (format #f "\
[icon theme]
Inherits = ~a~%"
                  icon-theme))))))

(define %home-theme-xdg-config
  (match-record-lambda <home-theme-configuration>
      (icon-theme cursor-theme cursor-size key-theme)
    `(("gtk-3.0/settings.ini"
       ,(plain-file "settings.ini"
          (format #f "\
[Settings]
gtk-theme-name = Adwaita
gtk-icon-theme-name = ~a
gtk-font-name = Sans
gtk-cursor-theme-name = ~a
gtk-cursor-theme-size = ~a
gtk-key-theme-name = ~a~%"
                  icon-theme cursor-theme cursor-size key-theme))))))

(define home-theme-service-type
  (service-type
    (name 'theme)
    (extensions
     (list (service-extension home-environment-variables-service-type
                              %home-theme-environment-variables)
           (service-extension home-profile-service-type
                              %home-theme-profile)
           (service-extension home-files-service-type
                              %home-theme-files)
           (service-extension home-xdg-configuration-files-service-type
                              %home-theme-xdg-config)))
    (default-value (home-theme-configuration))
    (description "Set up desktop themes.")))


;;;
;;; waybar
;;;

(define-configuration/no-serialization home-waybar-configuration
  (waybar
   (file-like (spec->pkg "waybar"))
   "")
  (config
   maybe-file-like
   "")
  (style
   maybe-file-like
   ""))

(define %home-waybar-xdg-config
  (match-record-lambda <home-waybar-configuration>
      (config style)
    `(,@(if (maybe-value-set? config)
            `(("waybar/config.jsonc" ,config))
            '())
      ,@(if (maybe-value-set? style)
            `(("waybar/style.css" ,style))
            '()))))

(define %home-waybar-shepherd
  (match-record-lambda <home-waybar-configuration>
      (waybar)
    (list (shepherd-service
            (documentation "Start waybar.")
            (provision '(waybar))
            (start
             #~(make-forkexec-constructor
                (list #$(file-append waybar "/bin/waybar"))))
            (stop #~(make-kill-destructor))))))

(define home-waybar-service-type
  (service-type
    (name 'waybar)
    (extensions
     (list (service-extension home-xdg-configuration-files-service-type
                              %home-waybar-xdg-config)
           (service-extension home-shepherd-service-type
                              %home-waybar-shepherd)))
    (default-value (home-waybar-configuration))
    (description "Run waybar, a status bar for Wayland compositors.")))


;;;
;;; Configuration file presets.
;;;

(define %rosenthal-set-keymap-script
  (program-file "set-keymap"
    (with-imported-modules (source-module-closure '((guix build utils)))
      #~(begin
          (use-modules (srfi srfi-1)
                       (srfi srfi-26)
                       (srfi srfi-37)
                       (ice-9 match)
                       (ice-9 popen)
                       (guix build utils))

          (define* (build-keyboard-layout file layout #:optional variant #:key model options)
            (define pipe
              (apply open-pipe* OPEN_READ
                     #$(file-append (spec->pkg "console-setup") "/bin/ckbcomp")
                     (string-append "-I" #$(spec->pkg "xkeyboard-config") "/share/X11/xkb")
                     "-rules" "base"
                     `(,@(if model
                             '("-model" ,model)
                             '())
                       ,layout
                       ,(or variant "")
                       ,(string-join options ","))))
            (mkdir-p (dirname file))
            (call-with-output-file file
              (lambda (output)
                (dump-port pipe output))))

          (define* (set-keyboard-layout layout #:optional variant #:key model options)
            (define file-name "/tmp/keymaps/console-keymap")
            (build-keyboard-layout file-name layout variant #:model model #:options options)
            (invoke "sudo" #$(file-append (spec->pkg "kbd") "/bin/loadkeys") file-name)
            (when (getenv "WAYLAND_DISPLAY")
              (substitute* (in-vicinity (getenv "XDG_CONFIG_HOME") "niri/config.kdl")
                (("^            (layout|variant|model|options) .*") "")
                (("^        xkb \\{.*" line)
                 (string-append
                  line
                  (format             #f "            layout ~s~%"  layout)
                  (if variant (format #f "            variant ~s~%" variant) "")
                  (if model   (format #f "            model ~s~%"   model)   "")
                  (format             #f "            options ~s~%" (string-join options ",")))))))

          (define (show-help-and-exit)
            (display "\
Usage: set-keymap LAYOUT [VARIANT] [-m MODEL] [-o OPTIONS]

OPTIONS are comma-separated e.g. \"ctrl:nocaps,grp:alt_shift_toggle\"

Examples:
set-keymap us
set-keymap us dvorak
set-keymap us dvorak -o ctrl:nocaps\n")
            (quit))

          (define (parse-options)
            (args-fold (cdr (program-arguments))
                       (list (option '(#\m "model") #t #f
                                     (lambda (opt name arg result)
                                       (alist-cons 'model arg result)))
                             (option '(#\o "options") #t #f
                                     (lambda (opt name arg result)
                                       (alist-cons 'options (string-split arg #\,)
                                                   result)))
                             (option '("help") #f #f
                                     (lambda _
                                       (show-help-and-exit))))
                       (lambda (opt name arg loads)
                         (error "Unrecognized option `~A'" name))
                       (lambda (opt loads) (cons opt loads))
                       '()))

          (let* ((opts (parse-options))
                 (model (assoc-ref opts 'model))
                 (options (or (assoc-ref opts 'options) '()))
                 (args (remove pair? (reverse opts))))
            (match args
              ((layout)
               (set-keyboard-layout layout #:model model #:options options))
              ((layout variant)
               (set-keyboard-layout layout variant #:model model #:options options))
              (_
               (show-help-and-exit))))))))

(define %rosenthal-skeletons
  `((".config/emacs/fonts.el"
     ,(local-file "../examples/emacs/fonts.el"))
    (".config/emacs/init.el"
     ,(local-file "../examples/emacs/init.el"))
    (".config/foot/foot.ini"
     ,(plain-file "foot.ini" "font=monospace:size=12\n"))
    (".config/mako/config"
     ,(local-file "../examples/mako.conf"))
    (".config/niri/config.kdl"
     ,(local-file "../examples/niri.kdl"))
    (".config/rofi/config.rasi"
     ,(plain-file "rofi.rasi" "\
configuration {
    icon-theme: \"Qogir\";
}
@theme \"/run/current-system/profile/share/rofi/themes/fullscreen-preview.rasi\"\n"))
    (".config/waybar/config.jsonc"
     ,(local-file "../examples/waybar/config.jsonc"))
    (".config/waybar/style.css"
     ,(local-file "../examples/waybar/style.css"))
    (".config/xfce4/helpers.rc"
     ,(plain-file "helpers.rc" "TerminalEmulator=rofi-sensible-terminal\n"))
    ;; Selected from the default skeletons.
    (".config/gdb/gdbinit" ,%default-gdbinit)
    (".config/nano/nanorc" ,%default-nanorc)
    (".guile" ,%default-dotguile)))


;;;
;;; Service presets.
;;;

(define* (rosenthal-desktop-services-for-system
          #:optional (system (or (%current-target-system)
                                 (%current-system))))
  (define %display-manager-service-type
    (if (string-prefix? "x86_64" system)
        gdm-service-type
        sddm-service-type))

  (define %term-font
    (file-append (spec->pkg "font-terminus") "/share/consolefonts/ter-132n"))

  (cons* (service greetd-service-type
           (greetd-configuration
             (greeter-supplementary-groups '("video" "input"))
             (terminals
              (map (lambda (x)
                     (greetd-terminal-configuration
                       (terminal-vt (number->string x))
                       (terminal-switch (eqv? 1 x))
                       (default-session-command
                         (cond
                          ((eqv? 1 x)
                           (greetd-tuigreet-session))
                          (else
                           (greetd-agreety-session
                             (command
                              (greetd-user-session
                                (command #~(getenv "SHELL"))))))))))
                   (iota 6 1)))))

         (service bluetooth-service-type
           (bluetooth-configuration
             (auto-enable? #t)))

         (service gvfs-service-type)

         ;; Screen lockers for Wayland environment.  No dependencies are pulled
         ;; in since we're using empty files.
         (service screen-locker-service-type
           (screen-locker-configuration
             (name "swaylock")
             (program (plain-file "empty" "")) ;Not used.
             (using-setuid? #f)))
         (service screen-locker-service-type
           (screen-locker-configuration
             (name "waylock")
             (program (plain-file "empty" "")) ;Not used.
             (using-setuid? #f)))

         ;; Add udev rules for backlight control.
         (simple-service 'backlight udev-service-type (specs->pkgs "light"))

         (modify-services %desktop-services
           (delete mingetty-service-type)
           (delete %display-manager-service-type)
           (delete screen-locker-service-type)

           ;; Use a font suitable for HiDPI monitors.
           (console-font-service-type
            _ => (map (lambda (num)
                        (cons (string-append "tty" (number->string num))
                              %term-font))
                      (iota 6 1))))))

(define-syntax %rosenthal-desktop-services
  (identifier-syntax (rosenthal-desktop-services-for-system)))

(define %rosenthal-desktop-home-services
  (cons* (service home-shepherd-service-type
           (home-shepherd-configuration
             ;; Start by WM to inherit environment variables for graphical session.
             (auto-start? #f)
             (daemonize? #f)))

         (service home-dbus-service-type)
         (service home-pipewire-service-type)

         (service home-blueman-applet-service-type)
         (service home-network-manager-applet-service-type)

         %base-home-services))
