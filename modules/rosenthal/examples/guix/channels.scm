;; info "(guix) Channels"
;; https://guix.gnu.org/manual/devel/en/html_node/Channels.html

(list (channel
        (inherit %default-guix-channel)
        (name 'guix)
        (url "https://git.guix.gnu.org/guix.git"))

      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

      (channel
        (name 'rosenthal)
        (url "https://codeberg.org/hako/rosenthal.git")
        (branch "trunk")
        (introduction
         (make-channel-introduction
          "7677db76330121a901604dfbad19077893865f35"
          (openpgp-fingerprint
           "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7")))))
