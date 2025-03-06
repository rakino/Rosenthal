;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages rust-crates)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo))

;;;
;;; This file is managed by ‘guix import’.  DO NOT add definitions manually.
;;;

(define crate-name->package-name
  (lambda (name)
    (downstream-package-name "rust-" name)))

(define* (crate-source name version hash #:key (patches '()) (snippet #f))
  (origin
    (method url-fetch)
    (uri (crate-uri name version))
    (file-name (string-append (crate-name->package-name name) "-" version ".tar.gz"))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (patches patches)
    (snippet snippet)))


;;;
;;; crates
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-aead-0.5.2
  (crate-source "aead" "0.5.2"
                "1c32aviraqag7926xcb9sybdm36v5vh9gnxpn4pxdwjc50zl28ni"))

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aliasable-0.1.3
  (crate-source "aliasable" "0.1.3"
                "1z8548zdjlm4ps1k0d7x68lfdyji02crwcc9rw3q3bb106f643r5"))

(define rust-aligned-vec-0.5.0
  (crate-source "aligned-vec" "0.5.0"
                "1lb8qjqfap028ylf8zap6rkwrnrqimc3v6h3cixycjrdx1y0vaaa"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-android-activity-0.6.0
  (crate-source "android-activity" "0.6.0"
                "0inh88x8x2fh62jg739s9hwyvdh8i920qf0qw7bhr802j9c7hsgg"))

(define rust-android-properties-0.2.2
  (crate-source "android-properties" "0.2.2"
                "016slvg269c0y120p9qd8vdfqa2jbw4j0g18gfw6p3ain44v4zpw"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-annotate-snippets-0.9.2
  (crate-source "annotate-snippets" "0.9.2"
                "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-wincon-3.0.7
  (crate-source "anstyle-wincon" "3.0.7"
                "0kmf0fq4c8yribdpdpylzz1zccpy84hizmcsac3wrac1f7kk8dfa"))

(define rust-anyhow-1.0.97
  (crate-source "anyhow" "1.0.97"
                "0kvspbiwncmmkdgrwjrimsmbmhzxc641p5ql99l2rjq6smmdbznw"))

(define rust-appendlist-1.4.0
  (crate-source "appendlist" "1.4.0"
                "1lnbl7mc7capcqj1z1ylxvm4h492sb9sr8pzww3q6lrhrmrxqjg1"))

(define rust-approx-0.4.0
  (crate-source "approx" "0.4.0"
                "0y52dg58lapl4pp1kqlznfw1blbki0nx6b0aw8kja2yi3gyhaaiz"))

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-arbitrary-1.4.1
  (crate-source "arbitrary" "1.4.1"
                "08zj2yanll5s5gsbmvgwvbq39iqzy3nia3yx3db3zwba08yhpqnx"))

(define rust-arboard-3.4.1
  (crate-source "arboard" "3.4.1"
                "1x2p8dfhzm3w0cpw81ab2rbyzvkzqs9g66xcakq4y0fd2v5rq2fz"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-arg-enum-proc-macro-0.3.4
  (crate-source "arg_enum_proc_macro" "0.3.4"
                "1sjdfd5a8j6r99cf0bpqrd6b160x9vz97y5rysycsjda358jms8a"))

(define rust-argon2-0.5.3
  (crate-source "argon2" "0.5.3"
                "0wn0kk97k49wxidfigmz1pdqmygqzi4h6w72ib7cpq765s4i0diw"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-ash-0.38.0+1.3.281
  (crate-source "ash" "0.38.0+1.3.281"
                "0vx4yf689v1rc680jvy8bnysx5sgd8f33wnp2vqaizh0v0v4kd0b"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-channel-2.3.1
  (crate-source "async-channel" "2.3.1"
                "0skvwxj6ysfc6d7bhczz9a2550260g62bm5gl0nmjxxyn007id49"))

(define rust-async-executor-1.13.1
  (crate-source "async-executor" "1.13.1"
                "1v6w1dbvsmw6cs4dk4lxj5dvrikc6xi479wikwaab2qy3h09mjih"))

(define rust-async-fs-2.1.2
  (crate-source "async-fs" "2.1.2"
                "0jp0p7lg9zqy2djgdmivbzx0yqmfn9sm2s9dkhaws3zlharhkkgb"))

(define rust-async-io-2.4.0
  (crate-source "async-io" "2.4.0"
                "0n8h0vy53n4vdkq529scqnkzm9vcl3r73za9nj81s2nfrhiv78j3"))

(define rust-async-lock-3.4.0
  (crate-source "async-lock" "3.4.0"
                "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz"))

(define rust-async-process-2.3.0
  (crate-source "async-process" "2.3.0"
                "1fr6cpqdw7hrmzns1983lgx86cg8vyz7nlrn0h0125iqq8fmy9b3"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-signal-0.2.10
  (crate-source "async-signal" "0.2.10"
                "1wxrq3871l00mil43nmh0akvwjjjnv0bn7n2pzwbvh00k0s00zk3"))

(define rust-async-stream-0.3.6
  (crate-source "async-stream" "0.3.6"
                "0xl4zqncrdmw2g6241wgr11dxdg4h7byy6bz3l6si03qyfk72nhb"))

(define rust-async-stream-impl-0.3.6
  (crate-source "async-stream-impl" "0.3.6"
                "0kaplfb5axsvf1gfs2gk6c4zx6zcsns0yf3ssk7iwni7bphlvhn7"))

(define rust-async-task-4.7.1
  (crate-source "async-task" "4.7.1"
                "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb"))

(define rust-async-trait-0.1.87
  (crate-source "async-trait" "0.1.87"
                "15swwmyl4nx7w03rq6ibb4x2c8rzbx9fpiag1kn4fhapb49yqmnm"))

(define rust-atoi-2.0.0
  (crate-source "atoi" "2.0.0"
                "0a05h42fggmy7h0ajjv6m7z72l924i7igbx13hk9d8pyign9k3gj"))

(define rust-atomic-0.6.0
  (crate-source "atomic" "0.6.0"
                "15193mfhmrq3p6vi1a10hw3n6kvzf5h32zikhby3mdj0ww1q10cd"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-av1-grain-0.2.3
  (crate-source "av1-grain" "0.2.3"
                "1gvqdh21bm1cfqiwyiinbqi0mg7x2lg2fwgmphma8ijxijfr0y36"))

(define rust-avif-serialize-0.8.3
  (crate-source "avif-serialize" "0.8.3"
                "13k0sy5qd6pyvfqzbd06zadz5cavq36fxn391j10ijzv9im2v4lq"))

(define rust-axum-0.7.9
  (crate-source "axum" "0.7.9"
                "07z7wqczi9i8xb4460rvn39p4wjqwr32hx907crd1vwb2fy8ijpd"))

(define rust-axum-core-0.4.5
  (crate-source "axum-core" "0.4.5"
                "16b1496c4gm387q20hkv5ic3k5bd6xmnvk50kwsy6ymr8rhvvwh9"))

(define rust-axum-server-0.7.1
  (crate-source "axum-server" "0.7.1"
                "1n67cx39cm9zsm0dwm0nla67qjswj90ccqrwq0x3kagn904ckfjn"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-base64ct-1.6.0
  (crate-source "base64ct" "1.6.0"
                "0nvdba4jb8aikv60az40x2w1y96sjdq8z3yp09rwzmkhiwv1lg4c"))

(define rust-beef-0.5.2
  (crate-source "beef" "0.5.2"
                "1c95lbnhld96iwwbyh5kzykbpysq0fnjfhwxa1mhap5qxgrl30is"))

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bindgen-0.69.5
  (crate-source "bindgen" "0.69.5"
                "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7"))

(define rust-bit-field-0.10.2
  (crate-source "bit_field" "0.10.2"
                "0qav5rpm4hqc33vmf4vc4r0mh51yjx5vmd9zhih26n9yjs3730nw"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.9.0
  (crate-source "bitflags" "2.9.0"
                "1gb5w7pxnmx8l2bjz1i6rkbwbm2167k294rhy6cl1y3vbc8i90jw"))

(define rust-bitstream-io-2.6.0
  (crate-source "bitstream-io" "2.6.0"
                "1cli390l1dhp9skygyjjnqvczp36b7f31mkx9ry3dg26330cv6b0"))

(define rust-blake2-0.10.6
  (crate-source "blake2" "0.10.6"
                "1zlf7w7gql12v61d9jcbbswa3dw8qxsjglylsiljp9f9b3a2ll26"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-blocking-1.6.1
  (crate-source "blocking" "1.6.1"
                "1si99l8zp7c4zq87y35ayjgc5c9b60jb8h0k14zfcs679z2l2gvh"))

(define rust-built-0.7.7
  (crate-source "built" "0.7.7"
                "0ywn0m11xm80pg6zrzq3sdj3vmzg3qs6baqnvfmkd377ly8n3van"))

(define rust-bumpalo-3.17.0
  (crate-source "bumpalo" "3.17.0"
                "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n"))

(define rust-by-address-1.2.1
  (crate-source "by_address" "1.2.1"
                "01idmag3lcwnnqrnnyik2gmbrr34drsi97q15ihvcbbidf2kryk4"))

(define rust-bytemuck-1.22.0
  (crate-source "bytemuck" "1.22.0"
                "0h6m8wh7iw98cn69k53plbyqff78c2yrs32l0fy4wqdcvc8grcdn"))

(define rust-bytemuck-derive-1.8.1
  (crate-source "bytemuck_derive" "1.8.1"
                "0ykwbnpm9y8hssp8kiaws2s4hljv4cl85mwgp1m67fzpnj9n59rz"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.10.0
  (crate-source "bytes" "1.10.0"
                "1ybcmdrlxrsrn7lnl0xrjg10j7zb4r01jjs5b2sqhrcwh62aq7gn"))

(define rust-cairo-rs-0.20.7
  (crate-source "cairo-rs" "0.20.7"
                "1xy02qa4mn9bwnhsbmkry4yjz230r66nvrkh4fn9dkw61m8val5f"))

(define rust-cairo-sys-rs-0.20.7
  (crate-source "cairo-sys-rs" "0.20.7"
                "1pwh4b4mdsipjl9lrg5p5bygbdk11kz6m5y7mbrb0ziwwjw6p2zi"))

(define rust-calloop-0.13.0
  (crate-source "calloop" "0.13.0"
                "1v5zgidnhsyml403rzr7vm99f8q6r5bxq5gxyiqkr8lcapwa57dr"))

(define rust-calloop-0.14.2
  (crate-source "calloop" "0.14.2"
                "1jzx8rmgndj1br4gnd4iaxayqi79g897lz6qdy2l670xcqj9g4hh"))

(define rust-calloop-wayland-source-0.3.0
  (crate-source "calloop-wayland-source" "0.3.0"
                "086x5mq16prrcwd9k6bw9an0sp8bj9l5daz4ziz5z4snf2c6m9lm"))

(define rust-calloop-wayland-source-0.4.0
  (crate-source "calloop-wayland-source" "0.4.0"
                "1bsxx4dz4k4icza63w108n8s1agm7890nl3syigaa9p0pcfplsl7"))

(define rust-cassowary-0.3.0
  (crate-source "cassowary" "0.3.0"
                "0lvanj0gsk6pc1chqrh4k5k0vi1rfbgzmsk46dwy3nmrqyw711nz"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-castaway-0.2.3
  (crate-source "castaway" "0.2.3"
                "1mf0wypwnkpa1hi0058vp8g7bjh2qraip2qv7dmak7mg1azfkfha"))

(define rust-cc-1.2.16
  (crate-source "cc" "1.2.16"
                "131bhgafc1i86vvjipkj0kwzz0hlpwrkl8mdbmzyq2g69calqwdy"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.1.1
  (crate-source "cfg_aliases" "0.1.1"
                "17p821nc6jm830vzl2lmwz60g3a30hcm33nk6l257i1rjdqw85px"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-expr-0.17.2
  (crate-source "cfg-expr" "0.17.2"
                "12a7zr6ff4i6mfwcv711dll0w5pr3dw1lvkaf4c4a66i1gjacjwd"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cgmath-0.18.0
  (crate-source "cgmath" "0.18.0"
                "05sk7c1c1jg5ygqvc3y77kxddp177gwazfibhd864ag3800x760s"))

(define rust-chacha20-0.9.1
  (crate-source "chacha20" "0.9.1"
                "0678wipx6kghp71hpzhl2qvx80q7caz3vm8vsvd07b1fpms3yqf3"))

(define rust-chrono-0.4.40
  (crate-source "chrono" "0.4.40"
                "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s"))

(define rust-chumsky-0.9.3
  (crate-source "chumsky" "0.9.3"
                "1jcnafc8rjfs1al08gqzyn0kpbaizgdwrd0ajqafspd18ikxdswf"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-clang-sys-1.8.1
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-4.5.31
  (crate-source "clap" "4.5.31"
                "0ryp6xjbdc9cbjjkafjl35j91pvv0ykislwqhr537bi9hkcv0yq2"))

(define rust-clap-builder-4.5.31
  (crate-source "clap_builder" "4.5.31"
                "0qyqd6kfcs41x29a95n15744jyv2v07srvwi6z9g7q3jl35y12am"))

(define rust-clap-complete-4.5.46
  (crate-source "clap_complete" "4.5.46"
                "166f2f6xr1jc8vhgjgpchwbfb12s1q3s1xakgvvnclrwla751igm"))

(define rust-clap-complete-nushell-4.5.5
  (crate-source "clap_complete_nushell" "4.5.5"
                "12miqxh9g7q37w11bgv55b32s0hdf6avf0lhagzc5psp6icv3a66"))

(define rust-clap-derive-4.5.28
  (crate-source "clap_derive" "4.5.28"
                "1vgigkhljp3r8r5lwdrn1ij93nafmjwh8cx77nppb9plqsaysk5z"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clipboard-win-5.4.0
  (crate-source "clipboard-win" "5.4.0"
                "14n87fc0vzbd0wdhqzvcs1lqgafsncplzcanhpik93xhhalfgvqm"))

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-colored-2.2.0
  (crate-source "colored" "2.2.0"
                "0g6s7j2qayjd7i3sivmwiawfdg8c8ldy0g2kl4vwk1yk16hjaxqi"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-compact-str-0.7.1
  (crate-source "compact_str" "0.7.1"
                "0gvvfc2c6pg1rwr2w36ra4674w3lzwg97vq2v6k791w30169qszq"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-condtype-1.3.0
  (crate-source "condtype" "1.3.0"
                "1by78npyhkc30jccc7kirvwip1fj0jhi2bwfmcw44dqz81xa1w5s"))

(define rust-config-0.13.4
  (crate-source "config" "0.13.4"
                "1jjag1x3rl77zjykbrykzhd5fsiv8vy40y4lxkj46xicjw8qwwr3"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-const-oid-0.9.6
  (crate-source "const-oid" "0.9.6"
                "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))

(define rust-container-of-0.5.1
  (crate-source "container_of" "0.5.1"
                "0as7g6gspvdbp4vl1a1834pzh481x9jp4clfgyl6c7vnhvmvpxc9"))

(define rust-convert-case-0.6.0
  (crate-source "convert_case" "0.6.0"
                "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c"))

(define rust-cookie-factory-0.3.3
  (crate-source "cookie-factory" "0.3.3"
                "18mka6fk3843qq3jw1fdfvzyv05kx7kcmirfbs2vg2kbw9qzm1cq"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-graphics-0.23.2
  (crate-source "core-graphics" "0.23.2"
                "10dhv3gk4kmbzl14xxkrhhky4fdp8h6nzff6h0019qgr6nz84xy0"))

(define rust-core-graphics-types-0.1.3
  (crate-source "core-graphics-types" "0.1.3"
                "1bxg8nxc8fk4kxnqyanhf36wq0zrjr552c58qy6733zn2ihhwfa5"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crc-3.2.1
  (crate-source "crc" "3.2.1"
                "0dnn23x68qakzc429s1y9k9y3g8fn5v9jwi63jcz151sngby9rk9"))

(define rust-crc-catalog-2.4.0
  (crate-source "crc-catalog" "2.4.0"
                "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-criterion-0.5.1
  (crate-source "criterion" "0.5.1"
                "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-crossbeam-channel-0.5.14
  (crate-source "crossbeam-channel" "0.5.14"
                "0wa41qybq5w8s70anb472myh4fid4aw6v65vws6wn528w9l6vfh6"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.12
  (crate-source "crossbeam-queue" "0.3.12"
                "059igaxckccj6ndmg45d5yf7cm4ps46c18m21afq3pwiiz1bnn0g"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crossterm-0.27.0
  (crate-source "crossterm" "0.27.0"
                "1pr413ki440xgddlmkrc4j1bfx1h8rpmll87zn8ykja1bm2gwxpl"))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crunchy-0.2.3
  (crate-source "crunchy" "0.2.3"
                "0aa9k4izp962qlsn5ndgw2zq62mizcpnkns8bxscgz3gqr35knj3"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-crypto-secretbox-0.1.1
  (crate-source "crypto_secretbox" "0.1.1"
                "1qa1w5s8dbyb88269zrmvbnillqahz394pl07bsds6gpmn3wzmmr"))

(define rust-csscolorparser-0.7.0
  (crate-source "csscolorparser" "0.7.0"
                "12423a53ikbzacavi157kf3xz9p93sn8gqvwsifvjzwahima3ya6"))

(define rust-cursor-icon-1.1.0
  (crate-source "cursor-icon" "1.1.0"
                "14brf4vd6az9hnszwzqj7xyfaymqx9806d4i7xmwlaja3wjsr9ln"))

(define rust-curve25519-dalek-4.1.3
  (crate-source "curve25519-dalek" "4.1.3"
                "1gmjb9dsknrr8lypmhkyjd67p1arb8mbfamlwxm7vph38my8pywp"))

(define rust-curve25519-dalek-derive-0.1.1
  (crate-source "curve25519-dalek-derive" "0.1.1"
                "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l"))

(define rust-darling-0.20.10
  (crate-source "darling" "0.20.10"
                "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg"))

(define rust-darling-core-0.20.10
  (crate-source "darling_core" "0.20.10"
                "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm"))

(define rust-darling-macro-0.20.10
  (crate-source "darling_macro" "0.20.10"
                "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-der-0.7.9
  (crate-source "der" "0.7.9"
                "1h4vzjfa1lczxdf8avfj9qlwh1qianqlxdy1g5rn762qnvkzhnzm"))

(define rust-deranged-0.3.11
  (crate-source "deranged" "0.3.11"
                "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl"))

(define rust-derive-new-0.6.0
  (crate-source "derive-new" "0.6.0"
                "1b8jv6jx0b8jgkz9kmz0ciqmnf74xkk0mmvkb5z1c87932kdwl6i"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-directories-5.0.1
  (crate-source "directories" "5.0.1"
                "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs"))

(define rust-directories-6.0.0
  (crate-source "directories" "6.0.0"
                "0zgy2w088v8w865c11dmc3dih899fgrhvrfp7g83h6v6ai60kx8n"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-sys-0.4.1
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-dirs-sys-0.5.0
  (crate-source "dirs-sys" "0.5.0"
                "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))

(define rust-dispatch-0.2.0
  (crate-source "dispatch" "0.2.0"
                "0fwjr9b7582ic5689zxj8lf7zl94iklhlns3yivrnv8c9fxr635x"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-divan-0.1.17
  (crate-source "divan" "0.1.17"
                "1i5qvk7h9fcrr89cxkmadpgm03sv7askplyqh8vb0a8b0a9k2n70"))

(define rust-divan-macros-0.1.17
  (crate-source "divan-macros" "0.1.17"
                "0z5n3f4m5c7flqnr6vg9qz51j9mjb1s2afcsfnqf7x9nwsc1vicd"))

(define rust-dlib-0.5.2
  (crate-source "dlib" "0.5.2"
                "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k"))

(define rust-dotenvy-0.15.7
  (crate-source "dotenvy" "0.15.7"
                "16s3n973n5aqym02692i1npb079n5mb0fwql42ikmwn8wnrrbbqs"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dpi-0.1.1
  (crate-source "dpi" "0.1.1"
                "0lzz48gpgbwdrw0s8vib0589ij9jizv1vzsphm4xd9kw58lhwp7j"))

(define rust-drm-0.14.1
  (crate-source "drm" "0.14.1"
                "0vvmj9n0wslrbw3rinpzlfyhwwgr02gqspy1al5gfh99dif8rg40"))

(define rust-drm-ffi-0.9.0
  (crate-source "drm-ffi" "0.9.0"
                "12vff80hdpp81gj5lqw25xnkppwsxc4wklpn8nc556wsv5ci9r6q"))

(define rust-drm-fourcc-2.2.0
  (crate-source "drm-fourcc" "2.2.0"
                "1x76v9a0pkgym4n6cah4barnai9gsssm7gjzxskw2agwibdvrbqa"))

(define rust-drm-sys-0.8.0
  (crate-source "drm-sys" "0.8.0"
                "1345z72hd2rna4qxd2zcpbzvw0z7ywfndk6g2ngdci69vg46dyxs"))

(define rust-dyn-clone-1.0.19
  (crate-source "dyn-clone" "1.0.19"
                "01ahm5abl20480v48nxy4ffyx80cs6263q9zf0gnrxpvm6w8yyhw"))

(define rust-ed25519-2.2.3
  (crate-source "ed25519" "2.2.3"
                "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i"))

(define rust-ed25519-dalek-2.1.1
  (crate-source "ed25519-dalek" "2.1.1"
                "0w88cafwglg9hjizldbmlza0ns3hls81zk1bcih3m5m3h67algaa"))

(define rust-either-1.14.0
  (crate-source "either" "1.14.0"
                "17fs0r9mnj632k4ff8c6zyq80zqvqb0wa9cgsyd5iprd159l74dp"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

(define rust-enumflags2-0.7.11
  (crate-source "enumflags2" "0.7.11"
                "0iwi60d54lgby0f29b5isikxraf0wvnqdmlddx68a62kbx34nbxs"))

(define rust-enumflags2-derive-0.7.11
  (crate-source "enumflags2_derive" "0.7.11"
                "0yfdjyrf9b4mi1r589azkyirjhzmdw29nqq0mdjnsyldlmjayk7w"))

(define rust-env-filter-0.1.3
  (crate-source "env_filter" "0.1.3"
                "1l4p6f845cylripc3zkxa0lklk8rn2q86fqm522p6l2cknjhavhq"))

(define rust-env-logger-0.11.6
  (crate-source "env_logger" "0.11.6"
                "1q30cqb2dfs3qrs0s30qdmqwi7n2gz4pniwd8a9gvhygwgcf7bnw"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.10
  (crate-source "errno" "0.3.10"
                "0pgblicz1kjz9wa9m0sghkhh2zw1fhq1mxzj7ndjm746kg5m5n1k"))

(define rust-error-code-3.3.1
  (crate-source "error-code" "3.3.1"
                "0bx9hw3pahzqym8jvb0ln4qsabnysgn98mikyh2afhk9rif31nd5"))

(define rust-etcetera-0.8.0
  (crate-source "etcetera" "0.8.0"
                "0hxrsn75dirbjhwgkdkh0pnpqrnq17ypyhjpjaypgax1hd91nv8k"))

(define rust-event-listener-5.4.0
  (crate-source "event-listener" "5.4.0"
                "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il"))

(define rust-event-listener-strategy-0.5.3
  (crate-source "event-listener-strategy" "0.5.3"
                "1ch5gf6knllyq12jkb5zdfag573dh44307q4pwwi2g37sc6lwgiw"))

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-fast-srgb8-1.0.0
  (crate-source "fast-srgb8" "1.0.0"
                "18g6xwwh4gnkyx1352hnvwagpv0n4y98yp2llm8vyvwxh487abnx"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-fiat-crypto-0.2.9
  (crate-source "fiat-crypto" "0.2.9"
                "07c1vknddv3ak7w89n85ik0g34nzzpms6yb845vrjnv9m4csbpi8"))

(define rust-field-offset-0.3.6
  (crate-source "field-offset" "0.3.6"
                "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq"))

(define rust-filedescriptor-0.8.3
  (crate-source "filedescriptor" "0.8.3"
                "0bb8qqa9h9sj2mzf09yqxn260qkcqvmhmyrmdjvyxcn94knmh1z4"))

(define rust-fixedbitset-0.4.2
  (crate-source "fixedbitset" "0.4.2"
                "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-flate2-1.1.0
  (crate-source "flate2" "1.1.0"
                "1p1qpmkkxky6y3869g2facflp0lmvgsbxq4bhkwpm69na9dazyhi"))

(define rust-float-cmp-0.9.0
  (crate-source "float-cmp" "0.9.0"
                "1i799ksbq7fj9rm9m82g1yqgm6xi3jnrmylddmqknmksajylpplq"))

(define rust-flume-0.11.1
  (crate-source "flume" "0.11.1"
                "15ch0slxa8sqsi6c73a0ky6vdnh48q8cxjf7rksa3243m394s3ns"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.4
  (crate-source "foldhash" "0.1.4"
                "0vsxw2iwpgs7yy6l7pndm7b8nllaq5vdxwnmjn1qpm5kyzhzvlm0"))

(define rust-foreign-types-0.5.0
  (crate-source "foreign-types" "0.5.0"
                "0rfr2zfxnx9rz3292z5nyk8qs2iirznn5ff3rd4vgdwza6mdjdyp"))

(define rust-foreign-types-macros-0.2.3
  (crate-source "foreign-types-macros" "0.2.3"
                "0hjpii8ny6l7h7jpns2cp9589016l8mlrpaigcnayjn9bdc6qp0s"))

(define rust-foreign-types-shared-0.3.1
  (crate-source "foreign-types-shared" "0.3.1"
                "0nykdvv41a3d4py61bylmlwjhhvdm0b3bcj9vxhqgxaxnp5ik6ma"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-fps-ticker-1.0.0
  (crate-source "fps_ticker" "1.0.0"
                "06cj5c5rk5grm2ajh4sabcppxr1h57gxfqacvi5psxb9zw2lj5py"))

(define rust-fs-err-2.11.0
  (crate-source "fs-err" "2.11.0"
                "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-intrusive-0.5.0
  (crate-source "futures-intrusive" "0.5.0"
                "0vwm08d1pli6bdaj0i7xhk3476qlx4pll6i0w03gzdnh7lh0r4qx"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-lite-2.6.0
  (crate-source "futures-lite" "2.6.0"
                "0cmmgszlmkwsac9pyw5rfjakmshgx4wmzmlyn6mmjs0jav4axvgm"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-fuzzy-matcher-0.3.7
  (crate-source "fuzzy-matcher" "0.3.7"
                "153csv8rsk2vxagb68kpmiknvdd3bzqj03x805khckck28rllqal"))

(define rust-gbm-0.18.0
  (crate-source "gbm" "0.18.0"
                "0skyaj51xlazaa24jdkxxi2g6pnw834k3yqlf2ly999wincjx1ff"))

(define rust-gbm-sys-0.4.0
  (crate-source "gbm-sys" "0.4.0"
                "0vzp28ip4w74p05ygs4p9m7sspggn2zvcykbpyv8ypbqrhm5yfn1"))

(define rust-gdk-pixbuf-0.20.9
  (crate-source "gdk-pixbuf" "0.20.9"
                "1l0llkzf7v634h5a8dz6935xkf3ma3fqm9vhpggiw8hazzbayqvm"))

(define rust-gdk-pixbuf-sys-0.20.7
  (crate-source "gdk-pixbuf-sys" "0.20.7"
                "0p0b3lrzamsz580dyrr5i99i32ppsjp6mfmvfrs9kgq2j9y5iwk7"))

(define rust-gdk4-0.9.6
  (crate-source "gdk4" "0.9.6"
                "0q1dld01fgj7qxj644by0fc242mcn36w3bagn4z1mkdfq7cwjl28"))

(define rust-gdk4-sys-0.9.6
  (crate-source "gdk4-sys" "0.9.6"
                "0fj722lp86fpa1b1i3s2anavdmcpybd0b47mkhknzd72k1bvjvkg"))

(define rust-generator-0.8.4
  (crate-source "generator" "0.8.4"
                "1p9qqk9nzarjdcl5fr4iylvsv446g0svlpk63lxis4ysrqad2syc"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-0.4.3
  (crate-source "gethostname" "0.4.3"
                "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.3.1
  (crate-source "getrandom" "0.3.1"
                "1y154yzby383p63ndw6zpfm0fz3vf6c0zdwc7df6vkl150wrr923"))

(define rust-gif-0.13.1
  (crate-source "gif" "0.13.1"
                "1whrkvdg26gp1r7f95c6800y6ijqw5y0z8rgj6xihpi136dxdciz"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-gio-0.20.9
  (crate-source "gio" "0.20.9"
                "11vl4zkb3zvrklr7zhdlcyb35rbrm8d0xpbjfpm89782z1q0rw54"))

(define rust-gio-sys-0.20.9
  (crate-source "gio-sys" "0.20.9"
                "17izngigdvv3gfda2qk059vcgihmxbaa7rjl3cz8r69618jva3hn"))

(define rust-git-version-0.3.9
  (crate-source "git-version" "0.3.9"
                "06ddi3px6l2ip0srn8512bsh8wrx4rzi65piya0vrz5h7nm6im8s"))

(define rust-git-version-macro-0.3.9
  (crate-source "git-version-macro" "0.3.9"
                "1h1s08fgh9bkwnc2hmjxcldv69hlxpq7a09cqdxsd5hb235hq0ak"))

(define rust-gl-generator-0.14.0
  (crate-source "gl_generator" "0.14.0"
                "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s"))

(define rust-glam-0.30.0
  (crate-source "glam" "0.30.0"
                "1ihxmcsrkflmhvdwncyn20if55fmq0lzsjhj9pyc41n4hfbdzz0p"))

(define rust-glib-0.20.9
  (crate-source "glib" "0.20.9"
                "11knyc1lgd0bkw42ysl4v2x3v9c7glqz5s9db8wyb7h5z2d82yvh"))

(define rust-glib-macros-0.20.7
  (crate-source "glib-macros" "0.20.7"
                "0s6yik6pgqg5wydcz5v0x8m1jz57m5bsd50zkkpvlw9fy3w02mki"))

(define rust-glib-sys-0.20.9
  (crate-source "glib-sys" "0.20.9"
                "1yxfqf6wllka0am0brqwwj18yb7q9xp79mhprgyd3zaclilqi4m8"))

(define rust-glob-0.3.2
  (crate-source "glob" "0.3.2"
                "1cm2w34b5w45fxr522h5b0fv1bxchfswcj560m3pnjbia7asvld8"))

(define rust-glow-0.16.0
  (crate-source "glow" "0.16.0"
                "022z12nlyfpy36fvp2szq792xix1xbgkznpmicf1c404sxhfmrf5"))

(define rust-gobject-sys-0.20.9
  (crate-source "gobject-sys" "0.20.9"
                "1qz7jrpfk0z8mhnz7fxxx208kc5ljryif7f84sfas6d4735s6wy7"))

(define rust-graphene-rs-0.20.9
  (crate-source "graphene-rs" "0.20.9"
                "06fy773j9r0v7xzwv3sl2pvw55i68q653jcjzbf6hbdkpw8mkg1w"))

(define rust-graphene-sys-0.20.7
  (crate-source "graphene-sys" "0.20.7"
                "0fnjh55lnrd8mgladbapfxak44swlbafqb5pg7l41wsva4wqv9hi"))

(define rust-gsk4-0.9.6
  (crate-source "gsk4" "0.9.6"
                "0mgqq5m6cm4q7ajjgw92z13z2ikpvh6zx2gwzdjrz30wjcpygxb1"))

(define rust-gsk4-sys-0.9.6
  (crate-source "gsk4-sys" "0.9.6"
                "1p1n4jhhxyvj7hb0cqhzvazrck0qw81sz36ydfj8avzsapg5jl3m"))

(define rust-gtk4-0.9.6
  (crate-source "gtk4" "0.9.6"
                "078911sc8wvnihlz3kq80chl0miz9z2g7rnds17rjc7ha484j75g"))

(define rust-gtk4-macros-0.9.5
  (crate-source "gtk4-macros" "0.9.5"
                "169rqfxfczivcpz7019slsrpkx8crqjka43ymxmikp838xn7il8f"))

(define rust-gtk4-sys-0.9.6
  (crate-source "gtk4-sys" "0.9.6"
                "1mh3xjkjb99y97z234cvyar08vcr7zblg1nrw48c6xsdwl0kpq21"))

(define rust-h2-0.3.26
  (crate-source "h2" "0.3.26"
                "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1"))

(define rust-h2-0.4.8
  (crate-source "h2" "0.4.8"
                "1hp3lijg1br982kzgglb5ks2ibg68a76z3rl052r8c5vyi7jj5sh"))

(define rust-half-2.4.1
  (crate-source "half" "2.4.1"
                "123q4zzw1x4309961i69igzd1wb7pj04aaii3kwasrz3599qrl3d"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.13.1
  (crate-source "hashbrown" "0.13.1"
                "0f602rk7pgdhw1s57g81822g7b2m5i2wibrpaqp11afk5kk8mzrk"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashlink-0.10.0
  (crate-source "hashlink" "0.10.0"
                "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hermit-abi-0.4.0
  (crate-source "hermit-abi" "0.4.0"
                "1k1zwllx6nfq417hy38x4akw1ivlv68ymvnzyxs76ffgsqcskxpv"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hkdf-0.12.4
  (crate-source "hkdf" "0.12.4"
                "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-http-0.2.12
  (crate-source "http" "0.2.12"
                "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))

(define rust-http-1.2.0
  (crate-source "http" "1.2.0"
                "1skglzdf98j5nzxlii540n11is0w4l80mi5sm3xrj716asps4v7i"))

(define rust-http-body-0.4.6
  (crate-source "http-body" "0.4.6"
                "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.2
  (crate-source "http-body-util" "0.1.2"
                "0kslwazg4400qnc2azkrgqqci0fppv12waicnsy5d8hncvbjjd3r"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-humantime-2.1.0
  (crate-source "humantime" "2.1.0"
                "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))

(define rust-hyper-0.14.32
  (crate-source "hyper" "0.14.32"
                "1rvcb0smz8q1i0y6p7rwxr02x5sclfg2hhxf3g0774zczn0cgps1"))

(define rust-hyper-1.6.0
  (crate-source "hyper" "1.6.0"
                "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc"))

(define rust-hyper-rustls-0.24.2
  (crate-source "hyper-rustls" "0.24.2"
                "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))

(define rust-hyper-rustls-0.27.5
  (crate-source "hyper-rustls" "0.27.5"
                "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d"))

(define rust-hyper-timeout-0.5.2
  (crate-source "hyper-timeout" "0.5.2"
                "1c431l5ckr698248yd6bnsmizjy2m1da02cbpmsnmkpvpxkdb41b"))

(define rust-hyper-util-0.1.10
  (crate-source "hyper-util" "0.1.10"
                "1d1iwrkysjhq63pg54zk3vfby1j7zmxzm9zzyfr4lwvp0szcybfz"))

(define rust-iana-time-zone-0.1.61
  (crate-source "iana-time-zone" "0.1.61"
                "085jjsls330yj1fnwykfzmb2f10zp6l7w4fhq81ng81574ghhpi3"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.0
  (crate-source "icu_locid_transform_data" "1.5.0"
                "0vkgjixm0wzp2n3v5mw4j89ly05bg3lx96jpdggbwlpqi0rzzj7x"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-data-1.5.0
  (crate-source "icu_normalizer_data" "1.5.0"
                "05lmk0zf0q7nzjnj5kbmsigj3qgr0rwicnn5pqi9n7krmbvzpjpq"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-data-1.5.0
  (crate-source "icu_properties_data" "1.5.0"
                "0scms7pd5a7yxx9hfl167f5qdf44as6r3bd8myhlngnxqgxyza37"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

(define rust-image-0.25.5
  (crate-source "image" "0.25.5"
                "0fsnfgg8hr66ag5nxipvb7d50kbg40qfpbsql59qkwa2ssp48vyd"))

(define rust-image-webp-0.2.1
  (crate-source "image-webp" "0.2.1"
                "0zwg4gpnp69dpn8pdhgjy14mawwi3md02mp1162al6s64bl02zdp"))

(define rust-imgref-1.11.0
  (crate-source "imgref" "1.11.0"
                "0254wzkakm31fdix6diqng0fkggknibh0b1iv570ap0djwykl9nh"))

(define rust-indenter-0.3.3
  (crate-source "indenter" "0.3.3"
                "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.7.1
  (crate-source "indexmap" "2.7.1"
                "0lmnm1zbr5gq3wic3d8a76gpvampridzwckfl97ckd5m08mrk74c"))

(define rust-indicatif-0.17.11
  (crate-source "indicatif" "0.17.11"
                "0db2b2r79r9x8x4lysq1ci9xm13c0xg0sqn3z960yh2bk2430fqq"))

(define rust-inout-0.1.4
  (crate-source "inout" "0.1.4"
                "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7"))

(define rust-input-0.9.1
  (crate-source "input" "0.9.1"
                "1abmv1djhynihipjppgsmw6nbp6pcgzk8rzi4v6wmyci9990kp7v"))

(define rust-input-sys-1.18.0
  (crate-source "input-sys" "1.18.0"
                "1c4y24wf0jixi52js4f7cjspbgi0bzzaqfhn8m91qcq03i6mnkxx"))

(define rust-insta-1.42.2
  (crate-source "insta" "1.42.2"
                "111hrdc3bxwp146kz2ffwdq0qypdjk8a2yzwr8mivlb7maxrl9ah"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-interim-0.1.2
  (crate-source "interim" "0.1.2"
                "1x5ykyv8bkv13398q3dpycg5943rw1jycvjbhi2yih30zw5hzzcs"))

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

(define rust-io-lifetimes-1.0.11
  (crate-source "io-lifetimes" "1.0.11"
                "1hph5lz4wd3drnn6saakwxr497liznpfnv70via6s0v8x6pbkrza"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-is-ci-1.2.0
  (crate-source "is_ci" "1.2.0"
                "0ifwvxmrsj4r29agfzr71bjq6y1bihkx38fbzafq5vl0jn1wjmbn"))

(define rust-is-terminal-0.4.15
  (crate-source "is-terminal" "0.4.15"
                "0dzdvjg3f10cfv8wi1dcnw6rq7mcwss1nzdwmrb9zkim7zaj76z1"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-iso8601-0.6.2
  (crate-source "iso8601" "0.6.2"
                "13f9a6izrm87dd66qcagw65lw714072a8y8hyjk23ar4z37pghf5"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.32
  (crate-source "jobserver" "0.1.32"
                "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8"))

(define rust-jpeg-decoder-0.3.1
  (crate-source "jpeg-decoder" "0.3.1"
                "1c1k53svpdyfhibkmm0ir5w0v3qmcmca8xr8vnnmizwf6pdagm7m"))

(define rust-js-sys-0.3.77
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-keyframe-1.1.1
  (crate-source "keyframe" "1.1.1"
                "1afr5ffns3k79xaqnw6rw3qn8sngwly6gxfnjn8d060mk3vqnw30"))

(define rust-khronos-api-3.1.0
  (crate-source "khronos_api" "3.1.0"
                "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))

(define rust-knuffel-3.2.0
  (crate-source "knuffel" "3.2.0"
                "04vl2xmdn280rcigv96v06a00v7gbxqggr0w9cqi2407qvfydgh4"))

(define rust-knuffel-derive-3.2.0
  (crate-source "knuffel-derive" "3.2.0"
                "0g98909l5wb1d1hcz61q53kvsmjadry2w3l47lg9dywwqib7z5wi"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lazycell-1.3.0
  (crate-source "lazycell" "1.3.0"
                "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))

(define rust-lebe-0.5.2
  (crate-source "lebe" "0.5.2"
                "1j2l6chx19qpa5gqcw434j83gyskq3g2cnffrbl3842ymlmpq203"))

(define rust-libadwaita-0.7.1
  (crate-source "libadwaita" "0.7.1"
                "0is205jr9nc1ynmcm62dkvvm765myp5ay4xmcb1hcxjyp2gyw4c6"))

(define rust-libadwaita-sys-0.7.1
  (crate-source "libadwaita-sys" "0.7.1"
                "1y9sr3glrda5ddzgngfqw4gigdx57qzx5dh4zbad8601aqis56dh"))

(define rust-libc-0.2.170
  (crate-source "libc" "0.2.170"
                "0a38q3avb6r6azxb7yfbjly5sbr8926z6c4sryyp33rgrf03cnw7"))

(define rust-libdisplay-info-0.2.2
  (crate-source "libdisplay-info" "0.2.2"
                "0avs90mwzbfkwc09xlvvdy0szrbi670y61c1w0l75hqd7blwy422"))

(define rust-libdisplay-info-derive-0.1.0
  (crate-source "libdisplay-info-derive" "0.1.0"
                "1vpwss66rmhdd0f85c3nwjshddmiarf4iya5v13aacmp6q8d677a"))

(define rust-libdisplay-info-sys-0.2.2
  (crate-source "libdisplay-info-sys" "0.2.2"
                "0v34vjczpj8hzxnx1nj5cxwf326m91gn7bi3l3zkfg72xij94kvz"))

(define rust-libfuzzer-sys-0.4.9
  (crate-source "libfuzzer-sys" "0.4.9"
                "0xfwg8shqvysl2bma2lyfcswbbdljajphflp795diwhc80nzay6g"
                #:snippet '(delete-file-recursively "libfuzzer")))

(define rust-libloading-0.7.4
  (crate-source "libloading" "0.7.4"
                "17wbccnjvhjd9ibh019xcd8kjvqws8lqgq86lqkpbgig7gyq0wxn"
                #:snippet '(for-each delete-file (find-files "tests" "\\.dll$"))))

(define rust-libloading-0.8.6
  (crate-source "libloading" "0.8.6"
                "0d2ccr88f8kv3x7va2ccjxalcjnhrci4j2kwxp7lfmbkpjs4wbzw"
                #:snippet '(for-each delete-file (find-files "tests" "\\.dll$"))))

(define rust-libm-0.2.11
  (crate-source "libm" "0.2.11"
                "1yjgk18rk71rjbqcw9l1zaqna89p9s603k7n327nqs8dn88vwmc3"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libseat-0.2.3
  (crate-source "libseat" "0.2.3"
                "0350b89h2xk5rdqqla162a2pak7yzbpfckqwg68cd42ppmdj8fn2"))

(define rust-libseat-sys-0.1.9
  (crate-source "libseat-sys" "0.1.9"
                "0997n2s413ggzi4amy0jbnfl8jvgpjnkxzycjs56ks2p0pjj2ihk"))

(define rust-libsqlite3-sys-0.30.1
  ;; TODO: Find a way to unbundle sqlite.
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"))

(define rust-libudev-sys-0.1.4
  (crate-source "libudev-sys" "0.1.4"
                "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

(define rust-linux-raw-sys-0.4.15
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.6.5
  (crate-source "linux-raw-sys" "0.6.5"
                "1mv3c1zz51ydcj768zavm8g06gz5jb1p7yigmmif7hz5whdmnf1a"))

(define rust-listenfd-1.0.2
  (crate-source "listenfd" "1.0.2"
                "1flxwfgp6rrcgg09szb7g84i3ij09jv43w1y1d6jkd198r5cayxq"))

(define rust-litemap-0.7.5
  (crate-source "litemap" "0.7.5"
                "0mi8ykav0s974ps79p438x04snh0cdb7lc864b42jws5375i9yr3"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-log-0.4.26
  (crate-source "log" "0.4.26"
                "17mvchkvhnm2zxyfagh2g9p861f0qx2g1sg2v14sww9nvjry5g9h"))

(define rust-logos-0.14.4
  (crate-source "logos" "0.14.4"
                "0n349vin9mx326fkz68bsa4vc5sdn9n8qnfz7n1yqynbz1p3albj"))

(define rust-logos-codegen-0.14.4
  (crate-source "logos-codegen" "0.14.4"
                "0gwnx7lk4y7xc4yk6pr0knrddard5z22rxaz9xrnc38cc1lh1y2r"))

(define rust-logos-derive-0.14.4
  (crate-source "logos-derive" "0.14.4"
                "07bk3q4jry9f8blrnsiy872ivilzy62xaglnn2ni5p590qmp5yr4"))

(define rust-loom-0.7.2
  (crate-source "loom" "0.7.2"
                "1jpszf9qxv8ydpsm2h9vcyvxvyxcfkhmmfbylzd4gfbc0k40v7j1"))

(define rust-loop9-0.1.5
  (crate-source "loop9" "0.1.5"
                "0qphc1c0cbbx43pwm6isnwzwbg6nsxjh7jah04n1sg5h4p0qgbhg"))

(define rust-lru-0.12.5
  (crate-source "lru" "0.12.5"
                "0f1a7cgqxbyhrmgaqqa11m3azwhcc36w0v5r4izgbhadl3sg8k13"))

(define rust-lz4-flex-0.10.0
  (crate-source "lz4_flex" "0.10.0"
                "10sgbj93sagbl0ngzqvnlkldzbfz5vnzr7fry8sgssy299cp534b"))

(define rust-mach2-0.4.2
  (crate-source "mach2" "0.4.2"
                "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matchit-0.7.3
  (crate-source "matchit" "0.7.3"
                "156bgdmmlv4crib31qhgg49nsjk88dxkdqp80ha2pk2rk6n6ax0f"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-memmap2-0.8.0
  (crate-source "memmap2" "0.8.0"
                "1vf3djv9s917fbvw5vclllpl22g12iph6cz11gn57ndhxwya19a3"))

(define rust-memmap2-0.9.5
  (crate-source "memmap2" "0.9.5"
                "0krpvvkpg4i3l05cv3q2xk24a1vj5c86gbrli2wzhj1qkpnpwgzx"))

(define rust-memoffset-0.6.5
  (crate-source "memoffset" "0.6.5"
                "1kkrzll58a3ayn5zdyy9i1f1v3mx0xgl29x0chq614zazba638ss"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-metrics-0.21.1
  (crate-source "metrics" "0.21.1"
                "1ibndxzk0sja8cgwrr73b9vzbgfvwzwxwkxqiivnmmwy00dazqzx"))

(define rust-metrics-exporter-prometheus-0.12.2
  (crate-source "metrics-exporter-prometheus" "0.12.2"
                "0l19s21jfmwm72cxfjq35xb79a5wi4fv7c1p993dnqj8gk7afkqx"))

(define rust-metrics-macros-0.7.1
  (crate-source "metrics-macros" "0.7.1"
                "0krmj7zyr4g14jdpk1jasi1w2nw64hqdxb2lfx4zxphp0vqgmd1q"))

(define rust-metrics-util-0.15.1
  (crate-source "metrics-util" "0.15.1"
                "0glpkmrj7zkg9b290x6qxf93kmd9b4b4sbkk1fs19l8y95pfvqjd"))

(define rust-miette-5.10.0
  (crate-source "miette" "5.10.0"
                "0vl5qvl3bgha6nnkdl7kiha6v4ypd6d51wyc4q1bvdpamr75ifsr"))

(define rust-miette-7.5.0
  (crate-source "miette" "7.5.0"
                "114lv0nx46lxc5pncz6iyrzcfhn5g9a5janzc8cgsdvvz1jm358s"))

(define rust-miette-derive-5.10.0
  (crate-source "miette-derive" "5.10.0"
                "0p33msrngkxlp5ajm8nijamii9vcwwpy8gfh4m53qnmrc0avrrs9"))

(define rust-miette-derive-7.5.0
  (crate-source "miette-derive" "7.5.0"
                "0irig3c79184h54zasn06yiz25znqrpvx8r72byr5gj9md2byidz"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.5
  (crate-source "miniz_oxide" "0.8.5"
                "1r9whkc61xri7m1cn4rjrjlhr32ab29nvfxcbg0ri5mmpgg08glf"))

(define rust-minspan-0.1.2
  (crate-source "minspan" "0.1.2"
                "0053r44iqmfilibz8da3367adxjjwibw6d849xifxq0yhfgf99pf"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

(define rust-multimap-0.10.0
  (crate-source "multimap" "0.10.0"
                "00vs2frqdhrr8iqx4y3fbq73ax5l12837fvbjrpi729d85alrz6y"))

(define rust-ndk-0.9.0
  (crate-source "ndk" "0.9.0"
                "1m32zpmi5w1pf3j47k6k5fw395dc7aj8d0mdpsv53lqkprxjxx63"))

(define rust-ndk-context-0.1.1
  (crate-source "ndk-context" "0.1.1"
                "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17"))

(define rust-ndk-sys-0.6.0+11769913
  (crate-source "ndk-sys" "0.6.0+11769913"
                "0wx8r6pji20if4xs04g73gxl98nmjrfc73z0v6w1ypv6a4qdlv7f"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-nix-0.27.1
  (crate-source "nix" "0.27.1"
                "0ly0kkmij5f0sqz35lx9czlbk6zpihb7yh1bsy4irzwfd2f4xc1f"))

(define rust-nix-0.28.0
  (crate-source "nix" "0.28.0"
                "1r0rylax4ycx3iqakwjvaa178jrrwiiwghcw95ndzy72zk25c8db"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-nom-8.0.0
  (crate-source "nom" "8.0.0"
                "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz"))

(define rust-noop-proc-macro-0.3.0
  (crate-source "noop_proc_macro" "0.3.0"
                "1j2v1c6ric4w9v12h34jghzmngcwmn0hll1ywly4h6lcm4rbnxh6"))

(define rust-ntapi-0.4.1
  (crate-source "ntapi" "0.4.1"
                "1r38zhbwdvkis2mzs6671cm1p6djgsl49i7bwxzrvhwicdf8k8z8"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.1
  (crate-source "nu-ansi-term" "0.50.1"
                "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-bigint-dig-0.8.4
  (crate-source "num-bigint-dig" "0.8.4"
                "0lb12df24wgxxbspz4gw1sf1kdqwvpdcpwq4fdlwg4gj41c1k16w"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-cpus-1.16.0
  (crate-source "num_cpus" "1.16.0"
                "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-enum-0.7.3
  (crate-source "num_enum" "0.7.3"
                "0yai0vafhy85mvhknzfqd7lm04hzaln7i5c599rhy8mj831kyqaf"))

(define rust-num-enum-derive-0.7.3
  (crate-source "num_enum_derive" "0.7.3"
                "0mksna1jj87ydh146gn6jcqkvvs920c3dgh0p4f3xk184kpl865g"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-iter-0.1.45
  (crate-source "num-iter" "0.1.45"
                "1gzm7vc5g9qsjjl3bqk9rz1h6raxhygbrcpbfl04swlh0i506a8l"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-threads-0.1.7
  (crate-source "num_threads" "0.1.7"
                "1ngajbmhrgyhzrlc4d5ga9ych1vrfcvfsiqz6zv0h2dpr2wrhwsw"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-number-prefix-0.4.0
  (crate-source "number_prefix" "0.4.0"
                "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3"))

(define rust-objc-sys-0.3.5
  (crate-source "objc-sys" "0.3.5"
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))

(define rust-objc2-0.5.2
  (crate-source "objc2" "0.5.2"
                "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))

(define rust-objc2-app-kit-0.2.2
  (crate-source "objc2-app-kit" "0.2.2"
                "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))

(define rust-objc2-cloud-kit-0.2.2
  (crate-source "objc2-cloud-kit" "0.2.2"
                "02dhjvmcq8c2bwj31jx423jygif1scs9f0lmlab0ayhw75b3ppbl"))

(define rust-objc2-contacts-0.2.2
  (crate-source "objc2-contacts" "0.2.2"
                "12a8m927xrrxa54xhqhqnkkl1a6l07pyrpnqfk9jz09kkh755zx5"))

(define rust-objc2-core-data-0.2.2
  (crate-source "objc2-core-data" "0.2.2"
                "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))

(define rust-objc2-core-image-0.2.2
  (crate-source "objc2-core-image" "0.2.2"
                "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))

(define rust-objc2-core-location-0.2.2
  (crate-source "objc2-core-location" "0.2.2"
                "10apgsrigqryvi4rcc0f6yfjflvrl83f4bi5hkr48ck89vizw300"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.2.2
  (crate-source "objc2-foundation" "0.2.2"
                "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))

(define rust-objc2-link-presentation-0.2.2
  (crate-source "objc2-link-presentation" "0.2.2"
                "160k4qh00yrx57dabn3hzas4r98kmk9bc0qsy1jvwday3irax8d1"))

(define rust-objc2-metal-0.2.2
  (crate-source "objc2-metal" "0.2.2"
                "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))

(define rust-objc2-quartz-core-0.2.2
  (crate-source "objc2-quartz-core" "0.2.2"
                "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))

(define rust-objc2-symbols-0.2.2
  (crate-source "objc2-symbols" "0.2.2"
                "1p04hjkxan18g2b7h9n2n8xxsvazapv2h6mfmmdk06zc7pz4ws0a"))

(define rust-objc2-ui-kit-0.2.2
  (crate-source "objc2-ui-kit" "0.2.2"
                "0vrb5r8z658l8c19bx78qks8c5hg956544yirf8npk90idwldfxq"))

(define rust-objc2-uniform-type-identifiers-0.2.2
  (crate-source "objc2-uniform-type-identifiers" "0.2.2"
                "1ziv4wkbxcaw015ypg0q49ycl7m14l3x56mpq2k1rznv92bmzyj4"))

(define rust-objc2-user-notifications-0.2.2
  (crate-source "objc2-user-notifications" "0.2.2"
                "1cscv2w3vxzaslz101ddv0z9ycrrs4ayikk4my4qd3im8bvcpkvn"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-once-cell-1.20.3
  (crate-source "once_cell" "1.20.3"
                "0bp6rgrsri1vfdcahsimk08zdiilv14ppgcnpbiw8hqyp2j64m4l"))

(define rust-oorandom-11.1.4
  (crate-source "oorandom" "11.1.4"
                "1sg4j19r5302a6jpn0kgfkbjnslrqr3ynxv8x2h2ddaaw7kvn45l"))

(define rust-opaque-debug-0.3.1
  (crate-source "opaque-debug" "0.3.1"
                "10b3w0kydz5jf1ydyli5nv10gdfp97xh79bgz327d273bs46b3f0"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-orbclient-0.3.48
  (crate-source "orbclient" "0.3.48"
                "0hzxjsvvsl5i9d3aqzc6kdcsch1i6flij5dkignhhkz2qb72c2xs"))

(define rust-ordered-float-5.0.0
  (crate-source "ordered-float" "5.0.0"
                "009z1k7w729ls2sfg4zknn9v63sk1zghnq54p2lwcjjkdvszkhg2"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-os-pipe-1.2.1
  (crate-source "os_pipe" "1.2.1"
                "10nrh0i507560rsiy4c79fajdmqgbr6dha2pbl9mncrlaq52pzaz"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-owo-colors-3.5.0
  (crate-source "owo-colors" "3.5.0"
                "0vyvry6ba1xmpd45hpi6savd8mbx09jpmvnnwkf6z62pk6s4zc61"))

(define rust-palette-0.7.6
  (crate-source "palette" "0.7.6"
                "1rmn02mv6cb112504qyg7pyfa83c08hxpk5sw7jc5v659hc73gsc"))

(define rust-palette-derive-0.7.6
  (crate-source "palette_derive" "0.7.6"
                "0c0xhpk1nqyq4jr2m8xnka7w47vqzc7m2vq9ih8wxyjv02phs0zm"))

(define rust-pango-0.20.9
  (crate-source "pango" "0.20.9"
                "1v7h4m7sz0x38il14jzsw7qphbpsw17a0hq8zj5w16ygp30ms7vb"))

(define rust-pango-sys-0.20.9
  (crate-source "pango-sys" "0.20.9"
                "1dds8ljd3ar05c9744s3xlcyg8bkg5a211mpkvj8zgbk2rsrpfqd"))

(define rust-pangocairo-0.20.7
  (crate-source "pangocairo" "0.20.7"
                "1dlp76pkknxfl6pi41zfcm9kdhchyzjs72pgl196aapa5yd51426"))

(define rust-pangocairo-sys-0.20.7
  (crate-source "pangocairo-sys" "0.20.7"
                "0rv7mnp2cnrvaq73c3dwf1szc0ngi312z4l3cyjac4br2hjarrjv"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.3
  (crate-source "parking_lot" "0.12.3"
                "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-password-hash-0.5.0
  (crate-source "password-hash" "0.5.0"
                "0ri1mim11zk0a9s40zdi288dfqvmdiryc7lw8vl46b59ifa08vrl"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-pbkdf2-0.11.0
  (crate-source "pbkdf2" "0.11.0"
                "05q9wqjvfrs4dvw03yn3bvcs4zghz0a7ycfa53pz2k2fqhp6k843"))

(define rust-pem-rfc7468-0.7.0
  (crate-source "pem-rfc7468" "0.7.0"
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-petgraph-0.6.5
  (crate-source "petgraph" "0.6.5"
                "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"))

(define rust-petgraph-0.7.1
  (crate-source "petgraph" "0.7.1"
                "0wkpppwrfv1h197asz1p4yfb4li5b1kw0nqllil67n6vj1qb6win"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-generator-0.11.3
  (crate-source "phf_generator" "0.11.3"
                "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w"))

(define rust-phf-macros-0.11.3
  (crate-source "phf_macros" "0.11.3"
                "05kjfbyb439344rhmlzzw0f9bwk9fp95mmw56zs7yfn1552c0jpq"))

(define rust-phf-shared-0.11.3
  (crate-source "phf_shared" "0.11.3"
                "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7"))

(define rust-pin-project-1.1.10
  (crate-source "pin-project" "1.1.10"
                "12kadbnfm1f43cyadw9gsbyln1cy7vj764wz5c8wxaiza3filzv7"))

(define rust-pin-project-internal-1.1.10
  (crate-source "pin-project-internal" "1.1.10"
                "0qgqzfl0f4lzaz7yl5llhbg97g68r15kljzihaw9wm64z17qx4bf"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-piper-0.2.4
  (crate-source "piper" "0.2.4"
                "0rn0mjjm0cwagdkay77wgmz3sqf8fqmv9d9czm79mvr2yj8c9j4n"))

(define rust-pixman-0.2.1
  (crate-source "pixman" "0.2.1"
                "1pqybqb7rmd58yr9xvmd8iix30znw5w71cq2wnlc16n1jva1g8nf"))

(define rust-pixman-sys-0.1.0
  (crate-source "pixman-sys" "0.1.0"
                "1nja8kc7zs1w4lhllvsgssa0b07n4cgwb0zyvqapj7g8i4z4i851"))

(define rust-pkcs1-0.7.5
  (crate-source "pkcs1" "0.7.5"
                "0zz4mil3nchnxljdfs2k5ab1cjqn7kq5lqp62n9qfix01zqvkzy8"))

(define rust-pkcs8-0.10.2
  (crate-source "pkcs8" "0.10.2"
                "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plotters-0.3.7
  (crate-source "plotters" "0.3.7"
                "0ixpy9svpmr2rkzkxvvdpysjjky4gw104d73n7pi2jbs7m06zsss"))

(define rust-plotters-backend-0.3.7
  (crate-source "plotters-backend" "0.3.7"
                "0ahpliim4hrrf7d4ispc2hwr7rzkn6d6nf7lyyrid2lm28yf2hnz"))

(define rust-plotters-svg-0.3.7
  (crate-source "plotters-svg" "0.3.7"
                "0w56sxaa2crpasa1zj0bhxzihlapqfkncggavyngg0w86anf5fji"))

(define rust-png-0.17.16
  (crate-source "png" "0.17.16"
                "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2"))

(define rust-polling-3.7.4
  (crate-source "polling" "3.7.4"
                "0bs4nhwfwsvlzlhah2gbhj3aa9ynvchv2g350wapswh26a65c156"))

(define rust-poly1305-0.8.0
  (crate-source "poly1305" "0.8.0"
                "1grs77skh7d8vi61ji44i8gpzs3r9x7vay50i6cg8baxfa8bsnc1"))

(define rust-portable-atomic-1.11.0
  (crate-source "portable-atomic" "1.11.0"
                "0glb2wngflvfmg789qbf6dbnwcf6ai212fs7n0lf1c66rd49n3im"))

(define rust-postmark-0.10.2
  (crate-source "postmark" "0.10.2"
                "10vd1xdlk189p8qphmihm9j28wdn5fclcgwc6z65fs43i4irihd8"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.20
  (crate-source "ppv-lite86" "0.2.20"
                "017ax9ssdnpww7nrl1hvqh2lzncpv04nnsibmnw9nxjnaqlpp5bp"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"))

(define rust-prettyplease-0.2.30
  (crate-source "prettyplease" "0.2.30"
                "12n09i4s7fvhdan7pxbwdf1gnz9bk62rmxncskkr9hkglm6z7k7i"))

(define rust-proc-macro-crate-3.2.0
  (crate-source "proc-macro-crate" "3.2.0"
                "0yzsqnavb3lmrcsmbrdjfrky9vcbl46v59xi9avn0796rb3likwf"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-profiling-1.0.16
  (crate-source "profiling" "1.0.16"
                "0kcz2xzg4qx01r5az8cf9ffjasi2srj56sna32igddh0vi7cggdg"))

(define rust-profiling-procmacros-1.0.16
  (crate-source "profiling-procmacros" "1.0.16"
                "0c7y2k4mz5dp2ksj1h4zbxsxq4plmjzccscdaml3h1pizdh2wpx6"))

(define rust-proptest-1.6.0
  (crate-source "proptest" "1.6.0"
                "0l4y4bb8hffv7cys7d59qwqdmvmqjfzz0x9vblc08209clqfkjhl"))

(define rust-proptest-derive-0.5.1
  (crate-source "proptest-derive" "0.5.1"
                "0jay6jwfvrwzz5bqpi4hxx3ax6kax06p0h29vgkxb0vl42nckqaf"))

(define rust-prost-0.13.5
  (crate-source "prost" "0.13.5"
                "1r8yi6zxxwv9gq5ia9p55nspgwmchs94sqpp64x33v5k3njgm5i7"))

(define rust-prost-build-0.13.5
  (crate-source "prost-build" "0.13.5"
                "1gw1mr0rmv15fc2yvn9jmxbqaj8qh80w5nn5x5s1932y8ijr8xmy"))

(define rust-prost-derive-0.13.5
  (crate-source "prost-derive" "0.13.5"
                "0kgc9gbzsa998xixblfi3kfydka64zqf6rmpm53b761cjxbxfmla"))

(define rust-prost-reflect-0.14.7
  (crate-source "prost-reflect" "0.14.7"
                "1cnxhfx0zq76pk7chi8lmgvzmmv54pcncvki8klcvxb25dcdspkv"))

(define rust-prost-types-0.13.5
  (crate-source "prost-types" "0.13.5"
                "05mx699wyg7cjil3hz7h8lp4dhi7xhy1lq5kjv1s3cfx6szw3hjj"))

(define rust-protox-0.7.2
  (crate-source "protox" "0.7.2"
                "0jmmcil88n15kdpac51fz9qjagchpy3pq3vjrj77nqxz67rjldbg"))

(define rust-protox-parse-0.7.0
  (crate-source "protox-parse" "0.7.0"
                "1pld0s1cg9favgy9bafkwlvmg65ky13rmhh0w050hb262p8n5953"))

(define rust-puffin-0.16.0
  (crate-source "puffin" "0.16.0"
                "08ass1hfdcq86y7dywa1jylzq57la95rgpcmd6yx82hs9symlhkn"))

(define rust-puffin-0.19.1
  (crate-source "puffin" "0.19.1"
                "07vlkf4i88475a80fhckayzxr9v4pkc21kwvpjkc2bn00mxsx7gs"))

(define rust-puffin-http-0.13.0
  (crate-source "puffin_http" "0.13.0"
                "14w1ihjlv48mpbh114yvgixdqdnzzipnmsg158l3v49m1ihgrgqk"))

(define rust-qoi-0.4.1
  (crate-source "qoi" "0.4.1"
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))

(define rust-quanta-0.11.1
  (crate-source "quanta" "0.11.1"
                "1axrw0nqc90bq671w05jd9460pmwg86c4r132mjsi4c2g8m6czm1"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.37.2
  (crate-source "quick-xml" "0.37.2"
                "00y0qagwbxd3lqarr13j35d6kwmni176znf5jrxxcyazwplmjn0n"))

(define rust-quinn-0.11.6
  (crate-source "quinn" "0.11.6"
                "1vq55p4kfc4zjxj58xrpf3kcjjqi4mn0wf52a5rzkiky4w46isb2"))

(define rust-quinn-proto-0.11.9
  (crate-source "quinn-proto" "0.11.9"
                "0p8k3iqd0rcxc7b6m2yyijhw4bpfwa61lyzigwvjwzax97rmxzm2"))

(define rust-quinn-udp-0.5.10
  (crate-source "quinn-udp" "0.5.10"
                "0i2rkq8lrkr89csw00mhnhp8zjh2prv4n5n65fwzd1b7hrak0vz4"))

(define rust-quote-1.0.39
  (crate-source "quote" "1.0.39"
                "00a8q2w3aacil4aqnndyv73k0x4lj55kp487k66nbq89x5693wf1"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-xorshift-0.3.0
  (crate-source "rand_xorshift" "0.3.0"
                "13vcag7gmqspzyabfl1gr9ykvxd2142q2agrj8dkyjmfqmgg4nyj"))

(define rust-ratatui-0.27.0
  (crate-source "ratatui" "0.27.0"
                "1lv8r99miibk6np2d2m0y6vf62jf5dr1x272ws6byalnnp2lcrfi"))

(define rust-rav1e-0.7.1
  (crate-source "rav1e" "0.7.1"
                "1sawva6nmj2fvynydbcirr3nb7wjyg0id2hz2771qnv6ly0cx1yd"))

(define rust-ravif-0.11.11
  (crate-source "ravif" "0.11.11"
                "1ij51acd3pkl3rr2ha3r3nc7pvg649m49bvyngpcv98fpnbgs4r4"))

(define rust-raw-cpuid-10.7.0
  (crate-source "raw-cpuid" "10.7.0"
                "0ckkg47m8wbdinqg4z4dx7ipi3d7fjxdnrwzikx70x46rdwpcabc"))

(define rust-raw-window-handle-0.6.2
  (crate-source "raw-window-handle" "0.6.2"
                "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0"))

(define rust-rayon-1.10.0
  (crate-source "rayon" "1.10.0"
                "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))

(define rust-rayon-core-1.12.1
  (crate-source "rayon-core" "1.12.1"
                "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-syscall-0.5.10
  (crate-source "redox_syscall" "0.5.10"
                "1l9b638qx72312yzh8ykvda9b3lqd9gf6yqn66b23a331ck0r30b"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-redox-users-0.5.0
  (crate-source "redox_users" "0.5.0"
                "0awxx66izdw6kz97r3zxrl5ms5f6dqi5l0f58mlsvlmx8wyrsvyx"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-lite-0.1.6
  (crate-source "regex-lite" "0.1.6"
                "0almvx3z75f611pdcd9mslh7zxg76zh3shifql4ndch6mn3rb92k"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-renderdoc-0.11.0
  (crate-source "renderdoc" "0.11.0"
                "04hycbzwqmzw25qnk0lwps70jgxi43cgmkjdvwbyzc183vnajb97"))

(define rust-renderdoc-sys-1.1.0
  (crate-source "renderdoc-sys" "1.1.0"
                "0cj8zjs7k0gvchcx3jhpg8r9bbqy8b1hsgbz0flcq2ydn12hmcqr"))

(define rust-reqwest-0.11.27
  (crate-source "reqwest" "0.11.27"
                "0qjary4hpplpgdi62d2m0xvbn6lnzckwffm0rgkm2x51023m6ryx"))

(define rust-reqwest-0.12.12
  (crate-source "reqwest" "0.12.12"
                "1nnigi6jcrqdd5k5myc53qdkdnrx8zjgan029q1w5hspf5039rs3"))

(define rust-rgb-0.8.50
  (crate-source "rgb" "0.8.50"
                "02ii3nsciska0sj23ggxaz8gj64ksw8nbpfjcwxlh037chb7sfap"))

(define rust-ring-0.17.11 'crates-crypto-rust-ring-0.17)

(define rust-rmp-0.8.14
  (crate-source "rmp" "0.8.14"
                "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2"))

(define rust-rpassword-7.3.1
  (crate-source "rpassword" "7.3.1"
                "0gvy3lcpph9vv1rl0cjfn72ylvmgbw2vklmj6w0iv4cpr3ijniw0"))

(define rust-rsa-0.9.7
  (crate-source "rsa" "0.9.7"
                "06amqm85raq26v6zg00fbf93lbj3kx559n2lpxc3wrvbbiy5vis7"))

(define rust-rtoolbox-0.0.2
  (crate-source "rtoolbox" "0.0.2"
                "03n9z8x353kylxhr9im8zawcisnmid3jiqrs8rbdn313cd7d4iy2"))

(define rust-runtime-format-0.1.3
  (crate-source "runtime-format" "0.1.3"
                "154c7jq7kbpc5acn2ysa2ilab2x0i5y7d34jwznni9xw71dqv589"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-0.23.23
  (crate-source "rustls" "0.23.23"
                "15gk2bmry78cps3ya38a7cn4jxc36xv1r7gndr0fbz40qjc6qya7"))

(define rust-rustls-native-certs-0.6.3
  (crate-source "rustls-native-certs" "0.6.3"
                "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))

(define rust-rustls-pemfile-2.2.0
  (crate-source "rustls-pemfile" "2.2.0"
                "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"))

(define rust-rustls-pki-types-1.11.0
  (crate-source "rustls-pki-types" "1.11.0"
                "0755isc0x5iymm3wsn59s0ad1pm9zidw7p34qfqlsjsac9jf4z4i"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-rustls-webpki-0.102.8
  (crate-source "rustls-webpki" "0.102.8"
                "1sdy8ks86b7jpabpnb2px2s7f1sq8v0nqf6fnlvwzm4vfk41pjk4"))

(define rust-rustversion-1.0.20
  (crate-source "rustversion" "1.0.20"
                "1lhwjb16dsm8brd18bn2bh0ryzc7qi29bi2jjsc6ny2zbwn3ivgd"))

(define rust-rusty-fork-0.3.0
  (crate-source "rusty-fork" "0.3.0"
                "0kxwq5c480gg6q0j3bg4zzyfh2kwmc3v2ba94jw8ncjc8mpcqgfb"))

(define rust-rusty-paserk-0.4.0
  (crate-source "rusty_paserk" "0.4.0"
                "0f0xqrjbvx7mb2ynnqni9ql8qlg3zzn504vnyjmyh7ilrlgailx1"))

(define rust-rusty-paseto-0.7.2
  (crate-source "rusty_paseto" "0.7.2"
                "09kqhfi2lnjhl9wjb26j6xg26k3w41i1ll3ardjw1ifali0ihl05"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-salsa20-0.10.2
  (crate-source "salsa20" "0.10.2"
                "04w211x17xzny53f83p8f7cj7k2hi8zck282q5aajwqzydd2z8lp"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"))

(define rust-schemars-0.8.22
  (crate-source "schemars" "0.8.22"
                "05an9nbi18ynyxv1rjmwbg6j08j0496hd64mjggh53mwp3hjmgrz"))

(define rust-schemars-derive-0.8.22
  (crate-source "schemars_derive" "0.8.22"
                "0kakyzrp5801s4i043l4ilv96lzimnlh01pap958h66n99w6bqij"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))

(define rust-sd-notify-0.4.5
  (crate-source "sd-notify" "0.4.5"
                "1x1bmz30x2i35j771rqyyan40473aqk0xjrh2dk9xdnqf7gylhxr"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-sys-2.14.0
  (crate-source "security-framework-sys" "2.14.0"
                "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9"))

(define rust-semver-1.0.26
  (crate-source "semver" "1.0.26"
                "1l5q2vb8fjkby657kdyfpvv40x2i2xqq9bg57pxqakfj92fgmrjn"))

(define rust-serde-1.0.218
  (crate-source "serde" "1.0.218"
                "0q6z4bnrwagnms0bds4886711l6mc68s979i49zd3xnvkg8wkpz8"))

(define rust-serde-derive-1.0.218
  (crate-source "serde_derive" "1.0.218"
                "0azqd74xbpb1v5vf6w1fdbgmwp39ljjfj25cib5rgrzlj7hh75gh"))

(define rust-serde-derive-internals-0.29.1
  (crate-source "serde_derive_internals" "0.29.1"
                "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq"))

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-path-to-error-0.1.17
  (crate-source "serde_path_to_error" "0.1.17"
                "0alb447z25dvczd6ll3vfjbf51pypn23mgs5hv8978vzjczv3yjr"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.8
  (crate-source "serde_spanned" "0.6.8"
                "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-serde-with-3.12.0
  (crate-source "serde_with" "3.12.0"
                "1ai9c3cbdgrsvmlc4qpg9z73y80yplk3k7zp45wp97xnzkrggdnn"))

(define rust-serde-with-macros-3.12.0
  (crate-source "serde_with_macros" "3.12.0"
                "13hznly0qq1rngsdh8gpnajab2knkrmvwwrbmii86g1s36jwl04d"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shellexpand-3.1.0
  (crate-source "shellexpand" "3.1.0"
                "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.17
  (crate-source "signal-hook" "0.3.17"
                "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))

(define rust-signal-hook-mio-0.2.4
  (crate-source "signal-hook-mio" "0.2.4"
                "1k8pl9aafiadr4czsg8zal9b4jdk6kq5985p90i19jc5sh31mnrl"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-signature-2.2.0
  (crate-source "signature" "2.2.0"
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simd-helpers-0.1.0
  (crate-source "simd_helpers" "0.1.0"
                "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-sketches-ddsketch-0.2.2
  (crate-source "sketches-ddsketch" "0.2.2"
                "0p6n1v0p0773d0b5qnsnw526g7hhlb08bx95wm0zb09xnwa6qqw5"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-slotmap-1.0.7
  (crate-source "slotmap" "1.0.7"
                "0amqb2fn9lcy1ri0risblkcp88dl0rnfmynw7lx0nqwza77lmzyv"))

(define rust-smallvec-1.14.0
  (crate-source "smallvec" "1.14.0"
                "1z8wpr53x6jisklqhkkvkgyi8s5cn69h2d2alhqfxahzxwiq7kvz"))

(define rust-smawk-0.3.2
  (crate-source "smawk" "0.3.2"
                "0344z1la39incggwn6nl45k8cbw2x10mr5j0qz85cdz9np0qihxp"))

(define rust-smithay-client-toolkit-0.19.2
  (crate-source "smithay-client-toolkit" "0.19.2"
                "05h05hg4dn3v6br5jbdbs5nalk076a64s7fn6i01nqzby2hxwmrl"))

(define rust-smol-str-0.2.2
  (crate-source "smol_str" "0.2.2"
                "1bfylqf2vnqaglw58930vpxm2rfzji5gjp15a2c0kh8aj6v8ylyx"))

(define rust-socket2-0.5.8
  (crate-source "socket2" "0.5.8"
                "1s7vjmb5gzp3iaqi94rh9r63k9cj00kjgbfn7gn60kmnk6fjcw69"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-spki-0.7.3
  (crate-source "spki" "0.7.3"
                "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"))

(define rust-sql-builder-3.1.1
  (crate-source "sql-builder" "3.1.1"
                "1h5xp47zz9chv545lpmal51fq3z162z2f99mb4lhcbgcsaaqs05i"))

(define rust-sqlx-0.8.3
  (crate-source "sqlx" "0.8.3"
                "0pvlpq0plgyxf5kikcv786pf0pjv8dx5shlvz72l510d7hxyf424"))

(define rust-sqlx-core-0.8.3
  (crate-source "sqlx-core" "0.8.3"
                "1q31dawr61wc6q2f12my4fw082mbv8sxwz1082msjsk76rlpn03a"))

(define rust-sqlx-macros-0.8.3
  (crate-source "sqlx-macros" "0.8.3"
                "047k67sylscv0gdhwwqrn0s33jy1mvq8rmqq6s8fygv4g2ny44ii"))

(define rust-sqlx-macros-core-0.8.3
  (crate-source "sqlx-macros-core" "0.8.3"
                "1bg7sn6l8dc4pzrqx2dwc3sp7dbn97msfqahpycnl55bqnn917sf"))

(define rust-sqlx-mysql-0.8.3
  (crate-source "sqlx-mysql" "0.8.3"
                "0czjzzjm2y6lkhxvvzrzwgp0pmlhymcnym20hn9n9kh01s7jfq25"))

(define rust-sqlx-postgres-0.8.3
  (crate-source "sqlx-postgres" "0.8.3"
                "04wnjl51kfx0qbfsfmhqdshpmw32vzz2p8dksmj6gvb3ydbqmff5"))

(define rust-sqlx-sqlite-0.8.3
  (crate-source "sqlx-sqlite" "0.8.3"
                "0h05ca26g428h4337k4nm0ww75bcdkiqzp883m7fc92v78fsfp7q"))

(define rust-stability-0.2.1
  (crate-source "stability" "0.2.1"
                "1b7w6qknq0w5y7s358j62pzi9kbh6g73lal3jx9aydpikl0ff16r"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-stringprep-0.1.5
  (crate-source "stringprep" "0.1.5"
                "1cb3jis4h2b767csk272zw92lc6jzfzvh8d6m1cd86yqjb9z6kbv"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strum-0.26.3
  (crate-source "strum" "0.26.3"
                "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g"))

(define rust-strum-macros-0.26.4
  (crate-source "strum_macros" "0.26.4"
                "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-supports-color-2.1.0
  (crate-source "supports-color" "2.1.0"
                "12csf7chawxinaapm9rh718nha9hggk6ra86fdaw9hxdagg8qffn"))

(define rust-supports-hyperlinks-2.1.0
  (crate-source "supports-hyperlinks" "2.1.0"
                "0g93nh1db3f9lyd0ry35bqjrxkg6sbysn36x9hgd9m5h5rlk2hpq"))

(define rust-supports-unicode-2.1.0
  (crate-source "supports-unicode" "2.1.0"
                "0yp703pvpzpmaw9mpncvwf0iqis4xmhs569ii1g20jhqvngc2l7q"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.99
  (crate-source "syn" "2.0.99"
                "1hizbzkwa6wgi77x9ck45p3fshrwfmj448qfcjfzv3z1h5994bp0"))

(define rust-sync-wrapper-0.1.2
  (crate-source "sync_wrapper" "0.1.2"
                "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-sysinfo-0.30.13
  (crate-source "sysinfo" "0.30.13"
                "1csbkx1hdlacgzw5ynjyfvgc1xg58w3h1rgh5gm2pysmxvd4snqa"))

(define rust-system-configuration-0.5.1
  (crate-source "system-configuration" "0.5.1"
                "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms"))

(define rust-system-configuration-sys-0.5.0
  (crate-source "system-configuration-sys" "0.5.0"
                "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"
                #:snippet '(delete-file "src/tests/lib/libteststatic.a")))

(define rust-system-deps-7.0.3
  (crate-source "system-deps" "7.0.3"
                "01d0fllzpkfybzadyaq1vlx70imzj56dxs4rk9w2f4ikkypkmlk6"
                #:snippet '(delete-file "src/tests/lib/libteststatic.a")))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-tempfile-3.17.1
  (crate-source "tempfile" "3.17.1"
                "0c52ggq5vy5mzgk5ly36cgzs1cig3cv6r1jarijmzxgkn6na1r92"))

(define rust-terminal-size-0.1.17
  (crate-source "terminal_size" "0.1.17"
                "1pq60ng1a7fjp597ifk1cqlz8fv9raz9xihddld1m1pfdia1lg33"))

(define rust-terminal-size-0.4.1
  (crate-source "terminal_size" "0.4.1"
                "1sd4nq55h9sjirkx0138zx711ddxq1k1a45lc77ninhzj9zl8ljk"))

(define rust-testing-logger-0.1.1
  (crate-source "testing_logger" "0.1.1"
                "087pi7y9iisspafyzblj41qvrw95dfb6px7pavlkmls5rckvg4kd"))

(define rust-textwrap-0.15.2
  (crate-source "textwrap" "0.15.2"
                "0galmidi6gpn308b1kv3r4qbb48j2926lcj0idwhdhlylhjybcxp"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-tiff-0.9.1
  (crate-source "tiff" "0.9.1"
                "0ghyxlz566dzc3scvgmzys11dhq2ri77kb8sznjakijlxby104xs"))

(define rust-time-0.3.37
  (crate-source "time" "0.3.37"
                "08bvydyc14plkwhchzia5bcdbmm0mk5fzilsdpjx06w6hf48drrm"))

(define rust-time-core-0.1.2
  (crate-source "time-core" "0.1.2"
                "1wx3qizcihw6z151hywfzzyd1y5dl804ydyxci6qm07vbakpr4pg"))

(define rust-time-macros-0.2.19
  (crate-source "time-macros" "0.2.19"
                "1pl558z26pp342l5y91n6dxb60xwhar975wk6jc4npiygq0ycd18"))

(define rust-tiny-bip39-1.0.0
  (crate-source "tiny-bip39" "1.0.0"
                "0q98iv3wgbd41wyxxd5is8sddi53k9ary45rbi5fi8dmb39r9k32"))

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tinyvec-1.9.0
  (crate-source "tinyvec" "1.9.0"
                "0w9w8qcifns9lzvlbfwa01y0skhr542anwa3rpn28rg82wgndcq9"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.43.0
  (crate-source "tokio" "1.43.0"
                "17pdm49ihlhfw3rpxix3kdh2ppl1yv7nwp1kxazi5r1xz97zlq9x"))

(define rust-tokio-macros-2.5.0
  (crate-source "tokio-macros" "2.5.0"
                "1f6az2xbvqp7am417b78d1za8axbvjvxnmkakz9vr8s52czx81kf"))

(define rust-tokio-rustls-0.24.1
  (crate-source "tokio-rustls" "0.24.1"
                "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))

(define rust-tokio-rustls-0.26.2
  (crate-source "tokio-rustls" "0.26.2"
                "16wf007q3584j46wc4s0zc4szj6280g23hka6x6bgs50l4v7nwlf"))

(define rust-tokio-stream-0.1.17
  (crate-source "tokio-stream" "0.1.17"
                "0ix0770hfp4x5rh5bl7vsnr3d4iz4ms43i522xw70xaap9xqv9gc"))

(define rust-tokio-util-0.7.13
  (crate-source "tokio-util" "0.7.13"
                "0y0h10a52c7hrldmr3410bp7j3fadq0jn9nf7awddgd2an6smz6p"))

(define rust-toml-0.5.11
  (crate-source "toml" "0.5.11"
                "0d2266nx8b3n22c7k24x4428z6di8n83a9n466jm7a2hipfz1xzl"))

(define rust-toml-0.8.20
  (crate-source "toml" "0.8.20"
                "0j012b37iz1mihksr6a928s6dzszxvblzg3l5wxp7azzsv6sb1yd"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

(define rust-toml-edit-0.22.24
  (crate-source "toml_edit" "0.22.24"
                "0x0lgp70x5cl9nla03xqs5vwwwlrwmd0djkdrp3h3lpdymgpkd0p"))

(define rust-tonic-0.12.3
  (crate-source "tonic" "0.12.3"
                "0ljd1lfjpw0vrm5wbv15x6nq2i38llsanls5rkzmdn2n0wrmnz47"))

(define rust-tonic-build-0.12.3
  (crate-source "tonic-build" "0.12.3"
                "04baqblgrlc0g8scnhpky5s0n4cljaixrrdrr6cv6wx7kq8cwmwm"))

(define rust-tonic-types-0.12.3
  (crate-source "tonic-types" "0.12.3"
                "0rxkz100jaiqlr47dim69mfhyq54c3lynnia75qi5l2713pdi080"))

(define rust-tower-0.4.13
  (crate-source "tower" "0.4.13"
                "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq"))

(define rust-tower-0.5.2
  (crate-source "tower" "0.5.2"
                "1ybmd59nm4abl9bsvy6rx31m4zvzp5rja2slzpn712y9b68ssffh"))

(define rust-tower-http-0.5.2
  (crate-source "tower-http" "0.5.2"
                "1xakj3x0anp55gjqibiwvzma5iz0w9pcjsr7qk97sx4qm4sd970y"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-attributes-0.1.28
  (crate-source "tracing-attributes" "0.1.28"
                "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-tracing-tree-0.4.0
  (crate-source "tracing-tree" "0.4.0"
                "175lqyfp6zq7jbj8m026xdp8p765pzgfdzfxahfggmdhy5wwlngl"))

(define rust-tracy-client-0.17.6
  (crate-source "tracy-client" "0.17.6"
                "0zkwz9aq97znyal3hz9wmxya97pj01ddpv92ha7l39a6fdw2s83k"))

(define rust-tracy-client-0.18.0
  (crate-source "tracy-client" "0.18.0"
                "1nrn739vanildbbzfdcsh8y1fzp2p848db49vmpvf0jv600jq2nr"))

(define rust-tracy-client-sys-0.24.3
  ;; TODO: Unbundle tracy-0.11.
  (crate-source "tracy-client-sys" "0.24.3"
                "0ps3iwb7q1fzs9pir6b0nqi8n7i67lci4jp6z4xrq8s8lmyz7zv9"))

(define rust-tree-magic-mini-3.1.6
  (crate-source "tree_magic_mini" "3.1.6"
                "0qwx2b0xfr00vdskl951cvh3m040zj5n8vm7ln4k6p143ybyiida"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-typed-builder-0.18.2
  (crate-source "typed-builder" "0.18.2"
                "1p9s9p7f3mnylrzdqbxj73d9dw95syma6pnnyfp3ys801s49qwvp"))

(define rust-typed-builder-macro-0.18.2
  (crate-source "typed-builder-macro" "0.18.2"
                "0qwfq0q2lkg4bkmcpsqajy3ss2sb2h47dj5zhfwvbp27ygx8sw8z"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-udev-0.9.3
  (crate-source "udev" "0.9.3"
                "17vy1yc6ipb5m2kc2d4lx2qpj45yr7grsjzm3y2gq0a4xblkfkmg"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicode-bidi-0.3.18
  (crate-source "unicode-bidi" "0.3.18"
                "1xcxwbsqa24b8vfchhzyyzgj0l6bn51ib5v8j6krha0m77dva72w"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-linebreak-0.1.5
  (crate-source "unicode-linebreak" "0.1.5"
                "07spj2hh3daajg335m4wdav6nfkl0f6c0q72lc37blr97hych29v"))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-properties-0.1.3
  (crate-source "unicode-properties" "0.1.3"
                "1l3mbgzwz8g14xcs09p4ww3hjkjcf0i1ih13nsg72bhj8n5jl3z7"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-truncate-1.1.0
  (crate-source "unicode-truncate" "1.1.0"
                "1gr7arjjhrhy8dww7hj8qqlws97xf9d276svr4hs6pxgllklcr5k"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-universal-hash-0.5.1
  (crate-source "universal-hash" "0.5.1"
                "1sh79x677zkncasa95wz05b36134822w6qxmi1ck05fwi33f47gw"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-utf16-iter-1.0.5
  (crate-source "utf16_iter" "1.0.5"
                "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.15.1
  (crate-source "uuid" "1.15.1"
                "11ymndpddvsjlais2fkaq4ln3n5xzn1vlr4b2bkcr6034kil1xg0"))

(define rust-v-frame-0.3.8
  (crate-source "v_frame" "0.3.8"
                "0az9nd6qi1gyikh9yb3lhm453kf7d5isd6xai3j13kds4jm2mwyn"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"
                #:snippet
                '(for-each delete-file-recursively
                           '("test-data/normalized/installed/arm64-ios"
                             "test-data/normalized/installed/x64-osx"
                             "test-data/normalized/installed/x64-windows"
                             "test-data/normalized/installed/x64-windows-static"
                             "test-data/normalized/installed/x86-windows"
                             "test-data/no-status/installed/x64-windows"))))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.13.3+wasi-0.2.2
  (crate-source "wasi" "0.13.3+wasi-0.2.2"
                "1lnapbvdcvi3kc749wzqvwrpd483win2kicn1faa4dja38p6v096"))

(define rust-wasite-0.1.0
  (crate-source "wasite" "0.1.0"
                "0nw5h9nmcl4fyf4j5d4mfdjfgvwi1cakpi349wc4zrr59wxxinmq"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-backend-0.2.100
  (crate-source "wasm-bindgen-backend" "0.2.100"
                "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig"))

(define rust-wasm-bindgen-futures-0.4.50
  (crate-source "wasm-bindgen-futures" "0.4.50"
                "0q8ymi6i9r3vxly551dhxcyai7nc491mspj0j1wbafxwq074fpam"))

(define rust-wasm-bindgen-macro-0.2.100
  (crate-source "wasm-bindgen-macro" "0.2.100"
                "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-wayland-backend-0.3.8
  (crate-source "wayland-backend" "0.3.8"
                "1gs7dw6s3lp9g6g0rhk4bh66wl41jnbkd27c6ynhv1x3xac8j85p"))

(define rust-wayland-client-0.31.8
  (crate-source "wayland-client" "0.31.8"
                "0gzpr9gdd8yk1crflxngg5iwa1szyyzp4i4zbgpslf1nsgihs4n2"))

(define rust-wayland-csd-frame-0.3.0
  (crate-source "wayland-csd-frame" "0.3.0"
                "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32"))

(define rust-wayland-cursor-0.31.8
  (crate-source "wayland-cursor" "0.31.8"
                "0pccjqiln1izjbdzprqiijhad4k00wmr5r003a44h1v5nv5jjc59"))

(define rust-wayland-egl-0.32.5
  (crate-source "wayland-egl" "0.32.5"
                "00lr5nlc7xa050p7rzlrndlc82iy0g29lhpxizs73qhh38j3hj2h"))

(define rust-wayland-protocols-0.31.2
  (crate-source "wayland-protocols" "0.31.2"
                "1x310l1p6p3p3l76nl1l2yava9408dy77s605917zadlp1jz70cg"))

(define rust-wayland-protocols-0.32.6
  (crate-source "wayland-protocols" "0.32.6"
                "1z0yahh48x8qzdbcallmxn5am5897hkk5d7p51ly6dwvhr3cz087"))

(define rust-wayland-protocols-misc-0.3.6
  (crate-source "wayland-protocols-misc" "0.3.6"
                "0m2spzy9diwc3sx0bqq7qx8qdip5lw1ns227bnqinv8220cfxdzy"))

(define rust-wayland-protocols-plasma-0.3.6
  (crate-source "wayland-protocols-plasma" "0.3.6"
                "1cs6gpgxybjvr60j6j824blsvz4hnmjwaah2cdkzvzh3cz3srjkw"))

(define rust-wayland-protocols-wlr-0.2.0
  (crate-source "wayland-protocols-wlr" "0.2.0"
                "1mjww9psk2nc5hm2q4s3qas30rbzfg1sb6qgw518fbbcdfvn27xd"))

(define rust-wayland-protocols-wlr-0.3.6
  (crate-source "wayland-protocols-wlr" "0.3.6"
                "1cpqb0d4ryf87x2wgca5n71wilhvc0jjva0zasbdgalmypk052i4"))

(define rust-wayland-scanner-0.31.6
  (crate-source "wayland-scanner" "0.31.6"
                "110ldnyfxjqvjssir1jf3ndlci7xy9lpv4aqg775y518bpyxlvw9"))

(define rust-wayland-server-0.31.7
  (crate-source "wayland-server" "0.31.7"
                "1jx410qa59vry55xm40dqgqa7d0cx7xs3a5qaxv8xzwcsrzbvylp"))

(define rust-wayland-sys-0.31.6
  (crate-source "wayland-sys" "0.31.6"
                "05b6i4lg2qrrz7l4h2b5fd7blkkvxq34i1yvlngsmmbpkhwvpknv"))

(define rust-web-sys-0.3.77
  (crate-source "web-sys" "0.3.77"
                "1lnmc1ffbq34qw91nndklqqm75rasaffj2g4f8h1yvqqz4pdvdik"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpki-roots-0.26.8
  (crate-source "webpki-roots" "0.26.8"
                "1jf54brni9lk4ak5pkma2pn18hli22gr7i7wp9zn2lzayy8v4412"))

(define rust-weezl-0.1.8
  (crate-source "weezl" "0.1.8"
                "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-whoami-1.5.2
  (crate-source "whoami" "1.5.2"
                "0vdvm6sga4v9515l6glqqfnmzp246nq66dd09cw5ri4fyn3mnb9p"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-0.52.0
  (crate-source "windows" "0.52.0"
                "1gnh210qjlprpd1szaq04rjm1zqgdm9j7l9absg0kawi2rwm72p4"))

(define rust-windows-0.58.0
  (crate-source "windows" "0.58.0"
                "1dkjj94b0gn91nn1n22cvm4afsj98f5qrhcl3112v6f4jcfx816x"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-core-0.58.0
  (crate-source "windows-core" "0.58.0"
                "16czypy425jzmiys4yb3pwsh7cm6grxn9kjp889iqnf2r17d99kb"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-implement-0.58.0
  (crate-source "windows-implement" "0.58.0"
                "16spr5z65z21qyv379rv2mb1s5q2i9ibd1p2pkn0dr9qr535pg9b"))

(define rust-windows-interface-0.58.0
  (crate-source "windows-interface" "0.58.0"
                "059mxmfvx3x88q74ms0qlxmj2pnidmr5mzn60hakn7f95m34qg05"))

(define rust-windows-link-0.1.0
  (crate-source "windows-link" "0.1.0"
                "1qr0srnkw148wbrws3726pm640h2vxgcdlxn0cxpbcg27irzvk3d"))

(define rust-windows-registry-0.2.0
  (crate-source "windows-registry" "0.2.0"
                "1c04923fq0rbvl3z0h67xr6rh2fgwkizhclhqv0j79i0nwdh0074"))

(define rust-windows-result-0.2.0
  (crate-source "windows-result" "0.2.0"
                "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x"))

(define rust-windows-strings-0.1.0
  (crate-source "windows-strings" "0.1.0"
                "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac"))

(define rust-windows-sys-0.45.0
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.48.0
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"
                #:snippet '(for-each delete-file (find-files "." "\\.a$"))))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"
                #:snippet '(for-each delete-file (find-files "." "\\.lib$"))))

(define rust-winit-0.30.9
  (crate-source "winit" "0.30.9"
                "1h1wmvhfcq2lg6c6715d7pgnv85508zm94ahcfvaiv68337yl2d8"))

(define rust-winnow-0.7.3
  (crate-source "winnow" "0.7.3"
                "1c9bmhpdwbdmll6b4l6skabz0296dchnmnxw84hh2y3ggyllwzqf"))

(define rust-winreg-0.50.0
  (crate-source "winreg" "0.50.0"
                "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))

(define rust-wio-0.2.2
  (crate-source "wio" "0.2.2"
                "199p404fp96w1f1c93bf1jrvaqwypxf3hmmldhww4jk4yhr9j4jx"))

(define rust-wit-bindgen-rt-0.33.0
  (crate-source "wit-bindgen-rt" "0.33.0"
                "0g4lwfp9x6a2i1hgjn8k14nr4fsnpd5izxhc75zpi2s5cvcg6s1j"
                #:snippet '(for-each delete-file (find-files "." "\\.(a|o)$"))))

(define rust-wl-clipboard-rs-0.8.1
  (crate-source "wl-clipboard-rs" "0.8.1"
                "1nwa0bg6jpq5sd8x94xgkj0yk7zcz2m3sg2mm26b35qlj5rigd0j"))

(define rust-wlcs-0.1.0
  (crate-source "wlcs" "0.1.0"
                "17k0nwn3f2z71rncb8glb4x15m5zmcbklnk71hpv739nrq2w769d"))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-x11-dl-2.21.0
  (crate-source "x11-dl" "2.21.0"
                "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq"))

(define rust-x11rb-0.13.1
  (crate-source "x11rb" "0.13.1"
                "04jyfm0xmc538v09pzsyr2w801yadsgvyl2p0p76hzzffg5gz4ax"))

(define rust-x11rb-protocol-0.13.1
  (crate-source "x11rb-protocol" "0.13.1"
                "0gfbxf2k7kbk577j3rjhfx7hm70kmwln6da7xyc4l2za0d2pq47c"))

(define rust-xcursor-0.3.8
  (crate-source "xcursor" "0.3.8"
                "0qazsl7h8nrbbzx84qrv39w8m2qc27g0mvrszgdls2v6n6k3vwqf"))

(define rust-xdg-home-1.3.0
  (crate-source "xdg-home" "1.3.0"
                "1xm122zz0wjc8p8cmchij0j9nw34hwncb39jc7dc0mgvb2rdl77c"))

(define rust-xkbcommon-0.7.0
  (crate-source "xkbcommon" "0.7.0"
                "07n9shhcls66wjvmk5pzqql46ipfdv7b8hbc384wgv9hk4jpv1hk"))

(define rust-xkbcommon-0.8.0
  (crate-source "xkbcommon" "0.8.0"
                "1j8s1sfwc6bw9phsca65rw3q3b5l2651v1s0pk5yxm6baa9wlrld"))

(define rust-xkbcommon-dl-0.4.2
  (crate-source "xkbcommon-dl" "0.4.2"
                "1iai0r3b5skd9vbr8z5b0qixiz8jblzfm778ddm8ba596a0dwffh"))

(define rust-xkeysym-0.2.1
  (crate-source "xkeysym" "0.2.1"
                "0mksx670cszyd7jln6s7dhkw11hdfv7blwwr3isq98k22ljh1k5r"))

(define rust-xml-rs-0.8.25
  (crate-source "xml-rs" "0.8.25"
                "1i73ajf6scni5bi1a51r19xykgrambdx5fkks0fyg5jqqbml1ff5"))

(define rust-xshell-0.2.7
  (crate-source "xshell" "0.2.7"
                "0g9pd9bfp0f35rzichic55k7p1mn8mqp607y5rimhiq14g390wly"))

(define rust-xshell-macros-0.2.7
  (crate-source "xshell-macros" "0.2.7"
                "0irm50jxdc92r0kd6yvl5p28jsfzha59brxk7z9w3jcf7z6h1b1j"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"))

(define rust-yansi-term-0.1.2
  (crate-source "yansi-term" "0.1.2"
                "1w8vjlvxba6yvidqdvxddx3crl6z66h39qxj8xi6aqayw2nk0p7y"))

(define rust-yoke-0.7.5
  (crate-source "yoke" "0.7.5"
                "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj"))

(define rust-yoke-derive-0.7.5
  (crate-source "yoke-derive" "0.7.5"
                "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013"))

(define rust-zbus-5.5.0
  (crate-source "zbus" "5.5.0"
                "0dmjaih7gi2d0fa37zzylvbmxqn80x4d7haxr5xn86za93v37hsr"))

(define rust-zbus-macros-5.5.0
  (crate-source "zbus_macros" "5.5.0"
                "1h4zf0wh647fvv97bnsr3ah64cgcnz1r8d10c2q3w2hdxc8as9gk"))

(define rust-zbus-names-4.2.0
  (crate-source "zbus_names" "4.2.0"
                "15ybdd6zic7simi9wjg0ywrxfq4sxg3z0wiyysadps3cpxj8xrkv"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zeroize-derive-1.4.2
  (crate-source "zeroize_derive" "1.4.2"
                "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

(define rust-zune-jpeg-0.4.14
  (crate-source "zune-jpeg" "0.4.14"
                "0a70sbnxxkgfm777i1xjkhyn8mx07swg5cabbi083pyysywbm9cr"))

(define rust-zvariant-5.4.0
  (crate-source "zvariant" "5.4.0"
                "1b53qpb3q7j233512s2684iy7wyydra31pi5vkxwygw98kh9xpxj"))

(define rust-zvariant-derive-5.4.0
  (crate-source "zvariant_derive" "5.4.0"
                "0bsbz68dsvkynnnpxpfchmhyl5bsgjjmcbazjg24rf5qhnm0q5vl"))

(define rust-zvariant-utils-3.2.0
  (crate-source "zvariant_utils" "3.2.0"
                "0d7wllndiv7vwgapmji3q9sxmzbaqfdxjwkqnx9vbmz58gpdyvp1"))

(define ssss-separator 'end-of-crates)


;;;
;;; *-cargo-inputs
;;;

(define-public atuin-cargo-inputs
  (list rust-addr2line-0.24.2
        rust-adler2-2.0.0
        rust-aead-0.5.2
        rust-ahash-0.8.11
        rust-aho-corasick-1.1.3
        rust-allocator-api2-0.2.21
        rust-android-tzdata-0.1.1
        rust-android-system-properties-0.1.5
        rust-anstream-0.6.18
        rust-anstyle-1.0.10
        rust-anstyle-parse-0.2.6
        rust-anstyle-query-1.1.2
        rust-anstyle-wincon-3.0.7
        rust-anyhow-1.0.97
        rust-approx-0.5.1
        rust-arboard-3.4.1
        rust-arc-swap-1.7.1
        rust-argon2-0.5.3
        rust-async-stream-0.3.6
        rust-async-stream-impl-0.3.6
        rust-async-trait-0.1.87
        rust-atoi-2.0.0
        rust-atomic-waker-1.1.2
        rust-autocfg-1.4.0
        rust-axum-0.7.9
        rust-axum-core-0.4.5
        rust-axum-server-0.7.1
        rust-backtrace-0.3.74
        rust-base64-0.21.7
        rust-base64-0.22.1
        rust-base64ct-1.6.0
        rust-beef-0.5.2
        rust-bitflags-1.3.2
        rust-bitflags-2.9.0
        rust-blake2-0.10.6
        rust-block-buffer-0.10.4
        rust-block2-0.5.1
        rust-bumpalo-3.17.0
        rust-by-address-1.2.1
        rust-bytemuck-1.22.0
        rust-byteorder-1.5.0
        rust-byteorder-lite-0.1.0
        rust-bytes-1.10.0
        rust-cassowary-0.3.0
        rust-castaway-0.2.3
        rust-cc-1.2.16
        rust-cfg-if-1.0.0
        rust-cfg-aliases-0.1.1
        rust-cfg-aliases-0.2.1
        rust-chacha20-0.9.1
        rust-chrono-0.4.40
        rust-cipher-0.4.4
        rust-clap-4.5.31
        rust-clap-builder-4.5.31
        rust-clap-complete-4.5.46
        rust-clap-complete-nushell-4.5.5
        rust-clap-derive-4.5.28
        rust-clap-lex-0.7.4
        rust-clipboard-win-5.4.0
        rust-colorchoice-1.0.3
        rust-colored-2.2.0
        rust-compact-str-0.7.1
        rust-concurrent-queue-2.5.0
        rust-condtype-1.3.0
        rust-config-0.13.4
        rust-console-0.15.11
        rust-const-oid-0.9.6
        rust-core-foundation-0.9.4
        rust-core-foundation-sys-0.8.7
        rust-core-graphics-0.23.2
        rust-core-graphics-types-0.1.3
        rust-cpufeatures-0.2.17
        rust-crc-3.2.1
        rust-crc-catalog-2.4.0
        rust-crc32fast-1.4.2
        rust-crossbeam-deque-0.8.6
        rust-crossbeam-epoch-0.9.18
        rust-crossbeam-queue-0.3.12
        rust-crossbeam-utils-0.8.21
        rust-crossterm-0.27.0
        rust-crossterm-winapi-0.9.1
        rust-crypto-common-0.1.6
        rust-crypto-secretbox-0.1.1
        rust-curve25519-dalek-4.1.3
        rust-curve25519-dalek-derive-0.1.1
        rust-darling-0.20.10
        rust-darling-core-0.20.10
        rust-darling-macro-0.20.10
        rust-dashmap-5.5.3
        rust-der-0.7.9
        rust-deranged-0.3.11
        rust-derive-new-0.6.0
        rust-diff-0.1.13
        rust-digest-0.10.7
        rust-directories-5.0.1
        rust-dirs-5.0.1
        rust-dirs-sys-0.4.1
        rust-displaydoc-0.2.5
        rust-divan-0.1.17
        rust-divan-macros-0.1.17
        rust-dotenvy-0.15.7
        rust-downcast-rs-1.2.1
        rust-ed25519-2.2.3
        rust-ed25519-dalek-2.1.1
        rust-either-1.14.0
        rust-encode-unicode-1.0.0
        rust-encoding-rs-0.8.35
        rust-env-filter-0.1.3
        rust-env-logger-0.11.6
        rust-equivalent-1.0.2
        rust-errno-0.3.10
        rust-error-code-3.3.1
        rust-etcetera-0.8.0
        rust-event-listener-5.4.0
        rust-eyre-0.6.12
        rust-fast-srgb8-1.0.0
        rust-fastrand-2.3.0
        rust-fdeflate-0.3.7
        rust-fiat-crypto-0.2.9
        rust-filedescriptor-0.8.3
        rust-fixedbitset-0.4.2
        rust-fixedbitset-0.5.7
        rust-flate2-1.1.0
        rust-flume-0.11.1
        rust-fnv-1.0.7
        rust-foldhash-0.1.4
        rust-foreign-types-0.5.0
        rust-foreign-types-macros-0.2.3
        rust-foreign-types-shared-0.3.1
        rust-form-urlencoded-1.2.1
        rust-fs-err-2.11.0
        rust-futures-0.3.31
        rust-futures-channel-0.3.31
        rust-futures-core-0.3.31
        rust-futures-executor-0.3.31
        rust-futures-intrusive-0.5.0
        rust-futures-io-0.3.31
        rust-futures-macro-0.3.31
        rust-futures-sink-0.3.31
        rust-futures-task-0.3.31
        rust-futures-util-0.3.31
        rust-fuzzy-matcher-0.3.7
        rust-generic-array-0.14.7
        rust-gethostname-0.4.3
        rust-getrandom-0.2.15
        rust-getrandom-0.3.1
        rust-gimli-0.31.1
        rust-h2-0.3.26
        rust-h2-0.4.8
        rust-hashbrown-0.12.3
        rust-hashbrown-0.13.1
        rust-hashbrown-0.14.5
        rust-hashbrown-0.15.2
        rust-hashlink-0.10.0
        rust-heck-0.5.0
        rust-hermit-abi-0.3.9
        rust-hex-0.4.3
        rust-hkdf-0.12.4
        rust-hmac-0.12.1
        rust-home-0.5.11
        rust-http-0.2.12
        rust-http-1.2.0
        rust-http-body-0.4.6
        rust-http-body-1.0.1
        rust-http-body-util-0.1.2
        rust-httparse-1.10.1
        rust-httpdate-1.0.3
        rust-humantime-2.1.0
        rust-hyper-0.14.32
        rust-hyper-1.6.0
        rust-hyper-rustls-0.24.2
        rust-hyper-rustls-0.27.5
        rust-hyper-timeout-0.5.2
        rust-hyper-util-0.1.10
        rust-iana-time-zone-0.1.61
        rust-iana-time-zone-haiku-0.1.2
        rust-icu-collections-1.5.0
        rust-icu-locid-1.5.0
        rust-icu-locid-transform-1.5.0
        rust-icu-locid-transform-data-1.5.0
        rust-icu-normalizer-1.5.0
        rust-icu-normalizer-data-1.5.0
        rust-icu-properties-1.5.1
        rust-icu-properties-data-1.5.0
        rust-icu-provider-1.5.0
        rust-icu-provider-macros-1.5.0
        rust-ident-case-1.0.1
        rust-idna-1.0.3
        rust-idna-adapter-1.2.0
        rust-image-0.25.5
        rust-indenter-0.3.3
        rust-indexmap-1.9.3
        rust-indexmap-2.7.1
        rust-indicatif-0.17.11
        rust-inout-0.1.4
        rust-interim-0.1.2
        rust-ipnet-2.11.0
        rust-is-terminal-polyfill-1.70.1
        rust-iso8601-0.6.2
        rust-itertools-0.13.0
        rust-itertools-0.14.0
        rust-itoa-1.0.15
        rust-jpeg-decoder-0.3.1
        rust-js-sys-0.3.77
        rust-lazy-static-1.5.0
        rust-libc-0.2.170
        rust-libm-0.2.11
        rust-libredox-0.1.3
        rust-libsqlite3-sys-0.30.1
        rust-linux-raw-sys-0.4.15
        rust-listenfd-1.0.2
        rust-litemap-0.7.5
        rust-lock-api-0.4.12
        rust-log-0.4.26
        rust-logos-0.14.4
        rust-logos-codegen-0.14.4
        rust-logos-derive-0.14.4
        rust-lru-0.12.5
        rust-mach2-0.4.2
        rust-matchers-0.1.0
        rust-matchit-0.7.3
        rust-md-5-0.10.6
        rust-memchr-2.7.4
        rust-metrics-0.21.1
        rust-metrics-exporter-prometheus-0.12.2
        rust-metrics-macros-0.7.1
        rust-metrics-util-0.15.1
        rust-miette-7.5.0
        rust-miette-derive-7.5.0
        rust-mime-0.3.17
        rust-minimal-lexical-0.2.1
        rust-miniz-oxide-0.8.5
        rust-minspan-0.1.2
        rust-mio-0.8.11
        rust-mio-1.0.3
        rust-multimap-0.10.0
        rust-nix-0.28.0
        rust-nom-7.1.3
        rust-nom-8.0.0
        rust-ntapi-0.4.1
        rust-nu-ansi-term-0.46.0
        rust-nu-ansi-term-0.50.1
        rust-num-bigint-dig-0.8.4
        rust-num-conv-0.1.0
        rust-num-integer-0.1.46
        rust-num-iter-0.1.45
        rust-num-traits-0.2.19
        rust-num-cpus-1.16.0
        rust-num-threads-0.1.7
        rust-number-prefix-0.4.0
        rust-objc-sys-0.3.5
        rust-objc2-0.5.2
        rust-objc2-app-kit-0.2.2
        rust-objc2-core-data-0.2.2
        rust-objc2-core-image-0.2.2
        rust-objc2-encode-4.1.0
        rust-objc2-foundation-0.2.2
        rust-objc2-metal-0.2.2
        rust-objc2-quartz-core-0.2.2
        rust-object-0.36.7
        rust-once-cell-1.20.3
        rust-opaque-debug-0.3.1
        rust-openssl-probe-0.1.6
        rust-option-ext-0.2.0
        rust-os-pipe-1.2.1
        rust-overload-0.1.1
        rust-palette-0.7.6
        rust-palette-derive-0.7.6
        rust-parking-2.2.1
        rust-parking-lot-0.12.3
        rust-parking-lot-core-0.9.10
        rust-password-hash-0.5.0
        rust-paste-1.0.15
        rust-pathdiff-0.2.3
        rust-pbkdf2-0.11.0
        rust-pem-rfc7468-0.7.0
        rust-percent-encoding-2.3.1
        rust-petgraph-0.6.5
        rust-petgraph-0.7.1
        rust-phf-0.11.3
        rust-phf-generator-0.11.3
        rust-phf-macros-0.11.3
        rust-phf-shared-0.11.3
        rust-pin-project-1.1.10
        rust-pin-project-internal-1.1.10
        rust-pin-project-lite-0.2.16
        rust-pin-utils-0.1.0
        rust-pkcs1-0.7.5
        rust-pkcs8-0.10.2
        rust-pkg-config-0.3.32
        rust-png-0.17.16
        rust-poly1305-0.8.0
        rust-portable-atomic-1.11.0
        rust-postmark-0.10.2
        rust-powerfmt-0.2.0
        rust-ppv-lite86-0.2.20
        rust-pretty-assertions-1.4.1
        rust-prettyplease-0.2.30
        rust-proc-macro2-1.0.94
        rust-prost-0.13.5
        rust-prost-build-0.13.5
        rust-prost-derive-0.13.5
        rust-prost-reflect-0.14.7
        rust-prost-types-0.13.5
        rust-protox-0.7.2
        rust-protox-parse-0.7.0
        rust-quanta-0.11.1
        rust-quick-xml-0.37.2
        rust-quinn-0.11.6
        rust-quinn-proto-0.11.9
        rust-quinn-udp-0.5.10
        rust-quote-1.0.39
        rust-rand-0.8.5
        rust-rand-chacha-0.3.1
        rust-rand-core-0.6.4
        rust-ratatui-0.27.0
        rust-raw-cpuid-10.7.0
        rust-rayon-1.10.0
        rust-rayon-core-1.12.1
        rust-redox-syscall-0.5.10
        rust-redox-users-0.4.6
        rust-regex-1.11.1
        rust-regex-automata-0.1.10
        rust-regex-automata-0.4.9
        rust-regex-lite-0.1.6
        rust-regex-syntax-0.6.29
        rust-regex-syntax-0.8.5
        rust-reqwest-0.11.27
        rust-reqwest-0.12.12
        ;; rust-ring-0.17.11
        rust-rmp-0.8.14
        rust-rpassword-7.3.1
        rust-rsa-0.9.7
        rust-rtoolbox-0.0.2
        rust-runtime-format-0.1.3
        rust-rustc-demangle-0.1.24
        rust-rustc-hash-1.1.0
        rust-rustc-hash-2.1.1
        rust-rustc-version-0.4.1
        rust-rustix-0.38.44
        rust-rustls-0.21.12
        rust-rustls-0.23.23
        rust-rustls-native-certs-0.6.3
        rust-rustls-pemfile-1.0.4
        rust-rustls-pemfile-2.2.0
        rust-rustls-pki-types-1.11.0
        rust-rustls-webpki-0.101.7
        rust-rustls-webpki-0.102.8
        rust-rustversion-1.0.20
        rust-rusty-paserk-0.4.0
        rust-rusty-paseto-0.7.2
        rust-ryu-1.0.20
        rust-salsa20-0.10.2
        rust-schannel-0.1.27
        rust-scopeguard-1.2.0
        rust-sct-0.7.1
        rust-security-framework-2.11.1
        rust-security-framework-sys-2.14.0
        rust-semver-1.0.26
        rust-serde-1.0.218
        rust-serde-derive-1.0.218
        rust-serde-json-1.0.140
        rust-serde-path-to-error-0.1.17
        rust-serde-regex-1.1.0
        rust-serde-urlencoded-0.7.1
        rust-serde-with-3.12.0
        rust-serde-with-macros-3.12.0
        rust-sha1-0.10.6
        rust-sha2-0.10.8
        rust-sharded-slab-0.1.7
        rust-shellexpand-3.1.0
        rust-shlex-1.3.0
        rust-signal-hook-0.3.17
        rust-signal-hook-mio-0.2.4
        rust-signal-hook-registry-1.4.2
        rust-signature-2.2.0
        rust-simd-adler32-0.3.7
        rust-siphasher-1.0.1
        rust-sketches-ddsketch-0.2.2
        rust-slab-0.4.9
        rust-smallvec-1.14.0
        rust-socket2-0.5.8
        rust-spin-0.9.8
        rust-spki-0.7.3
        rust-sql-builder-3.1.1
        rust-sqlx-0.8.3
        rust-sqlx-core-0.8.3
        rust-sqlx-macros-0.8.3
        rust-sqlx-macros-core-0.8.3
        rust-sqlx-mysql-0.8.3
        rust-sqlx-postgres-0.8.3
        rust-sqlx-sqlite-0.8.3
        rust-stability-0.2.1
        rust-stable-deref-trait-1.2.0
        rust-static-assertions-1.1.0
        rust-stringprep-0.1.5
        rust-strsim-0.11.1
        rust-strum-0.26.3
        rust-strum-macros-0.26.4
        rust-subtle-2.6.1
        rust-syn-2.0.99
        rust-sync-wrapper-0.1.2
        rust-sync-wrapper-1.0.2
        rust-synstructure-0.13.1
        rust-sysinfo-0.30.13
        rust-system-configuration-0.5.1
        rust-system-configuration-sys-0.5.0
        rust-tempfile-3.17.1
        rust-terminal-size-0.4.1
        rust-testing-logger-0.1.1
        rust-thiserror-1.0.69
        rust-thiserror-2.0.12
        rust-thiserror-impl-1.0.69
        rust-thiserror-impl-2.0.12
        rust-thread-local-1.1.8
        rust-tiff-0.9.1
        rust-time-0.3.37
        rust-time-core-0.1.2
        rust-time-macros-0.2.19
        rust-tiny-bip39-1.0.0
        rust-tinystr-0.7.6
        rust-tinyvec-1.9.0
        rust-tinyvec-macros-0.1.1
        rust-tokio-1.43.0
        rust-tokio-macros-2.5.0
        rust-tokio-rustls-0.24.1
        rust-tokio-rustls-0.26.2
        rust-tokio-stream-0.1.17
        rust-tokio-util-0.7.13
        rust-toml-0.5.11
        rust-tonic-0.12.3
        rust-tonic-build-0.12.3
        rust-tonic-types-0.12.3
        rust-tower-0.4.13
        rust-tower-0.5.2
        rust-tower-http-0.5.2
        rust-tower-layer-0.3.3
        rust-tower-service-0.3.3
        rust-tracing-0.1.41
        rust-tracing-attributes-0.1.28
        rust-tracing-core-0.1.33
        rust-tracing-log-0.2.0
        rust-tracing-subscriber-0.3.19
        rust-tracing-tree-0.4.0
        rust-tree-magic-mini-3.1.6
        rust-try-lock-0.2.5
        rust-typed-builder-0.18.2
        rust-typed-builder-macro-0.18.2
        rust-typenum-1.18.0
        rust-unicode-bidi-0.3.18
        rust-unicode-ident-1.0.18
        rust-unicode-normalization-0.1.24
        rust-unicode-properties-0.1.3
        rust-unicode-segmentation-1.12.0
        rust-unicode-truncate-1.1.0
        rust-unicode-width-0.1.14
        rust-unicode-width-0.2.0
        rust-universal-hash-0.5.1
        rust-untrusted-0.9.0
        rust-url-2.5.4
        rust-urlencoding-2.1.3
        rust-utf16-iter-1.0.5
        rust-utf8-iter-1.0.4
        rust-utf8parse-0.2.2
        rust-uuid-1.15.1
        rust-valuable-0.1.1
        rust-vcpkg-0.2.15
        rust-version-check-0.9.5
        rust-want-0.3.1
        rust-wasi-0.11.0+wasi-snapshot-preview1
        rust-wasi-0.13.3+wasi-0.2.2
        rust-wasite-0.1.0
        rust-wasm-bindgen-0.2.100
        rust-wasm-bindgen-backend-0.2.100
        rust-wasm-bindgen-futures-0.4.50
        rust-wasm-bindgen-macro-0.2.100
        rust-wasm-bindgen-macro-support-0.2.100
        rust-wasm-bindgen-shared-0.2.100
        rust-wayland-backend-0.3.8
        rust-wayland-client-0.31.8
        rust-wayland-protocols-0.31.2
        rust-wayland-protocols-wlr-0.2.0
        rust-wayland-scanner-0.31.6
        rust-wayland-sys-0.31.6
        rust-web-sys-0.3.77
        rust-web-time-1.1.0
        rust-webpki-roots-0.26.8
        rust-weezl-0.1.8
        rust-whoami-1.5.2
        rust-winapi-0.3.9
        rust-winapi-i686-pc-windows-gnu-0.4.0
        rust-winapi-x86-64-pc-windows-gnu-0.4.0
        rust-windows-0.52.0
        rust-windows-core-0.52.0
        rust-windows-link-0.1.0
        rust-windows-registry-0.2.0
        rust-windows-result-0.2.0
        rust-windows-strings-0.1.0
        rust-windows-sys-0.48.0
        rust-windows-sys-0.52.0
        rust-windows-sys-0.59.0
        rust-windows-targets-0.48.5
        rust-windows-targets-0.52.6
        rust-windows-aarch64-gnullvm-0.48.5
        rust-windows-aarch64-gnullvm-0.52.6
        rust-windows-aarch64-msvc-0.48.5
        rust-windows-aarch64-msvc-0.52.6
        rust-windows-i686-gnu-0.48.5
        rust-windows-i686-gnu-0.52.6
        rust-windows-i686-gnullvm-0.52.6
        rust-windows-i686-msvc-0.48.5
        rust-windows-i686-msvc-0.52.6
        rust-windows-x86-64-gnu-0.48.5
        rust-windows-x86-64-gnu-0.52.6
        rust-windows-x86-64-gnullvm-0.48.5
        rust-windows-x86-64-gnullvm-0.52.6
        rust-windows-x86-64-msvc-0.48.5
        rust-windows-x86-64-msvc-0.52.6
        rust-winreg-0.50.0
        rust-wit-bindgen-rt-0.33.0
        rust-wl-clipboard-rs-0.8.1
        rust-write16-1.0.0
        rust-writeable-0.5.5
        rust-x11rb-0.13.1
        rust-x11rb-protocol-0.13.1
        rust-yansi-1.0.1
        rust-yoke-0.7.5
        rust-yoke-derive-0.7.5
        rust-zerocopy-0.7.35
        rust-zerocopy-derive-0.7.35
        rust-zerofrom-0.1.6
        rust-zerofrom-derive-0.1.6
        rust-zeroize-1.8.1
        rust-zeroize-derive-1.4.2
        rust-zerovec-0.10.4
        rust-zerovec-derive-0.10.3))

(define-public rust-pipewire-cargo-inputs
  (list rust-aho-corasick-1.1.3
        rust-annotate-snippets-0.9.2
        rust-anstream-0.6.18
        rust-anstyle-1.0.10
        rust-anstyle-parse-0.2.6
        rust-anstyle-query-1.1.2
        rust-anstyle-wincon-3.0.7
        rust-anyhow-1.0.97
        rust-autocfg-1.4.0
        rust-bindgen-0.69.5
        rust-bitflags-2.9.0
        rust-cc-1.2.16
        rust-cexpr-0.6.0
        rust-cfg-expr-0.15.8
        rust-cfg-if-1.0.0
        rust-cfg-aliases-0.2.1
        rust-clang-sys-1.8.1
        rust-clap-4.5.31
        rust-clap-builder-4.5.31
        rust-clap-derive-4.5.28
        rust-clap-lex-0.7.4
        rust-colorchoice-1.0.3
        rust-convert-case-0.6.0
        rust-cookie-factory-0.3.3
        rust-either-1.14.0
        rust-equivalent-1.0.2
        rust-futures-0.3.31
        rust-futures-channel-0.3.31
        rust-futures-core-0.3.31
        rust-futures-executor-0.3.31
        rust-futures-io-0.3.31
        rust-futures-macro-0.3.31
        rust-futures-sink-0.3.31
        rust-futures-task-0.3.31
        rust-futures-util-0.3.31
        rust-glob-0.3.2
        rust-hashbrown-0.15.2
        rust-heck-0.5.0
        rust-indexmap-2.7.1
        rust-is-terminal-polyfill-1.70.1
        rust-itertools-0.12.1
        rust-lazy-static-1.5.0
        rust-lazycell-1.3.0
        rust-libc-0.2.170
        rust-libloading-0.8.6
        rust-memchr-2.7.4
        rust-minimal-lexical-0.2.1
        rust-nix-0.29.0
        rust-nom-7.1.3
        rust-once-cell-1.20.3
        rust-pin-project-lite-0.2.16
        rust-pin-utils-0.1.0
        rust-pkg-config-0.3.32
        rust-proc-macro2-1.0.94
        rust-quote-1.0.39
        rust-regex-1.11.1
        rust-regex-automata-0.4.9
        rust-regex-syntax-0.8.5
        rust-rustc-hash-1.1.0
        rust-serde-1.0.218
        rust-serde-derive-1.0.218
        rust-serde-spanned-0.6.8
        rust-shlex-1.3.0
        rust-slab-0.4.9
        rust-smallvec-1.14.0
        rust-strsim-0.11.1
        rust-syn-2.0.99
        rust-system-deps-6.2.2
        rust-target-lexicon-0.12.16
        rust-thiserror-1.0.69
        rust-thiserror-impl-1.0.69
        rust-toml-0.8.20
        rust-toml-datetime-0.6.8
        rust-toml-edit-0.22.24
        rust-unicode-ident-1.0.18
        rust-unicode-segmentation-1.12.0
        rust-unicode-width-0.1.14
        rust-utf8parse-0.2.2
        rust-version-compare-0.2.0
        rust-winapi-0.3.9
        rust-winapi-i686-pc-windows-gnu-0.4.0
        rust-winapi-x86-64-pc-windows-gnu-0.4.0
        rust-windows-sys-0.59.0
        rust-windows-targets-0.52.6
        rust-windows-aarch64-gnullvm-0.52.6
        rust-windows-aarch64-msvc-0.52.6
        rust-windows-i686-gnu-0.52.6
        rust-windows-i686-gnullvm-0.52.6
        rust-windows-i686-msvc-0.52.6
        rust-windows-x86-64-gnu-0.52.6
        rust-windows-x86-64-gnullvm-0.52.6
        rust-windows-x86-64-msvc-0.52.6
        rust-winnow-0.7.3
        rust-yansi-term-0.1.2))

(define-public rust-smithay-cargo-inputs
  (list rust-adler2-2.0.0
        rust-ahash-0.8.11
        rust-aho-corasick-1.1.3
        rust-aligned-vec-0.5.0
        rust-android-activity-0.6.0
        rust-android-properties-0.2.2
        rust-anes-0.1.6
        rust-anstream-0.6.18
        rust-anstyle-1.0.10
        rust-anstyle-parse-0.2.6
        rust-anstyle-query-1.1.2
        rust-anstyle-wincon-3.0.7
        rust-anyhow-1.0.97
        rust-appendlist-1.4.0
        rust-approx-0.4.0
        rust-arbitrary-1.4.1
        rust-arg-enum-proc-macro-0.3.4
        rust-arrayvec-0.7.6
        rust-as-raw-xcb-connection-1.0.1
        rust-ash-0.38.0+1.3.281
        rust-atomic-waker-1.1.2
        rust-autocfg-1.4.0
        rust-av1-grain-0.2.3
        rust-avif-serialize-0.8.3
        rust-bincode-1.3.3
        rust-bindgen-0.69.5
        rust-bit-field-0.10.2
        rust-bitflags-1.3.2
        rust-bitflags-2.9.0
        rust-bitstream-io-2.6.0
        rust-block2-0.5.1
        rust-built-0.7.7
        rust-bumpalo-3.17.0
        rust-bytemuck-1.22.0
        rust-bytemuck-derive-1.8.1
        rust-byteorder-1.5.0
        rust-byteorder-lite-0.1.0
        rust-bytes-1.10.0
        rust-calloop-0.13.0
        rust-calloop-0.14.2
        rust-calloop-wayland-source-0.3.0
        rust-cast-0.3.0
        rust-cc-1.2.16
        rust-cesu8-1.1.0
        rust-cexpr-0.6.0
        rust-cfg-expr-0.15.8
        rust-cfg-expr-0.17.2
        rust-cfg-if-1.0.0
        rust-cfg-aliases-0.2.1
        rust-cgmath-0.18.0
        rust-ciborium-0.2.2
        rust-ciborium-io-0.2.2
        rust-ciborium-ll-0.2.2
        rust-clang-sys-1.8.1
        rust-clap-4.5.31
        rust-clap-builder-4.5.31
        rust-clap-derive-4.5.28
        rust-clap-lex-0.7.4
        rust-color-quant-1.1.0
        rust-colorchoice-1.0.3
        rust-combine-4.6.7
        rust-concurrent-queue-2.5.0
        rust-container-of-0.5.1
        rust-core-foundation-0.9.4
        rust-core-foundation-sys-0.8.7
        rust-core-graphics-0.23.2
        rust-core-graphics-types-0.1.3
        rust-crc32fast-1.4.2
        rust-criterion-0.5.1
        rust-criterion-plot-0.5.0
        rust-crossbeam-channel-0.5.14
        rust-crossbeam-deque-0.8.6
        rust-crossbeam-epoch-0.9.18
        rust-crossbeam-utils-0.8.21
        rust-crunchy-0.2.3
        rust-cursor-icon-1.1.0
        rust-dispatch-0.2.0
        rust-dlib-0.5.2
        rust-downcast-rs-1.2.1
        rust-dpi-0.1.1
        rust-drm-0.14.1
        rust-drm-ffi-0.9.0
        rust-drm-fourcc-2.2.0
        rust-drm-sys-0.8.0
        rust-either-1.14.0
        rust-encoding-rs-0.8.35
        rust-equivalent-1.0.2
        rust-errno-0.3.10
        rust-exr-1.73.0
        rust-fastrand-2.3.0
        rust-fdeflate-0.3.7
        rust-flate2-1.1.0
        rust-float-cmp-0.9.0
        rust-foreign-types-0.5.0
        rust-foreign-types-macros-0.2.3
        rust-foreign-types-shared-0.3.1
        rust-fps-ticker-1.0.0
        rust-gbm-0.18.0
        rust-gbm-sys-0.4.0
        rust-generator-0.8.4
        rust-gethostname-0.4.3
        rust-getrandom-0.2.15
        rust-getrandom-0.3.1
        rust-gif-0.13.1
        rust-gl-generator-0.14.0
        rust-glob-0.3.2
        rust-glow-0.16.0
        rust-half-2.4.1
        rust-hashbrown-0.15.2
        rust-heck-0.5.0
        rust-hermit-abi-0.3.9
        rust-hermit-abi-0.4.0
        rust-home-0.5.11
        rust-image-0.25.5
        rust-image-webp-0.2.1
        rust-imgref-1.11.0
        rust-indexmap-2.7.1
        rust-input-0.9.1
        rust-input-sys-1.18.0
        rust-instant-0.1.13
        rust-interpolate-name-0.2.4
        rust-io-lifetimes-1.0.11
        rust-is-terminal-0.4.15
        rust-is-terminal-polyfill-1.70.1
        rust-itertools-0.10.5
        rust-itertools-0.12.1
        rust-itoa-1.0.15
        rust-jni-0.21.1
        rust-jni-sys-0.3.0
        rust-jobserver-0.1.32
        rust-jpeg-decoder-0.3.1
        rust-js-sys-0.3.77
        rust-khronos-api-3.1.0
        rust-lazy-static-1.5.0
        rust-lazycell-1.3.0
        rust-lebe-0.5.2
        rust-libc-0.2.170
        rust-libdisplay-info-0.2.2
        rust-libdisplay-info-derive-0.1.0
        rust-libdisplay-info-sys-0.2.2
        rust-libfuzzer-sys-0.4.9
        rust-libloading-0.7.4
        rust-libloading-0.8.6
        rust-libredox-0.1.3
        rust-libseat-0.2.3
        rust-libseat-sys-0.1.9
        rust-libudev-sys-0.1.4
        rust-linux-raw-sys-0.4.15
        rust-linux-raw-sys-0.6.5
        rust-lock-api-0.4.12
        rust-log-0.4.26
        rust-loom-0.7.2
        rust-loop9-0.1.5
        rust-lz4-flex-0.10.0
        rust-matchers-0.1.0
        rust-maybe-rayon-0.1.1
        rust-memchr-2.7.4
        rust-memmap2-0.8.0
        rust-memmap2-0.9.5
        rust-memoffset-0.6.5
        rust-memoffset-0.9.1
        rust-minimal-lexical-0.2.1
        rust-miniz-oxide-0.8.5
        rust-ndk-0.9.0
        rust-ndk-context-0.1.1
        rust-ndk-sys-0.6.0+11769913
        rust-new-debug-unreachable-1.0.6
        rust-nix-0.27.1
        rust-nom-7.1.3
        rust-noop-proc-macro-0.3.0
        rust-nu-ansi-term-0.46.0
        rust-num-bigint-0.4.6
        rust-num-derive-0.4.2
        rust-num-integer-0.1.46
        rust-num-rational-0.4.2
        rust-num-traits-0.2.19
        rust-num-enum-0.7.3
        rust-num-enum-derive-0.7.3
        rust-objc-sys-0.3.5
        rust-objc2-0.5.2
        rust-objc2-app-kit-0.2.2
        rust-objc2-cloud-kit-0.2.2
        rust-objc2-contacts-0.2.2
        rust-objc2-core-data-0.2.2
        rust-objc2-core-image-0.2.2
        rust-objc2-core-location-0.2.2
        rust-objc2-encode-4.1.0
        rust-objc2-foundation-0.2.2
        rust-objc2-link-presentation-0.2.2
        rust-objc2-metal-0.2.2
        rust-objc2-quartz-core-0.2.2
        rust-objc2-symbols-0.2.2
        rust-objc2-ui-kit-0.2.2
        rust-objc2-uniform-type-identifiers-0.2.2
        rust-objc2-user-notifications-0.2.2
        rust-once-cell-1.20.3
        rust-oorandom-11.1.4
        rust-orbclient-0.3.48
        rust-overload-0.1.1
        rust-parking-lot-0.12.3
        rust-parking-lot-core-0.9.10
        rust-paste-1.0.15
        rust-percent-encoding-2.3.1
        rust-pin-project-1.1.10
        rust-pin-project-internal-1.1.10
        rust-pin-project-lite-0.2.16
        rust-pixman-0.2.1
        rust-pixman-sys-0.1.0
        rust-pkg-config-0.3.32
        rust-plotters-0.3.7
        rust-plotters-backend-0.3.7
        rust-plotters-svg-0.3.7
        rust-png-0.17.16
        rust-polling-3.7.4
        rust-ppv-lite86-0.2.20
        rust-prettyplease-0.2.30
        rust-proc-macro-crate-3.2.0
        rust-proc-macro2-1.0.94
        rust-profiling-1.0.16
        rust-profiling-procmacros-1.0.16
        rust-puffin-0.16.0
        rust-puffin-0.19.1
        rust-puffin-http-0.13.0
        rust-qoi-0.4.1
        rust-quick-error-2.0.1
        rust-quick-xml-0.37.2
        rust-quote-1.0.39
        rust-rand-0.8.5
        rust-rand-chacha-0.3.1
        rust-rand-core-0.6.4
        rust-rav1e-0.7.1
        rust-ravif-0.11.11
        rust-raw-window-handle-0.6.2
        rust-rayon-1.10.0
        rust-rayon-core-1.12.1
        rust-redox-syscall-0.4.1
        rust-redox-syscall-0.5.10
        rust-regex-1.11.1
        rust-regex-automata-0.1.10
        rust-regex-automata-0.4.9
        rust-regex-syntax-0.6.29
        rust-regex-syntax-0.8.5
        rust-renderdoc-0.11.0
        rust-renderdoc-sys-1.1.0
        rust-rgb-0.8.50
        rust-rustc-hash-1.1.0
        rust-rustix-0.38.44
        rust-rustversion-1.0.20
        rust-ryu-1.0.20
        rust-same-file-1.0.6
        rust-scoped-tls-1.0.1
        rust-scopeguard-1.2.0
        rust-semver-1.0.26
        rust-serde-1.0.218
        rust-serde-derive-1.0.218
        rust-serde-json-1.0.140
        rust-serde-spanned-0.6.8
        rust-sharded-slab-0.1.7
        rust-shlex-1.3.0
        rust-simd-adler32-0.3.7
        rust-simd-helpers-0.1.0
        rust-slab-0.4.9
        rust-slotmap-1.0.7
        rust-smallvec-1.14.0
        rust-smithay-client-toolkit-0.19.2
        rust-smol-str-0.2.2
        rust-strsim-0.11.1
        rust-syn-2.0.99
        rust-system-deps-6.2.2
        rust-system-deps-7.0.3
        rust-target-lexicon-0.12.16
        rust-tempfile-3.17.1
        rust-thiserror-1.0.69
        rust-thiserror-2.0.12
        rust-thiserror-impl-1.0.69
        rust-thiserror-impl-2.0.12
        rust-thread-local-1.1.8
        rust-tiff-0.9.1
        rust-tinytemplate-1.2.1
        rust-toml-0.8.20
        rust-toml-datetime-0.6.8
        rust-toml-edit-0.22.24
        rust-tracing-0.1.41
        rust-tracing-attributes-0.1.28
        rust-tracing-core-0.1.33
        rust-tracing-log-0.2.0
        rust-tracing-subscriber-0.3.19
        rust-tracy-client-0.17.6
        rust-tracy-client-sys-0.24.3
        rust-udev-0.9.3
        rust-unicode-ident-1.0.18
        rust-unicode-segmentation-1.12.0
        rust-utf8parse-0.2.2
        rust-v-frame-0.3.8
        rust-valuable-0.1.1
        rust-version-compare-0.2.0
        rust-version-check-0.9.5
        rust-walkdir-2.5.0
        rust-wasi-0.11.0+wasi-snapshot-preview1
        rust-wasi-0.13.3+wasi-0.2.2
        rust-wasm-bindgen-0.2.100
        rust-wasm-bindgen-backend-0.2.100
        rust-wasm-bindgen-futures-0.4.50
        rust-wasm-bindgen-macro-0.2.100
        rust-wasm-bindgen-macro-support-0.2.100
        rust-wasm-bindgen-shared-0.2.100
        rust-wayland-backend-0.3.8
        rust-wayland-client-0.31.8
        rust-wayland-csd-frame-0.3.0
        rust-wayland-cursor-0.31.8
        rust-wayland-egl-0.32.5
        rust-wayland-protocols-0.32.6
        rust-wayland-protocols-misc-0.3.6
        rust-wayland-protocols-plasma-0.3.6
        rust-wayland-protocols-wlr-0.3.6
        rust-wayland-scanner-0.31.6
        rust-wayland-server-0.31.7
        rust-wayland-sys-0.31.6
        rust-web-sys-0.3.77
        rust-web-time-1.1.0
        rust-weezl-0.1.8
        rust-which-4.4.2
        rust-winapi-0.3.9
        rust-winapi-i686-pc-windows-gnu-0.4.0
        rust-winapi-util-0.1.9
        rust-winapi-x86-64-pc-windows-gnu-0.4.0
        rust-windows-0.58.0
        rust-windows-core-0.58.0
        rust-windows-implement-0.58.0
        rust-windows-interface-0.58.0
        rust-windows-result-0.2.0
        rust-windows-strings-0.1.0
        rust-windows-sys-0.45.0
        rust-windows-sys-0.48.0
        rust-windows-sys-0.52.0
        rust-windows-sys-0.59.0
        rust-windows-targets-0.42.2
        rust-windows-targets-0.48.5
        rust-windows-targets-0.52.6
        rust-windows-aarch64-gnullvm-0.42.2
        rust-windows-aarch64-gnullvm-0.48.5
        rust-windows-aarch64-gnullvm-0.52.6
        rust-windows-aarch64-msvc-0.42.2
        rust-windows-aarch64-msvc-0.48.5
        rust-windows-aarch64-msvc-0.52.6
        rust-windows-i686-gnu-0.42.2
        rust-windows-i686-gnu-0.48.5
        rust-windows-i686-gnu-0.52.6
        rust-windows-i686-gnullvm-0.52.6
        rust-windows-i686-msvc-0.42.2
        rust-windows-i686-msvc-0.48.5
        rust-windows-i686-msvc-0.52.6
        rust-windows-x86-64-gnu-0.42.2
        rust-windows-x86-64-gnu-0.48.5
        rust-windows-x86-64-gnu-0.52.6
        rust-windows-x86-64-gnullvm-0.42.2
        rust-windows-x86-64-gnullvm-0.48.5
        rust-windows-x86-64-gnullvm-0.52.6
        rust-windows-x86-64-msvc-0.42.2
        rust-windows-x86-64-msvc-0.48.5
        rust-windows-x86-64-msvc-0.52.6
        rust-winit-0.30.9
        rust-winnow-0.7.3
        rust-wio-0.2.2
        rust-wit-bindgen-rt-0.33.0
        rust-wlcs-0.1.0
        rust-x11-dl-2.21.0
        rust-x11rb-0.13.1
        rust-x11rb-protocol-0.13.1
        rust-xcursor-0.3.8
        rust-xkbcommon-0.7.0
        rust-xkbcommon-0.8.0
        rust-xkbcommon-dl-0.4.2
        rust-xkeysym-0.2.1
        rust-xml-rs-0.8.25
        rust-zerocopy-0.7.35
        rust-zerocopy-derive-0.7.35
        rust-zune-core-0.4.12
        rust-zune-inflate-0.2.54
        rust-zune-jpeg-0.4.14))

(define-public niri-cargo-inputs
  (list rust-adler2-2.0.0
        rust-ahash-0.8.11
        rust-aho-corasick-1.1.3
        rust-aliasable-0.1.3
        rust-allocator-api2-0.2.21
        rust-android-activity-0.6.0
        rust-android-properties-0.2.2
        rust-annotate-snippets-0.9.2
        rust-anstream-0.6.18
        rust-anstyle-1.0.10
        rust-anstyle-parse-0.2.6
        rust-anstyle-query-1.1.2
        rust-anstyle-wincon-3.0.7
        rust-anyhow-1.0.97
        rust-appendlist-1.4.0
        rust-approx-0.4.0
        rust-approx-0.5.1
        rust-arrayvec-0.7.6
        rust-as-raw-xcb-connection-1.0.1
        rust-async-broadcast-0.7.2
        rust-async-channel-2.3.1
        rust-async-executor-1.13.1
        rust-async-fs-2.1.2
        rust-async-io-2.4.0
        rust-async-lock-3.4.0
        rust-async-process-2.3.0
        rust-async-recursion-1.1.1
        rust-async-signal-0.2.10
        rust-async-task-4.7.1
        rust-async-trait-0.1.87
        rust-atomic-0.6.0
        rust-atomic-waker-1.1.2
        rust-autocfg-1.4.0
        rust-base64-0.21.7
        rust-bindgen-0.69.5
        rust-bit-set-0.8.0
        rust-bit-vec-0.8.0
        rust-bitflags-1.3.2
        rust-bitflags-2.9.0
        rust-block2-0.5.1
        rust-blocking-1.6.1
        rust-bumpalo-3.17.0
        rust-bytemuck-1.22.0
        rust-bytemuck-derive-1.8.1
        rust-byteorder-1.5.0
        rust-bytes-1.10.0
        rust-cairo-rs-0.20.7
        rust-cairo-sys-rs-0.20.7
        rust-calloop-0.13.0
        rust-calloop-0.14.2
        rust-calloop-wayland-source-0.3.0
        rust-calloop-wayland-source-0.4.0
        rust-cc-1.2.16
        rust-cesu8-1.1.0
        rust-cexpr-0.6.0
        rust-cfg-expr-0.15.8
        rust-cfg-expr-0.17.2
        rust-cfg-if-1.0.0
        rust-cfg-aliases-0.2.1
        rust-cgmath-0.18.0
        rust-chumsky-0.9.3
        rust-clang-sys-1.8.1
        rust-clap-4.5.31
        rust-clap-builder-4.5.31
        rust-clap-derive-4.5.28
        rust-clap-lex-0.7.4
        rust-colorchoice-1.0.3
        rust-combine-4.6.7
        rust-concurrent-queue-2.5.0
        rust-console-0.15.11
        rust-convert-case-0.6.0
        rust-cookie-factory-0.3.3
        rust-core-foundation-0.9.4
        rust-core-foundation-sys-0.8.7
        rust-core-graphics-0.23.2
        rust-core-graphics-types-0.1.3
        rust-crc32fast-1.4.2
        rust-crossbeam-deque-0.8.6
        rust-crossbeam-epoch-0.9.18
        rust-crossbeam-utils-0.8.21
        rust-csscolorparser-0.7.0
        rust-cursor-icon-1.1.0
        rust-diff-0.1.13
        rust-directories-6.0.0
        rust-dirs-sys-0.5.0
        rust-dispatch-0.2.0
        rust-displaydoc-0.2.5
        rust-dlib-0.5.2
        rust-downcast-rs-1.2.1
        rust-dpi-0.1.1
        rust-drm-0.14.1
        rust-drm-ffi-0.9.0
        rust-drm-fourcc-2.2.0
        rust-drm-sys-0.8.0
        rust-dyn-clone-1.0.19
        rust-either-1.14.0
        rust-encode-unicode-1.0.0
        rust-endi-1.1.0
        rust-enumflags2-0.7.11
        rust-enumflags2-derive-0.7.11
        rust-equivalent-1.0.2
        rust-errno-0.3.10
        rust-event-listener-5.4.0
        rust-event-listener-strategy-0.5.3
        rust-fastrand-2.3.0
        rust-fdeflate-0.3.7
        rust-field-offset-0.3.6
        rust-flate2-1.1.0
        rust-fnv-1.0.7
        rust-foreign-types-0.5.0
        rust-foreign-types-macros-0.2.3
        rust-foreign-types-shared-0.3.1
        rust-form-urlencoded-1.2.1
        rust-futures-0.3.31
        rust-futures-channel-0.3.31
        rust-futures-core-0.3.31
        rust-futures-executor-0.3.31
        rust-futures-io-0.3.31
        rust-futures-lite-2.6.0
        rust-futures-macro-0.3.31
        rust-futures-sink-0.3.31
        rust-futures-task-0.3.31
        rust-futures-util-0.3.31
        rust-gbm-0.18.0
        rust-gbm-sys-0.4.0
        rust-gdk-pixbuf-0.20.9
        rust-gdk-pixbuf-sys-0.20.7
        rust-gdk4-0.9.6
        rust-gdk4-sys-0.9.6
        rust-generator-0.8.4
        rust-gethostname-0.4.3
        rust-getrandom-0.2.15
        rust-getrandom-0.3.1
        rust-gio-0.20.9
        rust-gio-sys-0.20.9
        rust-git-version-0.3.9
        rust-git-version-macro-0.3.9
        rust-gl-generator-0.14.0
        rust-glam-0.30.0
        rust-glib-0.20.9
        rust-glib-macros-0.20.7
        rust-glib-sys-0.20.9
        rust-glob-0.3.2
        rust-gobject-sys-0.20.9
        rust-graphene-rs-0.20.9
        rust-graphene-sys-0.20.7
        rust-gsk4-0.9.6
        rust-gsk4-sys-0.9.6
        rust-gtk4-0.9.6
        rust-gtk4-macros-0.9.5
        rust-gtk4-sys-0.9.6
        rust-hashbrown-0.14.5
        rust-hashbrown-0.15.2
        rust-heck-0.4.1
        rust-heck-0.5.0
        rust-hermit-abi-0.3.9
        rust-hermit-abi-0.4.0
        rust-hex-0.4.3
        rust-icu-collections-1.5.0
        rust-icu-locid-1.5.0
        rust-icu-locid-transform-1.5.0
        rust-icu-locid-transform-data-1.5.0
        rust-icu-normalizer-1.5.0
        rust-icu-normalizer-data-1.5.0
        rust-icu-properties-1.5.1
        rust-icu-properties-data-1.5.0
        rust-icu-provider-1.5.0
        rust-icu-provider-macros-1.5.0
        rust-idna-1.0.3
        rust-idna-adapter-1.2.0
        rust-indexmap-2.7.1
        rust-input-0.9.1
        rust-input-sys-1.18.0
        rust-insta-1.42.2
        rust-io-lifetimes-1.0.11
        rust-is-terminal-0.4.15
        rust-is-ci-1.2.0
        rust-is-terminal-polyfill-1.70.1
        rust-itertools-0.12.1
        rust-itoa-1.0.15
        rust-jni-0.21.1
        rust-jni-sys-0.3.0
        rust-jobserver-0.1.32
        rust-js-sys-0.3.77
        rust-keyframe-1.1.1
        rust-khronos-api-3.1.0
        rust-knuffel-3.2.0
        rust-knuffel-derive-3.2.0
        rust-lazy-static-1.5.0
        rust-lazycell-1.3.0
        rust-libadwaita-0.7.1
        rust-libadwaita-sys-0.7.1
        rust-libc-0.2.170
        rust-libdisplay-info-0.2.2
        rust-libdisplay-info-derive-0.1.0
        rust-libdisplay-info-sys-0.2.2
        rust-libloading-0.8.6
        rust-libm-0.2.11
        rust-libredox-0.1.3
        rust-libseat-0.2.3
        rust-libseat-sys-0.1.9
        rust-libudev-sys-0.1.4
        rust-linked-hash-map-0.5.6
        rust-linux-raw-sys-0.4.15
        rust-linux-raw-sys-0.6.5
        rust-litemap-0.7.5
        rust-log-0.4.26
        rust-loom-0.7.2
        rust-matchers-0.1.0
        rust-memchr-2.7.4
        rust-memmap2-0.9.5
        rust-memoffset-0.9.1
        rust-miette-5.10.0
        rust-miette-derive-5.10.0
        rust-minimal-lexical-0.2.1
        rust-miniz-oxide-0.8.5
        rust-ndk-0.9.0
        rust-ndk-context-0.1.1
        rust-ndk-sys-0.6.0+11769913
        rust-nix-0.29.0
        rust-nom-7.1.3
        rust-nu-ansi-term-0.46.0
        rust-num-traits-0.2.19
        rust-num-enum-0.7.3
        rust-num-enum-derive-0.7.3
        rust-objc-sys-0.3.5
        rust-objc2-0.5.2
        rust-objc2-app-kit-0.2.2
        rust-objc2-cloud-kit-0.2.2
        rust-objc2-contacts-0.2.2
        rust-objc2-core-data-0.2.2
        rust-objc2-core-image-0.2.2
        rust-objc2-core-location-0.2.2
        rust-objc2-encode-4.1.0
        rust-objc2-foundation-0.2.2
        rust-objc2-link-presentation-0.2.2
        rust-objc2-metal-0.2.2
        rust-objc2-quartz-core-0.2.2
        rust-objc2-symbols-0.2.2
        rust-objc2-ui-kit-0.2.2
        rust-objc2-uniform-type-identifiers-0.2.2
        rust-objc2-user-notifications-0.2.2
        rust-once-cell-1.20.3
        rust-option-ext-0.2.0
        rust-orbclient-0.3.48
        rust-ordered-float-5.0.0
        rust-ordered-stream-0.2.0
        rust-overload-0.1.1
        rust-owo-colors-3.5.0
        rust-pango-0.20.9
        rust-pango-sys-0.20.9
        rust-pangocairo-0.20.7
        rust-pangocairo-sys-0.20.7
        rust-parking-2.2.1
        rust-paste-1.0.15
        rust-percent-encoding-2.3.1
        rust-phf-0.11.3
        rust-phf-generator-0.11.3
        rust-phf-macros-0.11.3
        rust-phf-shared-0.11.3
        rust-pin-project-1.1.10
        rust-pin-project-internal-1.1.10
        rust-pin-project-lite-0.2.16
        rust-pin-utils-0.1.0
        rust-piper-0.2.4
        rust-pixman-0.2.1
        rust-pixman-sys-0.1.0
        rust-pkg-config-0.3.32
        rust-png-0.17.16
        rust-polling-3.7.4
        rust-portable-atomic-1.11.0
        rust-ppv-lite86-0.2.20
        rust-pretty-assertions-1.4.1
        rust-proc-macro-crate-3.2.0
        rust-proc-macro-error-1.0.4
        rust-proc-macro-error-attr-1.0.4
        rust-proc-macro2-1.0.94
        rust-profiling-1.0.16
        rust-profiling-procmacros-1.0.16
        rust-proptest-1.6.0
        rust-proptest-derive-0.5.1
        rust-quick-error-1.2.3
        rust-quick-xml-0.37.2
        rust-quote-1.0.39
        rust-rand-0.8.5
        rust-rand-chacha-0.3.1
        rust-rand-core-0.6.4
        rust-rand-xorshift-0.3.0
        rust-raw-window-handle-0.6.2
        rust-rayon-1.10.0
        rust-rayon-core-1.12.1
        rust-redox-syscall-0.4.1
        rust-redox-syscall-0.5.10
        rust-redox-users-0.5.0
        rust-regex-1.11.1
        rust-regex-automata-0.1.10
        rust-regex-automata-0.4.9
        rust-regex-syntax-0.6.29
        rust-regex-syntax-0.8.5
        rust-rustc-hash-1.1.0
        rust-rustc-version-0.4.1
        rust-rustix-0.38.44
        rust-rustversion-1.0.20
        rust-rusty-fork-0.3.0
        rust-ryu-1.0.20
        rust-same-file-1.0.6
        rust-schemars-0.8.22
        rust-schemars-derive-0.8.22
        rust-scoped-tls-1.0.1
        rust-sd-notify-0.4.5
        rust-semver-1.0.26
        rust-serde-1.0.218
        rust-serde-derive-1.0.218
        rust-serde-derive-internals-0.29.1
        rust-serde-json-1.0.140
        rust-serde-repr-0.1.20
        rust-serde-spanned-0.6.8
        rust-sharded-slab-0.1.7
        rust-shlex-1.3.0
        rust-signal-hook-registry-1.4.2
        rust-simd-adler32-0.3.7
        rust-similar-2.7.0
        rust-siphasher-1.0.1
        rust-slab-0.4.9
        rust-smallvec-1.14.0
        rust-smawk-0.3.2
        rust-smithay-client-toolkit-0.19.2
        rust-smol-str-0.2.2
        rust-stable-deref-trait-1.2.0
        rust-static-assertions-1.1.0
        rust-strsim-0.11.1
        rust-supports-color-2.1.0
        rust-supports-hyperlinks-2.1.0
        rust-supports-unicode-2.1.0
        rust-syn-1.0.109
        rust-syn-2.0.99
        rust-synstructure-0.13.1
        rust-system-deps-6.2.2
        rust-system-deps-7.0.3
        rust-target-lexicon-0.12.16
        rust-tempfile-3.17.1
        rust-terminal-size-0.1.17
        rust-textwrap-0.15.2
        rust-thiserror-1.0.69
        rust-thiserror-2.0.12
        rust-thiserror-impl-1.0.69
        rust-thiserror-impl-2.0.12
        rust-thread-local-1.1.8
        rust-tinystr-0.7.6
        rust-toml-0.8.20
        rust-toml-datetime-0.6.8
        rust-toml-edit-0.22.24
        rust-tracing-0.1.41
        rust-tracing-attributes-0.1.28
        rust-tracing-core-0.1.33
        rust-tracing-log-0.2.0
        rust-tracing-subscriber-0.3.19
        rust-tracy-client-0.17.6
        rust-tracy-client-0.18.0
        rust-tracy-client-sys-0.24.3
        rust-udev-0.9.3
        rust-uds-windows-1.1.0
        rust-unarray-0.1.4
        rust-unicode-ident-1.0.18
        rust-unicode-linebreak-0.1.5
        rust-unicode-segmentation-1.12.0
        rust-unicode-width-0.1.14
        rust-url-2.5.4
        rust-utf16-iter-1.0.5
        rust-utf8-iter-1.0.4
        rust-utf8parse-0.2.2
        rust-valuable-0.1.1
        rust-version-compare-0.2.0
        rust-version-check-0.9.5
        rust-wait-timeout-0.2.1
        rust-walkdir-2.5.0
        rust-wasi-0.11.0+wasi-snapshot-preview1
        rust-wasi-0.13.3+wasi-0.2.2
        rust-wasm-bindgen-0.2.100
        rust-wasm-bindgen-backend-0.2.100
        rust-wasm-bindgen-futures-0.4.50
        rust-wasm-bindgen-macro-0.2.100
        rust-wasm-bindgen-macro-support-0.2.100
        rust-wasm-bindgen-shared-0.2.100
        rust-wayland-backend-0.3.8
        rust-wayland-client-0.31.8
        rust-wayland-csd-frame-0.3.0
        rust-wayland-cursor-0.31.8
        rust-wayland-egl-0.32.5
        rust-wayland-protocols-0.32.6
        rust-wayland-protocols-misc-0.3.6
        rust-wayland-protocols-plasma-0.3.6
        rust-wayland-protocols-wlr-0.3.6
        rust-wayland-scanner-0.31.6
        rust-wayland-server-0.31.7
        rust-wayland-sys-0.31.6
        rust-web-sys-0.3.77
        rust-web-time-1.1.0
        rust-winapi-0.3.9
        rust-winapi-i686-pc-windows-gnu-0.4.0
        rust-winapi-util-0.1.9
        rust-winapi-x86-64-pc-windows-gnu-0.4.0
        rust-windows-0.58.0
        rust-windows-core-0.58.0
        rust-windows-implement-0.58.0
        rust-windows-interface-0.58.0
        rust-windows-result-0.2.0
        rust-windows-strings-0.1.0
        rust-windows-sys-0.45.0
        rust-windows-sys-0.48.0
        rust-windows-sys-0.52.0
        rust-windows-sys-0.59.0
        rust-windows-targets-0.42.2
        rust-windows-targets-0.48.5
        rust-windows-targets-0.52.6
        rust-windows-aarch64-gnullvm-0.42.2
        rust-windows-aarch64-gnullvm-0.48.5
        rust-windows-aarch64-gnullvm-0.52.6
        rust-windows-aarch64-msvc-0.42.2
        rust-windows-aarch64-msvc-0.48.5
        rust-windows-aarch64-msvc-0.52.6
        rust-windows-i686-gnu-0.42.2
        rust-windows-i686-gnu-0.48.5
        rust-windows-i686-gnu-0.52.6
        rust-windows-i686-gnullvm-0.52.6
        rust-windows-i686-msvc-0.42.2
        rust-windows-i686-msvc-0.48.5
        rust-windows-i686-msvc-0.52.6
        rust-windows-x86-64-gnu-0.42.2
        rust-windows-x86-64-gnu-0.48.5
        rust-windows-x86-64-gnu-0.52.6
        rust-windows-x86-64-gnullvm-0.42.2
        rust-windows-x86-64-gnullvm-0.48.5
        rust-windows-x86-64-gnullvm-0.52.6
        rust-windows-x86-64-msvc-0.42.2
        rust-windows-x86-64-msvc-0.48.5
        rust-windows-x86-64-msvc-0.52.6
        rust-winit-0.30.9
        rust-winnow-0.7.3
        rust-wit-bindgen-rt-0.33.0
        rust-write16-1.0.0
        rust-writeable-0.5.5
        rust-x11-dl-2.21.0
        rust-x11rb-0.13.1
        rust-x11rb-protocol-0.13.1
        rust-xcursor-0.3.8
        rust-xdg-home-1.3.0
        rust-xkbcommon-0.8.0
        rust-xkbcommon-dl-0.4.2
        rust-xkeysym-0.2.1
        rust-xml-rs-0.8.25
        rust-xshell-0.2.7
        rust-xshell-macros-0.2.7
        rust-yansi-1.0.1
        rust-yansi-term-0.1.2
        rust-yoke-0.7.5
        rust-yoke-derive-0.7.5
        rust-zbus-5.5.0
        rust-zbus-macros-5.5.0
        rust-zbus-names-4.2.0
        rust-zerocopy-0.7.35
        rust-zerocopy-derive-0.7.35
        rust-zerofrom-0.1.6
        rust-zerofrom-derive-0.1.6
        rust-zerovec-0.10.4
        rust-zerovec-derive-0.10.3
        rust-zvariant-5.4.0
        rust-zvariant-derive-5.4.0
        rust-zvariant-utils-3.2.0))
