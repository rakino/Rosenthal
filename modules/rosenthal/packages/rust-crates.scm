;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages rust-crates)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (rosenthal packages wm)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  DO NOT add definitions manually.
;;;

;;;
;;; Rust dependencies fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aead-0.5.2
  (crate-source "aead" "0.5.2"
                "1c32aviraqag7926xcb9sybdm36v5vh9gnxpn4pxdwjc50zl28ni"))

(define rust-ahash-0.7.8
  (crate-source "ahash" "0.7.8"
                "1y9014qsy6gs9xld4ch7a6xi9bpki8vaciawxq4p75d8qvh7f549"))

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-0.7.20
  (crate-source "aho-corasick" "0.7.20"
                "1b3if3nav4qzgjz9bf75b2cv2h2yisrqfs0np70i38kgz4cn94yc"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anstyle-wincon-3.0.7
  (crate-source "anstyle-wincon" "3.0.7"
                "0kmf0fq4c8yribdpdpylzz1zccpy84hizmcsac3wrac1f7kk8dfa"))

(define rust-anyhow-1.0.97
  (crate-source "anyhow" "1.0.97"
                "0kvspbiwncmmkdgrwjrimsmbmhzxc641p5ql99l2rjq6smmdbznw"))

(define rust-anyhow-1.0.99
  (crate-source "anyhow" "1.0.99"
                "001icqvkfl28rxxmk99rm4gvdzxqngj5v50yg2bh3dzcvqfllrxh"))

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arboard-3.5.0
  (crate-source "arboard" "3.5.0"
                "0w1yqcx51153hy5w3y0702xjc9nmlhncw9f5l0rdwbl62pvj3py1"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-argon2-0.5.3
  (crate-source "argon2" "0.5.3"
                "0wn0kk97k49wxidfigmz1pdqmygqzi4h6w72ib7cpq765s4i0diw"))

(define rust-ariadne-0.5.1
  (crate-source "ariadne" "0.5.1"
                "13v3nziybxqq6bbjkgr7zilhq0qynv3yk83118s6z6p0lkff7x9n"))

(define rust-async-stream-0.3.6
  (crate-source "async-stream" "0.3.6"
                "0xl4zqncrdmw2g6241wgr11dxdg4h7byy6bz3l6si03qyfk72nhb"))

(define rust-async-stream-impl-0.3.6
  (crate-source "async-stream-impl" "0.3.6"
                "0kaplfb5axsvf1gfs2gk6c4zx6zcsns0yf3ssk7iwni7bphlvhn7"))

(define rust-async-trait-0.1.88
  (crate-source "async-trait" "0.1.88"
                "1dgxvz7g75cmz6vqqz0mri4xazc6a8xfj1db6r9fxz29lzyd6fg5"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-atoi-2.0.0
  (crate-source "atoi" "2.0.0"
                "0a05h42fggmy7h0ajjv6m7z72l924i7igbx13hk9d8pyign9k3gj"))

(define rust-atomic-0.6.1
  (crate-source "atomic" "0.6.1"
                "0h43ljcgbl6vk62hs6yk7zg7qn3myzvpw8k7isb9nzhkbdvvz758"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-axum-0.7.9
  (crate-source "axum" "0.7.9"
                "07z7wqczi9i8xb4460rvn39p4wjqwr32hx907crd1vwb2fy8ijpd"))

(define rust-axum-0.8.4
  (crate-source "axum" "0.8.4"
                "1d99kb3vcjnhbgrf6hysllf25hzagw7m1i1nidjpgsaa30n8c7h2"))

(define rust-axum-core-0.4.5
  (crate-source "axum-core" "0.4.5"
                "16b1496c4gm387q20hkv5ic3k5bd6xmnvk50kwsy6ymr8rhvvwh9"))

(define rust-axum-core-0.5.2
  (crate-source "axum-core" "0.5.2"
                "19kwzksb4hwr3qfbrhjbqf83z6fjyng14wrkzck6fj1g8784qik8"))

(define rust-axum-server-0.7.2
  (crate-source "axum-server" "0.7.2"
                "1azq83vhnwl3c8b7cvgrp9md02kahjjlmrxniwz0kw3d1pv0ap29"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

(define rust-backtrace-0.3.75
  (crate-source "backtrace" "0.3.75"
                "00hhizz29mvd7cdqyz5wrj98vqkihgcxmv2vl7z0d0f53qrac1k8"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-base64-simd-0.7.0
  (crate-source "base64-simd" "0.7.0"
                "1mg5ayj5z7imfyv06fhzi5rw289gv5yrfakxzsad22zz786d47bq"))

(define rust-base64ct-1.7.3
  (crate-source "base64ct" "1.7.3"
                "18scrpjl145msdb64q4nbb0plm4wbmp5lml134nz0c5rvxm5pql9"))

(define rust-beef-0.5.2
  (crate-source "beef" "0.5.2"
                "1c95lbnhld96iwwbyh5kzykbpysq0fnjfhwxa1mhap5qxgrl30is"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.9.0
  (crate-source "bitflags" "2.9.0"
                "1gb5w7pxnmx8l2bjz1i6rkbwbm2167k294rhy6cl1y3vbc8i90jw"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-bitvec-1.0.1
  (crate-source "bitvec" "1.0.1"
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))

(define rust-blake2-0.10.6
  (crate-source "blake2" "0.10.6"
                "1zlf7w7gql12v61d9jcbbswa3dw8qxsjglylsiljp9f9b3a2ll26"
                #:snippet '(delete-file-recursively "tests")))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-bumpalo-3.17.0
  (crate-source "bumpalo" "3.17.0"
                "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-by-address-1.2.1
  (crate-source "by_address" "1.2.1"
                "01idmag3lcwnnqrnnyik2gmbrr34drsi97q15ihvcbbidf2kryk4"))

(define rust-bytecheck-0.6.12
  (crate-source "bytecheck" "0.6.12"
                "1hmipv4yyxgbamcbw5r65wagv9khs033v9483s9kri9sw9ycbk93"))

(define rust-bytecheck-derive-0.6.12
  (crate-source "bytecheck_derive" "0.6.12"
                "0ng6230brd0hvqpbgcx83inn74mdv3abwn95x515bndwkz90dd1x"))

(define rust-bytemuck-1.22.0
  (crate-source "bytemuck" "1.22.0"
                "0h6m8wh7iw98cn69k53plbyqff78c2yrs32l0fy4wqdcvc8grcdn"))

(define rust-bytemuck-1.23.2
  (crate-source "bytemuck" "1.23.2"
                "0xs637lsr9p73ackjkmbjw80dp1dfdw0ydhdk0gzjcnzpkpfm59r"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

(define rust-calendrical-calculations-0.1.3
  (crate-source "calendrical_calculations" "0.1.3"
                "0f1xix1ph9v37ngxw1zq1id7hwv76bk74c4anb4jb1k6bplp6zz9"))

(define rust-capstone-0.13.0
  (crate-source "capstone" "0.13.0"
                "16mcsipj3x9fbk8lhvp4zqy551i8pnk9nl7r9bwy6hqprbazaph1"))

(define rust-capstone-sys-0.17.0
  ;; TODO: Check bundled sources.
  (crate-source "capstone-sys" "0.17.0"
                "1809b0is1415n13ayn3wwpknzhmfs7zq8hpccdw1kr512s6wnrr2"))

(define rust-cassowary-0.3.0
  (crate-source "cassowary" "0.3.0"
                "0lvanj0gsk6pc1chqrh4k5k0vi1rfbgzmsk46dwy3nmrqyw711nz"))

(define rust-castaway-0.2.3
  (crate-source "castaway" "0.2.3"
                "1mf0wypwnkpa1hi0058vp8g7bjh2qraip2qv7dmak7mg1azfkfha"))

(define rust-cc-1.2.18
  (crate-source "cc" "1.2.18"
                "0p6d2pfyrjgqpf2w399wzj4hmyffj6g0gyzg3pdy6xl3gmhlcl2j"))

(define rust-cc-1.2.36
  (crate-source "cc" "1.2.36"
                "0m3dg927zvvfvnxzifwk3rf041syg4y1wl3g3ayfsplfck9b6ljj"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-chacha20-0.9.1
  (crate-source "chacha20" "0.9.1"
                "0678wipx6kghp71hpzhl2qvx80q7caz3vm8vsvd07b1fpms3yqf3"
                #:snippet '(delete-file-recursively "tests")))

(define rust-chrono-0.4.40
  (crate-source "chrono" "0.4.40"
                "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s"))

(define rust-chrono-0.4.41
  (crate-source "chrono" "0.4.41"
                "0k8wy2mph0mgipq28vv3wirivhb31pqs7jyid0dzjivz0i9djsf4"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-clap-4.5.35
  (crate-source "clap" "4.5.35"
                "0i1rnz7mwbhs5qf10r6vmrkplkzm3477khkwz189rha49f9qdanq"))

(define rust-clap-4.5.47
  (crate-source "clap" "4.5.47"
                "0c99f6m4a7d4ffgahid49h0ci2pv4ccdf417f76nl4wx5n801b3y"))

(define rust-clap-builder-4.5.35
  (crate-source "clap_builder" "4.5.35"
                "1nczcw6cc49ap99nn3v3n0vrv7j74zin34palq6ji586vnrdn514"))

(define rust-clap-builder-4.5.47
  (crate-source "clap_builder" "4.5.47"
                "1mp1f0fz6wp9v87jc9372lg6r4514ja1l8cazf25hfz7a3vvpn9a"))

(define rust-clap-complete-4.5.47
  (crate-source "clap_complete" "4.5.47"
                "1dkzjgmi0c4jgq4cwvmzbaki9mxanll6d0mw5gwd8ji6x9w56vy0"))

(define rust-clap-complete-nushell-4.5.5
  (crate-source "clap_complete_nushell" "4.5.5"
                "12miqxh9g7q37w11bgv55b32s0hdf6avf0lhagzc5psp6icv3a66"))

(define rust-clap-derive-4.5.32
  (crate-source "clap_derive" "4.5.32"
                "1mqcag8qapb5yhygg2hi153kzmbf7w5hqp3nl3fvl5cn4yp6l5q9"))

(define rust-clap-derive-4.5.47
  (crate-source "clap_derive" "4.5.47"
                "174z9g13s85la2nmi8gv8ssjwz77im3rqg5isiinw6hg1fp7xzdv"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clap-lex-0.7.5
  (crate-source "clap_lex" "0.7.5"
                "0xb6pjza43irrl99axbhs12pxq4sr8x7xd36p703j57f5i3n2kxr"))

(define rust-clipboard-win-5.4.0
  (crate-source "clipboard-win" "5.4.0"
                "14n87fc0vzbd0wdhqzvcs1lqgafsncplzcanhpik93xhhalfgvqm"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colored-2.2.0
  (crate-source "colored" "2.2.0"
                "0g6s7j2qayjd7i3sivmwiawfdg8c8ldy0g2kl4vwk1yk16hjaxqi"))

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

(define rust-console-api-0.8.1
  (crate-source "console-api" "0.8.1"
                "0mr8xkzcb7mrcm2ca0ah1sj21rhpk207klscnql884hdrdg76c40"))

(define rust-console-subscriber-0.4.1
  (crate-source "console-subscriber" "0.4.1"
                "00badn9pjwbfd985vc7ngl7ai6mc3q58c3q43i5izlscdafalfb5"))

(define rust-const-oid-0.9.6
  (crate-source "const-oid" "0.9.6"
                "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))

(define rust-const-str-0.3.2
  (crate-source "const-str" "0.3.2"
                "1wxay9fr0ydvand3vidzcrhaapvjq4mg8fhw8axh441afrr7f1r1"))

(define rust-const-str-proc-macro-0.3.2
  (crate-source "const-str-proc-macro" "0.3.2"
                "0np2206r7w3c7lg5rg06wg8bby7fmrc13cp163jl2c2x5vfhy7jy"))

(define rust-convert-case-0.6.0
  (crate-source "convert_case" "0.6.0"
                "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c"))

(define rust-cookie-0.18.1
  (crate-source "cookie" "0.18.1"
                "0iy749flficrlvgr3hjmf3igr738lk81n5akzf4ym4cs6cxg7pjd"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-maths-0.1.1
  (crate-source "core_maths" "0.1.1"
                "0c0dv11ixxpc9bsx5xasvl98mb1dlprzcm6qq6ls3nsygw0mwx3p"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-cranelift-0.120.2
  (crate-source "cranelift" "0.120.2"
                "10m2chg7l4zk8j9rf2jrb1r4a815b0jpp23q65c9fddkpm0adg98"))

(define rust-cranelift-assembler-x64-0.120.2
  (crate-source "cranelift-assembler-x64" "0.120.2"
                "0s0zdzxlb1q36hj3j129jmbzkbp4rz63qyci50f3b3rdcc33w0m5"))

(define rust-cranelift-assembler-x64-meta-0.120.2
  (crate-source "cranelift-assembler-x64-meta" "0.120.2"
                "0rgxwhy904p7nfxs46pm39ir5s8y7q7582habjq6w7wc9hmh3i5i"))

(define rust-cranelift-bforest-0.120.2
  (crate-source "cranelift-bforest" "0.120.2"
                "15gqcxfv49zd5bhqvb2ry69i796iyb38nsq90iqkmvs294xqhvad"))

(define rust-cranelift-bitset-0.120.2
  (crate-source "cranelift-bitset" "0.120.2"
                "05sx2xlf4kprly6nri2q1hljryvqsrj543chdmqqmjn6xvljwyyv"))

(define rust-cranelift-codegen-0.120.2
  (crate-source "cranelift-codegen" "0.120.2"
                "1d7llixjsnkkiyfzgq9ljfjqfnwcbyjq7ag9q8dcxzbsay90inmf"))

(define rust-cranelift-codegen-meta-0.120.2
  (crate-source "cranelift-codegen-meta" "0.120.2"
                "05fdvxg6yrx1idw1930hd176crvg689k6ii2fiwc487rhw6lhqg4"))

(define rust-cranelift-codegen-shared-0.120.2
  (crate-source "cranelift-codegen-shared" "0.120.2"
                "189h78agxlzdj44pi9gz9nv0rbf99zv7mzxrqj26q310kz888rcp"))

(define rust-cranelift-control-0.120.2
  (crate-source "cranelift-control" "0.120.2"
                "0md9lqpq0rfywkmp4ckfgk95a1r0sj24rgwdm1d58z71yiq3vm26"))

(define rust-cranelift-entity-0.120.2
  (crate-source "cranelift-entity" "0.120.2"
                "1wcipz8l2s39jn0l2vhvziw6qwv23bhxdgvjhw641jr08mkihm6p"))

(define rust-cranelift-frontend-0.120.2
  (crate-source "cranelift-frontend" "0.120.2"
                "1szxw4ikc2q63dqwgsgjlzgv03g5nvgxcbf5yd2a71v6r28im2rw"))

(define rust-cranelift-isle-0.120.2
  (crate-source "cranelift-isle" "0.120.2"
                "11bjvdbyhcp811ys782bhxcbbh8c0jsc57mkjn915m1j6i7al6vi"))

(define rust-cranelift-jit-0.120.2
  (crate-source "cranelift-jit" "0.120.2"
                "1599x1acbzib88m8zw09xsfgxvwdaadbbb58ivc8x9kbw1mwxvl6"))

(define rust-cranelift-module-0.120.2
  (crate-source "cranelift-module" "0.120.2"
                "0k2w1452bd523bk2kjzf2hlml8snmxnlsx8gnp7k5kczwmsmn1p0"))

(define rust-cranelift-native-0.120.2
  (crate-source "cranelift-native" "0.120.2"
                "0gqs00i5gjnpxyhi2mffd76rs7qfdwsizw7nxbzwbg73cs33xj21"))

(define rust-cranelift-srcgen-0.120.2
  (crate-source "cranelift-srcgen" "0.120.2"
                "0ysdqsnm244zvm0gq05169bj6ll8f396gp0pdlk68p55hgbz9qq2"))

(define rust-crc-3.2.1
  (crate-source "crc" "3.2.1"
                "0dnn23x68qakzc429s1y9k9y3g8fn5v9jwi63jcz151sngby9rk9"))

(define rust-crc-catalog-2.4.0
  (crate-source "crc-catalog" "2.4.0"
                "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

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
                "1pr413ki440xgddlmkrc4j1bfx1h8rpmll87zn8ykja1bm2gwxpl"
                #:snippet '(delete-file-recursively "docs")))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-crypto-secretbox-0.1.1
  (crate-source "crypto_secretbox" "0.1.1"
                "1qa1w5s8dbyb88269zrmvbnillqahz394pl07bsds6gpmn3wzmmr"))

(define rust-cssparser-0.33.0
  (crate-source "cssparser" "0.33.0"
                "07i8k47fmym7kzs95qfhg6zrh4yyf2vl4460rmdyvyx06vck9scv"))

(define rust-cssparser-color-0.1.0
  (crate-source "cssparser-color" "0.1.0"
                "17qcjsrph1ywcdsx1ipqgmzaas4dbbir5djjmzbqjnfqc6d0jv2m"))

(define rust-cssparser-macros-0.6.1
  (crate-source "cssparser-macros" "0.6.1"
                "0cfkzj60avrnskdmaf7f8zw6pp3di4ylplk455zrzaf19ax8id8k"))

(define rust-curl-0.4.49
  (crate-source "curl" "0.4.49"
                "1g7dcrh4mwkn2q0iiga7vc8i68pd5jdby5aparpa6yxqs1nkpz3r"))

(define rust-curl-sys-0.4.83+curl-8.15.0
  ;; TODO: Check bundled sources.
  (crate-source "curl-sys" "0.4.83+curl-8.15.0"
                "10xl373idbap4rck2ym19dy3ysnlg544cak3h86b2z820krxlc2q"))

(define rust-curve25519-dalek-4.1.3
  (crate-source "curve25519-dalek" "4.1.3"
                "1gmjb9dsknrr8lypmhkyjd67p1arb8mbfamlwxm7vph38my8pywp"))

(define rust-curve25519-dalek-derive-0.1.1
  (crate-source "curve25519-dalek-derive" "0.1.1"
                "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l"))

(define rust-darling-0.20.11
  (crate-source "darling" "0.20.11"
                "1vmlphlrlw4f50z16p4bc9p5qwdni1ba95qmxfrrmzs6dh8lczzw"))

(define rust-darling-core-0.20.11
  (crate-source "darling_core" "0.20.11"
                "0bj1af6xl4ablnqbgn827m43b8fiicgv180749f5cphqdmcvj00d"))

(define rust-darling-macro-0.20.11
  (crate-source "darling_macro" "0.20.11"
                "1bbfbc2px6sj1pqqq97bgqn6c8xdnb2fmz66f7f40nrqrcybjd7w"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-data-encoding-2.9.0
  (crate-source "data-encoding" "2.9.0"
                "0xm46371aw613ghc12ay4vsnn49hpcmcwlijnqy8lbp2bpd308ra"))

(define rust-data-url-0.1.1
  (crate-source "data-url" "0.1.1"
                "14z15yiyklp5dv0k0q6pd83irrn0y8hj9y3fj17akkrbf37byc1s"))

(define rust-der-0.7.9
  (crate-source "der" "0.7.9"
                "1h4vzjfa1lczxdf8avfj9qlwh1qianqlxdy1g5rn762qnvkzhnzm"
                #:snippet '(delete-file-recursively "tests")))

(define rust-deranged-0.4.0
  (crate-source "deranged" "0.4.0"
                "13h6skwk411wzhf1l9l7d3yz5y6vg9d7s3dwhhb4a942r88nm7lw"))

(define rust-deranged-0.5.3
  (crate-source "deranged" "0.5.3"
                "1k473y8lzjac956dm3s1cs2rz364py9zd52y9fkbanws8b6vqc6n"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-directories-5.0.1
  (crate-source "directories" "5.0.1"
                "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-sys-0.4.1
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-divan-0.1.18
  (crate-source "divan" "0.1.18"
                "0pknjnbkcq4s0zr1wv6jhnqd2bn65248iap3xdg3pgg2gwqmd700"))

(define rust-divan-0.1.21
  (crate-source "divan" "0.1.21"
                "0cw9i6yrr78axsjpd7pb2vfzdpxm19bs7d1j1s5y13wbqxz4a1d4"))

(define rust-divan-macros-0.1.18
  (crate-source "divan-macros" "0.1.18"
                "0ph43fmy3ksafy6lqmh1z3idi8pmh5rjgh6yms8xnm77ga1fjk8g"))

(define rust-divan-macros-0.1.21
  (crate-source "divan-macros" "0.1.21"
                "08rkmilvqmdmgqb5msnk70psipx7bcz1fh5641j5sm2n160bqmlm"))

(define rust-document-features-0.2.11
  (crate-source "document-features" "0.2.11"
                "0pdhpbz687fk2rkgz45yy3gvbhlxliwb7g1lj3jbx1f1qr89n94m"))

(define rust-dotenvy-0.15.7
  (crate-source "dotenvy" "0.15.7"
                "16s3n973n5aqym02692i1npb079n5mb0fwql42ikmwn8wnrrbbqs"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dtoa-1.0.10
  (crate-source "dtoa" "1.0.10"
                "016gid01rarcdv57h049d7nr9daxc2hc2gqzx0mji57krywd7bfn"))

(define rust-dtoa-short-0.3.5
  (crate-source "dtoa-short" "0.3.5"
                "11rwnkgql5jilsmwxpx6hjzkgyrbdmx1d71s0jyrjqm5nski25fd"))

(define rust-dyn-clone-1.0.20
  (crate-source "dyn-clone" "1.0.20"
                "0m956cxcg8v2n8kmz6xs5zl13k2fak3zkapzfzzp7pxih6hix26h"))

(define rust-ed25519-2.2.3
  (crate-source "ed25519" "2.2.3"
                "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i"
                #:snippet '(delete-file-recursively "tests")))

(define rust-ed25519-dalek-2.1.1
  (crate-source "ed25519-dalek" "2.1.1"
                "0w88cafwglg9hjizldbmlza0ns3hls81zk1bcih3m5m3h67algaa"
                #:snippet '(for-each delete-file-recursively '("docs" "tests"))))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-env-filter-0.1.3
  (crate-source "env_filter" "0.1.3"
                "1l4p6f845cylripc3zkxa0lklk8rn2q86fqm522p6l2cknjhavhq"))

(define rust-env-home-0.1.0
  (crate-source "env_home" "0.1.0"
                "1zn08mk95rjh97831rky1n944k024qrwjhbcgb0xv9zhrh94xy67"))

(define rust-env-logger-0.11.8
  (crate-source "env_logger" "0.11.8"
                "17q6zbjam4wq75fa3m4gvvmv3rj3ch25abwbm84b28a0j3q67j0k"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-erased-serde-0.4.6
  (crate-source "erased-serde" "0.4.6"
                "1dx5hj16hsl143czwl2g7ymdi1y84lsjyyii2zprzjqzyn3xh170"))

(define rust-errno-0.3.11
  (crate-source "errno" "0.3.11"
                "0kjrrcaa5nvickysw7z3vm5p0l7l457idf1ff3z6ang8qwnx8vcp"))

(define rust-errno-0.3.13
  (crate-source "errno" "0.3.13"
                "1bd5g3srn66zr3bspac0150bvpg1s7zi6zwhwhlayivciz12m3kp"))

(define rust-error-code-3.3.1
  (crate-source "error-code" "3.3.1"
                "0bx9hw3pahzqym8jvb0ln4qsabnysgn98mikyh2afhk9rif31nd5"))

(define rust-etcetera-0.8.0
  (crate-source "etcetera" "0.8.0"
                "0hxrsn75dirbjhwgkdkh0pnpqrnq17ypyhjpjaypgax1hd91nv8k"))

(define rust-event-listener-5.4.0
  (crate-source "event-listener" "5.4.0"
                "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fast-srgb8-1.0.0
  (crate-source "fast-srgb8" "1.0.0"
                "18g6xwwh4gnkyx1352hnvwagpv0n4y98yp2llm8vyvwxh487abnx"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"
                #:snippet '(delete-file-recursively "tests")))

(define rust-fiat-crypto-0.2.9
  (crate-source "fiat-crypto" "0.2.9"
                "07c1vknddv3ak7w89n85ik0g34nzzpms6yb845vrjnv9m4csbpi8"))

(define rust-figment-0.10.19
  (crate-source "figment" "0.10.19"
                "1ww9sxdzjj2i80w7bq0kllnymjyrfb1cdx2h70ap5wqcdga1rc4c"))

(define rust-figment-file-provider-adapter-0.1.1
  (crate-source "figment_file_provider_adapter" "0.1.1"
                "0553h2yf5nlyhnhkywscmzaq8wih1njhi51h50vzlzkrjq41wgim"))

(define rust-filedescriptor-0.8.3
  (crate-source "filedescriptor" "0.8.3"
                "0bb8qqa9h9sj2mzf09yqxn260qkcqvmhmyrmdjvyxcn94knmh1z4"))

(define rust-find-msvc-tools-0.1.1
  (crate-source "find-msvc-tools" "0.1.1"
                "0b8rhghgjssjw9q8a3gg7f9kl8zhy9d7nqsc4s4nc52dyqq9knbz"))

(define rust-fixed-decimal-0.5.6
  (crate-source "fixed_decimal" "0.5.6"
                "1f1xqn2fxns768isv4h5vm1yi0j8npbfl825dvpcv7la26qvxsqg"))

(define rust-fixedbitset-0.4.2
  (crate-source "fixedbitset" "0.4.2"
                "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-flate2-1.1.1
  (crate-source "flate2" "1.1.1"
                "1kpycx57dqpkr3vp53b4nq75p9mflh0smxy8hkys4v4ndvkr5vbw"
                #:snippet '(for-each delete-file-recursively '("examples" "tests"))))

(define rust-flate2-1.1.2
  (crate-source "flate2" "1.1.2"
                "07abz7v50lkdr5fjw8zaw2v8gm2vbppc0f7nqm8x3v3gb6wpsgaa"))

(define rust-flume-0.11.1
  (crate-source "flume" "0.11.1"
                "15ch0slxa8sqsi6c73a0ky6vdnh48q8cxjf7rksa3243m394s3ns"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fs-err-2.11.0
  (crate-source "fs-err" "2.11.0"
                "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948"))

(define rust-fs-err-3.1.0
  (crate-source "fs-err" "3.1.0"
                "1al2sj8src02wwww70vv2gypsrs6wyzx6zlpk82h84m2qajbv28z"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

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

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-0.4.3
  (crate-source "gethostname" "0.4.3"
                "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.2
  (crate-source "getrandom" "0.3.2"
                "1w2mlixa1989v7czr68iji7h67yra2pbg3s480wsqjza1r2sizkk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-h2-0.3.26
  (crate-source "h2" "0.3.26"
                "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1"))

(define rust-h2-0.4.12
  (crate-source "h2" "0.4.12"
                "11hk5mpid8757z6n3v18jwb62ikffrgzjlrgpzqvkqdlzjfbdh7k"))

(define rust-h2-0.4.8
  (crate-source "h2" "0.4.8"
                "1hp3lijg1br982kzgglb5ks2ibg68a76z3rl052r8c5vyi7jj5sh"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.13.1
  (crate-source "hashbrown" "0.13.1"
                "0f602rk7pgdhw1s57g81822g7b2m5i2wibrpaqp11afk5kk8mzrk"))

(define rust-hashbrown-0.13.2
  (crate-source "hashbrown" "0.13.2"
                "03ji3n19j4b6mf2wlla81vsixcmlivglp6hgk79d1pcxfcrw38s3"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashlink-0.10.0
  (crate-source "hashlink" "0.10.0"
                "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk"))

(define rust-hdrhistogram-7.5.4
  (crate-source "hdrhistogram" "7.5.4"
                "07ai0r66l1n53f2757gv07za1l5g1bprb7zz4v75kpbky6c92p3n"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hkdf-0.12.4
  (crate-source "hkdf" "0.12.4"
                "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv"
                #:snippet '(delete-file-recursively "tests")))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"
                #:snippet '(delete-file-recursively "tests")))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-http-0.2.12
  (crate-source "http" "0.2.12"
                "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

(define rust-http-body-0.4.6
  (crate-source "http-body" "0.4.6"
                "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-humantime-2.2.0
  (crate-source "humantime" "2.2.0"
                "17rz8jhh1mcv4b03wnknhv1shwq2v9vhkhlfg884pprsig62l4cv"))

(define rust-hyper-0.14.32
  (crate-source "hyper" "0.14.32"
                "1rvcb0smz8q1i0y6p7rwxr02x5sclfg2hhxf3g0774zczn0cgps1"))

(define rust-hyper-1.6.0
  (crate-source "hyper" "1.6.0"
                "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc"))

(define rust-hyper-1.7.0
  (crate-source "hyper" "1.7.0"
                "07n59pxzlq621z611cbpvh7p4h9h15v0r7m5wgxygpx02d5aafpb"))

(define rust-hyper-rustls-0.24.2
  (crate-source "hyper-rustls" "0.24.2"
                "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))

(define rust-hyper-rustls-0.27.5
  (crate-source "hyper-rustls" "0.27.5"
                "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d"))

(define rust-hyper-timeout-0.5.2
  (crate-source "hyper-timeout" "0.5.2"
                "1c431l5ckr698248yd6bnsmizjy2m1da02cbpmsnmkpvpxkdb41b"))

(define rust-hyper-util-0.1.11
  (crate-source "hyper-util" "0.1.11"
                "1wj3svb1r6yv6kgk5fsz6wwajmngc4zxcw4wxpwlmpbgl8rvqys9"))

(define rust-hyper-util-0.1.16
  (crate-source "hyper-util" "0.1.16"
                "0pmw8gqkqjnsdrxdy5wd5q8z1gh7caxqk2an7b4s53byghkhb6wd"))

(define rust-iana-time-zone-0.1.63
  (crate-source "iana-time-zone" "0.1.63"
                "1n171f5lbc7bryzmp1h30zw86zbvl5480aq02z92lcdwvvjikjdh"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-icu-1.5.0
  (crate-source "icu" "1.5.0"
                "00hmwmggwhqfbwzjzqsafm8bq6szlr0abvyw025icgvhil0y7xfz"))

(define rust-icu-calendar-1.5.2
  (crate-source "icu_calendar" "1.5.2"
                "1zx8fli5aif3w7zwx5gxijxbsjsphkwr339h99izfdlsgw9v4rbj"))

(define rust-icu-calendar-data-1.5.1
  (crate-source "icu_calendar_data" "1.5.1"
                "1qg9zag3scrrksdc7bqf2lcjcmdlwx2g9d080qcn48c5gvkrj142"))

(define rust-icu-casemap-1.5.1
  (crate-source "icu_casemap" "1.5.1"
                "0px7gp1mijafkv26bihpfjqxjg0zmgwmyf7w4wpb2ccdkypciw4z"))

(define rust-icu-casemap-data-1.5.1
  (crate-source "icu_casemap_data" "1.5.1"
                "1nfmvsvnbid6aaj8caja8vn97sclpzdilqalrnjqa317fri9zg82"))

(define rust-icu-collator-1.5.0
  (crate-source "icu_collator" "1.5.0"
                "1ck6d7nrkq3p3hcwjnbhcyy16kz58dbs3sn3c7rmc7fkhwc3fw6k"))

(define rust-icu-collator-data-1.5.1
  (crate-source "icu_collator_data" "1.5.1"
                "1fwfqf5h8ky4xn0wl6df8a9n6bwqp3r5xsjdr8g9ja3xsy33jdbv"))

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-datetime-1.5.1
  (crate-source "icu_datetime" "1.5.1"
                "100pdcq2y452py0bxa7vzy1sfibm11z2xxbpgvbkzpq8bswfy5fi"))

(define rust-icu-datetime-data-1.5.1
  (crate-source "icu_datetime_data" "1.5.1"
                "0zahlvry7ajl92cdnnjp1rx0vzi7vf1p0sljg8dsng0jfr0g1xdy"))

(define rust-icu-decimal-1.5.0
  (crate-source "icu_decimal" "1.5.0"
                "1sxhx2jw01csjbn37mqv28pbn673wj28ipqybvc4h17chs7xk3zv"))

(define rust-icu-decimal-data-1.5.1
  (crate-source "icu_decimal_data" "1.5.1"
                "1qhah965n48j2izqx8bcm1pn8dkxxjb584cwga1nvkswgzcmvjb7"))

(define rust-icu-experimental-0.1.0
  (crate-source "icu_experimental" "0.1.0"
                "1dwky7095m459nh9ki51dlbpzijasz24nsax0rccfrd1havdfjl4"))

(define rust-icu-experimental-data-0.1.1
  (crate-source "icu_experimental_data" "0.1.1"
                "1pflgddixfbpvgwb6vc0gwvq90yvwz0h3x4g9rnjixdqmwpgj78j"))

(define rust-icu-list-1.5.0
  (crate-source "icu_list" "1.5.0"
                "0rf3czrm7831i7hmdasnxn8rlmaa61i7b2sfvn759dkmfwfxmzmv"))

(define rust-icu-list-data-1.5.1
  (crate-source "icu_list_data" "1.5.1"
                "084i0sd5qdch43zlgm42f1xmn5izqydbak1mx0dqz5gkvgxsgcaj"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.1
  (crate-source "icu_locid_transform_data" "1.5.1"
                "07gignya9gzynnyds88bmra4blq9jxzgrcss43vzk2q9h7byc5bm"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-data-1.5.1
  (crate-source "icu_normalizer_data" "1.5.1"
                "1dqcm86spcqcs4jnra81yqq3g5bpw6bvf5iz621spj5x52137s65"))

(define rust-icu-pattern-0.2.0
  (crate-source "icu_pattern" "0.2.0"
                "1r6p5y5dxdspwwvbj8ms3xf2f8h82al6irilvqbng389znm3czyb"))

(define rust-icu-plurals-1.5.0
  (crate-source "icu_plurals" "1.5.0"
                "055iwgyxmdwm197wvb8d9x16lrhirn4c39dh072xbnr5q3kp0nms"))

(define rust-icu-plurals-data-1.5.1
  (crate-source "icu_plurals_data" "1.5.1"
                "04nipymyc2xjvszh12vvzs06scw02wczi9bpda3nlzfb70r410x4"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-data-1.5.1
  (crate-source "icu_properties_data" "1.5.1"
                "1qm5vf17nyiwb87s3g9x9fsj32gkv4a7q7d2sblawx9vfncqgyw5"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-icu-segmenter-1.5.0
  (crate-source "icu_segmenter" "1.5.0"
                "1pmharib9s1hn5650d4lyd48145n1n14pja2gcnzqvrl29b745x7"))

(define rust-icu-segmenter-data-1.5.1
  (crate-source "icu_segmenter_data" "1.5.1"
                "1sx1rac5xxamqxr8x3cj91lkvlc4jblrrr4lqly3chcr2xsjgrd1"))

(define rust-icu-timezone-1.5.0
  (crate-source "icu_timezone" "1.5.0"
                "15nyqq5k231w51g3bmpbrsdnv1gfiam5s8w7qwha0farb1mbm4da"))

(define rust-icu-timezone-data-1.5.1
  (crate-source "icu_timezone_data" "1.5.1"
                "0s1f4g00mwmzv0r0hp0w0pk99qly9cmm78n2bc1ays522fvggp0s"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

(define rust-image-0.25.6
  (crate-source "image" "0.25.6"
                "06i522bq4qlwylwnlmcn0sgqg72swwan544aldbhi0drwr66cdfv"))

(define rust-image-0.25.8
  (crate-source "image" "0.25.8"
                "1rwill018gn2kwzv332kfs72ns0kwwnfxwacbhvk9lk9cwzfp7sj"))

(define rust-indenter-0.3.3
  (crate-source "indenter" "0.3.3"
                "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.11.0
  (crate-source "indexmap" "2.11.0"
                "1sb3nmhisf9pdwfcxzqlvx97xifcvlh5g0rqj9j7i7qg8f01jj7j"))

(define rust-indexmap-2.9.0
  (crate-source "indexmap" "2.9.0"
                "07m15a571yywmvqyb7ms717q9n42b46badbpsmx215jrg7dhv9yf"))

(define rust-indicatif-0.17.11
  (crate-source "indicatif" "0.17.11"
                "0db2b2r79r9x8x4lysq1ci9xm13c0xg0sqn3z960yh2bk2430fqq"))

(define rust-inetnum-0.1.1
  (crate-source "inetnum" "0.1.1"
                "06dcrk9j66xrh1930hmyn5vjx43hl1khbrglj4r91j8imkw0v503"))

(define rust-inlinable-string-0.1.15
  (crate-source "inlinable_string" "0.1.15"
                "1ysjci8yfvxgf51z0ny2nnwhxrclhmb3vbngin8v4bznhr3ybyn8"))

(define rust-inout-0.1.4
  (crate-source "inout" "0.1.4"
                "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7"))

(define rust-interim-0.1.2
  (crate-source "interim" "0.1.2"
                "1x5ykyv8bkv13398q3dpycg5943rw1jycvjbhi2yih30zw5hzzcs"))

(define rust-io-uring-0.7.10
  (crate-source "io-uring" "0.7.10"
                "0yvjyygwdcqjcgw8zp254hvjbm7as1c075dl50spdshas3aa4vq4"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-ipnet-trie-0.3.0
  (crate-source "ipnet-trie" "0.3.0"
                "1k2qj0fc7h4gw1jymxm86sdpxv2l4wl0p0k5aznj00hnrpn33yhb"))

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

(define rust-jiff-0.2.15
  (crate-source "jiff" "0.2.15"
                "0jby6kbs2ra33ji0rx4swcp66jzmcvgszc5v4izwfsgbn6w967xy"))

(define rust-jiff-0.2.5
  (crate-source "jiff" "0.2.5"
                "0q3jpq3scznmviiajldyf5xby38zgyvkxbrmgb9hf78r6416f0n1"))

(define rust-jiff-static-0.2.15
  (crate-source "jiff-static" "0.2.15"
                "1d4l4pvlhz3w487gyhnzvagpbparspv4c8f35qk6g5w9zx8k8d03"))

(define rust-jiff-static-0.2.5
  (crate-source "jiff-static" "0.2.5"
                "0k1v30mhbgh4zj2r9d7lfqlh5b20b5573cx0a4gip7rlkldf7pac"))

(define rust-jpeg-decoder-0.3.1
  (crate-source "jpeg-decoder" "0.3.1"
                "1c1k53svpdyfhibkmm0ir5w0v3qmcmca8xr8vnnmizwf6pdagm7m"
                #:snippet '(delete-file-recursively "benches")))

(define rust-js-sys-0.3.77
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-js-sys-0.3.78
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.78"
                "0f17vdkpbarm0qlbqb0p1fyiy4l9bs62zvw3fv0ywb29g0shc2qc"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-libc-0.2.171
  (crate-source "libc" "0.2.171"
                "1mipla3dy3l59pfa9xy4iw2vdgn8n30dzf4vdnasjflxdqhkg6f1"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libm-0.2.11
  (crate-source "libm" "0.2.11"
                "1yjgk18rk71rjbqcw9l1zaqna89p9s603k7n327nqs8dn88vwmc3"))

(define rust-libm-0.2.15
  (crate-source "libm" "0.2.15"
                "1plpzf0p829viazdj57yw5dhmlr8ywf3apayxc2f2bq5a6mvryzr"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libsqlite3-sys-0.30.1
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"))

(define rust-libyml-0.0.5
  (crate-source "libyml" "0.0.5"
                "106963pwg1gc3165bdlk8bbspmk919gk10vshhqglks3z8m700ik"))

(define rust-libz-sys-1.1.22
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.22"
                "07b5wxh0ska996kc0g2hanjhmb4di7ksm6ndljhr4pi0vykyfw4b"))

(define rust-lightningcss-1.0.0-alpha.67
  (crate-source "lightningcss" "1.0.0-alpha.67"
                "0j91k4p56jlz810scp454x79213z7z658xzdp1bd7vh5297bm3vr"))

(define rust-lightningcss-derive-1.0.0-alpha.43
  (crate-source "lightningcss-derive" "1.0.0-alpha.43"
                "0lh2hxppjs53hjdlzkgzn0zxa9f3jkq9wws1xp56g4r7s522ghc4"))

(define rust-linux-raw-sys-0.4.15
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.9.3
  (crate-source "linux-raw-sys" "0.9.3"
                "04zl7j4k1kgbn7rrl3i7yszaglgxp0c8dbwx8f1cabnjjwhb2zgy"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-listenfd-1.0.2
  (crate-source "listenfd" "1.0.2"
                "1flxwfgp6rrcgg09szb7g84i3ij09jv43w1y1d6jkd198r5cayxq"))

(define rust-litemap-0.7.5
  (crate-source "litemap" "0.7.5"
                "0mi8ykav0s974ps79p438x04snh0cdb7lc864b42jws5375i9yr3"))

(define rust-litrs-0.4.2
  (crate-source "litrs" "0.4.2"
                "1v8bxsrkm0w2k9nmbp8hsspy9i1lawajywqdw4hx87rjzqv41rgm"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-lock-api-0.4.13
  (crate-source "lock_api" "0.4.13"
                "0rd73p4299mjwl4hhlfj9qr88v3r0kc8s1nszkfmnq2ky43nb4wn"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-logos-0.14.4
  (crate-source "logos" "0.14.4"
                "0n349vin9mx326fkz68bsa4vc5sdn9n8qnfz7n1yqynbz1p3albj"))

(define rust-logos-codegen-0.14.4
  (crate-source "logos-codegen" "0.14.4"
                "0gwnx7lk4y7xc4yk6pr0knrddard5z22rxaz9xrnc38cc1lh1y2r"))

(define rust-logos-derive-0.14.4
  (crate-source "logos-derive" "0.14.4"
                "07bk3q4jry9f8blrnsiy872ivilzy62xaglnn2ni5p590qmp5yr4"))

(define rust-lru-0.12.5
  (crate-source "lru" "0.12.5"
                "0f1a7cgqxbyhrmgaqqa11m3azwhcc36w0v5r4izgbhadl3sg8k13"))

(define rust-lua-src-547.0.0
  ;; TODO: Check bundled sources.
  (crate-source "lua-src" "547.0.0"
                "0hjy276xbidvp8n0c6f1w76qamybiijfa0b7fj5rpd0p6ngg5nhy"))

(define rust-luajit-src-210.5.12+a4f56a4
  ;; TODO: Check bundled sources.
  (crate-source "luajit-src" "210.5.12+a4f56a4"
                "0wf6r910v56jm0aswh8rzjirl3z9aniaaifhckrdas2k5abfga5k"))

(define rust-mach2-0.4.2
  (crate-source "mach2" "0.4.2"
                "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r"))

(define rust-mach2-0.4.3
  (crate-source "mach2" "0.4.3"
                "0i6vcnbq5v51whgyidzhf7cbxqrmj2nkw8z0m2ib02rc60mjhh6n"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-matches-0.1.10
  (crate-source "matches" "0.1.10"
                "1994402fq4viys7pjhzisj4wcw894l53g798kkm2y74laxk0jci5"))

(define rust-matchit-0.7.3
  (crate-source "matchit" "0.7.3"
                "156bgdmmlv4crib31qhgg49nsjk88dxkdqp80ha2pk2rk6n6ax0f"))

(define rust-matchit-0.8.4
  (crate-source "matchit" "0.8.4"
                "1hzl48fwq1cn5dvshfly6vzkzqhfihya65zpj7nz7lfx82mgzqa7"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"
                #:snippet '(delete-file-recursively "tests")))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"
                #:snippet '(delete-file-recursively "src/tests")))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-memo-map-0.3.3
  (crate-source "memo-map" "0.3.3"
                "0yw5ria5j6kd8z0p2yiji1vc2x53zg348dv1fgip822n0x813l9q"))

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

(define rust-miette-7.5.0
  (crate-source "miette" "7.5.0"
                "114lv0nx46lxc5pncz6iyrzcfhn5g9a5janzc8cgsdvvz1jm358s"))

(define rust-miette-derive-7.5.0
  (crate-source "miette-derive" "7.5.0"
                "0irig3c79184h54zasn06yiz25znqrpvx8r72byr5gj9md2byidz"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-minify-html-0.16.4
  (crate-source "minify-html" "0.16.4"
                "1mdv9k2mw6way5kya4nmkr1c03m5b8d9wqipk8zal7ddg007dpzh"))

(define rust-minify-html-common-0.0.2
  (crate-source "minify-html-common" "0.0.2"
                "100p85mrv95rdrism2mbkpm9q0y25gf0kmybq08dxigxvx06nyk9"))

(define rust-minify-js-0.6.0
  (crate-source "minify-js" "0.6.0"
                "0bm9vlhn1dji43m0ck09b9ipc3i3wjmnql1y24j61mlbxr35bymi"))

(define rust-minijinja-2.12.0
  (crate-source "minijinja" "2.12.0"
                "1469k9gfrfmwhvm2dzpvxg6r01lribzaw0rggm5kycikabbn9wm9"))

(define rust-minijinja-embed-2.12.0
  (crate-source "minijinja-embed" "2.12.0"
                "1ba3ypbylqvh8rymmd5vw5v98iqyb295qwdvc2p4mabhaizccdql"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.7
  (crate-source "miniz_oxide" "0.8.7"
                "0c4lj692adnzw0h9j8l24d7imds3icpgdkk3b03zlhxf90zcww7z"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-minspan-0.1.2
  (crate-source "minspan" "0.1.2"
                "0053r44iqmfilibz8da3367adxjjwibw6d849xifxq0yhfgf99pf"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

(define rust-mio-1.0.4
  (crate-source "mio" "1.0.4"
                "073n3kam3nz8j8had35fd2nn7j6a33pi3y5w3kq608cari2d9gkq"))

(define rust-mlua-0.10.5
  (crate-source "mlua" "0.10.5"
                "1l1md9ff82h2ijdgvk5gvsdamff4469ln4vi2rkmzny7xgxzixf1"))

(define rust-mlua-sys-0.6.8
  ;; TODO: Check bundled sources.
  (crate-source "mlua-sys" "0.6.8"
                "14wafad18hzhw7m15l54g9c7fs9l1braklsi1vsgrjlr41z1y31q"))

(define rust-moxcms-0.7.5
  (crate-source "moxcms" "0.7.5"
                "026df3qpxn430dlngpj3gjip0m9280g3asvbia5dpsjsjfl2zlyx"))

(define rust-multimap-0.10.0
  (crate-source "multimap" "0.10.0"
                "00vs2frqdhrr8iqx4y3fbq73ax5l12837fvbjrpi729d85alrz6y"))

(define rust-nix-0.26.4
  (crate-source "nix" "0.26.4"
                "06xgl4ybb8pvjrbmc3xggbgk3kbs1j0c4c0nzdfrmpbgrkrym2sr"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-nom-8.0.0
  (crate-source "nom" "8.0.0"
                "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz"))

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

(define rust-objc2-0.6.0
  (crate-source "objc2" "0.6.0"
                "0nbf78zvqz3wnp23iianpwbds563fiz8b6bsnxizikyrj18zcc9m"))

(define rust-objc2-app-kit-0.3.0
  (crate-source "objc2-app-kit" "0.3.0"
                "1yyh2j44kjhj2n55y2vlj82lzsbfpngvivv9w1x2z3hpawrgj1jr"))

(define rust-objc2-core-foundation-0.3.0
  (crate-source "objc2-core-foundation" "0.3.0"
                "09frj2bc6w6dnpjfix1skq8g91kx7w788bqwiaa2c7a74l7zdsns"))

(define rust-objc2-core-graphics-0.3.0
  (crate-source "objc2-core-graphics" "0.3.0"
                "00nynwlbppcdp7q51cmq4x04sns0lqhhla8kcmmkarcbc81adp7q"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.3.0
  (crate-source "objc2-foundation" "0.3.0"
                "166rz3jh3b2nrwvldllp3ihq4lb4j5pkjnyv2naw70jb074wc89s"))

(define rust-objc2-io-surface-0.3.0
  (crate-source "objc2-io-surface" "0.3.0"
                "068wb02jcnna2qgir250nfvrsic4kz1rx7ks39p0h416wf3qn6hn"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-opaque-debug-0.3.1
  (crate-source "opaque-debug" "0.3.1"
                "10b3w0kydz5jf1ydyli5nv10gdfp97xh79bgz327d273bs46b3f0"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-ordered-float-2.10.1
  (crate-source "ordered-float" "2.10.1"
                "075i108hr95pr7hy4fgxivib5pky3b6b22rywya5qyd2wmkrvwb8"))

(define rust-os-pipe-1.2.1
  (crate-source "os_pipe" "1.2.1"
                "10nrh0i507560rsiy4c79fajdmqgbr6dha2pbl9mncrlaq52pzaz"))

(define rust-outref-0.1.0
  (crate-source "outref" "0.1.0"
                "1x61h7dl1cc6cj2f3zsalr8d98v0cw6497sykwxf74wjmqljh8kz"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"
                #:snippet '(delete-file "logo.png")))

(define rust-palette-0.7.6
  (crate-source "palette" "0.7.6"
                "1rmn02mv6cb112504qyg7pyfa83c08hxpk5sw7jc5v659hc73gsc"))

(define rust-palette-derive-0.7.6
  (crate-source "palette_derive" "0.7.6"
                "0c0xhpk1nqyq4jr2m8xnka7w47vqzc7m2vq9ih8wxyjv02phs0zm"))

(define rust-parcel-selectors-0.28.2
  (crate-source "parcel_selectors" "0.28.2"
                "15m1hvl6rj8k0nkb28dlvgkkkmiviaklyhdpq4z6pjr6mpqh7zal"))

(define rust-parcel-sourcemap-2.1.1
  (crate-source "parcel_sourcemap" "2.1.1"
                "1fsvw1mlqc5x4psj90jxrdbivq8sqvxi5zz3q2vv4s4047bp8ns8"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.3
  (crate-source "parking_lot" "0.12.3"
                "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))

(define rust-parking-lot-0.12.4
  (crate-source "parking_lot" "0.12.4"
                "04sab1c7304jg8k0d5b2pxbj1fvgzcf69l3n2mfpkdb96vs8pmbh"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-parking-lot-core-0.9.11
  (crate-source "parking_lot_core" "0.9.11"
                "19g4d6m5k4ggacinqprnn8xvdaszc3y5smsmbz1adcdmaqm8v0xw"))

(define rust-parse-js-0.20.1
  (crate-source "parse-js" "0.20.1"
                "03kijcsvms2aqkxc8yf0gy387n6zlw0y9yfrgr230nfb5pivahi7"))

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
                "05q9wqjvfrs4dvw03yn3bvcs4zghz0a7ycfa53pz2k2fqhp6k843"
                #:snippet '(delete-file-recursively "tests")))

(define rust-pear-0.2.9
  (crate-source "pear" "0.2.9"
                "0rxlyizzaqq6lswgyfdxjxd3dyb1jfml9gwfpbx5g1j8rq0amvmx"))

(define rust-pear-codegen-0.2.9
  (crate-source "pear_codegen" "0.2.9"
                "0izijffdd2xs762497mk0xr7xwmyw62dzdqjz12v70n0bnc5pasb"))

(define rust-pem-rfc7468-0.7.0
  (crate-source "pem-rfc7468" "0.7.0"
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"
                #:snippet '(delete-file-recursively "tests")))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-petgraph-0.6.5
  (crate-source "petgraph" "0.6.5"
                "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"
                #:snippet '(for-each delete-file-recursively '("assets"))))

(define rust-petgraph-0.7.1
  (crate-source "petgraph" "0.7.1"
                "0wkpppwrfv1h197asz1p4yfb4li5b1kw0nqllil67n6vj1qb6win"
                #:snippet '(delete-file-recursively "assets")))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-codegen-0.11.3
  (crate-source "phf_codegen" "0.11.3"
                "0si1n6zr93kzjs3wah04ikw8z6npsr39jw4dam8yi9czg2609y5f"))

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

(define rust-pkcs1-0.7.5
  (crate-source "pkcs1" "0.7.5"
                "0zz4mil3nchnxljdfs2k5ab1cjqn7kq5lqp62n9qfix01zqvkzy8"
                #:snippet '(delete-file-recursively "tests")))

(define rust-pkcs8-0.10.2
  (crate-source "pkcs8" "0.10.2"
                "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r"
                #:snippet '(delete-file-recursively "tests")))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-png-0.17.16
  (crate-source "png" "0.17.16"
                "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2"))

(define rust-png-0.18.0
  (crate-source "png" "0.18.0"
                "187jf0m873qn5biix8z7gjdsyf8r6vj3yr495pa0jja6i39wxflp"))

(define rust-poly1305-0.8.0
  (crate-source "poly1305" "0.8.0"
                "1grs77skh7d8vi61ji44i8gpzs3r9x7vay50i6cg8baxfa8bsnc1"
                #:snippet '(delete-file-recursively "src/fuzz")))

(define rust-portable-atomic-1.11.0
  (crate-source "portable-atomic" "1.11.0"
                "0glb2wngflvfmg789qbf6dbnwcf6ai212fs7n0lf1c66rd49n3im"))

(define rust-portable-atomic-1.11.1
  (crate-source "portable-atomic" "1.11.1"
                "10s4cx9y3jvw0idip09ar52s2kymq8rq9a668f793shn1ar6fhpq"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-postmark-0.10.2
  (crate-source "postmark" "0.10.2"
                "10vd1xdlk189p8qphmihm9j28wdn5fclcgwc6z65fs43i4irihd8"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-precomputed-hash-0.1.1
  (crate-source "precomputed-hash" "0.1.1"
                "075k9bfy39jhs53cb2fpb9klfakx2glxnf28zdw08ws6lgpq6lwj"))

(define rust-prefix-trie-0.6.0
  (crate-source "prefix-trie" "0.6.0"
                "1ahnp9y884pk2vrw8adk3lnxijk7xpyxia4v4fyqcjdsjl4r6pzb"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"
                #:snippet '(delete-file-recursively "examples")))

(define rust-prettyplease-0.2.32
  (crate-source "prettyplease" "0.2.32"
                "1xmdmwhsvqc8l5ns029vzjida4k3lp5ynin0xra43qsiki0wakk6"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.94
  (crate-source "proc-macro2" "1.0.94"
                "114wxb56gdj9vy44q0ll3l2x9niqzcbyqikydmlb5f3h5rsp26d3"))

(define rust-proc-macro2-diagnostics-0.10.1
  (crate-source "proc-macro2-diagnostics" "0.10.1"
                "1j48ipc80pykvhx6yhndfa774s58ax1h6sm6mlhf09ls76f6l1mg"))

(define rust-procfs-0.17.0
  (crate-source "procfs" "0.17.0"
                "17swyjqinpb745f07dpdi7c8q37hxvhx9xmmsi2dhxaj2kc74nyc"))

(define rust-procfs-core-0.17.0
  (crate-source "procfs-core" "0.17.0"
                "1v0jdbyc1rq1x22m0wn7n4iq4h86gdls38wqfg06zc29hcnz1793"))

(define rust-prometheus-0.14.0
  (crate-source "prometheus" "0.14.0"
                "0fpl98whrg5j4bpb3qfswii4yfa58zws7rl7rnd0m58bimnk599w"))

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

(define rust-protobuf-3.7.2
  (crate-source "protobuf" "3.7.2"
                "1x4riz4znnjsqpdxnhxj0aq8rfivmbv4hfqmd3gbbn77v96isnnn"))

(define rust-protobuf-support-3.7.2
  (crate-source "protobuf-support" "3.7.2"
                "1mnpn2q96bxm2vidh86m5p2x5z0z8rgfyixk1wlgjiqa3vrw4diy"))

(define rust-protox-0.7.2
  (crate-source "protox" "0.7.2"
                "0jmmcil88n15kdpac51fz9qjagchpy3pq3vjrj77nqxz67rjldbg"))

(define rust-protox-parse-0.7.0
  (crate-source "protox-parse" "0.7.0"
                "1pld0s1cg9favgy9bafkwlvmg65ky13rmhh0w050hb262p8n5953"))

(define rust-ptr-meta-0.1.4
  (crate-source "ptr_meta" "0.1.4"
                "1wd4wy0wxrcays4f1gy8gwcmxg7mskmivcv40p0hidh6xbvwqf07"))

(define rust-ptr-meta-derive-0.1.4
  (crate-source "ptr_meta_derive" "0.1.4"
                "1b69cav9wn67cixshizii0q5mlbl0lihx706vcrzm259zkdlbf0n"))

(define rust-pxfm-0.1.22
  (crate-source "pxfm" "0.1.22"
                "0w147kvqc2sdap5gz2hi87aridpnz2dskz0apygknkdcg4sp6vrp"))

(define rust-qrcode-0.14.1
  (crate-source "qrcode" "0.14.1"
                "1v693x68yg90wfpas5v4bf6cfmnq9dq54qfgd3kb33j07r3851yn"))

(define rust-quanta-0.11.1
  (crate-source "quanta" "0.11.1"
                "1axrw0nqc90bq671w05jd9460pmwg86c4r132mjsi4c2g8m6czm1"))

(define rust-quick-xml-0.37.4
  (crate-source "quick-xml" "0.37.4"
                "0s8krrf4ci10kcxfzdja7h7dz5kcp1mgndhgf0wghkrjvs48rkm4"))

(define rust-quinn-0.11.7
  (crate-source "quinn" "0.11.7"
                "04ihd2jibw0carrx081pwdkh8n0l03n9zjvxi21yyylnyak1bgf3"))

(define rust-quinn-proto-0.11.10
  (crate-source "quinn-proto" "0.11.10"
                "1k12m8y3k8dszv9ysb3hxp92cnhva6f670w176img6ywni77885q"))

(define rust-quinn-udp-0.5.11
  (crate-source "quinn-udp" "0.5.11"
                "1daa7c1hws33395zzw62i879dxr168fp8llaff87lx7cqrbhy7al"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-r-efi-5.2.0
  (crate-source "r-efi" "5.2.0"
                "1ig93jvpqyi87nc5kb6dri49p56q7r7qxrn8kfizmqkfj5nmyxkl"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.0
  (crate-source "rand" "0.9.0"
                "156dyvsfa6fjnv6nx5vzczay1scy5183dvjchd7bvs47xd5bjy9p"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.3
  (crate-source "rand_core" "0.9.3"
                "0f3xhf16yks5ic6kmgxcpv1ngdhp48mmfy4ag82i1wnwh8ws3ncr"))

(define rust-rand-pcg-0.9.0
  (crate-source "rand_pcg" "0.9.0"
                "1xr04g5zrzqi9n7vyzjznnyrmy55i8k34ripsb2gmdxazzvw72ml"))

(define rust-rand-regex-0.18.1
  (crate-source "rand_regex" "0.18.1"
                "0dyzikkp5bjfp8sv06ylbkf7kfyd0vir03j088g9lrqk5lw2xnq4"))

(define rust-rand-seeder-0.4.0
  (crate-source "rand_seeder" "0.4.0"
                "07l4fsxizhh3fxpvkvpapwamlfvqacnvp58c7valb5iwzkyjfaah"))

(define rust-ratatui-0.27.0
  (crate-source "ratatui" "0.27.0"
                "1lv8r99miibk6np2d2m0y6vf62jf5dr1x272ws6byalnnp2lcrfi"))

(define rust-raw-cpuid-10.7.0
  (crate-source "raw-cpuid" "10.7.0"
                "0ckkg47m8wbdinqg4z4dx7ipi3d7fjxdnrwzikx70x46rdwpcabc"))

(define rust-rayon-1.10.0
  (crate-source "rayon" "1.10.0"
                "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-core-1.12.1
  (crate-source "rayon-core" "1.12.1"
                "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-syscall-0.5.11
  (crate-source "redox_syscall" "0.5.11"
                "18qijn18r10haiglv4261wb0yh1agqqlvs0nxfy8yjbpsb307wfj"))

(define rust-redox-syscall-0.5.17
  (crate-source "redox_syscall" "0.5.17"
                "0xrvpchkaxph3r5ww2i04v9nwg3843fp3prf8kqlh1gv01b4c1sl"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-ref-cast-1.0.24
  (crate-source "ref-cast" "1.0.24"
                "1kx57g118vs9sqi6d2dcxy6vp8jbx8n5hilmv1sacip9vc8y82ja"))

(define rust-ref-cast-impl-1.0.24
  (crate-source "ref-cast-impl" "1.0.24"
                "1ir7dm7hpqqdgg60hlspsc1ck6wli7wa3xcqrsxz7wdz45f24r8i"))

(define rust-regalloc2-0.12.2
  (crate-source "regalloc2" "0.12.2"
                "0d4pllvcg7m2l3j8rmbfhf63pdi6jf5gimg6r25ry572gn1v25jj"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"
                #:snippet '(delete-file-recursively "tests")))

(define rust-regex-1.11.2
  (crate-source "regex" "1.11.2"
                "04k9rzxd11hcahpyihlswy6f1zqw7lspirv4imm4h0lcdl8gvmr3"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"
                #:snippet '(delete-file-recursively "tests")))

(define rust-regex-automata-0.2.0
  (crate-source "regex-automata" "0.2.0"
                "10ipg4bqygdi6ivfxkbv4a6kggwsdzhlkxrsdwr09f59ymiqfdp9"))

(define rust-regex-automata-0.4.10
  (crate-source "regex-automata" "0.4.10"
                "1mllcfmgjcl6d52d5k09lwwq9wj5mwxccix4bhmw5spy1gx5i53b"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"
                #:snippet '(delete-file-recursively "tests")))

(define rust-regex-lite-0.1.6
  (crate-source "regex-lite" "0.1.6"
                "0almvx3z75f611pdcd9mslh7zxg76zh3shifql4ndch6mn3rb92k"
                #:snippet '(delete-file-recursively "tests")))

(define rust-regex-lite-0.1.7
  (crate-source "regex-lite" "0.1.7"
                "0c5s0kyc4gch0l0rzhm54prgfs169l2zwfvnzn91rvv33hr42gwl"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-region-3.0.2
  (crate-source "region" "3.0.2"
                "19wrf7fg741jfnyz2314dv9m9hwssh816v27rpwsw2f07g8ypdp6"))

(define rust-rend-0.4.2
  (crate-source "rend" "0.4.2"
                "0z4rrkycva0lcw0hxq479h4amxj9syn5vq4vb2qid5v2ylj3izki"))

(define rust-reqwest-0.11.27
  (crate-source "reqwest" "0.11.27"
                "0qjary4hpplpgdi62d2m0xvbn6lnzckwffm0rgkm2x51023m6ryx"))

(define rust-reqwest-0.12.15
  (crate-source "reqwest" "0.12.15"
                "1fvvrl3jmsnlm99ldl0ariklrlsmrky06qabp7dc92ylznk4d76i"
                #:snippet '(delete-file-recursively "tests")))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-rkyv-0.7.45
  (crate-source "rkyv" "0.7.45"
                "16vp6m4sq41smhvym8ijy4id1hr3vm4na7wy4bc63qdrhmiws24h"))

(define rust-rkyv-derive-0.7.45
  (crate-source "rkyv_derive" "0.7.45"
                "1h1jwmyivx7g88d41gzcjrqnax98m9algjd49hx0laqab4kisgah"))

(define rust-rmp-0.8.14
  (crate-source "rmp" "0.8.14"
                "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2"))

(define rust-roto-0.6.0
  (crate-source "roto" "0.6.0"
                "1mq934vqiyw02xwk8psgmfzihr92p9vgrcs03mx3446wry65pv8v"))

(define rust-roto-macros-0.6.0
  (crate-source "roto-macros" "0.6.0"
                "1aijjc4yndb4lb6678s8dfy1ypy074f5wvmyk2w1qxgk40fvia39"))

(define rust-rpassword-7.3.1
  (crate-source "rpassword" "7.3.1"
                "0gvy3lcpph9vv1rl0cjfn72ylvmgbw2vklmj6w0iv4cpr3ijniw0"))

(define rust-rsa-0.9.8
  (crate-source "rsa" "0.9.8"
                "06v9zl604jsqjajm647l9jjirn7k4lc8lmvys6hmqshpxp0qm4kq"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rtoolbox-0.0.2
  (crate-source "rtoolbox" "0.0.2"
                "03n9z8x353kylxhr9im8zawcisnmid3jiqrs8rbdn313cd7d4iy2"))

(define rust-runtime-format-0.1.3
  (crate-source "runtime-format" "0.1.3"
                "154c7jq7kbpc5acn2ysa2ilab2x0i5y7d34jwznni9xw71dqv589"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-demangle-0.1.26
  (crate-source "rustc-demangle" "0.1.26"
                "1kja3nb0yhlm4j2p1hl8d7sjmn2g9fa1s4pj0qma5kj2lcndkxsn"))

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

(define rust-rustix-1.0.5
  (crate-source "rustix" "1.0.5"
                "1gsqrw9cp762ps9dl1d13n8mk5r0b6r2s002l1njxfylilwify6r"))

(define rust-rustix-1.0.8
  (crate-source "rustix" "1.0.8"
                "1j6ajqi61agdnh1avr4bplrsgydjw1n4mycdxw3v8g94pyx1y60i"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"
                #:snippet
                '(begin
                   (delete-file-recursively "src/testdata")
                   (for-each delete-file (find-files "." "\\.bin")))))

(define rust-rustls-0.23.25
  (crate-source "rustls" "0.23.25"
                "0g5idwxm04i71k3n66ml30zyfbgv6p85a7jky2i09v64i8cfjbl2"))

(define rust-rustls-native-certs-0.6.3
  (crate-source "rustls-native-certs" "0.6.3"
                "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustls-pemfile-2.2.0
  (crate-source "rustls-pemfile" "2.2.0"
                "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustls-pki-types-1.11.0
  (crate-source "rustls-pki-types" "1.11.0"
                "0755isc0x5iymm3wsn59s0ad1pm9zidw7p34qfqlsjsac9jf4z4i"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustls-webpki-0.103.1
  (crate-source "rustls-webpki" "0.103.1"
                "00rcdz0rb9ia2ivrq7412ry9qkvbh78pra2phl4p7kxck9vbiy7y"
                #:snippet '(delete-file-recursively "tests")))

(define rust-rustversion-1.0.20
  (crate-source "rustversion" "1.0.20"
                "1lhwjb16dsm8brd18bn2bh0ryzc7qi29bi2jjsc6ny2zbwn3ivgd"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

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

(define rust-sanitize-filename-0.6.0
  (crate-source "sanitize-filename" "0.6.0"
                "0kd37m2nd364vj09f330y853fmnw2yyy7hsmnxxnlwzbki7lz65w"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"
                #:snippet '(delete-file-recursively "test")))

(define rust-schemars-0.9.0
  (crate-source "schemars" "0.9.0"
                "0pqncln5hqbzbl2r3yayyr4a82jjf93h2cfxrn0xamvx77wr3lac"))

(define rust-schemars-1.0.4
  (crate-source "schemars" "1.0.4"
                "1l7w773jfk6mz0v8wpahp60aslksjijlbm65ysi4y5mwj520rll2"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"
                #:snippet '(delete-file-recursively "src/testdata")))

(define rust-sd-notify-0.4.5
  (crate-source "sd-notify" "0.4.5"
                "1x1bmz30x2i35j771rqyyan40473aqk0xjrh2dk9xdnqf7gylhxr"))

(define rust-seahash-4.1.0
  (crate-source "seahash" "4.1.0"
                "0sxsb64np6bvnppjz5hg4rqpnkczhsl8w8kf2a5lr1c08xppn40w"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-sys-2.14.0
  (crate-source "security-framework-sys" "2.14.0"
                "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9"))

(define rust-self-cell-1.2.0
  (crate-source "self_cell" "1.2.0"
                "0jg70srf4hzrw96x8iclgf6i8dfgm1x8ds2i7yzcgq0i8njraz8g"))

(define rust-semver-1.0.26
  (crate-source "semver" "1.0.26"
                "1l5q2vb8fjkby657kdyfpvv40x2i2xqq9bg57pxqakfj92fgmrjn"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-json-1.0.143
  (crate-source "serde_json" "1.0.143"
                "0njabwzldvj13ykrf1aaf4gh5cgl25kf9hzbpafbv3qh3ppsn0fl"))

(define rust-serde-path-to-error-0.1.17
  (crate-source "serde_path_to_error" "0.1.17"
                "0alb447z25dvczd6ll3vfjbf51pypn23mgs5hv8978vzjczv3yjr"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-serde-value-0.7.0
  (crate-source "serde-value" "0.7.0"
                "0b18ngk7n4f9zmwsfdkhgsp31192smzyl5z143qmx1qi28sa78gk"))

(define rust-serde-with-3.12.0
  (crate-source "serde_with" "3.12.0"
                "1ai9c3cbdgrsvmlc4qpg9z73y80yplk3k7zp45wp97xnzkrggdnn"))

(define rust-serde-with-3.14.0
  (crate-source "serde_with" "3.14.0"
                "1manlm83865xwlvgv8frc472x19b75pd89a54mpxpagg3zb5ri7j"))

(define rust-serde-with-macros-3.12.0
  (crate-source "serde_with_macros" "3.12.0"
                "13hznly0qq1rngsdh8gpnajab2knkrmvwwrbmii86g1s36jwl04d"))

(define rust-serde-with-macros-3.14.0
  (crate-source "serde_with_macros" "3.14.0"
                "03xk9ghj2s6n331r565mgh22w0749vnq50094nd0vkk5cmg9946y"))

(define rust-serde-yml-0.0.12
  (crate-source "serde_yml" "0.0.12"
                "1p8xwz4znd6fj962y22fdvvv16gb8c0hx4iv5hjplngiidcdvqjr"))

(define rust-sfv-0.13.0
  (crate-source "sfv" "0.13.0"
                "1a77jnz0r1d8bdjgslnbbby0fw6fbhmyi9fzgjmyaxq8qdnjknd8"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"
                #:snippet '(delete-file-recursively "tests")))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"
                #:snippet '(delete-file-recursively "tests")))

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

(define rust-signal-hook-registry-1.4.6
  (crate-source "signal-hook-registry" "1.4.6"
                "12y2v1ms5z111fymaw1v8k93m5chnkp21h0jknrydkj8zydp395j"))

(define rust-signature-2.2.0
  (crate-source "signature" "2.2.0"
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))

(define rust-simd-abstraction-0.7.1
  (crate-source "simd-abstraction" "0.7.1"
                "11v9hy8qg0b4qypz2p75ijv41ln1rssk6qilz0gwbbfaayfb5bcw"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simdutf8-0.1.5
  (crate-source "simdutf8" "0.1.5"
                "0vmpf7xaa0dnaikib5jlx6y4dxd3hxqz6l830qb079g7wcsgxag3"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-sketches-ddsketch-0.2.2
  (crate-source "sketches-ddsketch" "0.2.2"
                "0p6n1v0p0773d0b5qnsnw526g7hhlb08bx95wm0zb09xnwa6qqw5"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-smallvec-1.15.0
  (crate-source "smallvec" "1.15.0"
                "1sgfw8z729nlxk8k13dhs0a762wnaxmlx70a7xlf3wz989bjh5w9"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-socket2-0.5.10
  (crate-source "socket2" "0.5.10"
                "0y067ki5q946w91xlz2sb175pnfazizva6fi3kfp639mxnmpc8z2"))

(define rust-socket2-0.5.9
  (crate-source "socket2" "0.5.9"
                "1vzds1wwwi0a51fn10r98j7cx3ir4shvhykpbk7md2h5h1ydapsg"))

(define rust-socket2-0.6.0
  (crate-source "socket2" "0.6.0"
                "01qqdzfnr0bvdwq6wl56c9c4m2cvbxn43dfpcv8gjx208sph8d93"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-spki-0.7.3
  (crate-source "spki" "0.7.3"
                "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"
                #:snippet '(delete-file-recursively "tests")))

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

(define rust-symbol-table-0.4.0
  (crate-source "symbol_table" "0.4.0"
                "1g036zl39iqjqy0rw2xc84b3vwqf9k872g2fs62fd0mikzbgz6zi"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.100
  (crate-source "syn" "2.0.100"
                "18623wdkns03blpv65xsjn8fipl9p9hj98vlrnhin7nqran496mh"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-sync-wrapper-0.1.2
  (crate-source "sync_wrapper" "0.1.2"
                "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-sysinfo-0.30.13
  (crate-source "sysinfo" "0.30.13"
                "1csbkx1hdlacgzw5ynjyfvgc1xg58w3h1rgh5gm2pysmxvd4snqa"))

(define rust-system-configuration-0.5.1
  (crate-source "system-configuration" "0.5.1"
                "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms"))

(define rust-system-configuration-sys-0.5.0
  (crate-source "system-configuration-sys" "0.5.0"
                "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-target-lexicon-0.13.2
  (crate-source "target-lexicon" "0.13.2"
                "16m6smfz533im9dyxfhnzmpi4af75g2iii36ylc4gfmqvf6gf0p5"))

(define rust-tempfile-3.19.1
  (crate-source "tempfile" "3.19.1"
                "1grmcj8y6rcavndw2dm18ndzdimsq5f8lcrwyg627cdrcdvsqdvl"))

(define rust-tempfile-3.21.0
  (crate-source "tempfile" "3.21.0"
                "07kx58ibjk3ydq1gcb7q637fs5zkxaa550lxckhgg9p3427izdhm"))

(define rust-terminal-size-0.4.2
  (crate-source "terminal_size" "0.4.2"
                "1vdm5xhzn7sqcsr762vmnavkhid3hs8w8qjyh9iwrr1990f4iij5"))

(define rust-terminal-size-0.4.3
  (crate-source "terminal_size" "0.4.3"
                "1l7cicmz49c0cyskfp5a389rsai649xi7y032v73475ikjbwpf30"))

(define rust-testing-logger-0.1.1
  (crate-source "testing_logger" "0.1.1"
                "087pi7y9iisspafyzblj41qvrw95dfb6px7pavlkmls5rckvg4kd"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-tiff-0.9.1
  (crate-source "tiff" "0.9.1"
                "0ghyxlz566dzc3scvgmzys11dhq2ri77kb8sznjakijlxby104xs"
                #:snippet '(delete-file-recursively "tests")))

(define rust-tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
  ;; TODO: Check bundled sources.
  (crate-source "tikv-jemalloc-sys"
                "0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7"
                "0baf5vjpg9ipa388md4yxim77rdblnk8r95mnp1akbqjcj860g6d"))

(define rust-tikv-jemallocator-0.6.0
  (crate-source "tikv-jemallocator" "0.6.0"
                "0r985npb7d9hrbs3mb0bkfbv0nvzjpgvzsbpyj21bn0qhpqmzv2c"))

(define rust-time-0.3.41
  (crate-source "time" "0.3.41"
                "0h0cpiyya8cjlrh00d2r72bmgg4lsdcncs76qpwy0rn2kghijxla"))

(define rust-time-0.3.43
  (crate-source "time" "0.3.43"
                "0c90pxn59zccwdyvh8pn9ql04c32ky9kqqli7mc2vrqhxkqydgc3"))

(define rust-time-core-0.1.4
  (crate-source "time-core" "0.1.4"
                "0z5h9fknvdvbs2k2s1chpi3ab3jvgkfhdnqwrvixjngm263s7sf9"))

(define rust-time-core-0.1.6
  (crate-source "time-core" "0.1.6"
                "0sqwhg7n47gbffyr0zhipqcnskxgcgzz1ix8wirqs2rg3my8x1j0"))

(define rust-time-macros-0.2.22
  (crate-source "time-macros" "0.2.22"
                "0jcaxpw220han2bzbrdlpqhy1s5k9i8ri3lw6n5zv4zcja9p69im"))

(define rust-time-macros-0.2.24
  (crate-source "time-macros" "0.2.24"
                "1wzb6hnl35856f58cx259q7ijc4c7yis0qsnydvw5n8jbw9b1krh"))

(define rust-tiny-bip39-1.0.0
  (crate-source "tiny-bip39" "1.0.0"
                "0q98iv3wgbd41wyxxd5is8sddi53k9ary45rbi5fi8dmb39r9k32"))

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-1.9.0
  (crate-source "tinyvec" "1.9.0"
                "0w9w8qcifns9lzvlbfwa01y0skhr542anwa3rpn28rg82wgndcq9"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.44.2
  (crate-source "tokio" "1.44.2"
                "0j4w3qvlcqzgbxlnap0czvspqj6x461vyk1sbqcf97g4rci8if76"))

(define rust-tokio-1.47.1
  (crate-source "tokio" "1.47.1"
                "0f2hp5v3payg6x04ijj67si1wsdhksskhmjs2k9p5f7bmpyrmr49"))

(define rust-tokio-listener-0.5.1
  (crate-source "tokio-listener" "0.5.1"
                "1mnkj4gyhx3c0bk5q81pj11j2gn620y27lf0ljf88cgqsfkf2l0l"))

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

(define rust-tokio-util-0.7.14
  (crate-source "tokio-util" "0.7.14"
                "0d7hm1jrwpzryvni72fy5dg9blqs776wq5w38lwigk3g7swr15bb"
                #:snippet '(delete-file "src/sync/tests/mod.rs")))

(define rust-tokio-util-0.7.16
  (crate-source "tokio-util" "0.7.16"
                "1r9wdrg1k5hna3m0kc8kcb8jdb6n52g7vnw93kw2xxw4cyc7qc0l"))

(define rust-toml-0.5.11
  (crate-source "toml" "0.5.11"
                "0d2266nx8b3n22c7k24x4428z6di8n83a9n466jm7a2hipfz1xzl"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

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

(define rust-tower-http-0.6.6
  (crate-source "tower-http" "0.6.6"
                "1wh51y4rf03f91c6rvli6nwzsarx7097yx6sqlm75ag27pbjzj5d"))

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

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-serde-0.2.0
  (crate-source "tracing-serde" "0.2.0"
                "1wbgzi364vzfswfkvy48a3p0z5xmv98sx342r57sil70ggmiljvh"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-tracing-subscriber-0.3.20
  (crate-source "tracing-subscriber" "0.3.20"
                "1m9447bxq7236avgl6n5yb2aqwplrghm61dgipw03mh7ad7s2m10"))

(define rust-tracing-tree-0.4.0
  (crate-source "tracing-tree" "0.4.0"
                "175lqyfp6zq7jbj8m026xdp8p765pzgfdzfxahfggmdhy5wwlngl"))

;; TODO: Unbundle tracy, environment variables TRACY_CLIENT_LIB and
;; TRACY_CLIENT_LIB_PATH might be helpful.

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

(define rust-typeid-1.0.3
  (crate-source "typeid" "1.0.3"
                "0727ypay2p6mlw72gz3yxkqayzdmjckw46sxqpaj08v0b0r64zdw"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-uncased-0.9.10
  (crate-source "uncased" "0.9.10"
                "15q6r6g4fszr8c2lzg9z9k9g52h8g29h24awda3d72cyw37qzf71"))

(define rust-unicode-bidi-0.3.18
  (crate-source "unicode-bidi" "0.3.18"
                "1xcxwbsqa24b8vfchhzyyzgj0l6bn51ib5v8j6krha0m77dva72w"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"
                #:snippet '(delete-file-recursively "tests")))

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
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"
                #:snippet '(delete-file-recursively "mk")))

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

(define rust-uuid-1.16.0
  (crate-source "uuid" "1.16.0"
                "1a9dkv6jm4lz7ip9l9i1mcx7sh389xjsr03l6jgwqjpmkdvpm3s5"))

(define rust-uuid-1.18.1
  (crate-source "uuid" "1.18.1"
                "18kh01qmfayn4psap52x8xdjkzw2q8bcbpnhhxjs05dr22mbi1rg"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"
                #:snippet '(delete-file-recursively "test-data")))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-vlq-0.5.1
  (crate-source "vlq" "0.5.1"
                "1zygijgl47gasi0zx34ak1jq2n4qmk0cx2zpn13shba157npxpb5"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasi-0.14.3+wasi-0.2.4
  (crate-source "wasi" "0.14.3+wasi-0.2.4"
                "158c0cy561zlncw71hjh1pficw60p1nj7ki8kqm2gpbv0f1swlba"))

(define rust-wasite-0.1.0
  (crate-source "wasite" "0.1.0"
                "0nw5h9nmcl4fyf4j5d4mfdjfgvwi1cakpi349wc4zrr59wxxinmq"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-0.2.101
  (crate-source "wasm-bindgen" "0.2.101"
                "0fv0yrfx170gf7i4dds4c69dxh8axp247wyip2dm4nylmmf9253y"))

(define rust-wasm-bindgen-backend-0.2.100
  (crate-source "wasm-bindgen-backend" "0.2.100"
                "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig"))

(define rust-wasm-bindgen-backend-0.2.101
  (crate-source "wasm-bindgen-backend" "0.2.101"
                "1fwkzc2z701g2rm2jq4m20a0lkc6qqq5r3a407yj6yfahalip3g2"))

(define rust-wasm-bindgen-futures-0.4.50
  (crate-source "wasm-bindgen-futures" "0.4.50"
                "0q8ymi6i9r3vxly551dhxcyai7nc491mspj0j1wbafxwq074fpam"))

(define rust-wasm-bindgen-macro-0.2.100
  (crate-source "wasm-bindgen-macro" "0.2.100"
                "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz"))

(define rust-wasm-bindgen-macro-0.2.101
  (crate-source "wasm-bindgen-macro" "0.2.101"
                "038vxk2yg11c3qv9iyasqcm70dw8sr2xmyaxqjq7bxzgwcx4cgbw"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-macro-support-0.2.101
  (crate-source "wasm-bindgen-macro-support" "0.2.101"
                "1ajjqmdbi7ybdpw41avskjfdqnxpc9v547gmr8izj4c2n24wxd3v"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-wasm-bindgen-shared-0.2.101
  (crate-source "wasm-bindgen-shared" "0.2.101"
                "1h94nvm5p8zyr3718x4zhdz7rcmd0rir0b46a1ljqx8k7d58ahzi"))

(define rust-wasmtime-jit-icache-coherence-33.0.2
  (crate-source "wasmtime-jit-icache-coherence" "33.0.2"
                "068ny6wdailqvpiv9glrkqa4fnfsfxpr46phng04aah6rd0fkw3s"))

(define rust-wayland-backend-0.3.8
  (crate-source "wayland-backend" "0.3.8"
                "1gs7dw6s3lp9g6g0rhk4bh66wl41jnbkd27c6ynhv1x3xac8j85p"))

(define rust-wayland-client-0.31.8
  (crate-source "wayland-client" "0.31.8"
                "0gzpr9gdd8yk1crflxngg5iwa1szyyzp4i4zbgpslf1nsgihs4n2"))

;; TODO: Use our packaged protocols.
(define rust-wayland-protocols-0.32.6
  (crate-source "wayland-protocols" "0.32.6"
                "1z0yahh48x8qzdbcallmxn5am5897hkk5d7p51ly6dwvhr3cz087"))

(define rust-wayland-protocols-wlr-0.3.6
  (crate-source "wayland-protocols-wlr" "0.3.6"
                "1cpqb0d4ryf87x2wgca5n71wilhvc0jjva0zasbdgalmypk052i4"))

(define rust-wayland-scanner-0.31.6
  (crate-source "wayland-scanner" "0.31.6"
                "110ldnyfxjqvjssir1jf3ndlci7xy9lpv4aqg775y518bpyxlvw9"))

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
                "1jf54brni9lk4ak5pkma2pn18hli22gr7i7wp9zn2lzayy8v4412"
                #:snippet '(delete-file-recursively "tests")))

(define rust-weezl-0.1.8
  (crate-source "weezl" "0.1.8"
                "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k"))

(define rust-which-7.0.3
  (crate-source "which" "7.0.3"
                "0qj7lx7v98hcs0rfw4xqw1ssn47v6h7hhak0ai4bbrfk7z747mi4"))

(define rust-whoami-1.6.0
  (crate-source "whoami" "1.6.0"
                "19q2vm5ax3bgwffbywn4ad62anc1f4l1ky61h0y2qjdb30qx3539"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"
                #:snippet '(delete-file-recursively "lib")))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-0.52.0
  (crate-source "windows" "0.52.0"
                "1gnh210qjlprpd1szaq04rjm1zqgdm9j7l9absg0kawi2rwm72p4"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-core-0.61.0
  (crate-source "windows-core" "0.61.0"
                "104915nsby2cgp322pqqkmj2r82v5sg4hil0hxddg1hc67gc2qs7"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-implement-0.60.0
  (crate-source "windows-implement" "0.60.0"
                "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-link-0.1.1
  (crate-source "windows-link" "0.1.1"
                "0f2cq7imbrppsmmnz8899hfhg07cp5gq6rh0bjhb1qb6nwshk13n"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-registry-0.4.0
  (crate-source "windows-registry" "0.4.0"
                "18wbgr6z6765qdnasi8mmvxhvp82xd1zlvd6s7pp2l5lvn8av1j2"))

(define rust-windows-result-0.3.2
  (crate-source "windows-result" "0.3.2"
                "0li2f76anf0rg7i966d9qs5iprsg555g9rgyzj7gcpfr9wdd2ky6"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-strings-0.3.1
  (crate-source "windows-strings" "0.3.1"
                "06bkhkyclbfchcsv5bnhz77r290k20m15glj2xq60ra0bp64iyl7"))

(define rust-windows-strings-0.4.0
  (crate-source "windows-strings" "0.4.0"
                "15rg6a0ha1d231wwps2qlgyqrgkyj1r8v9vsb8nlbvih4ijajavs"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-sys-0.48.0
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.0
  (crate-source "windows-targets" "0.53.0"
                "12yakpjizhfpppz1i3zgcwxlbar8axrp9j87fmywpydarvlcgr5i"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"
                #:snippet '(delete-file-recursively "lib")))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winreg-0.50.0
  (crate-source "winreg" "0.50.0"
                "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-wit-bindgen-0.45.0
  (crate-source "wit-bindgen" "0.45.0"
                "053q28k1hn9qgm0l05gr9751d8q34zcz6lbzviwxiqxs3n1q68h5"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"
                #:snippet '(for-each delete-file (find-files "." "\\.(a|o)$"))))

(define rust-wl-clipboard-rs-0.9.2
  (crate-source "wl-clipboard-rs" "0.9.2"
                "1sxsaspzix3xiq6wi1l1g55acgi04sv6r7gxz94zar80wv8ghpwf"))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define rust-x11rb-0.13.1
  (crate-source "x11rb" "0.13.1"
                "04jyfm0xmc538v09pzsyr2w801yadsgvyl2p0p76hzzffg5gz4ax"))

(define rust-x11rb-protocol-0.13.1
  (crate-source "x11rb-protocol" "0.13.1"
                "0gfbxf2k7kbk577j3rjhfx7hm70kmwln6da7xyc4l2za0d2pq47c"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"
                #:snippet '(delete-file-recursively ".github")))

(define rust-yoke-0.7.5
  (crate-source "yoke" "0.7.5"
                "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj"))

(define rust-yoke-derive-0.7.5
  (crate-source "yoke-derive" "0.7.5"
                "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-0.8.24
  (crate-source "zerocopy" "0.8.24"
                "0yb8hyzfnwzr2wg4p7cnqmjps8fsw8xqnprafgpmfs8qisigx1i5"))

(define rust-zerocopy-0.8.26
  (crate-source "zerocopy" "0.8.26"
                "0bvsj0qzq26zc6nlrm3z10ihvjspyngs7n0jw1fz031i7h6xsf8h"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define rust-zerocopy-derive-0.8.24
  (crate-source "zerocopy-derive" "0.8.24"
                "1gk9047pbq1yjj2jyiv0s37nqc53maqbmhcsjp6lhi2w7kvai5m9"))

(define rust-zerocopy-derive-0.8.26
  (crate-source "zerocopy-derive" "0.8.26"
                "10aiywi5qkha0mpsnb1zjwi44wl2rhdncaf3ykbp4i9nqm65pkwy"))

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

(define rust-zerotrie-0.1.3
  (crate-source "zerotrie" "0.1.3"
                "07qa5ljss8j706iy0rd023naamwly4jfwz0pc1gmqcw7bpalsngv"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (atuin =>
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
                                  rust-arboard-3.5.0
                                  rust-arc-swap-1.7.1
                                  rust-argon2-0.5.3
                                  rust-async-stream-0.3.6
                                  rust-async-stream-impl-0.3.6
                                  rust-async-trait-0.1.88
                                  rust-atoi-2.0.0
                                  rust-atomic-waker-1.1.2
                                  rust-autocfg-1.4.0
                                  rust-axum-0.7.9
                                  rust-axum-core-0.4.5
                                  rust-axum-server-0.7.2
                                  rust-backtrace-0.3.74
                                  rust-base64-0.21.7
                                  rust-base64-0.22.1
                                  rust-base64ct-1.7.3
                                  rust-beef-0.5.2
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.9.0
                                  rust-blake2-0.10.6
                                  rust-block-buffer-0.10.4
                                  rust-bumpalo-3.17.0
                                  rust-by-address-1.2.1
                                  rust-bytemuck-1.22.0
                                  rust-byteorder-1.5.0
                                  rust-byteorder-lite-0.1.0
                                  rust-bytes-1.10.1
                                  rust-cassowary-0.3.0
                                  rust-castaway-0.2.3
                                  rust-cc-1.2.18
                                  rust-cfg-if-1.0.0
                                  rust-cfg-aliases-0.2.1
                                  rust-chacha20-0.9.1
                                  rust-chrono-0.4.40
                                  rust-cipher-0.4.4
                                  rust-clap-4.5.35
                                  rust-clap-builder-4.5.35
                                  rust-clap-complete-4.5.47
                                  rust-clap-complete-nushell-4.5.5
                                  rust-clap-derive-4.5.32
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
                                  rust-darling-0.20.11
                                  rust-darling-core-0.20.11
                                  rust-darling-macro-0.20.11
                                  rust-dashmap-5.5.3
                                  rust-der-0.7.9
                                  rust-deranged-0.4.0
                                  rust-diff-0.1.13
                                  rust-digest-0.10.7
                                  rust-directories-5.0.1
                                  rust-dirs-5.0.1
                                  rust-dirs-sys-0.4.1
                                  rust-displaydoc-0.2.5
                                  rust-divan-0.1.18
                                  rust-divan-macros-0.1.18
                                  rust-dotenvy-0.15.7
                                  rust-downcast-rs-1.2.1
                                  rust-ed25519-2.2.3
                                  rust-ed25519-dalek-2.1.1
                                  rust-either-1.15.0
                                  rust-encode-unicode-1.0.0
                                  rust-encoding-rs-0.8.35
                                  rust-env-filter-0.1.3
                                  rust-env-logger-0.11.8
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.11
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
                                  rust-flate2-1.1.1
                                  rust-flume-0.11.1
                                  rust-fnv-1.0.7
                                  rust-foldhash-0.1.5
                                  rust-form-urlencoded-1.2.1
                                  rust-fs-err-2.11.0
                                  rust-fs-err-3.1.0
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
                                  rust-getrandom-0.3.2
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
                                  rust-http-1.3.1
                                  rust-http-body-0.4.6
                                  rust-http-body-1.0.1
                                  rust-http-body-util-0.1.3
                                  rust-httparse-1.10.1
                                  rust-httpdate-1.0.3
                                  rust-humantime-2.2.0
                                  rust-hyper-0.14.32
                                  rust-hyper-1.6.0
                                  rust-hyper-rustls-0.24.2
                                  rust-hyper-rustls-0.27.5
                                  rust-hyper-timeout-0.5.2
                                  rust-hyper-util-0.1.11
                                  rust-iana-time-zone-0.1.63
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-icu-collections-1.5.0
                                  rust-icu-locid-1.5.0
                                  rust-icu-locid-transform-1.5.0
                                  rust-icu-locid-transform-data-1.5.1
                                  rust-icu-normalizer-1.5.0
                                  rust-icu-normalizer-data-1.5.1
                                  rust-icu-properties-1.5.1
                                  rust-icu-properties-data-1.5.1
                                  rust-icu-provider-1.5.0
                                  rust-icu-provider-macros-1.5.0
                                  rust-ident-case-1.0.1
                                  rust-idna-1.0.3
                                  rust-idna-adapter-1.2.0
                                  rust-image-0.25.6
                                  rust-indenter-0.3.3
                                  rust-indexmap-1.9.3
                                  rust-indexmap-2.9.0
                                  rust-indicatif-0.17.11
                                  rust-inout-0.1.4
                                  rust-interim-0.1.2
                                  rust-ipnet-2.11.0
                                  rust-is-terminal-polyfill-1.70.1
                                  rust-iso8601-0.6.2
                                  rust-itertools-0.13.0
                                  rust-itertools-0.14.0
                                  rust-itoa-1.0.15
                                  rust-jiff-0.2.5
                                  rust-jiff-static-0.2.5
                                  rust-jpeg-decoder-0.3.1
                                  rust-js-sys-0.3.77
                                  rust-lazy-static-1.5.0
                                  rust-libc-0.2.171
                                  rust-libm-0.2.11
                                  rust-libredox-0.1.3
                                  rust-libsqlite3-sys-0.30.1
                                  rust-linux-raw-sys-0.4.15
                                  rust-linux-raw-sys-0.9.3
                                  rust-listenfd-1.0.2
                                  rust-litemap-0.7.5
                                  rust-lock-api-0.4.12
                                  rust-log-0.4.27
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
                                  rust-miniz-oxide-0.8.7
                                  rust-minspan-0.1.2
                                  rust-mio-0.8.11
                                  rust-mio-1.0.3
                                  rust-multimap-0.10.0
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
                                  rust-objc2-0.6.0
                                  rust-objc2-app-kit-0.3.0
                                  rust-objc2-core-foundation-0.3.0
                                  rust-objc2-core-graphics-0.3.0
                                  rust-objc2-encode-4.1.0
                                  rust-objc2-foundation-0.3.0
                                  rust-objc2-io-surface-0.3.0
                                  rust-object-0.36.7
                                  rust-once-cell-1.21.3
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
                                  rust-portable-atomic-util-0.2.4
                                  rust-postmark-0.10.2
                                  rust-powerfmt-0.2.0
                                  rust-ppv-lite86-0.2.21
                                  rust-pretty-assertions-1.4.1
                                  rust-prettyplease-0.2.32
                                  rust-proc-macro2-1.0.94
                                  rust-prost-0.13.5
                                  rust-prost-build-0.13.5
                                  rust-prost-derive-0.13.5
                                  rust-prost-reflect-0.14.7
                                  rust-prost-types-0.13.5
                                  rust-protox-0.7.2
                                  rust-protox-parse-0.7.0
                                  rust-quanta-0.11.1
                                  rust-quick-xml-0.37.4
                                  rust-quinn-0.11.7
                                  rust-quinn-proto-0.11.10
                                  rust-quinn-udp-0.5.11
                                  rust-quote-1.0.40
                                  rust-r-efi-5.2.0
                                  rust-rand-0.8.5
                                  rust-rand-0.9.0
                                  rust-rand-chacha-0.3.1
                                  rust-rand-chacha-0.9.0
                                  rust-rand-core-0.6.4
                                  rust-rand-core-0.9.3
                                  rust-ratatui-0.27.0
                                  rust-raw-cpuid-10.7.0
                                  rust-rayon-1.10.0
                                  rust-rayon-core-1.12.1
                                  rust-redox-syscall-0.5.11
                                  rust-redox-users-0.4.6
                                  rust-regex-1.11.1
                                  rust-regex-automata-0.1.10
                                  rust-regex-automata-0.4.9
                                  rust-regex-lite-0.1.6
                                  rust-regex-syntax-0.6.29
                                  rust-regex-syntax-0.8.5
                                  rust-reqwest-0.11.27
                                  rust-reqwest-0.12.15
                                  rust-ring-0.17.14
                                  rust-rmp-0.8.14
                                  rust-rpassword-7.3.1
                                  rust-rsa-0.9.8
                                  rust-rtoolbox-0.0.2
                                  rust-runtime-format-0.1.3
                                  rust-rustc-demangle-0.1.24
                                  rust-rustc-hash-1.1.0
                                  rust-rustc-hash-2.1.1
                                  rust-rustc-version-0.4.1
                                  rust-rustix-0.38.44
                                  rust-rustix-1.0.5
                                  rust-rustls-0.21.12
                                  rust-rustls-0.23.25
                                  rust-rustls-native-certs-0.6.3
                                  rust-rustls-pemfile-1.0.4
                                  rust-rustls-pemfile-2.2.0
                                  rust-rustls-pki-types-1.11.0
                                  rust-rustls-webpki-0.101.7
                                  rust-rustls-webpki-0.103.1
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
                                  rust-serde-1.0.219
                                  rust-serde-derive-1.0.219
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
                                  rust-smallvec-1.15.0
                                  rust-socket2-0.5.9
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
                                  rust-syn-2.0.100
                                  rust-sync-wrapper-0.1.2
                                  rust-sync-wrapper-1.0.2
                                  rust-synstructure-0.13.1
                                  rust-sysinfo-0.30.13
                                  rust-system-configuration-0.5.1
                                  rust-system-configuration-sys-0.5.0
                                  rust-tempfile-3.19.1
                                  rust-terminal-size-0.4.2
                                  rust-testing-logger-0.1.1
                                  rust-thiserror-1.0.69
                                  rust-thiserror-2.0.12
                                  rust-thiserror-impl-1.0.69
                                  rust-thiserror-impl-2.0.12
                                  rust-thread-local-1.1.8
                                  rust-tiff-0.9.1
                                  rust-time-0.3.41
                                  rust-time-core-0.1.4
                                  rust-time-macros-0.2.22
                                  rust-tiny-bip39-1.0.0
                                  rust-tinystr-0.7.6
                                  rust-tinyvec-1.9.0
                                  rust-tinyvec-macros-0.1.1
                                  rust-tokio-1.44.2
                                  rust-tokio-macros-2.5.0
                                  rust-tokio-rustls-0.24.1
                                  rust-tokio-rustls-0.26.2
                                  rust-tokio-stream-0.1.17
                                  rust-tokio-util-0.7.14
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
                                  rust-uuid-1.16.0
                                  rust-valuable-0.1.1
                                  rust-vcpkg-0.2.15
                                  rust-version-check-0.9.5
                                  rust-want-0.3.1
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-wasi-0.14.2+wasi-0.2.4
                                  rust-wasite-0.1.0
                                  rust-wasm-bindgen-0.2.100
                                  rust-wasm-bindgen-backend-0.2.100
                                  rust-wasm-bindgen-futures-0.4.50
                                  rust-wasm-bindgen-macro-0.2.100
                                  rust-wasm-bindgen-macro-support-0.2.100
                                  rust-wasm-bindgen-shared-0.2.100
                                  rust-wayland-backend-0.3.8
                                  rust-wayland-client-0.31.8
                                  rust-wayland-protocols-0.32.6
                                  rust-wayland-protocols-wlr-0.3.6
                                  rust-wayland-scanner-0.31.6
                                  rust-wayland-sys-0.31.6
                                  rust-web-sys-0.3.77
                                  rust-web-time-1.1.0
                                  rust-webpki-roots-0.26.8
                                  rust-weezl-0.1.8
                                  rust-whoami-1.6.0
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-0.52.0
                                  rust-windows-core-0.52.0
                                  rust-windows-core-0.61.0
                                  rust-windows-implement-0.60.0
                                  rust-windows-interface-0.59.1
                                  rust-windows-link-0.1.1
                                  rust-windows-registry-0.4.0
                                  rust-windows-result-0.3.2
                                  rust-windows-strings-0.3.1
                                  rust-windows-strings-0.4.0
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.52.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.6
                                  rust-windows-targets-0.53.0
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-gnullvm-0.53.0
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-aarch64-msvc-0.53.0
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnu-0.53.0
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-gnullvm-0.53.0
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-i686-msvc-0.53.0
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnu-0.53.0
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-gnullvm-0.53.0
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-windows-x86-64-msvc-0.53.0
                                  rust-winreg-0.50.0
                                  rust-wit-bindgen-rt-0.39.0
                                  rust-wl-clipboard-rs-0.9.2
                                  rust-write16-1.0.0
                                  rust-writeable-0.5.5
                                  rust-x11rb-0.13.1
                                  rust-x11rb-protocol-0.13.1
                                  rust-yansi-1.0.1
                                  rust-yoke-0.7.5
                                  rust-yoke-derive-0.7.5
                                  rust-zerocopy-0.7.35
                                  rust-zerocopy-0.8.24
                                  rust-zerocopy-derive-0.7.35
                                  rust-zerocopy-derive-0.8.24
                                  rust-zerofrom-0.1.6
                                  rust-zerofrom-derive-0.1.6
                                  rust-zeroize-1.8.1
                                  rust-zeroize-derive-1.4.2
                                  rust-zerovec-0.10.4
                                  rust-zerovec-derive-0.10.3))
                     (iocaine =>
                              (list rust-addr2line-0.24.2
                               rust-adler2-2.0.1
                               rust-ahash-0.7.8
                               rust-ahash-0.8.12
                               rust-aho-corasick-0.7.20
                               rust-aho-corasick-1.1.3
                               rust-allocator-api2-0.2.21
                               rust-android-tzdata-0.1.1
                               rust-android-system-properties-0.1.5
                               rust-anstream-0.6.20
                               rust-anstyle-1.0.11
                               rust-anstyle-parse-0.2.7
                               rust-anstyle-query-1.1.4
                               rust-anstyle-wincon-3.0.10
                               rust-anyhow-1.0.99
                               rust-arbitrary-1.4.2
                               rust-ariadne-0.5.1
                               rust-async-stream-0.3.6
                               rust-async-stream-impl-0.3.6
                               rust-async-trait-0.1.89
                               rust-atomic-0.6.1
                               rust-atomic-waker-1.1.2
                               rust-autocfg-1.5.0
                               rust-axum-0.7.9
                               rust-axum-0.8.4
                               rust-axum-core-0.4.5
                               rust-axum-core-0.5.2
                               rust-backtrace-0.3.75
                               rust-base64-0.21.7
                               rust-base64-0.22.1
                               rust-base64-simd-0.7.0
                               rust-bitflags-1.3.2
                               rust-bitflags-2.9.4
                               rust-bitvec-1.0.1
                               rust-bstr-1.12.0
                               rust-bumpalo-3.19.0
                               rust-bytecheck-0.6.12
                               rust-bytecheck-derive-0.6.12
                               rust-bytemuck-1.23.2
                               rust-byteorder-1.5.0
                               rust-byteorder-lite-0.1.0
                               rust-bytes-1.10.1
                               rust-calendrical-calculations-0.1.3
                               rust-capstone-0.13.0
                               rust-capstone-sys-0.17.0
                               rust-cc-1.2.36
                               rust-cfg-if-1.0.3
                               rust-chrono-0.4.41
                               rust-clap-4.5.47
                               rust-clap-builder-4.5.47
                               rust-clap-derive-4.5.47
                               rust-clap-lex-0.7.5
                               rust-colorchoice-1.0.4
                               rust-condtype-1.3.0
                               rust-console-api-0.8.1
                               rust-console-subscriber-0.4.1
                               rust-const-str-0.3.2
                               rust-const-str-proc-macro-0.3.2
                               rust-convert-case-0.6.0
                               rust-cookie-0.18.1
                               rust-core-foundation-sys-0.8.7
                               rust-core-maths-0.1.1
                               rust-cranelift-0.120.2
                               rust-cranelift-assembler-x64-0.120.2
                               rust-cranelift-assembler-x64-meta-0.120.2
                               rust-cranelift-bforest-0.120.2
                               rust-cranelift-bitset-0.120.2
                               rust-cranelift-codegen-0.120.2
                               rust-cranelift-codegen-meta-0.120.2
                               rust-cranelift-codegen-shared-0.120.2
                               rust-cranelift-control-0.120.2
                               rust-cranelift-entity-0.120.2
                               rust-cranelift-frontend-0.120.2
                               rust-cranelift-isle-0.120.2
                               rust-cranelift-jit-0.120.2
                               rust-cranelift-module-0.120.2
                               rust-cranelift-native-0.120.2
                               rust-cranelift-srcgen-0.120.2
                               rust-crc32fast-1.5.0
                               rust-crossbeam-channel-0.5.15
                               rust-crossbeam-deque-0.8.6
                               rust-crossbeam-epoch-0.9.18
                               rust-crossbeam-utils-0.8.21
                               rust-cssparser-0.33.0
                               rust-cssparser-color-0.1.0
                               rust-cssparser-macros-0.6.1
                               rust-curl-0.4.49
                               rust-curl-sys-0.4.83+curl-8.15.0
                               rust-darling-0.20.11
                               rust-darling-core-0.20.11
                               rust-darling-macro-0.20.11
                               rust-dashmap-5.5.3
                               rust-data-encoding-2.9.0
                               rust-data-url-0.1.1
                               rust-deranged-0.5.3
                               rust-displaydoc-0.2.5
                               rust-divan-0.1.21
                               rust-divan-macros-0.1.21
                               rust-document-features-0.2.11
                               rust-dtoa-1.0.10
                               rust-dtoa-short-0.3.5
                               rust-dyn-clone-1.0.20
                               rust-either-1.15.0
                               rust-env-filter-0.1.3
                               rust-env-home-0.1.0
                               rust-env-logger-0.11.8
                               rust-equivalent-1.0.2
                               rust-erased-serde-0.4.6
                               rust-errno-0.3.13
                               rust-fallible-iterator-0.3.0
                               rust-fastrand-2.3.0
                               rust-fdeflate-0.3.7
                               rust-figment-0.10.19
                               rust-figment-file-provider-adapter-0.1.1
                               rust-find-msvc-tools-0.1.1
                               rust-fixed-decimal-0.5.6
                               rust-flate2-1.1.2
                               rust-fnv-1.0.7
                               rust-foldhash-0.1.5
                               rust-form-urlencoded-1.2.2
                               rust-funty-2.0.0
                               rust-futures-channel-0.3.31
                               rust-futures-core-0.3.31
                               rust-futures-macro-0.3.31
                               rust-futures-sink-0.3.31
                               rust-futures-task-0.3.31
                               rust-futures-util-0.3.31
                               rust-getrandom-0.2.16
                               rust-getrandom-0.3.3
                               rust-gimli-0.31.1
                               rust-h2-0.4.12
                               rust-hashbrown-0.12.3
                               rust-hashbrown-0.13.2
                               rust-hashbrown-0.14.5
                               rust-hashbrown-0.15.5
                               rust-hdrhistogram-7.5.4
                               rust-heck-0.5.0
                               rust-hex-0.4.3
                               rust-http-1.3.1
                               rust-http-body-1.0.1
                               rust-http-body-util-0.1.3
                               rust-httparse-1.10.1
                               rust-httpdate-1.0.3
                               rust-humantime-2.2.0
                               rust-hyper-1.7.0
                               rust-hyper-timeout-0.5.2
                               rust-hyper-util-0.1.16
                               rust-iana-time-zone-0.1.63
                               rust-iana-time-zone-haiku-0.1.2
                               rust-icu-1.5.0
                               rust-icu-calendar-1.5.2
                               rust-icu-calendar-data-1.5.1
                               rust-icu-casemap-1.5.1
                               rust-icu-casemap-data-1.5.1
                               rust-icu-collator-1.5.0
                               rust-icu-collator-data-1.5.1
                               rust-icu-collections-1.5.0
                               rust-icu-datetime-1.5.1
                               rust-icu-datetime-data-1.5.1
                               rust-icu-decimal-1.5.0
                               rust-icu-decimal-data-1.5.1
                               rust-icu-experimental-0.1.0
                               rust-icu-experimental-data-0.1.1
                               rust-icu-list-1.5.0
                               rust-icu-list-data-1.5.1
                               rust-icu-locid-1.5.0
                               rust-icu-locid-transform-1.5.0
                               rust-icu-locid-transform-data-1.5.1
                               rust-icu-normalizer-1.5.0
                               rust-icu-normalizer-data-1.5.1
                               rust-icu-pattern-0.2.0
                               rust-icu-plurals-1.5.0
                               rust-icu-plurals-data-1.5.1
                               rust-icu-properties-1.5.1
                               rust-icu-properties-data-1.5.1
                               rust-icu-provider-1.5.0
                               rust-icu-provider-macros-1.5.0
                               rust-icu-segmenter-1.5.0
                               rust-icu-segmenter-data-1.5.1
                               rust-icu-timezone-1.5.0
                               rust-icu-timezone-data-1.5.1
                               rust-ident-case-1.0.1
                               rust-image-0.25.8
                               rust-indexmap-1.9.3
                               rust-indexmap-2.11.0
                               rust-inetnum-0.1.1
                               rust-inlinable-string-0.1.15
                               rust-io-uring-0.7.10
                               rust-ipnet-2.11.0
                               rust-ipnet-trie-0.3.0
                               rust-is-terminal-polyfill-1.70.1
                               rust-itertools-0.10.5
                               rust-itertools-0.12.1
                               rust-itertools-0.14.0
                               rust-itoa-1.0.15
                               rust-jiff-0.2.15
                               rust-jiff-static-0.2.15
                               rust-js-sys-0.3.78
                               rust-lazy-static-1.5.0
                               rust-libc-0.2.175
                               rust-libm-0.2.15
                               rust-libyml-0.0.5
                               rust-libz-sys-1.1.22
                               rust-lightningcss-1.0.0-alpha.67
                               rust-lightningcss-derive-1.0.0-alpha.43
                               rust-linux-raw-sys-0.4.15
                               rust-linux-raw-sys-0.9.4
                               rust-litemap-0.7.5
                               rust-litrs-0.4.2
                               rust-lock-api-0.4.13
                               rust-log-0.4.28
                               rust-lua-src-547.0.0
                               rust-luajit-src-210.5.12+a4f56a4
                               rust-mach2-0.4.3
                               rust-matchers-0.2.0
                               rust-matches-0.1.10
                               rust-matchit-0.7.3
                               rust-matchit-0.8.4
                               rust-memchr-2.7.5
                               rust-memo-map-0.3.3
                               rust-mime-0.3.17
                               rust-minify-html-0.16.4
                               rust-minify-html-common-0.0.2
                               rust-minify-js-0.6.0
                               rust-minijinja-2.12.0
                               rust-minijinja-embed-2.12.0
                               rust-minimal-lexical-0.2.1
                               rust-miniz-oxide-0.8.9
                               rust-mio-1.0.4
                               rust-mlua-0.10.5
                               rust-mlua-sys-0.6.8
                               rust-moxcms-0.7.5
                               rust-nix-0.26.4
                               rust-nom-7.1.3
                               rust-nu-ansi-term-0.50.1
                               rust-num-bigint-0.4.6
                               rust-num-conv-0.1.0
                               rust-num-integer-0.1.46
                               rust-num-rational-0.4.2
                               rust-num-traits-0.2.19
                               rust-object-0.36.7
                               rust-once-cell-1.21.3
                               rust-once-cell-polyfill-1.70.1
                               rust-ordered-float-2.10.1
                               rust-outref-0.1.0
                               rust-parcel-selectors-0.28.2
                               rust-parcel-sourcemap-2.1.1
                               rust-parking-lot-0.12.4
                               rust-parking-lot-core-0.9.11
                               rust-parse-js-0.20.1
                               rust-paste-1.0.15
                               rust-pathdiff-0.2.3
                               rust-pear-0.2.9
                               rust-pear-codegen-0.2.9
                               rust-percent-encoding-2.3.2
                               rust-phf-0.11.3
                               rust-phf-codegen-0.11.3
                               rust-phf-generator-0.11.3
                               rust-phf-macros-0.11.3
                               rust-phf-shared-0.11.3
                               rust-pin-project-1.1.10
                               rust-pin-project-internal-1.1.10
                               rust-pin-project-lite-0.2.16
                               rust-pin-utils-0.1.0
                               rust-pkg-config-0.3.32
                               rust-png-0.18.0
                               rust-portable-atomic-1.11.1
                               rust-portable-atomic-util-0.2.4
                               rust-powerfmt-0.2.0
                               rust-ppv-lite86-0.2.21
                               rust-precomputed-hash-0.1.1
                               rust-prefix-trie-0.6.0
                               rust-proc-macro2-1.0.101
                               rust-proc-macro2-diagnostics-0.10.1
                               rust-procfs-0.17.0
                               rust-procfs-core-0.17.0
                               rust-prometheus-0.14.0
                               rust-prost-0.13.5
                               rust-prost-derive-0.13.5
                               rust-prost-types-0.13.5
                               rust-protobuf-3.7.2
                               rust-protobuf-support-3.7.2
                               rust-ptr-meta-0.1.4
                               rust-ptr-meta-derive-0.1.4
                               rust-pxfm-0.1.22
                               rust-qrcode-0.14.1
                               rust-quote-1.0.40
                               rust-r-efi-5.3.0
                               rust-radium-0.7.0
                               rust-rand-0.8.5
                               rust-rand-0.9.2
                               rust-rand-chacha-0.3.1
                               rust-rand-core-0.6.4
                               rust-rand-core-0.9.3
                               rust-rand-pcg-0.9.0
                               rust-rand-regex-0.18.1
                               rust-rand-seeder-0.4.0
                               rust-rayon-1.11.0
                               rust-rayon-core-1.13.0
                               rust-redox-syscall-0.5.17
                               rust-ref-cast-1.0.24
                               rust-ref-cast-impl-1.0.24
                               rust-regalloc2-0.12.2
                               rust-regex-1.11.2
                               rust-regex-automata-0.2.0
                               rust-regex-automata-0.4.10
                               rust-regex-lite-0.1.7
                               rust-regex-syntax-0.8.6
                               rust-region-3.0.2
                               rust-rend-0.4.2
                               rust-rkyv-0.7.45
                               rust-rkyv-derive-0.7.45
                               rust-roto-0.6.0
                               rust-roto-macros-0.6.0
                               rust-rustc-demangle-0.1.26
                               rust-rustc-hash-1.1.0
                               rust-rustc-hash-2.1.1
                               rust-rustix-0.38.44
                               rust-rustix-1.0.8
                               rust-rustversion-1.0.22
                               rust-ryu-1.0.20
                               rust-sanitize-filename-0.6.0
                               rust-schannel-0.1.27
                               rust-schemars-0.9.0
                               rust-schemars-1.0.4
                               rust-scopeguard-1.2.0
                               rust-sd-notify-0.4.5
                               rust-seahash-4.1.0
                               rust-self-cell-1.2.0
                               rust-semver-1.0.26
                               rust-serde-1.0.219
                               rust-serde-value-0.7.0
                               rust-serde-derive-1.0.219
                               rust-serde-json-1.0.143
                               rust-serde-path-to-error-0.1.17
                               rust-serde-regex-1.1.0
                               rust-serde-spanned-0.6.9
                               rust-serde-urlencoded-0.7.1
                               rust-serde-with-3.14.0
                               rust-serde-with-macros-3.14.0
                               rust-serde-yml-0.0.12
                               rust-sfv-0.13.0
                               rust-sharded-slab-0.1.7
                               rust-shlex-1.3.0
                               rust-signal-hook-registry-1.4.6
                               rust-simd-abstraction-0.7.1
                               rust-simd-adler32-0.3.7
                               rust-simdutf8-0.1.5
                               rust-siphasher-1.0.1
                               rust-slab-0.4.11
                               rust-smallvec-1.15.1
                               rust-socket2-0.5.10
                               rust-socket2-0.6.0
                               rust-stable-deref-trait-1.2.0
                               rust-strsim-0.11.1
                               rust-symbol-table-0.4.0
                               rust-syn-1.0.109
                               rust-syn-2.0.106
                               rust-sync-wrapper-1.0.2
                               rust-synstructure-0.13.2
                               rust-tap-1.0.1
                               rust-target-lexicon-0.13.2
                               rust-tempfile-3.21.0
                               rust-terminal-size-0.4.3
                               rust-thiserror-1.0.69
                               rust-thiserror-2.0.16
                               rust-thiserror-impl-1.0.69
                               rust-thiserror-impl-2.0.16
                               rust-thread-local-1.1.9
                               rust-tikv-jemalloc-sys-0.6.0+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
                               rust-tikv-jemallocator-0.6.0
                               rust-time-0.3.43
                               rust-time-core-0.1.6
                               rust-time-macros-0.2.24
                               rust-tinystr-0.7.6
                               rust-tinyvec-1.10.0
                               rust-tinyvec-macros-0.1.1
                               rust-tokio-1.47.1
                               rust-tokio-listener-0.5.1
                               rust-tokio-macros-2.5.0
                               rust-tokio-stream-0.1.17
                               rust-tokio-util-0.7.16
                               rust-toml-0.8.23
                               rust-toml-datetime-0.6.11
                               rust-toml-edit-0.22.27
                               rust-toml-write-0.1.2
                               rust-tonic-0.12.3
                               rust-tower-0.4.13
                               rust-tower-0.5.2
                               rust-tower-http-0.6.6
                               rust-tower-layer-0.3.3
                               rust-tower-service-0.3.3
                               rust-tracing-0.1.41
                               rust-tracing-attributes-0.1.30
                               rust-tracing-core-0.1.34
                               rust-tracing-log-0.2.0
                               rust-tracing-serde-0.2.0
                               rust-tracing-subscriber-0.3.20
                               rust-try-lock-0.2.5
                               rust-typeid-1.0.3
                               rust-uncased-0.9.10
                               rust-unicode-ident-1.0.18
                               rust-unicode-segmentation-1.12.0
                               rust-unicode-width-0.1.14
                               rust-utf16-iter-1.0.5
                               rust-utf8-iter-1.0.4
                               rust-utf8parse-0.2.2
                               rust-uuid-1.18.1
                               rust-valuable-0.1.1
                               rust-vcpkg-0.2.15
                               rust-version-check-0.9.5
                               rust-vlq-0.5.1
                               rust-want-0.3.1
                               rust-wasi-0.11.1+wasi-snapshot-preview1
                               rust-wasi-0.14.3+wasi-0.2.4
                               rust-wasm-bindgen-0.2.101
                               rust-wasm-bindgen-backend-0.2.101
                               rust-wasm-bindgen-macro-0.2.101
                               rust-wasm-bindgen-macro-support-0.2.101
                               rust-wasm-bindgen-shared-0.2.101
                               rust-wasmtime-jit-icache-coherence-33.0.2
                               rust-which-7.0.3
                               rust-windows-core-0.61.2
                               rust-windows-implement-0.60.0
                               rust-windows-interface-0.59.1
                               rust-windows-link-0.1.3
                               rust-windows-result-0.3.4
                               rust-windows-strings-0.4.2
                               rust-windows-sys-0.52.0
                               rust-windows-sys-0.59.0
                               rust-windows-sys-0.60.2
                               rust-windows-targets-0.52.6
                               rust-windows-targets-0.53.3
                               rust-windows-aarch64-gnullvm-0.52.6
                               rust-windows-aarch64-gnullvm-0.53.0
                               rust-windows-aarch64-msvc-0.52.6
                               rust-windows-aarch64-msvc-0.53.0
                               rust-windows-i686-gnu-0.52.6
                               rust-windows-i686-gnu-0.53.0
                               rust-windows-i686-gnullvm-0.52.6
                               rust-windows-i686-gnullvm-0.53.0
                               rust-windows-i686-msvc-0.52.6
                               rust-windows-i686-msvc-0.53.0
                               rust-windows-x86-64-gnu-0.52.6
                               rust-windows-x86-64-gnu-0.53.0
                               rust-windows-x86-64-gnullvm-0.52.6
                               rust-windows-x86-64-gnullvm-0.53.0
                               rust-windows-x86-64-msvc-0.52.6
                               rust-windows-x86-64-msvc-0.53.0
                               rust-winnow-0.7.13
                               rust-winsafe-0.0.19
                               rust-wit-bindgen-0.45.0
                               rust-write16-1.0.0
                               rust-writeable-0.5.5
                               rust-wyz-0.5.1
                               rust-yansi-1.0.1
                               rust-yoke-0.7.5
                               rust-yoke-derive-0.7.5
                               rust-zerocopy-0.8.26
                               rust-zerocopy-derive-0.8.26
                               rust-zerofrom-0.1.6
                               rust-zerofrom-derive-0.1.6
                               rust-zerotrie-0.1.3
                               rust-zerovec-0.10.4
                               rust-zerovec-derive-0.10.3)))
