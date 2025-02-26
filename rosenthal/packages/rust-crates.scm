;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (rosenthal packages))

;;;
;;; *-cargo-inputs and crates definitions
;;;
;;; This file is managed by ‘guix import’.  DO NOT add definitions manually.
;;;
(define-public atuin-cargo-inputs
  (delay (list rust-addr2line-0.24.2
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
               rust-anyhow-1.0.96
               rust-approx-0.5.1
               rust-arboard-3.4.1
               rust-arc-swap-1.7.1
               rust-argon2-0.5.3
               rust-async-stream-0.3.6
               rust-async-stream-impl-0.3.6
               rust-async-trait-0.1.86
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
               rust-bitflags-2.8.0
               rust-blake2-0.10.6
               rust-block-buffer-0.10.4
               rust-block2-0.5.1
               rust-bumpalo-3.17.0
               rust-by-address-1.2.1
               rust-bytemuck-1.21.0
               rust-byteorder-1.5.0
               rust-byteorder-lite-0.1.0
               rust-bytes-1.10.0
               rust-cassowary-0.3.0
               rust-castaway-0.2.3
               rust-cc-1.2.15
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
               rust-console-0.15.10
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
               rust-httparse-1.10.0
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
               rust-itoa-1.0.14
               rust-jpeg-decoder-0.3.1
               rust-js-sys-0.3.77
               rust-lazy-static-1.5.0
               rust-libc-0.2.170
               rust-libm-0.2.11
               rust-libredox-0.1.3
               rust-libsqlite3-sys-0.30.1
               rust-linux-raw-sys-0.4.15
               rust-listenfd-1.0.2
               rust-litemap-0.7.4
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
               rust-pin-project-1.1.9
               rust-pin-project-internal-1.1.9
               rust-pin-project-lite-0.2.16
               rust-pin-utils-0.1.0
               rust-pkcs1-0.7.5
               rust-pkcs8-0.10.2
               rust-pkg-config-0.3.31
               rust-png-0.17.16
               rust-poly1305-0.8.0
               rust-portable-atomic-1.11.0
               rust-postmark-0.10.2
               rust-powerfmt-0.2.0
               rust-ppv-lite86-0.2.20
               rust-pretty-assertions-1.4.1
               rust-prettyplease-0.2.29
               rust-proc-macro2-1.0.93
               rust-prost-0.13.5
               rust-prost-build-0.13.5
               rust-prost-derive-0.13.5
               rust-prost-reflect-0.14.6
               rust-prost-types-0.13.5
               rust-protox-0.7.2
               rust-protox-parse-0.7.0
               rust-quanta-0.11.1
               rust-quick-xml-0.37.2
               rust-quinn-0.11.6
               rust-quinn-proto-0.11.9
               rust-quinn-udp-0.5.10
               rust-quote-1.0.38
               rust-rand-0.8.5
               rust-rand-chacha-0.3.1
               rust-rand-core-0.6.4
               rust-ratatui-0.27.0
               rust-raw-cpuid-10.7.0
               rust-rayon-1.10.0
               rust-rayon-core-1.12.1
               rust-redox-syscall-0.5.9
               rust-redox-users-0.4.6
               rust-regex-1.11.1
               rust-regex-automata-0.1.10
               rust-regex-automata-0.4.9
               rust-regex-lite-0.1.6
               rust-regex-syntax-0.6.29
               rust-regex-syntax-0.8.5
               rust-reqwest-0.11.27
               rust-reqwest-0.12.12
               rust-ring-0.17.11
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
               rust-rustversion-1.0.19
               rust-rusty-paserk-0.4.0
               rust-rusty-paseto-0.7.2
               rust-ryu-1.0.19
               rust-salsa20-0.10.2
               rust-schannel-0.1.27
               rust-scopeguard-1.2.0
               rust-sct-0.7.1
               rust-security-framework-2.11.1
               rust-security-framework-sys-2.14.0
               rust-semver-1.0.25
               rust-serde-1.0.218
               rust-serde-derive-1.0.218
               rust-serde-json-1.0.139
               rust-serde-path-to-error-0.1.16
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
               rust-syn-2.0.98
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
               rust-thiserror-2.0.11
               rust-thiserror-impl-1.0.69
               rust-thiserror-impl-2.0.11
               rust-thread-local-1.1.8
               rust-tiff-0.9.1
               rust-time-0.3.37
               rust-time-core-0.1.2
               rust-time-macros-0.2.19
               rust-tiny-bip39-1.0.0
               rust-tinystr-0.7.6
               rust-tinyvec-1.8.1
               rust-tinyvec-macros-0.1.1
               rust-tokio-1.43.0
               rust-tokio-macros-2.5.0
               rust-tokio-rustls-0.24.1
               rust-tokio-rustls-0.26.1
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
               rust-unicode-ident-1.0.17
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
               rust-uuid-1.15.0
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
               rust-zerofrom-0.1.5
               rust-zerofrom-derive-0.1.5
               rust-zeroize-1.8.1
               rust-zeroize-derive-1.4.2
               rust-zerovec-0.10.4
               rust-zerovec-derive-0.10.3)))

(define-public niri-cargo-inputs
  (delay (list rust-adler2-2.0.0
               rust-ahash-0.8.11
               rust-aho-corasick-1.1.3
               rust-allocator-api2-0.2.21
               rust-android-activity-0.6.0
               rust-android-properties-0.2.2
               rust-annotate-snippets-0.9.2
               rust-anstream-0.6.18
               rust-anstyle-1.0.10
               rust-anstyle-parse-0.2.6
               rust-anstyle-query-1.1.2
               rust-anstyle-wincon-3.0.7
               rust-anyhow-1.0.96
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
               rust-async-trait-0.1.86
               rust-atomic-0.6.0
               rust-atomic-waker-1.1.2
               rust-autocfg-1.4.0
               rust-base64-0.21.7
               rust-bindgen-0.69.5
               rust-bit-set-0.8.0
               rust-bit-vec-0.8.0
               rust-bitflags-1.3.2
               rust-bitflags-2.8.0
               rust-block2-0.5.1
               rust-blocking-1.6.1
               rust-bumpalo-3.17.0
               rust-bytemuck-1.21.0
               rust-bytemuck-derive-1.8.1
               rust-byteorder-1.5.0
               rust-bytes-1.10.0
               rust-cairo-rs-0.20.7
               rust-cairo-sys-rs-0.20.7
               rust-calloop-0.13.0
               rust-calloop-0.14.2
               rust-calloop-wayland-source-0.3.0
               rust-calloop-wayland-source-0.4.0
               rust-cc-1.2.15
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
               rust-console-0.15.10
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
               rust-dyn-clone-1.0.18
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
               rust-insta-1.42.1
               rust-io-lifetimes-1.0.11
               rust-is-terminal-0.4.15
               rust-is-ci-1.2.0
               rust-is-terminal-polyfill-1.70.1
               rust-itertools-0.12.1
               rust-itoa-1.0.14
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
               rust-libspa-0.8.0
               rust-libspa-sys-0.8.0
               rust-libudev-sys-0.1.4
               rust-linked-hash-map-0.5.6
               rust-linux-raw-sys-0.4.15
               rust-linux-raw-sys-0.6.5
               rust-litemap-0.7.4
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
               rust-nix-0.27.1
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
               rust-pin-project-1.1.9
               rust-pin-project-internal-1.1.9
               rust-pin-project-lite-0.2.16
               rust-pin-utils-0.1.0
               rust-piper-0.2.4
               rust-pipewire-0.8.0
               rust-pipewire-sys-0.8.0
               rust-pixman-0.2.1
               rust-pixman-sys-0.1.0
               rust-pkg-config-0.3.31
               rust-png-0.17.16
               rust-polling-3.7.4
               rust-portable-atomic-1.11.0
               rust-ppv-lite86-0.2.20
               rust-pretty-assertions-1.4.1
               rust-proc-macro-crate-3.2.0
               rust-proc-macro-error-1.0.4
               rust-proc-macro-error-attr-1.0.4
               rust-proc-macro2-1.0.93
               rust-profiling-1.0.16
               rust-profiling-procmacros-1.0.16
               rust-proptest-1.6.0
               rust-proptest-derive-0.5.1
               rust-quick-error-1.2.3
               rust-quick-xml-0.37.2
               rust-quote-1.0.38
               rust-rand-0.8.5
               rust-rand-chacha-0.3.1
               rust-rand-core-0.6.4
               rust-rand-xorshift-0.3.0
               rust-raw-window-handle-0.6.2
               rust-rayon-1.10.0
               rust-rayon-core-1.12.1
               rust-redox-syscall-0.4.1
               rust-redox-syscall-0.5.9
               rust-redox-users-0.5.0
               rust-regex-1.11.1
               rust-regex-automata-0.1.10
               rust-regex-automata-0.4.9
               rust-regex-syntax-0.6.29
               rust-regex-syntax-0.8.5
               rust-rustc-hash-1.1.0
               rust-rustc-version-0.4.1
               rust-rustix-0.38.44
               rust-rustversion-1.0.19
               rust-rusty-fork-0.3.0
               rust-ryu-1.0.19
               rust-same-file-1.0.6
               rust-schemars-0.8.22
               rust-schemars-derive-0.8.22
               rust-scoped-tls-1.0.1
               rust-sd-notify-0.4.5
               rust-semver-1.0.25
               rust-serde-1.0.218
               rust-serde-derive-1.0.218
               rust-serde-derive-internals-0.29.1
               rust-serde-json-1.0.139
               rust-serde-repr-0.1.19
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
               rust-smithay-0.4.4
               rust-smithay-client-toolkit-0.19.2
               rust-smithay-drm-extras-0.1.0
               rust-smol-str-0.2.2
               rust-stable-deref-trait-1.2.0
               rust-static-assertions-1.1.0
               rust-strsim-0.11.1
               rust-supports-color-2.1.0
               rust-supports-hyperlinks-2.1.0
               rust-supports-unicode-2.1.0
               rust-syn-1.0.109
               rust-syn-2.0.98
               rust-synstructure-0.13.1
               rust-system-deps-6.2.2
               rust-system-deps-7.0.3
               rust-target-lexicon-0.12.16
               rust-tempfile-3.17.1
               rust-terminal-size-0.1.17
               rust-textwrap-0.15.2
               rust-thiserror-1.0.69
               rust-thiserror-2.0.11
               rust-thiserror-impl-1.0.69
               rust-thiserror-impl-2.0.11
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
               rust-unicode-ident-1.0.17
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
               rust-zerofrom-0.1.5
               rust-zerofrom-derive-0.1.5
               rust-zerovec-0.10.4
               rust-zerovec-derive-0.10.3
               rust-zvariant-5.4.0
               rust-zvariant-derive-5.4.0
               rust-zvariant-utils-3.2.0)))

(define-public rust-addr2line-0.24.2
  (let ((name "rust-addr2line")
        (version "0.24.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "addr2line" "0.24.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz")))))
(define-public rust-adler2-2.0.0
  (let ((name "rust-adler2")
        (version "2.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "adler2" "2.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si")))))
(define-public rust-aead-0.5.2
  (let ((name "rust-aead")
        (version "0.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "aead" "0.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1c32aviraqag7926xcb9sybdm36v5vh9gnxpn4pxdwjc50zl28ni")))))
(define-public rust-ahash-0.8.11
  (let ((name "rust-ahash")
        (version "0.8.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ahash" "0.8.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8")))))
(define-public rust-aho-corasick-1.1.3
  (let ((name "rust-aho-corasick")
        (version "1.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "aho-corasick" "1.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f")))))
(define-public rust-allocator-api2-0.2.21
  (let ((name "rust-allocator-api2")
        (version "0.2.21"))
    (origin
      (method url-fetch)
      (uri (crate-uri "allocator-api2" "0.2.21"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8")))))
(define-public rust-android-activity-0.6.0
  (let ((name "rust-android-activity")
        (version "0.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "android-activity" "0.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0inh88x8x2fh62jg739s9hwyvdh8i920qf0qw7bhr802j9c7hsgg")))))

(define-public rust-android-properties-0.2.2
  (let ((name "rust-android-properties")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "android-properties" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "016slvg269c0y120p9qd8vdfqa2jbw4j0g18gfw6p3ain44v4zpw")))))

(define-public rust-android-system-properties-0.1.5
  (let ((name "rust-android-system-properties")
        (version "0.1.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "android_system_properties" "0.1.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1")))))

(define-public rust-android-tzdata-0.1.1
  (let ((name "rust-android-tzdata")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "android-tzdata" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9")))))
(define-public rust-annotate-snippets-0.9.2
  (let ((name "rust-annotate-snippets")
        (version "0.9.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "annotate-snippets" "0.9.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc")))))

(define-public rust-anstream-0.6.18
  (let ((name "rust-anstream")
        (version "0.6.18"))
    (origin
      (method url-fetch)
      (uri (crate-uri "anstream" "0.6.18"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a")))))
(define-public rust-anstyle-1.0.10
  (let ((name "rust-anstyle")
        (version "1.0.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "anstyle" "1.0.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m")))))
(define-public rust-anstyle-parse-0.2.6
  (let ((name "rust-anstyle-parse")
        (version "0.2.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "anstyle-parse" "0.2.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v")))))
(define-public rust-anstyle-query-1.1.2
  (let ((name "rust-anstyle-query")
        (version "1.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "anstyle-query" "1.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r")))))
(define-public rust-anstyle-wincon-3.0.7
  (let ((name "rust-anstyle-wincon")
        (version "3.0.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "anstyle-wincon" "3.0.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kmf0fq4c8yribdpdpylzz1zccpy84hizmcsac3wrac1f7kk8dfa")))))
(define-public rust-anyhow-1.0.96
  (let ((name "rust-anyhow")
        (version "1.0.96"))
    (origin
      (method url-fetch)
      (uri (crate-uri "anyhow" "1.0.96"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x0b2lk76lfgj069jadmn9zi1wscwz45nwfjgnvbdnc99qc4v5kb")))))
(define-public rust-appendlist-1.4.0
  (let ((name "rust-appendlist")
        (version "1.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "appendlist" "1.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lnbl7mc7capcqj1z1ylxvm4h492sb9sr8pzww3q6lrhrmrxqjg1")))))

(define-public rust-approx-0.4.0
  (let ((name "rust-approx")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "approx" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0y52dg58lapl4pp1kqlznfw1blbki0nx6b0aw8kja2yi3gyhaaiz")))))

(define-public rust-approx-0.5.1
  (let ((name "rust-approx")
        (version "0.5.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "approx" "0.5.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa")))))
(define-public rust-arboard-3.4.1
  (let ((name "rust-arboard")
        (version "3.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "arboard" "3.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x2p8dfhzm3w0cpw81ab2rbyzvkzqs9g66xcakq4y0fd2v5rq2fz")))))
(define-public rust-arc-swap-1.7.1
  (let ((name "rust-arc-swap")
        (version "1.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "arc-swap" "1.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9")))))
(define-public rust-argon2-0.5.3
  (let ((name "rust-argon2")
        (version "0.5.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "argon2" "0.5.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wn0kk97k49wxidfigmz1pdqmygqzi4h6w72ib7cpq765s4i0diw")))))
(define-public rust-arrayvec-0.7.6
  (let ((name "rust-arrayvec")
        (version "0.7.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "arrayvec" "0.7.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw")))))

(define-public rust-as-raw-xcb-connection-1.0.1
  (let ((name "rust-as-raw-xcb-connection")
        (version "1.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "as-raw-xcb-connection" "1.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p")))))

(define-public rust-async-broadcast-0.7.2
  (let ((name "rust-async-broadcast")
        (version "0.7.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-broadcast" "0.7.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3")))))

(define-public rust-async-channel-2.3.1
  (let ((name "rust-async-channel")
        (version "2.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-channel" "2.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0skvwxj6ysfc6d7bhczz9a2550260g62bm5gl0nmjxxyn007id49")))))

(define-public rust-async-executor-1.13.1
  (let ((name "rust-async-executor")
        (version "1.13.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-executor" "1.13.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1v6w1dbvsmw6cs4dk4lxj5dvrikc6xi479wikwaab2qy3h09mjih")))))

(define-public rust-async-fs-2.1.2
  (let ((name "rust-async-fs")
        (version "2.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-fs" "2.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jp0p7lg9zqy2djgdmivbzx0yqmfn9sm2s9dkhaws3zlharhkkgb")))))

(define-public rust-async-io-2.4.0
  (let ((name "rust-async-io")
        (version "2.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-io" "2.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0n8h0vy53n4vdkq529scqnkzm9vcl3r73za9nj81s2nfrhiv78j3")))))

(define-public rust-async-lock-3.4.0
  (let ((name "rust-async-lock")
        (version "3.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-lock" "3.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz")))))

(define-public rust-async-process-2.3.0
  (let ((name "rust-async-process")
        (version "2.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-process" "2.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1fr6cpqdw7hrmzns1983lgx86cg8vyz7nlrn0h0125iqq8fmy9b3")))))

(define-public rust-async-recursion-1.1.1
  (let ((name "rust-async-recursion")
        (version "1.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-recursion" "1.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv")))))

(define-public rust-async-signal-0.2.10
  (let ((name "rust-async-signal")
        (version "0.2.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-signal" "0.2.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1wxrq3871l00mil43nmh0akvwjjjnv0bn7n2pzwbvh00k0s00zk3")))))

(define-public rust-async-stream-0.3.6
  (let ((name "rust-async-stream")
        (version "0.3.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-stream" "0.3.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0xl4zqncrdmw2g6241wgr11dxdg4h7byy6bz3l6si03qyfk72nhb")))))
(define-public rust-async-stream-impl-0.3.6
  (let ((name "rust-async-stream-impl")
        (version "0.3.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-stream-impl" "0.3.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kaplfb5axsvf1gfs2gk6c4zx6zcsns0yf3ssk7iwni7bphlvhn7")))))
(define-public rust-async-task-4.7.1
  (let ((name "rust-async-task")
        (version "4.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-task" "4.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb")))))

(define-public rust-async-trait-0.1.86
  (let ((name "rust-async-trait")
        (version "0.1.86"))
    (origin
      (method url-fetch)
      (uri (crate-uri "async-trait" "0.1.86"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17g7pk7fxbsf61hdbmf727q56bcqvdpjapxw5wd7gwvb114xfkb4")))))
(define-public rust-atoi-2.0.0
  (let ((name "rust-atoi")
        (version "2.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atoi" "2.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0a05h42fggmy7h0ajjv6m7z72l924i7igbx13hk9d8pyign9k3gj")))))
(define-public rust-atomic-0.6.0
  (let ((name "rust-atomic")
        (version "0.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atomic" "0.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15193mfhmrq3p6vi1a10hw3n6kvzf5h32zikhby3mdj0ww1q10cd")))))

(define-public rust-atomic-waker-1.1.2
  (let ((name "rust-atomic-waker")
        (version "1.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atomic-waker" "1.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m")))))
(define-public rust-atuin-client-18.4.0
  (let ((name "rust-atuin-client")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-client" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ryg3cj1hayr774rswj7nk71bvjgkw91s80jc3finfk0sapi6pq1")))))
(define-public rust-atuin-common-18.4.0
  (let ((name "rust-atuin-common")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-common" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1l3hprwyvc95343l1ls0hqn8pixr5w9vnpshfbrb1n2p8yg5zzpw")))))
(define-public rust-atuin-daemon-18.4.0
  (let ((name "rust-atuin-daemon")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-daemon" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wlyrpjm30khd7m51z6xfiny2wc63hdbvzqv6fxwa1lfsffxa3ma")))))
(define-public rust-atuin-dotfiles-18.4.0
  (let ((name "rust-atuin-dotfiles")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-dotfiles" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0adln7gfrr38zy9slxi33k0ma4a0iabz5jqpv8anrag2fab0951k")))))
(define-public rust-atuin-history-18.4.0
  (let ((name "rust-atuin-history")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-history" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1qy0x84cjjrj5ix9x5gxbdn042drrrr9v2d02dl8fz07w60rbmz5")))))
(define-public rust-atuin-server-18.4.0
  (let ((name "rust-atuin-server")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-server" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15p074z5ng24ln89bwcvicw3zvp140aily5aclyydlsfr81l8w5a")))))
(define-public rust-atuin-server-database-18.4.0
  (let ((name "rust-atuin-server-database")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-server-database" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "08g1pim38jpixa6d13z5vcir9y7f85a2799w7rwm8r19b24y5yhz")))))
(define-public rust-atuin-server-postgres-18.4.0
  (let ((name "rust-atuin-server-postgres")
        (version "18.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "atuin-server-postgres" "18.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0k4bv7fzzpdh1yl2bg5b04gp0mlkb861x545wp8gggijk280ih99")))))
(define-public rust-autocfg-1.4.0
  (let ((name "rust-autocfg")
        (version "1.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "autocfg" "1.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc")))))
(define-public rust-axum-0.7.9
  (let ((name "rust-axum")
        (version "0.7.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "axum" "0.7.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "07z7wqczi9i8xb4460rvn39p4wjqwr32hx907crd1vwb2fy8ijpd")))))
(define-public rust-axum-core-0.4.5
  (let ((name "rust-axum-core")
        (version "0.4.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "axum-core" "0.4.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16b1496c4gm387q20hkv5ic3k5bd6xmnvk50kwsy6ymr8rhvvwh9")))))
(define-public rust-axum-server-0.7.1
  (let ((name "rust-axum-server")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "axum-server" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1n67cx39cm9zsm0dwm0nla67qjswj90ccqrwq0x3kagn904ckfjn")))))
(define-public rust-backtrace-0.3.74
  (let ((name "rust-backtrace")
        (version "0.3.74"))
    (origin
      (method url-fetch)
      (uri (crate-uri "backtrace" "0.3.74"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld")))))
(define-public rust-base64-0.21.7
  (let ((name "rust-base64")
        (version "0.21.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "base64" "0.21.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx")))))
(define-public rust-base64-0.22.1
  (let ((name "rust-base64")
        (version "0.22.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "base64" "0.22.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj")))))
(define-public rust-base64ct-1.6.0
  (let ((name "rust-base64ct")
        (version "1.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "base64ct" "1.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0nvdba4jb8aikv60az40x2w1y96sjdq8z3yp09rwzmkhiwv1lg4c")))))
(define-public rust-beef-0.5.2
  (let ((name "rust-beef")
        (version "0.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "beef" "0.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1c95lbnhld96iwwbyh5kzykbpysq0fnjfhwxa1mhap5qxgrl30is")))))
(define-public rust-bindgen-0.69.5
  (let ((name "rust-bindgen")
        (version "0.69.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bindgen" "0.69.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7")))))

(define-public rust-bit-set-0.8.0
  (let ((name "rust-bit-set")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bit-set" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008")))))

(define-public rust-bit-vec-0.8.0
  (let ((name "rust-bit-vec")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bit-vec" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy")))))

(define-public rust-bitflags-1.3.2
  (let ((name "rust-bitflags")
        (version "1.3.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bitflags" "1.3.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy")))))
(define-public rust-bitflags-2.8.0
  (let ((name "rust-bitflags")
        (version "2.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bitflags" "2.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dixc6168i98652jxf0z9nbyn0zcis3g6hi6qdr7z5dbhcygas4g")))))
(define-public rust-blake2-0.10.6
  (let ((name "rust-blake2")
        (version "0.10.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "blake2" "0.10.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1zlf7w7gql12v61d9jcbbswa3dw8qxsjglylsiljp9f9b3a2ll26")))))
(define-public rust-block-buffer-0.10.4
  (let ((name "rust-block-buffer")
        (version "0.10.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "block-buffer" "0.10.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h")))))
(define-public rust-block2-0.5.1
  (let ((name "rust-block2")
        (version "0.5.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "block2" "0.5.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc")))))
(define-public rust-blocking-1.6.1
  (let ((name "rust-blocking")
        (version "1.6.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "blocking" "1.6.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1si99l8zp7c4zq87y35ayjgc5c9b60jb8h0k14zfcs679z2l2gvh")))))

(define-public rust-bumpalo-3.17.0
  (let ((name "rust-bumpalo")
        (version "3.17.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bumpalo" "3.17.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gxxsn2fsjmv03g8p3m749mczv2k4m8xspifs5l7bcx0vx3gna0n")))))
(define-public rust-by-address-1.2.1
  (let ((name "rust-by-address")
        (version "1.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "by_address" "1.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "01idmag3lcwnnqrnnyik2gmbrr34drsi97q15ihvcbbidf2kryk4")))))
(define-public rust-bytemuck-1.21.0
  (let ((name "rust-bytemuck")
        (version "1.21.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bytemuck" "1.21.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "18wj81x9xhqcd6985r8qxmbik6szjfjfj62q3xklw8h2p3x7srgg")))))
(define-public rust-bytemuck-derive-1.8.1
  (let ((name "rust-bytemuck-derive")
        (version "1.8.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bytemuck_derive" "1.8.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ykwbnpm9y8hssp8kiaws2s4hljv4cl85mwgp1m67fzpnj9n59rz")))))

(define-public rust-byteorder-1.5.0
  (let ((name "rust-byteorder")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "byteorder" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z")))))
(define-public rust-byteorder-lite-0.1.0
  (let ((name "rust-byteorder-lite")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "byteorder-lite" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg")))))
(define-public rust-bytes-1.10.0
  (let ((name "rust-bytes")
        (version "1.10.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "bytes" "1.10.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ybcmdrlxrsrn7lnl0xrjg10j7zb4r01jjs5b2sqhrcwh62aq7gn")))))
(define-public rust-cairo-rs-0.20.7
  (let ((name "rust-cairo-rs")
        (version "0.20.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cairo-rs" "0.20.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xy02qa4mn9bwnhsbmkry4yjz230r66nvrkh4fn9dkw61m8val5f")))))

(define-public rust-cairo-sys-rs-0.20.7
  (let ((name "rust-cairo-sys-rs")
        (version "0.20.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cairo-sys-rs" "0.20.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pwh4b4mdsipjl9lrg5p5bygbdk11kz6m5y7mbrb0ziwwjw6p2zi")))))

(define-public rust-calloop-0.13.0
  (let ((name "rust-calloop")
        (version "0.13.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "calloop" "0.13.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1v5zgidnhsyml403rzr7vm99f8q6r5bxq5gxyiqkr8lcapwa57dr")))))

(define-public rust-calloop-0.14.2
  (let ((name "rust-calloop")
        (version "0.14.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "calloop" "0.14.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jzx8rmgndj1br4gnd4iaxayqi79g897lz6qdy2l670xcqj9g4hh")))))

(define-public rust-calloop-wayland-source-0.3.0
  (let ((name "rust-calloop-wayland-source")
        (version "0.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "calloop-wayland-source" "0.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "086x5mq16prrcwd9k6bw9an0sp8bj9l5daz4ziz5z4snf2c6m9lm")))))

(define-public rust-calloop-wayland-source-0.4.0
  (let ((name "rust-calloop-wayland-source")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "calloop-wayland-source" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1bsxx4dz4k4icza63w108n8s1agm7890nl3syigaa9p0pcfplsl7")))))

(define-public rust-cassowary-0.3.0
  (let ((name "rust-cassowary")
        (version "0.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cassowary" "0.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0lvanj0gsk6pc1chqrh4k5k0vi1rfbgzmsk46dwy3nmrqyw711nz")))))
(define-public rust-castaway-0.2.3
  (let ((name "rust-castaway")
        (version "0.2.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "castaway" "0.2.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1mf0wypwnkpa1hi0058vp8g7bjh2qraip2qv7dmak7mg1azfkfha")))))
(define-public rust-cc-1.2.15
  (let ((name "rust-cc")
        (version "1.2.15"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cc" "1.2.15"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1bq1c3qbarhx3z10bfpk8df2kq2akx7k0v68sm1z8xx5xrcy4dn7")))))
(define-public rust-cesu8-1.1.0
  (let ((name "rust-cesu8")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cesu8" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd")))))

(define-public rust-cexpr-0.6.0
  (let ((name "rust-cexpr")
        (version "0.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cexpr" "0.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g")))))

(define-public rust-cfg-aliases-0.1.1
  (let ((name "rust-cfg-aliases")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cfg_aliases" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17p821nc6jm830vzl2lmwz60g3a30hcm33nk6l257i1rjdqw85px")))))

(define-public rust-cfg-aliases-0.2.1
  (let ((name "rust-cfg-aliases")
        (version "0.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cfg_aliases" "0.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1")))))

(define-public rust-cfg-expr-0.15.8
  (let ((name "rust-cfg-expr")
        (version "0.15.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cfg-expr" "0.15.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh")))))

(define-public rust-cfg-expr-0.17.2
  (let ((name "rust-cfg-expr")
        (version "0.17.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cfg-expr" "0.17.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12a7zr6ff4i6mfwcv711dll0w5pr3dw1lvkaf4c4a66i1gjacjwd")))))

(define-public rust-cfg-if-1.0.0
  (let ((name "rust-cfg-if")
        (version "1.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cfg-if" "1.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds")))))
(define-public rust-cgmath-0.18.0
  (let ((name "rust-cgmath")
        (version "0.18.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cgmath" "0.18.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05sk7c1c1jg5ygqvc3y77kxddp177gwazfibhd864ag3800x760s")))))

(define-public rust-chacha20-0.9.1
  (let ((name "rust-chacha20")
        (version "0.9.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "chacha20" "0.9.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0678wipx6kghp71hpzhl2qvx80q7caz3vm8vsvd07b1fpms3yqf3")))))
(define-public rust-chrono-0.4.40
  (let ((name "rust-chrono")
        (version "0.4.40"))
    (origin
      (method url-fetch)
      (uri (crate-uri "chrono" "0.4.40"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0z334kqnvq5zx6xsq1k6zk8g9z14fgk2w3vkn4n13pvi3mhn8y8s")))))
(define-public rust-chumsky-0.9.3
  (let ((name "rust-chumsky")
        (version "0.9.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "chumsky" "0.9.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jcnafc8rjfs1al08gqzyn0kpbaizgdwrd0ajqafspd18ikxdswf")))))

(define-public rust-cipher-0.4.4
  (let ((name "rust-cipher")
        (version "0.4.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cipher" "0.4.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp")))))
(define-public rust-clang-sys-1.8.1
  (let ((name "rust-clang-sys")
        (version "1.8.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clang-sys" "1.8.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb")))))

(define-public rust-clap-4.5.31
  (let ((name "rust-clap")
        (version "4.5.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clap" "4.5.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ryp6xjbdc9cbjjkafjl35j91pvv0ykislwqhr537bi9hkcv0yq2")))))
(define-public rust-clap-builder-4.5.31
  (let ((name "rust-clap-builder")
        (version "4.5.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clap_builder" "4.5.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0qyqd6kfcs41x29a95n15744jyv2v07srvwi6z9g7q3jl35y12am")))))
(define-public rust-clap-complete-4.5.46
  (let ((name "rust-clap-complete")
        (version "4.5.46"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clap_complete" "4.5.46"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "166f2f6xr1jc8vhgjgpchwbfb12s1q3s1xakgvvnclrwla751igm")))))
(define-public rust-clap-complete-nushell-4.5.5
  (let ((name "rust-clap-complete-nushell")
        (version "4.5.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clap_complete_nushell" "4.5.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12miqxh9g7q37w11bgv55b32s0hdf6avf0lhagzc5psp6icv3a66")))))
(define-public rust-clap-derive-4.5.28
  (let ((name "rust-clap-derive")
        (version "4.5.28"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clap_derive" "4.5.28"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1vgigkhljp3r8r5lwdrn1ij93nafmjwh8cx77nppb9plqsaysk5z")))))
(define-public rust-clap-lex-0.7.4
  (let ((name "rust-clap-lex")
        (version "0.7.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clap_lex" "0.7.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl")))))
(define-public rust-clipboard-win-5.4.0
  (let ((name "rust-clipboard-win")
        (version "5.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "clipboard-win" "5.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "14n87fc0vzbd0wdhqzvcs1lqgafsncplzcanhpik93xhhalfgvqm")))))
(define-public rust-colorchoice-1.0.3
  (let ((name "rust-colorchoice")
        (version "1.0.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "colorchoice" "1.0.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv")))))
(define-public rust-colored-2.2.0
  (let ((name "rust-colored")
        (version "2.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "colored" "2.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0g6s7j2qayjd7i3sivmwiawfdg8c8ldy0g2kl4vwk1yk16hjaxqi")))))
(define-public rust-combine-4.6.7
  (let ((name "rust-combine")
        (version "4.6.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "combine" "4.6.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms")))))

(define-public rust-compact-str-0.7.1
  (let ((name "rust-compact-str")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "compact_str" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gvvfc2c6pg1rwr2w36ra4674w3lzwg97vq2v6k791w30169qszq")))))
(define-public rust-concurrent-queue-2.5.0
  (let ((name "rust-concurrent-queue")
        (version "2.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "concurrent-queue" "2.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c")))))
(define-public rust-condtype-1.3.0
  (let ((name "rust-condtype")
        (version "1.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "condtype" "1.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1by78npyhkc30jccc7kirvwip1fj0jhi2bwfmcw44dqz81xa1w5s")))))

(define-public rust-config-0.13.4
  (let ((name "rust-config")
        (version "0.13.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "config" "0.13.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jjag1x3rl77zjykbrykzhd5fsiv8vy40y4lxkj46xicjw8qwwr3")))))
(define-public rust-console-0.15.10
  (let ((name "rust-console")
        (version "0.15.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "console" "0.15.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "06q4ag46machxp5w381x1v9l2g7d801q6sawvxcpidarh36nwg7a")))))
(define-public rust-const-oid-0.9.6
  (let ((name "rust-const-oid")
        (version "0.9.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "const-oid" "0.9.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2")))))
(define-public rust-convert-case-0.6.0
  (let ((name "rust-convert-case")
        (version "0.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "convert_case" "0.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c")))))

(define-public rust-cookie-factory-0.3.3
  (let ((name "rust-cookie-factory")
        (version "0.3.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cookie-factory" "0.3.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "18mka6fk3843qq3jw1fdfvzyv05kx7kcmirfbs2vg2kbw9qzm1cq")))))

(define-public rust-core-foundation-0.9.4
  (let ((name "rust-core-foundation")
        (version "0.9.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "core-foundation" "0.9.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci")))))
(define-public rust-core-foundation-sys-0.8.7
  (let ((name "rust-core-foundation-sys")
        (version "0.8.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "core-foundation-sys" "0.8.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp")))))
(define-public rust-core-graphics-0.23.2
  (let ((name "rust-core-graphics")
        (version "0.23.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "core-graphics" "0.23.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10dhv3gk4kmbzl14xxkrhhky4fdp8h6nzff6h0019qgr6nz84xy0")))))
(define-public rust-core-graphics-types-0.1.3
  (let ((name "rust-core-graphics-types")
        (version "0.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "core-graphics-types" "0.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1bxg8nxc8fk4kxnqyanhf36wq0zrjr552c58qy6733zn2ihhwfa5")))))
(define-public rust-cpufeatures-0.2.17
  (let ((name "rust-cpufeatures")
        (version "0.2.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cpufeatures" "0.2.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar")))))
(define-public rust-crc-3.2.1
  (let ((name "rust-crc")
        (version "3.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crc" "3.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dnn23x68qakzc429s1y9k9y3g8fn5v9jwi63jcz151sngby9rk9")))))
(define-public rust-crc-catalog-2.4.0
  (let ((name "rust-crc-catalog")
        (version "2.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crc-catalog" "2.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr")))))
(define-public rust-crc32fast-1.4.2
  (let ((name "rust-crc32fast")
        (version "1.4.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crc32fast" "1.4.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9")))))
(define-public rust-crossbeam-deque-0.8.6
  (let ((name "rust-crossbeam-deque")
        (version "0.8.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-deque" "0.8.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx")))))
(define-public rust-crossbeam-epoch-0.9.18
  (let ((name "rust-crossbeam-epoch")
        (version "0.9.18"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-epoch" "0.9.18"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv")))))
(define-public rust-crossbeam-queue-0.3.12
  (let ((name "rust-crossbeam-queue")
        (version "0.3.12"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-queue" "0.3.12"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "059igaxckccj6ndmg45d5yf7cm4ps46c18m21afq3pwiiz1bnn0g")))))
(define-public rust-crossbeam-utils-0.8.21
  (let ((name "rust-crossbeam-utils")
        (version "0.8.21"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crossbeam-utils" "0.8.21"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh")))))
(define-public rust-crossterm-0.27.0
  (let ((name "rust-crossterm")
        (version "0.27.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crossterm" "0.27.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pr413ki440xgddlmkrc4j1bfx1h8rpmll87zn8ykja1bm2gwxpl")))))
(define-public rust-crossterm-winapi-0.9.1
  (let ((name "rust-crossterm-winapi")
        (version "0.9.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crossterm_winapi" "0.9.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc")))))
(define-public rust-crypto-common-0.1.6
  (let ((name "rust-crypto-common")
        (version "0.1.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crypto-common" "0.1.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv")))))
(define-public rust-crypto-secretbox-0.1.1
  (let ((name "rust-crypto-secretbox")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "crypto_secretbox" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1qa1w5s8dbyb88269zrmvbnillqahz394pl07bsds6gpmn3wzmmr")))))
(define-public rust-csscolorparser-0.7.0
  (let ((name "rust-csscolorparser")
        (version "0.7.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "csscolorparser" "0.7.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12423a53ikbzacavi157kf3xz9p93sn8gqvwsifvjzwahima3ya6")))))

(define-public rust-cursor-icon-1.1.0
  (let ((name "rust-cursor-icon")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "cursor-icon" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "14brf4vd6az9hnszwzqj7xyfaymqx9806d4i7xmwlaja3wjsr9ln")))))

(define-public rust-curve25519-dalek-4.1.3
  (let ((name "rust-curve25519-dalek")
        (version "4.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "curve25519-dalek" "4.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gmjb9dsknrr8lypmhkyjd67p1arb8mbfamlwxm7vph38my8pywp")))))
(define-public rust-curve25519-dalek-derive-0.1.1
  (let ((name "rust-curve25519-dalek-derive")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "curve25519-dalek-derive" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l")))))
(define-public rust-darling-0.20.10
  (let ((name "rust-darling")
        (version "0.20.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "darling" "0.20.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg")))))
(define-public rust-darling-core-0.20.10
  (let ((name "rust-darling-core")
        (version "0.20.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "darling_core" "0.20.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm")))))
(define-public rust-darling-macro-0.20.10
  (let ((name "rust-darling-macro")
        (version "0.20.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "darling_macro" "0.20.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk")))))
(define-public rust-dashmap-5.5.3
  (let ((name "rust-dashmap")
        (version "5.5.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dashmap" "5.5.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp")))))
(define-public rust-der-0.7.9
  (let ((name "rust-der")
        (version "0.7.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "der" "0.7.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h4vzjfa1lczxdf8avfj9qlwh1qianqlxdy1g5rn762qnvkzhnzm")))))
(define-public rust-deranged-0.3.11
  (let ((name "rust-deranged")
        (version "0.3.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "deranged" "0.3.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl")))))
(define-public rust-derive-new-0.6.0
  (let ((name "rust-derive-new")
        (version "0.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "derive-new" "0.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1b8jv6jx0b8jgkz9kmz0ciqmnf74xkk0mmvkb5z1c87932kdwl6i")))))
(define-public rust-diff-0.1.13
  (let ((name "rust-diff")
        (version "0.1.13"))
    (origin
      (method url-fetch)
      (uri (crate-uri "diff" "0.1.13"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an")))))

(define-public rust-digest-0.10.7
  (let ((name "rust-digest")
        (version "0.10.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "digest" "0.10.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy")))))
(define-public rust-directories-5.0.1
  (let ((name "rust-directories")
        (version "5.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "directories" "5.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs")))))
(define-public rust-directories-6.0.0
  (let ((name "rust-directories")
        (version "6.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "directories" "6.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0zgy2w088v8w865c11dmc3dih899fgrhvrfp7g83h6v6ai60kx8n")))))

(define-public rust-dirs-5.0.1
  (let ((name "rust-dirs")
        (version "5.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dirs" "5.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24")))))
(define-public rust-dirs-sys-0.4.1
  (let ((name "rust-dirs-sys")
        (version "0.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dirs-sys" "0.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj")))))
(define-public rust-dirs-sys-0.5.0
  (let ((name "rust-dirs-sys")
        (version "0.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dirs-sys" "0.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0")))))

(define-public rust-dispatch-0.2.0
  (let ((name "rust-dispatch")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dispatch" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0fwjr9b7582ic5689zxj8lf7zl94iklhlns3yivrnv8c9fxr635x")))))

(define-public rust-displaydoc-0.2.5
  (let ((name "rust-displaydoc")
        (version "0.2.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "displaydoc" "0.2.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp")))))
(define-public rust-divan-0.1.17
  (let ((name "rust-divan")
        (version "0.1.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "divan" "0.1.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1i5qvk7h9fcrr89cxkmadpgm03sv7askplyqh8vb0a8b0a9k2n70")))))

(define-public rust-divan-macros-0.1.17
  (let ((name "rust-divan-macros")
        (version "0.1.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "divan-macros" "0.1.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0z5n3f4m5c7flqnr6vg9qz51j9mjb1s2afcsfnqf7x9nwsc1vicd")))))

(define-public rust-dlib-0.5.2
  (let ((name "rust-dlib")
        (version "0.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dlib" "0.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k")))))

(define-public rust-dotenvy-0.15.7
  (let ((name "rust-dotenvy")
        (version "0.15.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dotenvy" "0.15.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16s3n973n5aqym02692i1npb079n5mb0fwql42ikmwn8wnrrbbqs")))))
(define-public rust-downcast-rs-1.2.1
  (let ((name "rust-downcast-rs")
        (version "1.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "downcast-rs" "1.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm")))))
(define-public rust-dpi-0.1.1
  (let ((name "rust-dpi")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dpi" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0lzz48gpgbwdrw0s8vib0589ij9jizv1vzsphm4xd9kw58lhwp7j")))))

(define-public rust-drm-0.14.1
  (let ((name "rust-drm")
        (version "0.14.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "drm" "0.14.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vvmj9n0wslrbw3rinpzlfyhwwgr02gqspy1al5gfh99dif8rg40")))))

(define-public rust-drm-ffi-0.9.0
  (let ((name "rust-drm-ffi")
        (version "0.9.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "drm-ffi" "0.9.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12vff80hdpp81gj5lqw25xnkppwsxc4wklpn8nc556wsv5ci9r6q")))))

(define-public rust-drm-fourcc-2.2.0
  (let ((name "rust-drm-fourcc")
        (version "2.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "drm-fourcc" "2.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x76v9a0pkgym4n6cah4barnai9gsssm7gjzxskw2agwibdvrbqa")))))

(define-public rust-drm-sys-0.8.0
  (let ((name "rust-drm-sys")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "drm-sys" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1345z72hd2rna4qxd2zcpbzvw0z7ywfndk6g2ngdci69vg46dyxs")))))

(define-public rust-dyn-clone-1.0.18
  (let ((name "rust-dyn-clone")
        (version "1.0.18"))
    (origin
      (method url-fetch)
      (uri (crate-uri "dyn-clone" "1.0.18"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dag651ph5q0mcax74y4m1k9hyl737q1v03isck3mzxsfd7g9vpy")))))

(define-public rust-ed25519-2.2.3
  (let ((name "rust-ed25519")
        (version "2.2.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ed25519" "2.2.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i")))))
(define-public rust-ed25519-dalek-2.1.1
  (let ((name "rust-ed25519-dalek")
        (version "2.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ed25519-dalek" "2.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0w88cafwglg9hjizldbmlza0ns3hls81zk1bcih3m5m3h67algaa")))))
(define-public rust-either-1.14.0
  (let ((name "rust-either")
        (version "1.14.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "either" "1.14.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17fs0r9mnj632k4ff8c6zyq80zqvqb0wa9cgsyd5iprd159l74dp")))))
(define-public rust-encode-unicode-1.0.0
  (let ((name "rust-encode-unicode")
        (version "1.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "encode_unicode" "1.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail")))))
(define-public rust-encoding-rs-0.8.35
  (let ((name "rust-encoding-rs")
        (version "0.8.35"))
    (origin
      (method url-fetch)
      (uri (crate-uri "encoding_rs" "0.8.35"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm")))))
(define-public rust-endi-1.1.0
  (let ((name "rust-endi")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "endi" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53")))))

(define-public rust-enumflags2-0.7.11
  (let ((name "rust-enumflags2")
        (version "0.7.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "enumflags2" "0.7.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0iwi60d54lgby0f29b5isikxraf0wvnqdmlddx68a62kbx34nbxs")))))

(define-public rust-enumflags2-derive-0.7.11
  (let ((name "rust-enumflags2-derive")
        (version "0.7.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "enumflags2_derive" "0.7.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0yfdjyrf9b4mi1r589azkyirjhzmdw29nqq0mdjnsyldlmjayk7w")))))

(define-public rust-env-filter-0.1.3
  (let ((name "rust-env-filter")
        (version "0.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "env_filter" "0.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1l4p6f845cylripc3zkxa0lklk8rn2q86fqm522p6l2cknjhavhq")))))
(define-public rust-env-logger-0.11.6
  (let ((name "rust-env-logger")
        (version "0.11.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "env_logger" "0.11.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1q30cqb2dfs3qrs0s30qdmqwi7n2gz4pniwd8a9gvhygwgcf7bnw")))))
(define-public rust-equivalent-1.0.2
  (let ((name "rust-equivalent")
        (version "1.0.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "equivalent" "1.0.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7")))))
(define-public rust-errno-0.3.10
  (let ((name "rust-errno")
        (version "0.3.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "errno" "0.3.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0pgblicz1kjz9wa9m0sghkhh2zw1fhq1mxzj7ndjm746kg5m5n1k")))))
(define-public rust-error-code-3.3.1
  (let ((name "rust-error-code")
        (version "3.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "error-code" "3.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bx9hw3pahzqym8jvb0ln4qsabnysgn98mikyh2afhk9rif31nd5")))))
(define-public rust-etcetera-0.8.0
  (let ((name "rust-etcetera")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "etcetera" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hxrsn75dirbjhwgkdkh0pnpqrnq17ypyhjpjaypgax1hd91nv8k")))))
(define-public rust-event-listener-5.4.0
  (let ((name "rust-event-listener")
        (version "5.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "event-listener" "5.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il")))))
(define-public rust-event-listener-strategy-0.5.3
  (let ((name "rust-event-listener-strategy")
        (version "0.5.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "event-listener-strategy" "0.5.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ch5gf6knllyq12jkb5zdfag573dh44307q4pwwi2g37sc6lwgiw")))))

(define-public rust-eyre-0.6.12
  (let ((name "rust-eyre")
        (version "0.6.12"))
    (origin
      (method url-fetch)
      (uri (crate-uri "eyre" "0.6.12"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw")))))
(define-public rust-fast-srgb8-1.0.0
  (let ((name "rust-fast-srgb8")
        (version "1.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fast-srgb8" "1.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "18g6xwwh4gnkyx1352hnvwagpv0n4y98yp2llm8vyvwxh487abnx")))))
(define-public rust-fastrand-2.3.0
  (let ((name "rust-fastrand")
        (version "2.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fastrand" "2.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p")))))
(define-public rust-fdeflate-0.3.7
  (let ((name "rust-fdeflate")
        (version "0.3.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fdeflate" "0.3.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y")))))
(define-public rust-fiat-crypto-0.2.9
  (let ((name "rust-fiat-crypto")
        (version "0.2.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fiat-crypto" "0.2.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "07c1vknddv3ak7w89n85ik0g34nzzpms6yb845vrjnv9m4csbpi8")))))
(define-public rust-field-offset-0.3.6
  (let ((name "rust-field-offset")
        (version "0.3.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "field-offset" "0.3.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq")))))

(define-public rust-filedescriptor-0.8.3
  (let ((name "rust-filedescriptor")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "filedescriptor" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bb8qqa9h9sj2mzf09yqxn260qkcqvmhmyrmdjvyxcn94knmh1z4")))))
(define-public rust-fixedbitset-0.4.2
  (let ((name "rust-fixedbitset")
        (version "0.4.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fixedbitset" "0.4.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc")))))
(define-public rust-fixedbitset-0.5.7
  (let ((name "rust-fixedbitset")
        (version "0.5.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fixedbitset" "0.5.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx")))))
(define-public rust-flate2-1.1.0
  (let ((name "rust-flate2")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "flate2" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1p1qpmkkxky6y3869g2facflp0lmvgsbxq4bhkwpm69na9dazyhi")))))
(define-public rust-flume-0.11.1
  (let ((name "rust-flume")
        (version "0.11.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "flume" "0.11.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15ch0slxa8sqsi6c73a0ky6vdnh48q8cxjf7rksa3243m394s3ns")))))
(define-public rust-fnv-1.0.7
  (let ((name "rust-fnv")
        (version "1.0.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fnv" "1.0.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz")))))
(define-public rust-foldhash-0.1.4
  (let ((name "rust-foldhash")
        (version "0.1.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "foldhash" "0.1.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vsxw2iwpgs7yy6l7pndm7b8nllaq5vdxwnmjn1qpm5kyzhzvlm0")))))
(define-public rust-foreign-types-0.5.0
  (let ((name "rust-foreign-types")
        (version "0.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "foreign-types" "0.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rfr2zfxnx9rz3292z5nyk8qs2iirznn5ff3rd4vgdwza6mdjdyp")))))
(define-public rust-foreign-types-macros-0.2.3
  (let ((name "rust-foreign-types-macros")
        (version "0.2.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "foreign-types-macros" "0.2.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hjpii8ny6l7h7jpns2cp9589016l8mlrpaigcnayjn9bdc6qp0s")))))
(define-public rust-foreign-types-shared-0.3.1
  (let ((name "rust-foreign-types-shared")
        (version "0.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "foreign-types-shared" "0.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0nykdvv41a3d4py61bylmlwjhhvdm0b3bcj9vxhqgxaxnp5ik6ma")))))
(define-public rust-form-urlencoded-1.2.1
  (let ((name "rust-form-urlencoded")
        (version "1.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "form_urlencoded" "1.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1")))))
(define-public rust-fs-err-2.11.0
  (let ((name "rust-fs-err")
        (version "2.11.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fs-err" "2.11.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hdajzh5sjvvdjg0n15j91mv8ydvb7ff6m909frvdmg1bw81z948")))))
(define-public rust-futures-0.3.31
  (let ((name "rust-futures")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35")))))
(define-public rust-futures-channel-0.3.31
  (let ((name "rust-futures-channel")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-channel" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd")))))
(define-public rust-futures-core-0.3.31
  (let ((name "rust-futures-core")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-core" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5")))))
(define-public rust-futures-executor-0.3.31
  (let ((name "rust-futures-executor")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-executor" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y")))))
(define-public rust-futures-intrusive-0.5.0
  (let ((name "rust-futures-intrusive")
        (version "0.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-intrusive" "0.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vwm08d1pli6bdaj0i7xhk3476qlx4pll6i0w03gzdnh7lh0r4qx")))))
(define-public rust-futures-io-0.3.31
  (let ((name "rust-futures-io")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-io" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y")))))
(define-public rust-futures-lite-2.6.0
  (let ((name "rust-futures-lite")
        (version "2.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-lite" "2.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0cmmgszlmkwsac9pyw5rfjakmshgx4wmzmlyn6mmjs0jav4axvgm")))))

(define-public rust-futures-macro-0.3.31
  (let ((name "rust-futures-macro")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-macro" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn")))))
(define-public rust-futures-sink-0.3.31
  (let ((name "rust-futures-sink")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-sink" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5")))))
(define-public rust-futures-task-0.3.31
  (let ((name "rust-futures-task")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-task" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr")))))
(define-public rust-futures-util-0.3.31
  (let ((name "rust-futures-util")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "futures-util" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z")))))
(define-public rust-fuzzy-matcher-0.3.7
  (let ((name "rust-fuzzy-matcher")
        (version "0.3.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "fuzzy-matcher" "0.3.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "153csv8rsk2vxagb68kpmiknvdd3bzqj03x805khckck28rllqal")))))
(define-public rust-gbm-0.18.0
  (let ((name "rust-gbm")
        (version "0.18.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gbm" "0.18.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0skyaj51xlazaa24jdkxxi2g6pnw834k3yqlf2ly999wincjx1ff")))))

(define-public rust-gbm-sys-0.4.0
  (let ((name "rust-gbm-sys")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gbm-sys" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vzp28ip4w74p05ygs4p9m7sspggn2zvcykbpyv8ypbqrhm5yfn1")))))

(define-public rust-gdk-pixbuf-0.20.9
  (let ((name "rust-gdk-pixbuf")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gdk-pixbuf" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1l0llkzf7v634h5a8dz6935xkf3ma3fqm9vhpggiw8hazzbayqvm")))))

(define-public rust-gdk-pixbuf-sys-0.20.7
  (let ((name "rust-gdk-pixbuf-sys")
        (version "0.20.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gdk-pixbuf-sys" "0.20.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0p0b3lrzamsz580dyrr5i99i32ppsjp6mfmvfrs9kgq2j9y5iwk7")))))

(define-public rust-gdk4-0.9.6
  (let ((name "rust-gdk4")
        (version "0.9.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gdk4" "0.9.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0q1dld01fgj7qxj644by0fc242mcn36w3bagn4z1mkdfq7cwjl28")))))

(define-public rust-gdk4-sys-0.9.6
  (let ((name "rust-gdk4-sys")
        (version "0.9.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gdk4-sys" "0.9.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0fj722lp86fpa1b1i3s2anavdmcpybd0b47mkhknzd72k1bvjvkg")))))

(define-public rust-generator-0.8.4
  (let ((name "rust-generator")
        (version "0.8.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "generator" "0.8.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1p9qqk9nzarjdcl5fr4iylvsv446g0svlpk63lxis4ysrqad2syc")))))

(define-public rust-generic-array-0.14.7
  (let ((name "rust-generic-array")
        (version "0.14.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "generic-array" "0.14.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45")))))
(define-public rust-gethostname-0.4.3
  (let ((name "rust-gethostname")
        (version "0.4.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gethostname" "0.4.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1")))))
(define-public rust-getrandom-0.2.15
  (let ((name "rust-getrandom")
        (version "0.2.15"))
    (origin
      (method url-fetch)
      (uri (crate-uri "getrandom" "0.2.15"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4")))))
(define-public rust-getrandom-0.3.1
  (let ((name "rust-getrandom")
        (version "0.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "getrandom" "0.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1y154yzby383p63ndw6zpfm0fz3vf6c0zdwc7df6vkl150wrr923")))))
(define-public rust-gimli-0.31.1
  (let ((name "rust-gimli")
        (version "0.31.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gimli" "0.31.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7")))))
(define-public rust-gio-0.20.9
  (let ((name "rust-gio")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gio" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "11vl4zkb3zvrklr7zhdlcyb35rbrm8d0xpbjfpm89782z1q0rw54")))))

(define-public rust-gio-sys-0.20.9
  (let ((name "rust-gio-sys")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gio-sys" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17izngigdvv3gfda2qk059vcgihmxbaa7rjl3cz8r69618jva3hn")))))

(define-public rust-git-version-0.3.9
  (let ((name "rust-git-version")
        (version "0.3.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "git-version" "0.3.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "06ddi3px6l2ip0srn8512bsh8wrx4rzi65piya0vrz5h7nm6im8s")))))

(define-public rust-git-version-macro-0.3.9
  (let ((name "rust-git-version-macro")
        (version "0.3.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "git-version-macro" "0.3.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h1s08fgh9bkwnc2hmjxcldv69hlxpq7a09cqdxsd5hb235hq0ak")))))

(define-public rust-gl-generator-0.14.0
  (let ((name "rust-gl-generator")
        (version "0.14.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gl_generator" "0.14.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s")))))

(define-public rust-glam-0.30.0
  (let ((name "rust-glam")
        (version "0.30.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "glam" "0.30.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ihxmcsrkflmhvdwncyn20if55fmq0lzsjhj9pyc41n4hfbdzz0p")))))

(define-public rust-glib-0.20.9
  (let ((name "rust-glib")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "glib" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "11knyc1lgd0bkw42ysl4v2x3v9c7glqz5s9db8wyb7h5z2d82yvh")))))

(define-public rust-glib-macros-0.20.7
  (let ((name "rust-glib-macros")
        (version "0.20.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "glib-macros" "0.20.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0s6yik6pgqg5wydcz5v0x8m1jz57m5bsd50zkkpvlw9fy3w02mki")))))

(define-public rust-glib-sys-0.20.9
  (let ((name "rust-glib-sys")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "glib-sys" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1yxfqf6wllka0am0brqwwj18yb7q9xp79mhprgyd3zaclilqi4m8")))))

(define-public rust-glob-0.3.2
  (let ((name "rust-glob")
        (version "0.3.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "glob" "0.3.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cm2w34b5w45fxr522h5b0fv1bxchfswcj560m3pnjbia7asvld8")))))

(define-public rust-gobject-sys-0.20.9
  (let ((name "rust-gobject-sys")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gobject-sys" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1qz7jrpfk0z8mhnz7fxxx208kc5ljryif7f84sfas6d4735s6wy7")))))

(define-public rust-graphene-rs-0.20.9
  (let ((name "rust-graphene-rs")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "graphene-rs" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "06fy773j9r0v7xzwv3sl2pvw55i68q653jcjzbf6hbdkpw8mkg1w")))))

(define-public rust-graphene-sys-0.20.7
  (let ((name "rust-graphene-sys")
        (version "0.20.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "graphene-sys" "0.20.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0fnjh55lnrd8mgladbapfxak44swlbafqb5pg7l41wsva4wqv9hi")))))

(define-public rust-gsk4-0.9.6
  (let ((name "rust-gsk4")
        (version "0.9.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gsk4" "0.9.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0mgqq5m6cm4q7ajjgw92z13z2ikpvh6zx2gwzdjrz30wjcpygxb1")))))

(define-public rust-gsk4-sys-0.9.6
  (let ((name "rust-gsk4-sys")
        (version "0.9.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gsk4-sys" "0.9.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1p1n4jhhxyvj7hb0cqhzvazrck0qw81sz36ydfj8avzsapg5jl3m")))))

(define-public rust-gtk4-0.9.6
  (let ((name "rust-gtk4")
        (version "0.9.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gtk4" "0.9.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "078911sc8wvnihlz3kq80chl0miz9z2g7rnds17rjc7ha484j75g")))))

(define-public rust-gtk4-macros-0.9.5
  (let ((name "rust-gtk4-macros")
        (version "0.9.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gtk4-macros" "0.9.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "169rqfxfczivcpz7019slsrpkx8crqjka43ymxmikp838xn7il8f")))))

(define-public rust-gtk4-sys-0.9.6
  (let ((name "rust-gtk4-sys")
        (version "0.9.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "gtk4-sys" "0.9.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1mh3xjkjb99y97z234cvyar08vcr7zblg1nrw48c6xsdwl0kpq21")))))

(define-public rust-h2-0.3.26
  (let ((name "rust-h2")
        (version "0.3.26"))
    (origin
      (method url-fetch)
      (uri (crate-uri "h2" "0.3.26"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1")))))
(define-public rust-h2-0.4.8
  (let ((name "rust-h2")
        (version "0.4.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "h2" "0.4.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1hp3lijg1br982kzgglb5ks2ibg68a76z3rl052r8c5vyi7jj5sh")))))
(define-public rust-hashbrown-0.12.3
  (let ((name "rust-hashbrown")
        (version "0.12.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hashbrown" "0.12.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la")))))
(define-public rust-hashbrown-0.13.1
  (let ((name "rust-hashbrown")
        (version "0.13.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hashbrown" "0.13.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0f602rk7pgdhw1s57g81822g7b2m5i2wibrpaqp11afk5kk8mzrk")))))
(define-public rust-hashbrown-0.14.5
  (let ((name "rust-hashbrown")
        (version "0.14.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hashbrown" "0.14.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5")))))
(define-public rust-hashbrown-0.15.2
  (let ((name "rust-hashbrown")
        (version "0.15.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hashbrown" "0.15.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz")))))
(define-public rust-hashlink-0.10.0
  (let ((name "rust-hashlink")
        (version "0.10.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hashlink" "0.10.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h8lzvnl9qxi3zyagivzz2p1hp6shgddfmccyf6jv7s1cdicz0kk")))))
(define-public rust-heck-0.4.1
  (let ((name "rust-heck")
        (version "0.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "heck" "0.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m")))))

(define-public rust-heck-0.5.0
  (let ((name "rust-heck")
        (version "0.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "heck" "0.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113")))))
(define-public rust-hermit-abi-0.3.9
  (let ((name "rust-hermit-abi")
        (version "0.3.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hermit-abi" "0.3.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj")))))
(define-public rust-hermit-abi-0.4.0
  (let ((name "rust-hermit-abi")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hermit-abi" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1k1zwllx6nfq417hy38x4akw1ivlv68ymvnzyxs76ffgsqcskxpv")))))

(define-public rust-hex-0.4.3
  (let ((name "rust-hex")
        (version "0.4.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hex" "0.4.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z")))))
(define-public rust-hkdf-0.12.4
  (let ((name "rust-hkdf")
        (version "0.12.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hkdf" "0.12.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv")))))
(define-public rust-hmac-0.12.1
  (let ((name "rust-hmac")
        (version "0.12.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hmac" "0.12.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc")))))
(define-public rust-home-0.5.11
  (let ((name "rust-home")
        (version "0.5.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "home" "0.5.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq")))))
(define-public rust-http-0.2.12
  (let ((name "rust-http")
        (version "0.2.12"))
    (origin
      (method url-fetch)
      (uri (crate-uri "http" "0.2.12"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730")))))
(define-public rust-http-1.2.0
  (let ((name "rust-http")
        (version "1.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "http" "1.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1skglzdf98j5nzxlii540n11is0w4l80mi5sm3xrj716asps4v7i")))))
(define-public rust-http-body-0.4.6
  (let ((name "rust-http-body")
        (version "0.4.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "http-body" "0.4.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw")))))
(define-public rust-http-body-1.0.1
  (let ((name "rust-http-body")
        (version "1.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "http-body" "1.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy")))))
(define-public rust-http-body-util-0.1.2
  (let ((name "rust-http-body-util")
        (version "0.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "http-body-util" "0.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kslwazg4400qnc2azkrgqqci0fppv12waicnsy5d8hncvbjjd3r")))))
(define-public rust-httparse-1.10.0
  (let ((name "rust-httparse")
        (version "1.10.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "httparse" "1.10.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ahyh51rrpizd7m1jy5p791xfwn9cnmv0snd2q528h3i9vghimzj")))))
(define-public rust-httpdate-1.0.3
  (let ((name "rust-httpdate")
        (version "1.0.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "httpdate" "1.0.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz")))))
(define-public rust-humantime-2.1.0
  (let ((name "rust-humantime")
        (version "2.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "humantime" "2.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls")))))
(define-public rust-hyper-0.14.32
  (let ((name "rust-hyper")
        (version "0.14.32"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hyper" "0.14.32"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1rvcb0smz8q1i0y6p7rwxr02x5sclfg2hhxf3g0774zczn0cgps1")))))
(define-public rust-hyper-1.6.0
  (let ((name "rust-hyper")
        (version "1.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hyper" "1.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "103ggny2k31z0iq2gzwk2vbx601wx6xkpjpxn40hr3p3b0b5fayc")))))
(define-public rust-hyper-rustls-0.24.2
  (let ((name "rust-hyper-rustls")
        (version "0.24.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hyper-rustls" "0.24.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc")))))
(define-public rust-hyper-rustls-0.27.5
  (let ((name "rust-hyper-rustls")
        (version "0.27.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hyper-rustls" "0.27.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cjr3yf3x5mr3194llsfibacl6j7n2dknii2dwjha4ysyf1ia69d")))))
(define-public rust-hyper-timeout-0.5.2
  (let ((name "rust-hyper-timeout")
        (version "0.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hyper-timeout" "0.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1c431l5ckr698248yd6bnsmizjy2m1da02cbpmsnmkpvpxkdb41b")))))
(define-public rust-hyper-util-0.1.10
  (let ((name "rust-hyper-util")
        (version "0.1.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "hyper-util" "0.1.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1d1iwrkysjhq63pg54zk3vfby1j7zmxzm9zzyfr4lwvp0szcybfz")))))
(define-public rust-iana-time-zone-0.1.61
  (let ((name "rust-iana-time-zone")
        (version "0.1.61"))
    (origin
      (method url-fetch)
      (uri (crate-uri "iana-time-zone" "0.1.61"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "085jjsls330yj1fnwykfzmb2f10zp6l7w4fhq81ng81574ghhpi3")))))
(define-public rust-iana-time-zone-haiku-0.1.2
  (let ((name "rust-iana-time-zone-haiku")
        (version "0.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "iana-time-zone-haiku" "0.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k")))))
(define-public rust-icu-collections-1.5.0
  (let ((name "rust-icu-collections")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_collections" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv")))))
(define-public rust-icu-locid-1.5.0
  (let ((name "rust-icu-locid")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_locid" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k")))))
(define-public rust-icu-locid-transform-1.5.0
  (let ((name "rust-icu-locid-transform")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_locid_transform" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81")))))
(define-public rust-icu-locid-transform-data-1.5.0
  (let ((name "rust-icu-locid-transform-data")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_locid_transform_data" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vkgjixm0wzp2n3v5mw4j89ly05bg3lx96jpdggbwlpqi0rzzj7x")))))
(define-public rust-icu-normalizer-1.5.0
  (let ((name "rust-icu-normalizer")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_normalizer" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr")))))
(define-public rust-icu-normalizer-data-1.5.0
  (let ((name "rust-icu-normalizer-data")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_normalizer_data" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05lmk0zf0q7nzjnj5kbmsigj3qgr0rwicnn5pqi9n7krmbvzpjpq")))))
(define-public rust-icu-properties-1.5.1
  (let ((name "rust-icu-properties")
        (version "1.5.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_properties" "1.5.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk")))))
(define-public rust-icu-properties-data-1.5.0
  (let ((name "rust-icu-properties-data")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_properties_data" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0scms7pd5a7yxx9hfl167f5qdf44as6r3bd8myhlngnxqgxyza37")))))
(define-public rust-icu-provider-1.5.0
  (let ((name "rust-icu-provider")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_provider" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f")))))
(define-public rust-icu-provider-macros-1.5.0
  (let ((name "rust-icu-provider-macros")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "icu_provider_macros" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y")))))
(define-public rust-ident-case-1.0.1
  (let ((name "rust-ident-case")
        (version "1.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ident_case" "1.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r")))))
(define-public rust-idna-1.0.3
  (let ((name "rust-idna")
        (version "1.0.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "idna" "1.0.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8")))))
(define-public rust-idna-adapter-1.2.0
  (let ((name "rust-idna-adapter")
        (version "1.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "idna_adapter" "1.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns")))))
(define-public rust-image-0.25.5
  (let ((name "rust-image")
        (version "0.25.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "image" "0.25.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0fsnfgg8hr66ag5nxipvb7d50kbg40qfpbsql59qkwa2ssp48vyd")))))
(define-public rust-indenter-0.3.3
  (let ((name "rust-indenter")
        (version "0.3.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "indenter" "0.3.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf")))))
(define-public rust-indexmap-1.9.3
  (let ((name "rust-indexmap")
        (version "1.9.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "indexmap" "1.9.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx")))))
(define-public rust-indexmap-2.7.1
  (let ((name "rust-indexmap")
        (version "2.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "indexmap" "2.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0lmnm1zbr5gq3wic3d8a76gpvampridzwckfl97ckd5m08mrk74c")))))
(define-public rust-indicatif-0.17.11
  (let ((name "rust-indicatif")
        (version "0.17.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "indicatif" "0.17.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0db2b2r79r9x8x4lysq1ci9xm13c0xg0sqn3z960yh2bk2430fqq")))))
(define-public rust-inout-0.1.4
  (let ((name "rust-inout")
        (version "0.1.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "inout" "0.1.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7")))))
(define-public rust-input-0.9.1
  (let ((name "rust-input")
        (version "0.9.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "input" "0.9.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1abmv1djhynihipjppgsmw6nbp6pcgzk8rzi4v6wmyci9990kp7v")))))

(define-public rust-input-sys-1.18.0
  (let ((name "rust-input-sys")
        (version "1.18.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "input-sys" "1.18.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1c4y24wf0jixi52js4f7cjspbgi0bzzaqfhn8m91qcq03i6mnkxx")))))

(define-public rust-insta-1.42.1
  (let ((name "rust-insta")
        (version "1.42.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "insta" "1.42.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "11law2fzalj7pb5sy044xr2spzyzv8rnq5ay6rmqk4qdwcjv3hbi")))))

(define-public rust-interim-0.1.2
  (let ((name "rust-interim")
        (version "0.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "interim" "0.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x5ykyv8bkv13398q3dpycg5943rw1jycvjbhi2yih30zw5hzzcs")))))
(define-public rust-io-lifetimes-1.0.11
  (let ((name "rust-io-lifetimes")
        (version "1.0.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "io-lifetimes" "1.0.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1hph5lz4wd3drnn6saakwxr497liznpfnv70via6s0v8x6pbkrza")))))

(define-public rust-ipnet-2.11.0
  (let ((name "rust-ipnet")
        (version "2.11.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ipnet" "2.11.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6")))))
(define-public rust-is-ci-1.2.0
  (let ((name "rust-is-ci")
        (version "1.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "is_ci" "1.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ifwvxmrsj4r29agfzr71bjq6y1bihkx38fbzafq5vl0jn1wjmbn")))))

(define-public rust-is-terminal-0.4.15
  (let ((name "rust-is-terminal")
        (version "0.4.15"))
    (origin
      (method url-fetch)
      (uri (crate-uri "is-terminal" "0.4.15"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dzdvjg3f10cfv8wi1dcnw6rq7mcwss1nzdwmrb9zkim7zaj76z1")))))

(define-public rust-is-terminal-polyfill-1.70.1
  (let ((name "rust-is-terminal-polyfill")
        (version "1.70.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "is_terminal_polyfill" "1.70.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr")))))
(define-public rust-iso8601-0.6.2
  (let ((name "rust-iso8601")
        (version "0.6.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "iso8601" "0.6.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "13f9a6izrm87dd66qcagw65lw714072a8y8hyjk23ar4z37pghf5")))))
(define-public rust-itertools-0.12.1
  (let ((name "rust-itertools")
        (version "0.12.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "itertools" "0.12.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads")))))

(define-public rust-itertools-0.13.0
  (let ((name "rust-itertools")
        (version "0.13.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "itertools" "0.13.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1")))))
(define-public rust-itertools-0.14.0
  (let ((name "rust-itertools")
        (version "0.14.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "itertools" "0.14.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b")))))
(define-public rust-itoa-1.0.14
  (let ((name "rust-itoa")
        (version "1.0.14"))
    (origin
      (method url-fetch)
      (uri (crate-uri "itoa" "1.0.14"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0x26kr9m062mafaxgcf2p6h2x7cmixm0zw95aipzn2hr3d5jlnnp")))))
(define-public rust-jni-0.21.1
  (let ((name "rust-jni")
        (version "0.21.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "jni" "0.21.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs")))))

(define-public rust-jni-sys-0.3.0
  (let ((name "rust-jni-sys")
        (version "0.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "jni-sys" "0.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf")))))

(define-public rust-jobserver-0.1.32
  (let ((name "rust-jobserver")
        (version "0.1.32"))
    (origin
      (method url-fetch)
      (uri (crate-uri "jobserver" "0.1.32"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8")))))

(define-public rust-jpeg-decoder-0.3.1
  (let ((name "rust-jpeg-decoder")
        (version "0.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "jpeg-decoder" "0.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1c1k53svpdyfhibkmm0ir5w0v3qmcmca8xr8vnnmizwf6pdagm7m")))))
(define-public rust-js-sys-0.3.77
  (let ((name "rust-js-sys")
        (version "0.3.77"))
    (origin
      (method url-fetch)
      (uri (crate-uri "js-sys" "0.3.77"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw")))))
(define-public rust-keyframe-1.1.1
  (let ((name "rust-keyframe")
        (version "1.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "keyframe" "1.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1afr5ffns3k79xaqnw6rw3qn8sngwly6gxfnjn8d060mk3vqnw30")))))

(define-public rust-khronos-api-3.1.0
  (let ((name "rust-khronos-api")
        (version "3.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "khronos_api" "3.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2")))))

(define-public rust-knuffel-3.2.0
  (let ((name "rust-knuffel")
        (version "3.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "knuffel" "3.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04vl2xmdn280rcigv96v06a00v7gbxqggr0w9cqi2407qvfydgh4")))))

(define-public rust-knuffel-derive-3.2.0
  (let ((name "rust-knuffel-derive")
        (version "3.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "knuffel-derive" "3.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0g98909l5wb1d1hcz61q53kvsmjadry2w3l47lg9dywwqib7z5wi")))))

(define-public rust-lazy-static-1.5.0
  (let ((name "rust-lazy-static")
        (version "1.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "lazy_static" "1.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv")))))
(define-public rust-lazycell-1.3.0
  (let ((name "rust-lazycell")
        (version "1.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "lazycell" "1.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3")))))

(define-public rust-libadwaita-0.7.1
  (let ((name "rust-libadwaita")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libadwaita" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0is205jr9nc1ynmcm62dkvvm765myp5ay4xmcb1hcxjyp2gyw4c6")))))

(define-public rust-libadwaita-sys-0.7.1
  (let ((name "rust-libadwaita-sys")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libadwaita-sys" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1y9sr3glrda5ddzgngfqw4gigdx57qzx5dh4zbad8601aqis56dh")))))

(define-public rust-libc-0.2.170
  (let ((name "rust-libc")
        (version "0.2.170"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libc" "0.2.170"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0a38q3avb6r6azxb7yfbjly5sbr8926z6c4sryyp33rgrf03cnw7")))))
(define-public rust-libdisplay-info-0.2.2
  (let ((name "rust-libdisplay-info")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libdisplay-info" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0avs90mwzbfkwc09xlvvdy0szrbi670y61c1w0l75hqd7blwy422")))))

(define-public rust-libdisplay-info-derive-0.1.0
  (let ((name "rust-libdisplay-info-derive")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libdisplay-info-derive" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1vpwss66rmhdd0f85c3nwjshddmiarf4iya5v13aacmp6q8d677a")))))

(define-public rust-libdisplay-info-sys-0.2.2
  (let ((name "rust-libdisplay-info-sys")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libdisplay-info-sys" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0v34vjczpj8hzxnx1nj5cxwf326m91gn7bi3l3zkfg72xij94kvz")))))

(define-public rust-libloading-0.8.6
  (let ((name "rust-libloading")
        (version "0.8.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libloading" "0.8.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0d2ccr88f8kv3x7va2ccjxalcjnhrci4j2kwxp7lfmbkpjs4wbzw")))))

(define-public rust-libm-0.2.11
  (let ((name "rust-libm")
        (version "0.2.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libm" "0.2.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1yjgk18rk71rjbqcw9l1zaqna89p9s603k7n327nqs8dn88vwmc3")))))
(define-public rust-libredox-0.1.3
  (let ((name "rust-libredox")
        (version "0.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libredox" "0.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0")))))
(define-public rust-libseat-0.2.3
  (let ((name "rust-libseat")
        (version "0.2.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libseat" "0.2.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0350b89h2xk5rdqqla162a2pak7yzbpfckqwg68cd42ppmdj8fn2")))))

(define-public rust-libseat-sys-0.1.9
  (let ((name "rust-libseat-sys")
        (version "0.1.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libseat-sys" "0.1.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0997n2s413ggzi4amy0jbnfl8jvgpjnkxzycjs56ks2p0pjj2ihk")))))

(define-public rust-libspa-0.8.0
  (let ((name "rust-libspa")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libspa" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "044qs48yl0llp2dmrgwxj9y1pgfy09i6fhq661zqqb9a3fwa9wv5"))
      (patches (rosenthal-patches "rust-libspa.patch")))))

(define-public rust-libspa-sys-0.8.0
  (let ((name "rust-libspa-sys")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libspa-sys" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "07yh4i5grzbxkchg6dnxlwbdw2wm5jnd7ffbhl77jr0388b9f3dz"))
      (patches (rosenthal-patches "rust-libspa-sys.patch")))))

(define-public rust-libsqlite3-sys-0.30.1
  (let ((name "rust-libsqlite3-sys")
        (version "0.30.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libsqlite3-sys" "0.30.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f")))))
(define-public rust-libudev-sys-0.1.4
  (let ((name "rust-libudev-sys")
        (version "0.1.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "libudev-sys" "0.1.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w")))))

(define-public rust-linked-hash-map-0.5.6
  (let ((name "rust-linked-hash-map")
        (version "0.5.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "linked-hash-map" "0.5.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7")))))

(define-public rust-linux-raw-sys-0.4.15
  (let ((name "rust-linux-raw-sys")
        (version "0.4.15"))
    (origin
      (method url-fetch)
      (uri (crate-uri "linux-raw-sys" "0.4.15"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j")))))
(define-public rust-linux-raw-sys-0.6.5
  (let ((name "rust-linux-raw-sys")
        (version "0.6.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "linux-raw-sys" "0.6.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1mv3c1zz51ydcj768zavm8g06gz5jb1p7yigmmif7hz5whdmnf1a")))))

(define-public rust-listenfd-1.0.2
  (let ((name "rust-listenfd")
        (version "1.0.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "listenfd" "1.0.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1flxwfgp6rrcgg09szb7g84i3ij09jv43w1y1d6jkd198r5cayxq")))))
(define-public rust-litemap-0.7.4
  (let ((name "rust-litemap")
        (version "0.7.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "litemap" "0.7.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "012ili3vppd4952sh6y3qwcd0jkd0bq2qpr9h7cppc8sj11k7saf")))))
(define-public rust-lock-api-0.4.12
  (let ((name "rust-lock-api")
        (version "0.4.12"))
    (origin
      (method url-fetch)
      (uri (crate-uri "lock_api" "0.4.12"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7")))))
(define-public rust-log-0.4.26
  (let ((name "rust-log")
        (version "0.4.26"))
    (origin
      (method url-fetch)
      (uri (crate-uri "log" "0.4.26"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17mvchkvhnm2zxyfagh2g9p861f0qx2g1sg2v14sww9nvjry5g9h")))))
(define-public rust-logos-0.14.4
  (let ((name "rust-logos")
        (version "0.14.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "logos" "0.14.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0n349vin9mx326fkz68bsa4vc5sdn9n8qnfz7n1yqynbz1p3albj")))))
(define-public rust-logos-codegen-0.14.4
  (let ((name "rust-logos-codegen")
        (version "0.14.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "logos-codegen" "0.14.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gwnx7lk4y7xc4yk6pr0knrddard5z22rxaz9xrnc38cc1lh1y2r")))))
(define-public rust-logos-derive-0.14.4
  (let ((name "rust-logos-derive")
        (version "0.14.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "logos-derive" "0.14.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "07bk3q4jry9f8blrnsiy872ivilzy62xaglnn2ni5p590qmp5yr4")))))
(define-public rust-loom-0.7.2
  (let ((name "rust-loom")
        (version "0.7.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "loom" "0.7.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jpszf9qxv8ydpsm2h9vcyvxvyxcfkhmmfbylzd4gfbc0k40v7j1")))))

(define-public rust-lru-0.12.5
  (let ((name "rust-lru")
        (version "0.12.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "lru" "0.12.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0f1a7cgqxbyhrmgaqqa11m3azwhcc36w0v5r4izgbhadl3sg8k13")))))
(define-public rust-mach2-0.4.2
  (let ((name "rust-mach2")
        (version "0.4.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "mach2" "0.4.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "02gpyq89rcrqdbz4hgp5bpjas21dllxfc70jgw8vj0iaxg6mbf8r")))))
(define-public rust-matchers-0.1.0
  (let ((name "rust-matchers")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "matchers" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2")))))
(define-public rust-matchit-0.7.3
  (let ((name "rust-matchit")
        (version "0.7.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "matchit" "0.7.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "156bgdmmlv4crib31qhgg49nsjk88dxkdqp80ha2pk2rk6n6ax0f")))))
(define-public rust-md-5-0.10.6
  (let ((name "rust-md-5")
        (version "0.10.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "md-5" "0.10.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq")))))
(define-public rust-memchr-2.7.4
  (let ((name "rust-memchr")
        (version "2.7.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "memchr" "2.7.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq")))))
(define-public rust-memmap2-0.9.5
  (let ((name "rust-memmap2")
        (version "0.9.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "memmap2" "0.9.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0krpvvkpg4i3l05cv3q2xk24a1vj5c86gbrli2wzhj1qkpnpwgzx")))))

(define-public rust-memoffset-0.9.1
  (let ((name "rust-memoffset")
        (version "0.9.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "memoffset" "0.9.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028")))))

(define-public rust-metrics-0.21.1
  (let ((name "rust-metrics")
        (version "0.21.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "metrics" "0.21.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ibndxzk0sja8cgwrr73b9vzbgfvwzwxwkxqiivnmmwy00dazqzx")))))
(define-public rust-metrics-exporter-prometheus-0.12.2
  (let ((name "rust-metrics-exporter-prometheus")
        (version "0.12.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "metrics-exporter-prometheus" "0.12.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0l19s21jfmwm72cxfjq35xb79a5wi4fv7c1p993dnqj8gk7afkqx")))))
(define-public rust-metrics-macros-0.7.1
  (let ((name "rust-metrics-macros")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "metrics-macros" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0krmj7zyr4g14jdpk1jasi1w2nw64hqdxb2lfx4zxphp0vqgmd1q")))))
(define-public rust-metrics-util-0.15.1
  (let ((name "rust-metrics-util")
        (version "0.15.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "metrics-util" "0.15.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0glpkmrj7zkg9b290x6qxf93kmd9b4b4sbkk1fs19l8y95pfvqjd")))))
(define-public rust-miette-5.10.0
  (let ((name "rust-miette")
        (version "5.10.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "miette" "5.10.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vl5qvl3bgha6nnkdl7kiha6v4ypd6d51wyc4q1bvdpamr75ifsr")))))

(define-public rust-miette-7.5.0
  (let ((name "rust-miette")
        (version "7.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "miette" "7.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "114lv0nx46lxc5pncz6iyrzcfhn5g9a5janzc8cgsdvvz1jm358s")))))
(define-public rust-miette-derive-5.10.0
  (let ((name "rust-miette-derive")
        (version "5.10.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "miette-derive" "5.10.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0p33msrngkxlp5ajm8nijamii9vcwwpy8gfh4m53qnmrc0avrrs9")))))

(define-public rust-miette-derive-7.5.0
  (let ((name "rust-miette-derive")
        (version "7.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "miette-derive" "7.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0irig3c79184h54zasn06yiz25znqrpvx8r72byr5gj9md2byidz")))))
(define-public rust-mime-0.3.17
  (let ((name "rust-mime")
        (version "0.3.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "mime" "0.3.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8")))))
(define-public rust-minimal-lexical-0.2.1
  (let ((name "rust-minimal-lexical")
        (version "0.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "minimal-lexical" "0.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8")))))
(define-public rust-miniz-oxide-0.8.5
  (let ((name "rust-miniz-oxide")
        (version "0.8.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "miniz_oxide" "0.8.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1r9whkc61xri7m1cn4rjrjlhr32ab29nvfxcbg0ri5mmpgg08glf")))))
(define-public rust-minspan-0.1.2
  (let ((name "rust-minspan")
        (version "0.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "minspan" "0.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0053r44iqmfilibz8da3367adxjjwibw6d849xifxq0yhfgf99pf")))))
(define-public rust-mio-0.8.11
  (let ((name "rust-mio")
        (version "0.8.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "mio" "0.8.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4")))))
(define-public rust-mio-1.0.3
  (let ((name "rust-mio")
        (version "1.0.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "mio" "1.0.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8")))))
(define-public rust-multimap-0.10.0
  (let ((name "rust-multimap")
        (version "0.10.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "multimap" "0.10.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "00vs2frqdhrr8iqx4y3fbq73ax5l12837fvbjrpi729d85alrz6y")))))
(define-public rust-ndk-0.9.0
  (let ((name "rust-ndk")
        (version "0.9.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ndk" "0.9.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1m32zpmi5w1pf3j47k6k5fw395dc7aj8d0mdpsv53lqkprxjxx63")))))

(define-public rust-ndk-context-0.1.1
  (let ((name "rust-ndk-context")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ndk-context" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17")))))

(define-public rust-ndk-sys-0.6.0+11769913
  (let ((name "rust-ndk-sys")
        (version "0.6.0+11769913"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ndk-sys" "0.6.0+11769913"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wx8r6pji20if4xs04g73gxl98nmjrfc73z0v6w1ypv6a4qdlv7f")))))

(define-public rust-nix-0.27.1
  (let ((name "rust-nix")
        (version "0.27.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "nix" "0.27.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ly0kkmij5f0sqz35lx9czlbk6zpihb7yh1bsy4irzwfd2f4xc1f")))))

(define-public rust-nix-0.28.0
  (let ((name "rust-nix")
        (version "0.28.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "nix" "0.28.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1r0rylax4ycx3iqakwjvaa178jrrwiiwghcw95ndzy72zk25c8db")))))
(define-public rust-nix-0.29.0
  (let ((name "rust-nix")
        (version "0.29.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "nix" "0.29.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki")))))

(define-public rust-nom-7.1.3
  (let ((name "rust-nom")
        (version "7.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "nom" "7.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj")))))
(define-public rust-nom-8.0.0
  (let ((name "rust-nom")
        (version "8.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "nom" "8.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz")))))
(define-public rust-ntapi-0.4.1
  (let ((name "rust-ntapi")
        (version "0.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ntapi" "0.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1r38zhbwdvkis2mzs6671cm1p6djgsl49i7bwxzrvhwicdf8k8z8")))))
(define-public rust-nu-ansi-term-0.46.0
  (let ((name "rust-nu-ansi-term")
        (version "0.46.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "nu-ansi-term" "0.46.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p")))))
(define-public rust-nu-ansi-term-0.50.1
  (let ((name "rust-nu-ansi-term")
        (version "0.50.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "nu-ansi-term" "0.50.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl")))))
(define-public rust-num-bigint-dig-0.8.4
  (let ((name "rust-num-bigint-dig")
        (version "0.8.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num-bigint-dig" "0.8.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0lb12df24wgxxbspz4gw1sf1kdqwvpdcpwq4fdlwg4gj41c1k16w")))))
(define-public rust-num-conv-0.1.0
  (let ((name "rust-num-conv")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num-conv" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai")))))
(define-public rust-num-cpus-1.16.0
  (let ((name "rust-num-cpus")
        (version "1.16.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num_cpus" "1.16.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1")))))

(define-public rust-num-enum-0.7.3
  (let ((name "rust-num-enum")
        (version "0.7.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num_enum" "0.7.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0yai0vafhy85mvhknzfqd7lm04hzaln7i5c599rhy8mj831kyqaf")))))

(define-public rust-num-enum-derive-0.7.3
  (let ((name "rust-num-enum-derive")
        (version "0.7.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num_enum_derive" "0.7.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0mksna1jj87ydh146gn6jcqkvvs920c3dgh0p4f3xk184kpl865g")))))

(define-public rust-num-integer-0.1.46
  (let ((name "rust-num-integer")
        (version "0.1.46"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num-integer" "0.1.46"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr")))))
(define-public rust-num-iter-0.1.45
  (let ((name "rust-num-iter")
        (version "0.1.45"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num-iter" "0.1.45"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gzm7vc5g9qsjjl3bqk9rz1h6raxhygbrcpbfl04swlh0i506a8l")))))
(define-public rust-num-threads-0.1.7
  (let ((name "rust-num-threads")
        (version "0.1.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num_threads" "0.1.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ngajbmhrgyhzrlc4d5ga9ych1vrfcvfsiqz6zv0h2dpr2wrhwsw")))))

(define-public rust-num-traits-0.2.19
  (let ((name "rust-num-traits")
        (version "0.2.19"))
    (origin
      (method url-fetch)
      (uri (crate-uri "num-traits" "0.2.19"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787")))))
(define-public rust-number-prefix-0.4.0
  (let ((name "rust-number-prefix")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "number_prefix" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1wvh13wvlajqxkb1filsfzbrnq0vrmrw298v2j3sy82z1rm282w3")))))
(define-public rust-objc-sys-0.3.5
  (let ((name "rust-objc-sys")
        (version "0.3.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc-sys" "0.3.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd")))))
(define-public rust-objc2-0.5.2
  (let ((name "rust-objc2")
        (version "0.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2" "0.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6")))))
(define-public rust-objc2-app-kit-0.2.2
  (let ((name "rust-objc2-app-kit")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-app-kit" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74")))))
(define-public rust-objc2-cloud-kit-0.2.2
  (let ((name "rust-objc2-cloud-kit")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-cloud-kit" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "02dhjvmcq8c2bwj31jx423jygif1scs9f0lmlab0ayhw75b3ppbl")))))

(define-public rust-objc2-contacts-0.2.2
  (let ((name "rust-objc2-contacts")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-contacts" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12a8m927xrrxa54xhqhqnkkl1a6l07pyrpnqfk9jz09kkh755zx5")))))

(define-public rust-objc2-core-data-0.2.2
  (let ((name "rust-objc2-core-data")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-core-data" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1")))))
(define-public rust-objc2-core-image-0.2.2
  (let ((name "rust-objc2-core-image")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-core-image" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm")))))
(define-public rust-objc2-core-location-0.2.2
  (let ((name "rust-objc2-core-location")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-core-location" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10apgsrigqryvi4rcc0f6yfjflvrl83f4bi5hkr48ck89vizw300")))))

(define-public rust-objc2-encode-4.1.0
  (let ((name "rust-objc2-encode")
        (version "4.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-encode" "4.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg")))))
(define-public rust-objc2-foundation-0.2.2
  (let ((name "rust-objc2-foundation")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-foundation" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf")))))
(define-public rust-objc2-link-presentation-0.2.2
  (let ((name "rust-objc2-link-presentation")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-link-presentation" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "160k4qh00yrx57dabn3hzas4r98kmk9bc0qsy1jvwday3irax8d1")))))

(define-public rust-objc2-metal-0.2.2
  (let ((name "rust-objc2-metal")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-metal" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x")))))
(define-public rust-objc2-quartz-core-0.2.2
  (let ((name "rust-objc2-quartz-core")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-quartz-core" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4")))))
(define-public rust-objc2-symbols-0.2.2
  (let ((name "rust-objc2-symbols")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-symbols" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1p04hjkxan18g2b7h9n2n8xxsvazapv2h6mfmmdk06zc7pz4ws0a")))))

(define-public rust-objc2-ui-kit-0.2.2
  (let ((name "rust-objc2-ui-kit")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-ui-kit" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vrb5r8z658l8c19bx78qks8c5hg956544yirf8npk90idwldfxq")))))

(define-public rust-objc2-uniform-type-identifiers-0.2.2
  (let ((name "rust-objc2-uniform-type-identifiers")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-uniform-type-identifiers" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ziv4wkbxcaw015ypg0q49ycl7m14l3x56mpq2k1rznv92bmzyj4")))))

(define-public rust-objc2-user-notifications-0.2.2
  (let ((name "rust-objc2-user-notifications")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "objc2-user-notifications" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cscv2w3vxzaslz101ddv0z9ycrrs4ayikk4my4qd3im8bvcpkvn")))))

(define-public rust-object-0.36.7
  (let ((name "rust-object")
        (version "0.36.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "object" "0.36.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532")))))
(define-public rust-once-cell-1.20.3
  (let ((name "rust-once-cell")
        (version "1.20.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "once_cell" "1.20.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bp6rgrsri1vfdcahsimk08zdiilv14ppgcnpbiw8hqyp2j64m4l")))))
(define-public rust-opaque-debug-0.3.1
  (let ((name "rust-opaque-debug")
        (version "0.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "opaque-debug" "0.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10b3w0kydz5jf1ydyli5nv10gdfp97xh79bgz327d273bs46b3f0")))))
(define-public rust-openssl-probe-0.1.6
  (let ((name "rust-openssl-probe")
        (version "0.1.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "openssl-probe" "0.1.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh")))))
(define-public rust-option-ext-0.2.0
  (let ((name "rust-option-ext")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "option-ext" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04")))))
(define-public rust-orbclient-0.3.48
  (let ((name "rust-orbclient")
        (version "0.3.48"))
    (origin
      (method url-fetch)
      (uri (crate-uri "orbclient" "0.3.48"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hzxjsvvsl5i9d3aqzc6kdcsch1i6flij5dkignhhkz2qb72c2xs")))))

(define-public rust-ordered-float-5.0.0
  (let ((name "rust-ordered-float")
        (version "5.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ordered-float" "5.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "009z1k7w729ls2sfg4zknn9v63sk1zghnq54p2lwcjjkdvszkhg2")))))

(define-public rust-ordered-stream-0.2.0
  (let ((name "rust-ordered-stream")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ordered-stream" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls")))))

(define-public rust-os-pipe-1.2.1
  (let ((name "rust-os-pipe")
        (version "1.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "os_pipe" "1.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10nrh0i507560rsiy4c79fajdmqgbr6dha2pbl9mncrlaq52pzaz")))))
(define-public rust-overload-0.1.1
  (let ((name "rust-overload")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "overload" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i")))))
(define-public rust-owo-colors-3.5.0
  (let ((name "rust-owo-colors")
        (version "3.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "owo-colors" "3.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vyvry6ba1xmpd45hpi6savd8mbx09jpmvnnwkf6z62pk6s4zc61")))))

(define-public rust-palette-0.7.6
  (let ((name "rust-palette")
        (version "0.7.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "palette" "0.7.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1rmn02mv6cb112504qyg7pyfa83c08hxpk5sw7jc5v659hc73gsc")))))
(define-public rust-palette-derive-0.7.6
  (let ((name "rust-palette-derive")
        (version "0.7.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "palette_derive" "0.7.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0c0xhpk1nqyq4jr2m8xnka7w47vqzc7m2vq9ih8wxyjv02phs0zm")))))
(define-public rust-pango-0.20.9
  (let ((name "rust-pango")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pango" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1v7h4m7sz0x38il14jzsw7qphbpsw17a0hq8zj5w16ygp30ms7vb")))))

(define-public rust-pango-sys-0.20.9
  (let ((name "rust-pango-sys")
        (version "0.20.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pango-sys" "0.20.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1dds8ljd3ar05c9744s3xlcyg8bkg5a211mpkvj8zgbk2rsrpfqd")))))

(define-public rust-pangocairo-0.20.7
  (let ((name "rust-pangocairo")
        (version "0.20.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pangocairo" "0.20.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1dlp76pkknxfl6pi41zfcm9kdhchyzjs72pgl196aapa5yd51426")))))

(define-public rust-pangocairo-sys-0.20.7
  (let ((name "rust-pangocairo-sys")
        (version "0.20.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pangocairo-sys" "0.20.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rv7mnp2cnrvaq73c3dwf1szc0ngi312z4l3cyjac4br2hjarrjv")))))

(define-public rust-parking-2.2.1
  (let ((name "rust-parking")
        (version "2.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "parking" "2.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk")))))
(define-public rust-parking-lot-0.12.3
  (let ((name "rust-parking-lot")
        (version "0.12.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "parking_lot" "0.12.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi")))))
(define-public rust-parking-lot-core-0.9.10
  (let ((name "rust-parking-lot-core")
        (version "0.9.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "parking_lot_core" "0.9.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y")))))
(define-public rust-password-hash-0.5.0
  (let ((name "rust-password-hash")
        (version "0.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "password-hash" "0.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ri1mim11zk0a9s40zdi288dfqvmdiryc7lw8vl46b59ifa08vrl")))))
(define-public rust-paste-1.0.15
  (let ((name "rust-paste")
        (version "1.0.15"))
    (origin
      (method url-fetch)
      (uri (crate-uri "paste" "1.0.15"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p")))))
(define-public rust-pathdiff-0.2.3
  (let ((name "rust-pathdiff")
        (version "0.2.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pathdiff" "0.2.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z")))))
(define-public rust-pbkdf2-0.11.0
  (let ((name "rust-pbkdf2")
        (version "0.11.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pbkdf2" "0.11.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05q9wqjvfrs4dvw03yn3bvcs4zghz0a7ycfa53pz2k2fqhp6k843")))))
(define-public rust-pem-rfc7468-0.7.0
  (let ((name "rust-pem-rfc7468")
        (version "0.7.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pem-rfc7468" "0.7.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8")))))
(define-public rust-percent-encoding-2.3.1
  (let ((name "rust-percent-encoding")
        (version "2.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "percent-encoding" "2.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573")))))
(define-public rust-petgraph-0.6.5
  (let ((name "rust-petgraph")
        (version "0.6.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "petgraph" "0.6.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl")))))
(define-public rust-petgraph-0.7.1
  (let ((name "rust-petgraph")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "petgraph" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wkpppwrfv1h197asz1p4yfb4li5b1kw0nqllil67n6vj1qb6win")))))
(define-public rust-phf-0.11.3
  (let ((name "rust-phf")
        (version "0.11.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "phf" "0.11.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz")))))
(define-public rust-phf-generator-0.11.3
  (let ((name "rust-phf-generator")
        (version "0.11.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "phf_generator" "0.11.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w")))))
(define-public rust-phf-macros-0.11.3
  (let ((name "rust-phf-macros")
        (version "0.11.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "phf_macros" "0.11.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05kjfbyb439344rhmlzzw0f9bwk9fp95mmw56zs7yfn1552c0jpq")))))
(define-public rust-phf-shared-0.11.3
  (let ((name "rust-phf-shared")
        (version "0.11.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "phf_shared" "0.11.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7")))))
(define-public rust-pin-project-1.1.9
  (let ((name "rust-pin-project")
        (version "1.1.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pin-project" "1.1.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0z9jgkcqg00fxjrbbcjlkiy940hbf5gp5gq6jiq0gzki2hgfgqnz")))))
(define-public rust-pin-project-internal-1.1.9
  (let ((name "rust-pin-project-internal")
        (version "1.1.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pin-project-internal" "1.1.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rzsqcf4mdrpx39w6wvw3wjjc3y9mgmy6irwnq548l5xwpk5ks7n")))))
(define-public rust-pin-project-lite-0.2.16
  (let ((name "rust-pin-project-lite")
        (version "0.2.16"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pin-project-lite" "0.2.16"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v")))))
(define-public rust-pin-utils-0.1.0
  (let ((name "rust-pin-utils")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pin-utils" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb")))))
(define-public rust-piper-0.2.4
  (let ((name "rust-piper")
        (version "0.2.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "piper" "0.2.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rn0mjjm0cwagdkay77wgmz3sqf8fqmv9d9czm79mvr2yj8c9j4n")))))

(define-public rust-pipewire-0.8.0
  (let ((name "rust-pipewire")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pipewire" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nldg1hz4v0qr26lzdxqpvrac4zbc3pb6436sl392425bjx4brh8"))
      (patches (rosenthal-patches "rust-pipewire.patch")))))

(define-public rust-pipewire-sys-0.8.0
  (let ((name "rust-pipewire-sys")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pipewire-sys" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04hiy3rl8v3j2dfzp04gr7r8l5azzqqsvqdzwa7sipdij27ii7l4")))))

(define-public rust-pixman-0.2.1
  (let ((name "rust-pixman")
        (version "0.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pixman" "0.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pqybqb7rmd58yr9xvmd8iix30znw5w71cq2wnlc16n1jva1g8nf")))))

(define-public rust-pixman-sys-0.1.0
  (let ((name "rust-pixman-sys")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pixman-sys" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nja8kc7zs1w4lhllvsgssa0b07n4cgwb0zyvqapj7g8i4z4i851")))))

(define-public rust-pkcs1-0.7.5
  (let ((name "rust-pkcs1")
        (version "0.7.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pkcs1" "0.7.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0zz4mil3nchnxljdfs2k5ab1cjqn7kq5lqp62n9qfix01zqvkzy8")))))
(define-public rust-pkcs8-0.10.2
  (let ((name "rust-pkcs8")
        (version "0.10.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pkcs8" "0.10.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r")))))
(define-public rust-pkg-config-0.3.31
  (let ((name "rust-pkg-config")
        (version "0.3.31"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pkg-config" "0.3.31"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm")))))
(define-public rust-png-0.17.16
  (let ((name "rust-png")
        (version "0.17.16"))
    (origin
      (method url-fetch)
      (uri (crate-uri "png" "0.17.16"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2")))))
(define-public rust-polling-3.7.4
  (let ((name "rust-polling")
        (version "3.7.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "polling" "3.7.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bs4nhwfwsvlzlhah2gbhj3aa9ynvchv2g350wapswh26a65c156")))))

(define-public rust-poly1305-0.8.0
  (let ((name "rust-poly1305")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "poly1305" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1grs77skh7d8vi61ji44i8gpzs3r9x7vay50i6cg8baxfa8bsnc1")))))
(define-public rust-portable-atomic-1.11.0
  (let ((name "rust-portable-atomic")
        (version "1.11.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "portable-atomic" "1.11.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0glb2wngflvfmg789qbf6dbnwcf6ai212fs7n0lf1c66rd49n3im")))))
(define-public rust-postmark-0.10.2
  (let ((name "rust-postmark")
        (version "0.10.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "postmark" "0.10.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10vd1xdlk189p8qphmihm9j28wdn5fclcgwc6z65fs43i4irihd8")))))
(define-public rust-powerfmt-0.2.0
  (let ((name "rust-powerfmt")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "powerfmt" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3")))))
(define-public rust-ppv-lite86-0.2.20
  (let ((name "rust-ppv-lite86")
        (version "0.2.20"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ppv-lite86" "0.2.20"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "017ax9ssdnpww7nrl1hvqh2lzncpv04nnsibmnw9nxjnaqlpp5bp")))))
(define-public rust-pretty-assertions-1.4.1
  (let ((name "rust-pretty-assertions")
        (version "1.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "pretty_assertions" "1.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s")))))

(define-public rust-prettyplease-0.2.29
  (let ((name "rust-prettyplease")
        (version "0.2.29"))
    (origin
      (method url-fetch)
      (uri (crate-uri "prettyplease" "0.2.29"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1b2zi42b5x3yckpin4ina1gr3n2m9zvvjmwdlhzzwz8zdv8cw939")))))
(define-public rust-proc-macro-crate-3.2.0
  (let ((name "rust-proc-macro-crate")
        (version "3.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "proc-macro-crate" "3.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0yzsqnavb3lmrcsmbrdjfrky9vcbl46v59xi9avn0796rb3likwf")))))

(define-public rust-proc-macro-error-1.0.4
  (let ((name "rust-proc-macro-error")
        (version "1.0.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "proc-macro-error" "1.0.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs")))))

(define-public rust-proc-macro-error-attr-1.0.4
  (let ((name "rust-proc-macro-error-attr")
        (version "1.0.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "proc-macro-error-attr" "1.0.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1")))))

(define-public rust-proc-macro2-1.0.93
  (let ((name "rust-proc-macro2")
        (version "1.0.93"))
    (origin
      (method url-fetch)
      (uri (crate-uri "proc-macro2" "1.0.93"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "169dw9wch753if1mgyi2nfl1il77gslvh6y2q46qplprwml6m530")))))
(define-public rust-profiling-1.0.16
  (let ((name "rust-profiling")
        (version "1.0.16"))
    (origin
      (method url-fetch)
      (uri (crate-uri "profiling" "1.0.16"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kcz2xzg4qx01r5az8cf9ffjasi2srj56sna32igddh0vi7cggdg")))))

(define-public rust-profiling-procmacros-1.0.16
  (let ((name "rust-profiling-procmacros")
        (version "1.0.16"))
    (origin
      (method url-fetch)
      (uri (crate-uri "profiling-procmacros" "1.0.16"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0c7y2k4mz5dp2ksj1h4zbxsxq4plmjzccscdaml3h1pizdh2wpx6")))))

(define-public rust-proptest-1.6.0
  (let ((name "rust-proptest")
        (version "1.6.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "proptest" "1.6.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0l4y4bb8hffv7cys7d59qwqdmvmqjfzz0x9vblc08209clqfkjhl")))))

(define-public rust-proptest-derive-0.5.1
  (let ((name "rust-proptest-derive")
        (version "0.5.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "proptest-derive" "0.5.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jay6jwfvrwzz5bqpi4hxx3ax6kax06p0h29vgkxb0vl42nckqaf")))))

(define-public rust-prost-0.13.5
  (let ((name "rust-prost")
        (version "0.13.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "prost" "0.13.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1r8yi6zxxwv9gq5ia9p55nspgwmchs94sqpp64x33v5k3njgm5i7")))))
(define-public rust-prost-build-0.13.5
  (let ((name "rust-prost-build")
        (version "0.13.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "prost-build" "0.13.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gw1mr0rmv15fc2yvn9jmxbqaj8qh80w5nn5x5s1932y8ijr8xmy")))))
(define-public rust-prost-derive-0.13.5
  (let ((name "rust-prost-derive")
        (version "0.13.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "prost-derive" "0.13.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kgc9gbzsa998xixblfi3kfydka64zqf6rmpm53b761cjxbxfmla")))))
(define-public rust-prost-reflect-0.14.6
  (let ((name "rust-prost-reflect")
        (version "0.14.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "prost-reflect" "0.14.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1phrl9f1ngxsab04xhrn4iyzhrwdj9yfga8sqvf3ccb06gviicx7")))))
(define-public rust-prost-types-0.13.5
  (let ((name "rust-prost-types")
        (version "0.13.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "prost-types" "0.13.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05mx699wyg7cjil3hz7h8lp4dhi7xhy1lq5kjv1s3cfx6szw3hjj")))))
(define-public rust-protox-0.7.2
  (let ((name "rust-protox")
        (version "0.7.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "protox" "0.7.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jmmcil88n15kdpac51fz9qjagchpy3pq3vjrj77nqxz67rjldbg")))))
(define-public rust-protox-parse-0.7.0
  (let ((name "rust-protox-parse")
        (version "0.7.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "protox-parse" "0.7.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pld0s1cg9favgy9bafkwlvmg65ky13rmhh0w050hb262p8n5953")))))
(define-public rust-quanta-0.11.1
  (let ((name "rust-quanta")
        (version "0.11.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "quanta" "0.11.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1axrw0nqc90bq671w05jd9460pmwg86c4r132mjsi4c2g8m6czm1")))))
(define-public rust-quick-error-1.2.3
  (let ((name "rust-quick-error")
        (version "1.2.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "quick-error" "1.2.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51")))))

(define-public rust-quick-xml-0.37.2
  (let ((name "rust-quick-xml")
        (version "0.37.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "quick-xml" "0.37.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "00y0qagwbxd3lqarr13j35d6kwmni176znf5jrxxcyazwplmjn0n")))))
(define-public rust-quinn-0.11.6
  (let ((name "rust-quinn")
        (version "0.11.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "quinn" "0.11.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1vq55p4kfc4zjxj58xrpf3kcjjqi4mn0wf52a5rzkiky4w46isb2")))))
(define-public rust-quinn-proto-0.11.9
  (let ((name "rust-quinn-proto")
        (version "0.11.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "quinn-proto" "0.11.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0p8k3iqd0rcxc7b6m2yyijhw4bpfwa61lyzigwvjwzax97rmxzm2")))))
(define-public rust-quinn-udp-0.5.10
  (let ((name "rust-quinn-udp")
        (version "0.5.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "quinn-udp" "0.5.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0i2rkq8lrkr89csw00mhnhp8zjh2prv4n5n65fwzd1b7hrak0vz4")))))
(define-public rust-quote-1.0.38
  (let ((name "rust-quote")
        (version "1.0.38"))
    (origin
      (method url-fetch)
      (uri (crate-uri "quote" "1.0.38"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1k0s75w61k6ch0rs263r4j69b7vj1wadqgb9dia4ylc9mymcqk8f")))))
(define-public rust-rand-0.8.5
  (let ((name "rust-rand")
        (version "0.8.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rand" "0.8.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl")))))
(define-public rust-rand-chacha-0.3.1
  (let ((name "rust-rand-chacha")
        (version "0.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rand_chacha" "0.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6")))))
(define-public rust-rand-core-0.6.4
  (let ((name "rust-rand-core")
        (version "0.6.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rand_core" "0.6.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc")))))
(define-public rust-rand-xorshift-0.3.0
  (let ((name "rust-rand-xorshift")
        (version "0.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rand_xorshift" "0.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "13vcag7gmqspzyabfl1gr9ykvxd2142q2agrj8dkyjmfqmgg4nyj")))))

(define-public rust-ratatui-0.27.0
  (let ((name "rust-ratatui")
        (version "0.27.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ratatui" "0.27.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lv8r99miibk6np2d2m0y6vf62jf5dr1x272ws6byalnnp2lcrfi")))))
(define-public rust-raw-cpuid-10.7.0
  (let ((name "rust-raw-cpuid")
        (version "10.7.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "raw-cpuid" "10.7.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ckkg47m8wbdinqg4z4dx7ipi3d7fjxdnrwzikx70x46rdwpcabc")))))
(define-public rust-raw-window-handle-0.6.2
  (let ((name "rust-raw-window-handle")
        (version "0.6.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "raw-window-handle" "0.6.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0")))))

(define-public rust-rayon-1.10.0
  (let ((name "rust-rayon")
        (version "1.10.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rayon" "1.10.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l")))))
(define-public rust-rayon-core-1.12.1
  (let ((name "rust-rayon-core")
        (version "1.12.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rayon-core" "1.12.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l")))))
(define-public rust-redox-syscall-0.4.1
  (let ((name "rust-redox-syscall")
        (version "0.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "redox_syscall" "0.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7")))))

(define-public rust-redox-syscall-0.5.9
  (let ((name "rust-redox-syscall")
        (version "0.5.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "redox_syscall" "0.5.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bvykdad226m3nqzkbb95piglyfn7m2yxp4r10m9xr4q7qr6idc2")))))
(define-public rust-redox-users-0.4.6
  (let ((name "rust-redox-users")
        (version "0.4.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "redox_users" "0.4.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s")))))
(define-public rust-redox-users-0.5.0
  (let ((name "rust-redox-users")
        (version "0.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "redox_users" "0.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0awxx66izdw6kz97r3zxrl5ms5f6dqi5l0f58mlsvlmx8wyrsvyx")))))

(define-public rust-regex-1.11.1
  (let ((name "rust-regex")
        (version "1.11.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "regex" "1.11.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m")))))
(define-public rust-regex-automata-0.1.10
  (let ((name "rust-regex-automata")
        (version "0.1.10"))
    (origin
      (method url-fetch)
      (uri (crate-uri "regex-automata" "0.1.10"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc")))))
(define-public rust-regex-automata-0.4.9
  (let ((name "rust-regex-automata")
        (version "0.4.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "regex-automata" "0.4.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0")))))
(define-public rust-regex-lite-0.1.6
  (let ((name "rust-regex-lite")
        (version "0.1.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "regex-lite" "0.1.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0almvx3z75f611pdcd9mslh7zxg76zh3shifql4ndch6mn3rb92k")))))

(define-public rust-regex-syntax-0.6.29
  (let ((name "rust-regex-syntax")
        (version "0.6.29"))
    (origin
      (method url-fetch)
      (uri (crate-uri "regex-syntax" "0.6.29"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi")))))
(define-public rust-regex-syntax-0.8.5
  (let ((name "rust-regex-syntax")
        (version "0.8.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "regex-syntax" "0.8.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b")))))
(define-public rust-reqwest-0.11.27
  (let ((name "rust-reqwest")
        (version "0.11.27"))
    (origin
      (method url-fetch)
      (uri (crate-uri "reqwest" "0.11.27"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0qjary4hpplpgdi62d2m0xvbn6lnzckwffm0rgkm2x51023m6ryx")))))
(define-public rust-reqwest-0.12.12
  (let ((name "rust-reqwest")
        (version "0.12.12"))
    (origin
      (method url-fetch)
      (uri (crate-uri "reqwest" "0.12.12"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nnigi6jcrqdd5k5myc53qdkdnrx8zjgan029q1w5hspf5039rs3")))))
(define-public rust-ring-0.17.11
  (let ((name "rust-ring")
        (version "0.17.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ring" "0.17.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wzyhdbf71ndd14kkpyj2a6nvczvli2mndzv2al7r26k4yp4jlys")))))
(define-public rust-rmp-0.8.14
  (let ((name "rust-rmp")
        (version "0.8.14"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rmp" "0.8.14"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1i1l6dhv7vws5vp0ikakj44fk597xi59g3j6ng1q55x3dz0xg3i2")))))
(define-public rust-rpassword-7.3.1
  (let ((name "rust-rpassword")
        (version "7.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rpassword" "7.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gvy3lcpph9vv1rl0cjfn72ylvmgbw2vklmj6w0iv4cpr3ijniw0")))))
(define-public rust-rsa-0.9.7
  (let ((name "rust-rsa")
        (version "0.9.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rsa" "0.9.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "06amqm85raq26v6zg00fbf93lbj3kx559n2lpxc3wrvbbiy5vis7")))))
(define-public rust-rtoolbox-0.0.2
  (let ((name "rust-rtoolbox")
        (version "0.0.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rtoolbox" "0.0.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03n9z8x353kylxhr9im8zawcisnmid3jiqrs8rbdn313cd7d4iy2")))))
(define-public rust-runtime-format-0.1.3
  (let ((name "rust-runtime-format")
        (version "0.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "runtime-format" "0.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "154c7jq7kbpc5acn2ysa2ilab2x0i5y7d34jwznni9xw71dqv589")))))
(define-public rust-rustc-demangle-0.1.24
  (let ((name "rust-rustc-demangle")
        (version "0.1.24"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-demangle" "0.1.24"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi")))))
(define-public rust-rustc-hash-1.1.0
  (let ((name "rust-rustc-hash")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-hash" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08")))))
(define-public rust-rustc-hash-2.1.1
  (let ((name "rust-rustc-hash")
        (version "2.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc-hash" "2.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm")))))
(define-public rust-rustc-version-0.4.1
  (let ((name "rust-rustc-version")
        (version "0.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustc_version" "0.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg")))))
(define-public rust-rustix-0.38.44
  (let ((name "rust-rustix")
        (version "0.38.44"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustix" "0.38.44"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx")))))
(define-public rust-rustls-0.21.12
  (let ((name "rust-rustls")
        (version "0.21.12"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls" "0.21.12"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz")))))
(define-public rust-rustls-0.23.23
  (let ((name "rust-rustls")
        (version "0.23.23"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls" "0.23.23"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15gk2bmry78cps3ya38a7cn4jxc36xv1r7gndr0fbz40qjc6qya7")))))
(define-public rust-rustls-native-certs-0.6.3
  (let ((name "rust-rustls-native-certs")
        (version "0.6.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls-native-certs" "0.6.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9")))))
(define-public rust-rustls-pemfile-1.0.4
  (let ((name "rust-rustls-pemfile")
        (version "1.0.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls-pemfile" "1.0.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w")))))
(define-public rust-rustls-pemfile-2.2.0
  (let ((name "rust-rustls-pemfile")
        (version "2.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls-pemfile" "2.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw")))))
(define-public rust-rustls-pki-types-1.11.0
  (let ((name "rust-rustls-pki-types")
        (version "1.11.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls-pki-types" "1.11.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0755isc0x5iymm3wsn59s0ad1pm9zidw7p34qfqlsjsac9jf4z4i")))))
(define-public rust-rustls-webpki-0.101.7
  (let ((name "rust-rustls-webpki")
        (version "0.101.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls-webpki" "0.101.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb")))))
(define-public rust-rustls-webpki-0.102.8
  (let ((name "rust-rustls-webpki")
        (version "0.102.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustls-webpki" "0.102.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1sdy8ks86b7jpabpnb2px2s7f1sq8v0nqf6fnlvwzm4vfk41pjk4")))))
(define-public rust-rustversion-1.0.19
  (let ((name "rust-rustversion")
        (version "1.0.19"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rustversion" "1.0.19"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1m39qd65jcd1xgqzdm3017ppimiggh2446xngwp1ngr8hjbmpi7p")))))
(define-public rust-rusty-fork-0.3.0
  (let ((name "rust-rusty-fork")
        (version "0.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rusty-fork" "0.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kxwq5c480gg6q0j3bg4zzyfh2kwmc3v2ba94jw8ncjc8mpcqgfb")))))

(define-public rust-rusty-paserk-0.4.0
  (let ((name "rust-rusty-paserk")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rusty_paserk" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0f0xqrjbvx7mb2ynnqni9ql8qlg3zzn504vnyjmyh7ilrlgailx1")))))
(define-public rust-rusty-paseto-0.7.2
  (let ((name "rust-rusty-paseto")
        (version "0.7.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "rusty_paseto" "0.7.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09kqhfi2lnjhl9wjb26j6xg26k3w41i1ll3ardjw1ifali0ihl05")))))
(define-public rust-ryu-1.0.19
  (let ((name "rust-ryu")
        (version "1.0.19"))
    (origin
      (method url-fetch)
      (uri (crate-uri "ryu" "1.0.19"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pg6a0b80m32ahygsdkwzs3bfydk4snw695akz4rqxj4lv8a58bf")))))
(define-public rust-salsa20-0.10.2
  (let ((name "rust-salsa20")
        (version "0.10.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "salsa20" "0.10.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04w211x17xzny53f83p8f7cj7k2hi8zck282q5aajwqzydd2z8lp")))))
(define-public rust-same-file-1.0.6
  (let ((name "rust-same-file")
        (version "1.0.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "same-file" "1.0.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k")))))

(define-public rust-schannel-0.1.27
  (let ((name "rust-schannel")
        (version "0.1.27"))
    (origin
      (method url-fetch)
      (uri (crate-uri "schannel" "0.1.27"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z")))))
(define-public rust-schemars-0.8.22
  (let ((name "rust-schemars")
        (version "0.8.22"))
    (origin
      (method url-fetch)
      (uri (crate-uri "schemars" "0.8.22"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05an9nbi18ynyxv1rjmwbg6j08j0496hd64mjggh53mwp3hjmgrz")))))

(define-public rust-schemars-derive-0.8.22
  (let ((name "rust-schemars-derive")
        (version "0.8.22"))
    (origin
      (method url-fetch)
      (uri (crate-uri "schemars_derive" "0.8.22"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kakyzrp5801s4i043l4ilv96lzimnlh01pap958h66n99w6bqij")))))

(define-public rust-scoped-tls-1.0.1
  (let ((name "rust-scoped-tls")
        (version "1.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "scoped-tls" "1.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1")))))

(define-public rust-scopeguard-1.2.0
  (let ((name "rust-scopeguard")
        (version "1.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "scopeguard" "1.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l")))))
(define-public rust-sct-0.7.1
  (let ((name "rust-sct")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sct" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s")))))
(define-public rust-sd-notify-0.4.5
  (let ((name "rust-sd-notify")
        (version "0.4.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sd-notify" "0.4.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x1bmz30x2i35j771rqyyan40473aqk0xjrh2dk9xdnqf7gylhxr")))))

(define-public rust-security-framework-2.11.1
  (let ((name "rust-security-framework")
        (version "2.11.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "security-framework" "2.11.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9")))))
(define-public rust-security-framework-sys-2.14.0
  (let ((name "rust-security-framework-sys")
        (version "2.14.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "security-framework-sys" "2.14.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9")))))
(define-public rust-semver-1.0.25
  (let ((name "rust-semver")
        (version "1.0.25"))
    (origin
      (method url-fetch)
      (uri (crate-uri "semver" "1.0.25"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "00sy306qpi7vfand7dxm2vc76nlc8fkh1rrhdy0qh12v50nzx7gp")))))
(define-public rust-serde-1.0.218
  (let ((name "rust-serde")
        (version "1.0.218"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde" "1.0.218"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0q6z4bnrwagnms0bds4886711l6mc68s979i49zd3xnvkg8wkpz8")))))
(define-public rust-serde-derive-1.0.218
  (let ((name "rust-serde-derive")
        (version "1.0.218"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_derive" "1.0.218"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0azqd74xbpb1v5vf6w1fdbgmwp39ljjfj25cib5rgrzlj7hh75gh")))))
(define-public rust-serde-derive-internals-0.29.1
  (let ((name "rust-serde-derive-internals")
        (version "0.29.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_derive_internals" "0.29.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04g7macx819vbnxhi52cx0nhxi56xlhrybgwybyy7fb9m4h6mlhq")))))

(define-public rust-serde-json-1.0.139
  (let ((name "rust-serde-json")
        (version "1.0.139"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_json" "1.0.139"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "19kj3irpa22a7djz1jaf4wambzh7psiqa6zyafqnb76crhx6ry24")))))
(define-public rust-serde-path-to-error-0.1.16
  (let ((name "rust-serde-path-to-error")
        (version "0.1.16"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_path_to_error" "0.1.16"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "19hlz2359l37ifirskpcds7sxg0gzpqvfilibs7whdys0128i6dg")))))
(define-public rust-serde-regex-1.1.0
  (let ((name "rust-serde-regex")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_regex" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8")))))
(define-public rust-serde-repr-0.1.19
  (let ((name "rust-serde-repr")
        (version "0.1.19"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_repr" "0.1.19"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1sb4cplc33z86pzlx38234xr141wr3cmviqgssiadisgl8dlar3c")))))

(define-public rust-serde-spanned-0.6.8
  (let ((name "rust-serde-spanned")
        (version "0.6.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_spanned" "0.6.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47")))))

(define-public rust-serde-urlencoded-0.7.1
  (let ((name "rust-serde-urlencoded")
        (version "0.7.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_urlencoded" "0.7.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk")))))
(define-public rust-serde-with-3.12.0
  (let ((name "rust-serde-with")
        (version "3.12.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_with" "3.12.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ai9c3cbdgrsvmlc4qpg9z73y80yplk3k7zp45wp97xnzkrggdnn")))))
(define-public rust-serde-with-macros-3.12.0
  (let ((name "rust-serde-with-macros")
        (version "3.12.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "serde_with_macros" "3.12.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "13hznly0qq1rngsdh8gpnajab2knkrmvwwrbmii86g1s36jwl04d")))))
(define-public rust-sha1-0.10.6
  (let ((name "rust-sha1")
        (version "0.10.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sha1" "0.10.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3")))))
(define-public rust-sha2-0.10.8
  (let ((name "rust-sha2")
        (version "0.10.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sha2" "0.10.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr")))))
(define-public rust-sharded-slab-0.1.7
  (let ((name "rust-sharded-slab")
        (version "0.1.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sharded-slab" "0.1.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l")))))
(define-public rust-shellexpand-3.1.0
  (let ((name "rust-shellexpand")
        (version "3.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "shellexpand" "3.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys")))))
(define-public rust-shlex-1.3.0
  (let ((name "rust-shlex")
        (version "1.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "shlex" "1.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg")))))
(define-public rust-signal-hook-0.3.17
  (let ((name "rust-signal-hook")
        (version "0.3.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "signal-hook" "0.3.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6")))))
(define-public rust-signal-hook-mio-0.2.4
  (let ((name "rust-signal-hook-mio")
        (version "0.2.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "signal-hook-mio" "0.2.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1k8pl9aafiadr4czsg8zal9b4jdk6kq5985p90i19jc5sh31mnrl")))))
(define-public rust-signal-hook-registry-1.4.2
  (let ((name "rust-signal-hook-registry")
        (version "1.4.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "signal-hook-registry" "1.4.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9")))))
(define-public rust-signature-2.2.0
  (let ((name "rust-signature")
        (version "2.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "signature" "2.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p")))))
(define-public rust-simd-adler32-0.3.7
  (let ((name "rust-simd-adler32")
        (version "0.3.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "simd-adler32" "0.3.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn")))))
(define-public rust-similar-2.7.0
  (let ((name "rust-similar")
        (version "2.7.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "similar" "2.7.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv")))))

(define-public rust-siphasher-1.0.1
  (let ((name "rust-siphasher")
        (version "1.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "siphasher" "1.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an")))))
(define-public rust-sketches-ddsketch-0.2.2
  (let ((name "rust-sketches-ddsketch")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sketches-ddsketch" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0p6n1v0p0773d0b5qnsnw526g7hhlb08bx95wm0zb09xnwa6qqw5")))))
(define-public rust-slab-0.4.9
  (let ((name "rust-slab")
        (version "0.4.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "slab" "0.4.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg")))))
(define-public rust-smallvec-1.14.0
  (let ((name "rust-smallvec")
        (version "1.14.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "smallvec" "1.14.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1z8wpr53x6jisklqhkkvkgyi8s5cn69h2d2alhqfxahzxwiq7kvz")))))
(define-public rust-smawk-0.3.2
  (let ((name "rust-smawk")
        (version "0.3.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "smawk" "0.3.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0344z1la39incggwn6nl45k8cbw2x10mr5j0qz85cdz9np0qihxp")))))

(define-public rust-smithay-0.4.4
  (let ((name "rust-smithay")
        (version "0.4.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "smithay" "0.4.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0nd3wq104pls1ql9kmgfp7gfbi6jxzmgddl457zqgfq9b2q4nhd0"))
      (patches (rosenthal-patches "rust-smithay.patch")))))

(define-public rust-smithay-client-toolkit-0.19.2
  (let ((name "rust-smithay-client-toolkit")
        (version "0.19.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "smithay-client-toolkit" "0.19.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05h05hg4dn3v6br5jbdbs5nalk076a64s7fn6i01nqzby2hxwmrl")))))

(define-public rust-smithay-drm-extras-0.1.0
  (let ((name "rust-smithay-drm-extras")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "smithay-drm-extras" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15hflf1yn04inm94m7b49gfx3c75h850ny6vv8bc4m9rnag10s7s")))))

(define-public rust-smol-str-0.2.2
  (let ((name "rust-smol-str")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "smol_str" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1bfylqf2vnqaglw58930vpxm2rfzji5gjp15a2c0kh8aj6v8ylyx")))))

(define-public rust-socket2-0.5.8
  (let ((name "rust-socket2")
        (version "0.5.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "socket2" "0.5.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1s7vjmb5gzp3iaqi94rh9r63k9cj00kjgbfn7gn60kmnk6fjcw69")))))
(define-public rust-spin-0.9.8
  (let ((name "rust-spin")
        (version "0.9.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "spin" "0.9.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039")))))
(define-public rust-spki-0.7.3
  (let ((name "rust-spki")
        (version "0.7.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "spki" "0.7.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr")))))
(define-public rust-sql-builder-3.1.1
  (let ((name "rust-sql-builder")
        (version "3.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sql-builder" "3.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h5xp47zz9chv545lpmal51fq3z162z2f99mb4lhcbgcsaaqs05i")))))
(define-public rust-sqlx-0.8.3
  (let ((name "rust-sqlx")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sqlx" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0pvlpq0plgyxf5kikcv786pf0pjv8dx5shlvz72l510d7hxyf424")))))
(define-public rust-sqlx-core-0.8.3
  (let ((name "rust-sqlx-core")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sqlx-core" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1q31dawr61wc6q2f12my4fw082mbv8sxwz1082msjsk76rlpn03a")))))
(define-public rust-sqlx-macros-0.8.3
  (let ((name "rust-sqlx-macros")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sqlx-macros" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "047k67sylscv0gdhwwqrn0s33jy1mvq8rmqq6s8fygv4g2ny44ii")))))
(define-public rust-sqlx-macros-core-0.8.3
  (let ((name "rust-sqlx-macros-core")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sqlx-macros-core" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1bg7sn6l8dc4pzrqx2dwc3sp7dbn97msfqahpycnl55bqnn917sf")))))
(define-public rust-sqlx-mysql-0.8.3
  (let ((name "rust-sqlx-mysql")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sqlx-mysql" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0czjzzjm2y6lkhxvvzrzwgp0pmlhymcnym20hn9n9kh01s7jfq25")))))
(define-public rust-sqlx-postgres-0.8.3
  (let ((name "rust-sqlx-postgres")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sqlx-postgres" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04wnjl51kfx0qbfsfmhqdshpmw32vzz2p8dksmj6gvb3ydbqmff5")))))
(define-public rust-sqlx-sqlite-0.8.3
  (let ((name "rust-sqlx-sqlite")
        (version "0.8.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sqlx-sqlite" "0.8.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0h05ca26g428h4337k4nm0ww75bcdkiqzp883m7fc92v78fsfp7q")))))
(define-public rust-stability-0.2.1
  (let ((name "rust-stability")
        (version "0.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "stability" "0.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1b7w6qknq0w5y7s358j62pzi9kbh6g73lal3jx9aydpikl0ff16r")))))
(define-public rust-stable-deref-trait-1.2.0
  (let ((name "rust-stable-deref-trait")
        (version "1.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "stable_deref_trait" "1.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8")))))
(define-public rust-static-assertions-1.1.0
  (let ((name "rust-static-assertions")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "static_assertions" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2")))))
(define-public rust-stringprep-0.1.5
  (let ((name "rust-stringprep")
        (version "0.1.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "stringprep" "0.1.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cb3jis4h2b767csk272zw92lc6jzfzvh8d6m1cd86yqjb9z6kbv")))))
(define-public rust-strsim-0.11.1
  (let ((name "rust-strsim")
        (version "0.11.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "strsim" "0.11.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x")))))
(define-public rust-strum-0.26.3
  (let ((name "rust-strum")
        (version "0.26.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "strum" "0.26.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g")))))
(define-public rust-strum-macros-0.26.4
  (let ((name "rust-strum-macros")
        (version "0.26.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "strum_macros" "0.26.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc")))))
(define-public rust-subtle-2.6.1
  (let ((name "rust-subtle")
        (version "2.6.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "subtle" "2.6.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk")))))
(define-public rust-supports-color-2.1.0
  (let ((name "rust-supports-color")
        (version "2.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "supports-color" "2.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12csf7chawxinaapm9rh718nha9hggk6ra86fdaw9hxdagg8qffn")))))

(define-public rust-supports-hyperlinks-2.1.0
  (let ((name "rust-supports-hyperlinks")
        (version "2.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "supports-hyperlinks" "2.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0g93nh1db3f9lyd0ry35bqjrxkg6sbysn36x9hgd9m5h5rlk2hpq")))))

(define-public rust-supports-unicode-2.1.0
  (let ((name "rust-supports-unicode")
        (version "2.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "supports-unicode" "2.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0yp703pvpzpmaw9mpncvwf0iqis4xmhs569ii1g20jhqvngc2l7q")))))

(define-public rust-syn-1.0.109
  (let ((name "rust-syn")
        (version "1.0.109"))
    (origin
      (method url-fetch)
      (uri (crate-uri "syn" "1.0.109"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj")))))

(define-public rust-syn-2.0.98
  (let ((name "rust-syn")
        (version "2.0.98"))
    (origin
      (method url-fetch)
      (uri (crate-uri "syn" "2.0.98"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cfk0qqbl4fbr3dz61nw21d5amvl4rym6nxwnfsw43mf90d7y51n")))))
(define-public rust-sync-wrapper-0.1.2
  (let ((name "rust-sync-wrapper")
        (version "0.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sync_wrapper" "0.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0")))))
(define-public rust-sync-wrapper-1.0.2
  (let ((name "rust-sync-wrapper")
        (version "1.0.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sync_wrapper" "1.0.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb")))))
(define-public rust-synstructure-0.13.1
  (let ((name "rust-synstructure")
        (version "0.13.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "synstructure" "0.13.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8")))))
(define-public rust-sysinfo-0.30.13
  (let ((name "rust-sysinfo")
        (version "0.30.13"))
    (origin
      (method url-fetch)
      (uri (crate-uri "sysinfo" "0.30.13"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1csbkx1hdlacgzw5ynjyfvgc1xg58w3h1rgh5gm2pysmxvd4snqa")))))
(define-public rust-system-configuration-0.5.1
  (let ((name "rust-system-configuration")
        (version "0.5.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "system-configuration" "0.5.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms")))))
(define-public rust-system-configuration-sys-0.5.0
  (let ((name "rust-system-configuration-sys")
        (version "0.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "system-configuration-sys" "0.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7")))))
(define-public rust-system-deps-6.2.2
  (let ((name "rust-system-deps")
        (version "6.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "system-deps" "6.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3")))))

(define-public rust-system-deps-7.0.3
  (let ((name "rust-system-deps")
        (version "7.0.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "system-deps" "7.0.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "01d0fllzpkfybzadyaq1vlx70imzj56dxs4rk9w2f4ikkypkmlk6")))))

(define-public rust-target-lexicon-0.12.16
  (let ((name "rust-target-lexicon")
        (version "0.12.16"))
    (origin
      (method url-fetch)
      (uri (crate-uri "target-lexicon" "0.12.16"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31")))))

(define-public rust-tempfile-3.17.1
  (let ((name "rust-tempfile")
        (version "3.17.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tempfile" "3.17.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0c52ggq5vy5mzgk5ly36cgzs1cig3cv6r1jarijmzxgkn6na1r92")))))
(define-public rust-terminal-size-0.1.17
  (let ((name "rust-terminal-size")
        (version "0.1.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "terminal_size" "0.1.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pq60ng1a7fjp597ifk1cqlz8fv9raz9xihddld1m1pfdia1lg33")))))

(define-public rust-terminal-size-0.4.1
  (let ((name "rust-terminal-size")
        (version "0.4.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "terminal_size" "0.4.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1sd4nq55h9sjirkx0138zx711ddxq1k1a45lc77ninhzj9zl8ljk")))))

(define-public rust-testing-logger-0.1.1
  (let ((name "rust-testing-logger")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "testing_logger" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "087pi7y9iisspafyzblj41qvrw95dfb6px7pavlkmls5rckvg4kd")))))

(define-public rust-textwrap-0.15.2
  (let ((name "rust-textwrap")
        (version "0.15.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "textwrap" "0.15.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0galmidi6gpn308b1kv3r4qbb48j2926lcj0idwhdhlylhjybcxp")))))

(define-public rust-thiserror-1.0.69
  (let ((name "rust-thiserror")
        (version "1.0.69"))
    (origin
      (method url-fetch)
      (uri (crate-uri "thiserror" "1.0.69"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn")))))
(define-public rust-thiserror-2.0.11
  (let ((name "rust-thiserror")
        (version "2.0.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "thiserror" "2.0.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1z0649rpa8c2smzx129bz4qvxmdihj30r2km6vfpcv9yny2g4lnl")))))
(define-public rust-thiserror-impl-1.0.69
  (let ((name "rust-thiserror-impl")
        (version "1.0.69"))
    (origin
      (method url-fetch)
      (uri (crate-uri "thiserror-impl" "1.0.69"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg")))))
(define-public rust-thiserror-impl-2.0.11
  (let ((name "rust-thiserror-impl")
        (version "2.0.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "thiserror-impl" "2.0.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1hkkn7p2y4cxbffcrprybkj0qy1rl1r6waxmxqvr764axaxc3br6")))))
(define-public rust-thread-local-1.1.8
  (let ((name "rust-thread-local")
        (version "1.1.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "thread_local" "1.1.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb")))))
(define-public rust-tiff-0.9.1
  (let ((name "rust-tiff")
        (version "0.9.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tiff" "0.9.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ghyxlz566dzc3scvgmzys11dhq2ri77kb8sznjakijlxby104xs")))))
(define-public rust-time-0.3.37
  (let ((name "rust-time")
        (version "0.3.37"))
    (origin
      (method url-fetch)
      (uri (crate-uri "time" "0.3.37"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "08bvydyc14plkwhchzia5bcdbmm0mk5fzilsdpjx06w6hf48drrm")))))
(define-public rust-time-core-0.1.2
  (let ((name "rust-time-core")
        (version "0.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "time-core" "0.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1wx3qizcihw6z151hywfzzyd1y5dl804ydyxci6qm07vbakpr4pg")))))
(define-public rust-time-macros-0.2.19
  (let ((name "rust-time-macros")
        (version "0.2.19"))
    (origin
      (method url-fetch)
      (uri (crate-uri "time-macros" "0.2.19"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pl558z26pp342l5y91n6dxb60xwhar975wk6jc4npiygq0ycd18")))))
(define-public rust-tiny-bip39-1.0.0
  (let ((name "rust-tiny-bip39")
        (version "1.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tiny-bip39" "1.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0q98iv3wgbd41wyxxd5is8sddi53k9ary45rbi5fi8dmb39r9k32")))))
(define-public rust-tinystr-0.7.6
  (let ((name "rust-tinystr")
        (version "0.7.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tinystr" "0.7.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi")))))
(define-public rust-tinyvec-1.8.1
  (let ((name "rust-tinyvec")
        (version "1.8.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tinyvec" "1.8.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1s41rv7n39sjsxz3kd3d4adw45ndkxz1d18rfbz2wd7s9n8bhb82")))))
(define-public rust-tinyvec-macros-0.1.1
  (let ((name "rust-tinyvec-macros")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tinyvec_macros" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z")))))
(define-public rust-tokio-1.43.0
  (let ((name "rust-tokio")
        (version "1.43.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio" "1.43.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17pdm49ihlhfw3rpxix3kdh2ppl1yv7nwp1kxazi5r1xz97zlq9x")))))
(define-public rust-tokio-macros-2.5.0
  (let ((name "rust-tokio-macros")
        (version "2.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-macros" "2.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1f6az2xbvqp7am417b78d1za8axbvjvxnmkakz9vr8s52czx81kf")))))
(define-public rust-tokio-rustls-0.24.1
  (let ((name "rust-tokio-rustls")
        (version "0.24.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-rustls" "0.24.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2")))))
(define-public rust-tokio-rustls-0.26.1
  (let ((name "rust-tokio-rustls")
        (version "0.26.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-rustls" "0.26.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dxz4bhkn4bwnvzjqvqlg70ba5fslnmf9r6yr87wzq5cx9shjvaz")))))
(define-public rust-tokio-stream-0.1.17
  (let ((name "rust-tokio-stream")
        (version "0.1.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-stream" "0.1.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ix0770hfp4x5rh5bl7vsnr3d4iz4ms43i522xw70xaap9xqv9gc")))))
(define-public rust-tokio-util-0.7.13
  (let ((name "rust-tokio-util")
        (version "0.7.13"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tokio-util" "0.7.13"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0y0h10a52c7hrldmr3410bp7j3fadq0jn9nf7awddgd2an6smz6p")))))
(define-public rust-toml-0.5.11
  (let ((name "rust-toml")
        (version "0.5.11"))
    (origin
      (method url-fetch)
      (uri (crate-uri "toml" "0.5.11"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0d2266nx8b3n22c7k24x4428z6di8n83a9n466jm7a2hipfz1xzl")))))
(define-public rust-toml-0.8.20
  (let ((name "rust-toml")
        (version "0.8.20"))
    (origin
      (method url-fetch)
      (uri (crate-uri "toml" "0.8.20"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0j012b37iz1mihksr6a928s6dzszxvblzg3l5wxp7azzsv6sb1yd")))))

(define-public rust-toml-datetime-0.6.8
  (let ((name "rust-toml-datetime")
        (version "0.6.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "toml_datetime" "0.6.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd")))))

(define-public rust-toml-edit-0.22.24
  (let ((name "rust-toml-edit")
        (version "0.22.24"))
    (origin
      (method url-fetch)
      (uri (crate-uri "toml_edit" "0.22.24"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0x0lgp70x5cl9nla03xqs5vwwwlrwmd0djkdrp3h3lpdymgpkd0p")))))

(define-public rust-tonic-0.12.3
  (let ((name "rust-tonic")
        (version "0.12.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tonic" "0.12.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ljd1lfjpw0vrm5wbv15x6nq2i38llsanls5rkzmdn2n0wrmnz47")))))
(define-public rust-tonic-build-0.12.3
  (let ((name "rust-tonic-build")
        (version "0.12.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tonic-build" "0.12.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04baqblgrlc0g8scnhpky5s0n4cljaixrrdrr6cv6wx7kq8cwmwm")))))
(define-public rust-tonic-types-0.12.3
  (let ((name "rust-tonic-types")
        (version "0.12.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tonic-types" "0.12.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rxkz100jaiqlr47dim69mfhyq54c3lynnia75qi5l2713pdi080")))))
(define-public rust-tower-0.4.13
  (let ((name "rust-tower")
        (version "0.4.13"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tower" "0.4.13"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq")))))
(define-public rust-tower-0.5.2
  (let ((name "rust-tower")
        (version "0.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tower" "0.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ybmd59nm4abl9bsvy6rx31m4zvzp5rja2slzpn712y9b68ssffh")))))
(define-public rust-tower-http-0.5.2
  (let ((name "rust-tower-http")
        (version "0.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tower-http" "0.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xakj3x0anp55gjqibiwvzma5iz0w9pcjsr7qk97sx4qm4sd970y")))))
(define-public rust-tower-layer-0.3.3
  (let ((name "rust-tower-layer")
        (version "0.3.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tower-layer" "0.3.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j")))))
(define-public rust-tower-service-0.3.3
  (let ((name "rust-tower-service")
        (version "0.3.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tower-service" "0.3.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd")))))
(define-public rust-tracing-0.1.41
  (let ((name "rust-tracing")
        (version "0.1.41"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracing" "0.1.41"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq")))))
(define-public rust-tracing-attributes-0.1.28
  (let ((name "rust-tracing-attributes")
        (version "0.1.28"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracing-attributes" "0.1.28"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir")))))
(define-public rust-tracing-core-0.1.33
  (let ((name "rust-tracing-core")
        (version "0.1.33"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracing-core" "0.1.33"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6")))))
(define-public rust-tracing-log-0.2.0
  (let ((name "rust-tracing-log")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracing-log" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf")))))
(define-public rust-tracing-subscriber-0.3.19
  (let ((name "rust-tracing-subscriber")
        (version "0.3.19"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracing-subscriber" "0.3.19"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678")))))
(define-public rust-tracing-tree-0.4.0
  (let ((name "rust-tracing-tree")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracing-tree" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "175lqyfp6zq7jbj8m026xdp8p765pzgfdzfxahfggmdhy5wwlngl")))))
(define-public rust-tracy-client-0.17.6
  (let ((name "rust-tracy-client")
        (version "0.17.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracy-client" "0.17.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0zkwz9aq97znyal3hz9wmxya97pj01ddpv92ha7l39a6fdw2s83k")))))

(define-public rust-tracy-client-0.18.0
  (let ((name "rust-tracy-client")
        (version "0.18.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracy-client" "0.18.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nrn739vanildbbzfdcsh8y1fzp2p848db49vmpvf0jv600jq2nr")))))

(define-public rust-tracy-client-sys-0.24.3
  (let ((name "rust-tracy-client-sys")
        (version "0.24.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tracy-client-sys" "0.24.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ps3iwb7q1fzs9pir6b0nqi8n7i67lci4jp6z4xrq8s8lmyz7zv9")))))

(define-public rust-tree-magic-mini-3.1.6
  (let ((name "rust-tree-magic-mini")
        (version "3.1.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "tree_magic_mini" "3.1.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0qwx2b0xfr00vdskl951cvh3m040zj5n8vm7ln4k6p143ybyiida")))))
(define-public rust-try-lock-0.2.5
  (let ((name "rust-try-lock")
        (version "0.2.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "try-lock" "0.2.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4")))))
(define-public rust-typed-builder-0.18.2
  (let ((name "rust-typed-builder")
        (version "0.18.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "typed-builder" "0.18.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1p9s9p7f3mnylrzdqbxj73d9dw95syma6pnnyfp3ys801s49qwvp")))))
(define-public rust-typed-builder-macro-0.18.2
  (let ((name "rust-typed-builder-macro")
        (version "0.18.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "typed-builder-macro" "0.18.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0qwfq0q2lkg4bkmcpsqajy3ss2sb2h47dj5zhfwvbp27ygx8sw8z")))))
(define-public rust-typenum-1.18.0
  (let ((name "rust-typenum")
        (version "1.18.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "typenum" "1.18.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x")))))
(define-public rust-udev-0.9.3
  (let ((name "rust-udev")
        (version "0.9.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "udev" "0.9.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "17vy1yc6ipb5m2kc2d4lx2qpj45yr7grsjzm3y2gq0a4xblkfkmg")))))

(define-public rust-uds-windows-1.1.0
  (let ((name "rust-uds-windows")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "uds_windows" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9")))))

(define-public rust-unarray-0.1.4
  (let ((name "rust-unarray")
        (version "0.1.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unarray" "0.1.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa")))))

(define-public rust-unicode-bidi-0.3.18
  (let ((name "rust-unicode-bidi")
        (version "0.3.18"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-bidi" "0.3.18"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xcxwbsqa24b8vfchhzyyzgj0l6bn51ib5v8j6krha0m77dva72w")))))
(define-public rust-unicode-ident-1.0.17
  (let ((name "rust-unicode-ident")
        (version "1.0.17"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-ident" "1.0.17"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gpdxvaskz04whays5igg4zyca0dl7vdy2arsfxb13kpjcx4gqh0")))))
(define-public rust-unicode-linebreak-0.1.5
  (let ((name "rust-unicode-linebreak")
        (version "0.1.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-linebreak" "0.1.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "07spj2hh3daajg335m4wdav6nfkl0f6c0q72lc37blr97hych29v")))))

(define-public rust-unicode-normalization-0.1.24
  (let ((name "rust-unicode-normalization")
        (version "0.1.24"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-normalization" "0.1.24"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh")))))
(define-public rust-unicode-properties-0.1.3
  (let ((name "rust-unicode-properties")
        (version "0.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-properties" "0.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1l3mbgzwz8g14xcs09p4ww3hjkjcf0i1ih13nsg72bhj8n5jl3z7")))))
(define-public rust-unicode-segmentation-1.12.0
  (let ((name "rust-unicode-segmentation")
        (version "1.12.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-segmentation" "1.12.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n")))))
(define-public rust-unicode-truncate-1.1.0
  (let ((name "rust-unicode-truncate")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-truncate" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gr7arjjhrhy8dww7hj8qqlws97xf9d276svr4hs6pxgllklcr5k")))))
(define-public rust-unicode-width-0.1.14
  (let ((name "rust-unicode-width")
        (version "0.1.14"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-width" "0.1.14"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx")))))
(define-public rust-unicode-width-0.2.0
  (let ((name "rust-unicode-width")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "unicode-width" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z")))))
(define-public rust-universal-hash-0.5.1
  (let ((name "rust-universal-hash")
        (version "0.5.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "universal-hash" "0.5.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1sh79x677zkncasa95wz05b36134822w6qxmi1ck05fwi33f47gw")))))
(define-public rust-untrusted-0.9.0
  (let ((name "rust-untrusted")
        (version "0.9.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "untrusted" "0.9.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf")))))
(define-public rust-url-2.5.4
  (let ((name "rust-url")
        (version "2.5.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "url" "2.5.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j")))))
(define-public rust-urlencoding-2.1.3
  (let ((name "rust-urlencoding")
        (version "2.1.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "urlencoding" "2.1.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s")))))
(define-public rust-utf16-iter-1.0.5
  (let ((name "rust-utf16-iter")
        (version "1.0.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "utf16_iter" "1.0.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8")))))
(define-public rust-utf8-iter-1.0.4
  (let ((name "rust-utf8-iter")
        (version "1.0.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "utf8_iter" "1.0.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn")))))
(define-public rust-utf8parse-0.2.2
  (let ((name "rust-utf8parse")
        (version "0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "utf8parse" "0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6")))))
(define-public rust-uuid-1.15.0
  (let ((name "rust-uuid")
        (version "1.15.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "uuid" "1.15.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xrfv5c3q8j07bikl7vfi0hanyn1i1cs0nm0szc50ix13kxcm3dx")))))
(define-public rust-valuable-0.1.1
  (let ((name "rust-valuable")
        (version "0.1.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "valuable" "0.1.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs")))))
(define-public rust-vcpkg-0.2.15
  (let ((name "rust-vcpkg")
        (version "0.2.15"))
    (origin
      (method url-fetch)
      (uri (crate-uri "vcpkg" "0.2.15"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc")))))
(define-public rust-version-check-0.9.5
  (let ((name "rust-version-check")
        (version "0.9.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "version_check" "0.9.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb")))))
(define-public rust-version-compare-0.2.0
  (let ((name "rust-version-compare")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "version-compare" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5")))))

(define-public rust-wait-timeout-0.2.1
  (let ((name "rust-wait-timeout")
        (version "0.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wait-timeout" "0.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09")))))

(define-public rust-walkdir-2.5.0
  (let ((name "rust-walkdir")
        (version "2.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "walkdir" "2.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99")))))

(define-public rust-want-0.3.1
  (let ((name "rust-want")
        (version "0.3.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "want" "0.3.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz")))))
(define-public rust-wasi-0.11.0+wasi-snapshot-preview1
  (let ((name "rust-wasi")
        (version "0.11.0+wasi-snapshot-preview1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasi" "0.11.0+wasi-snapshot-preview1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw")))))
(define-public rust-wasi-0.13.3+wasi-0.2.2
  (let ((name "rust-wasi")
        (version "0.13.3+wasi-0.2.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasi" "0.13.3+wasi-0.2.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lnapbvdcvi3kc749wzqvwrpd483win2kicn1faa4dja38p6v096")))))
(define-public rust-wasite-0.1.0
  (let ((name "rust-wasite")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasite" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0nw5h9nmcl4fyf4j5d4mfdjfgvwi1cakpi349wc4zrr59wxxinmq")))))
(define-public rust-wasm-bindgen-0.2.100
  (let ((name "rust-wasm-bindgen")
        (version "0.2.100"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen" "0.2.100"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y")))))
(define-public rust-wasm-bindgen-backend-0.2.100
  (let ((name "rust-wasm-bindgen-backend")
        (version "0.2.100"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-backend" "0.2.100"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig")))))
(define-public rust-wasm-bindgen-futures-0.4.50
  (let ((name "rust-wasm-bindgen-futures")
        (version "0.4.50"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-futures" "0.4.50"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0q8ymi6i9r3vxly551dhxcyai7nc491mspj0j1wbafxwq074fpam")))))
(define-public rust-wasm-bindgen-macro-0.2.100
  (let ((name "rust-wasm-bindgen-macro")
        (version "0.2.100"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-macro" "0.2.100"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz")))))
(define-public rust-wasm-bindgen-macro-support-0.2.100
  (let ((name "rust-wasm-bindgen-macro-support")
        (version "0.2.100"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-macro-support" "0.2.100"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a")))))
(define-public rust-wasm-bindgen-shared-0.2.100
  (let ((name "rust-wasm-bindgen-shared")
        (version "0.2.100"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wasm-bindgen-shared" "0.2.100"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s")))))
(define-public rust-wayland-backend-0.3.8
  (let ((name "rust-wayland-backend")
        (version "0.3.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-backend" "0.3.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gs7dw6s3lp9g6g0rhk4bh66wl41jnbkd27c6ynhv1x3xac8j85p")))))
(define-public rust-wayland-client-0.31.8
  (let ((name "rust-wayland-client")
        (version "0.31.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-client" "0.31.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gzpr9gdd8yk1crflxngg5iwa1szyyzp4i4zbgpslf1nsgihs4n2")))))
(define-public rust-wayland-csd-frame-0.3.0
  (let ((name "rust-wayland-csd-frame")
        (version "0.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-csd-frame" "0.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32")))))

(define-public rust-wayland-cursor-0.31.8
  (let ((name "rust-wayland-cursor")
        (version "0.31.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-cursor" "0.31.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0pccjqiln1izjbdzprqiijhad4k00wmr5r003a44h1v5nv5jjc59")))))

(define-public rust-wayland-egl-0.32.5
  (let ((name "rust-wayland-egl")
        (version "0.32.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-egl" "0.32.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "00lr5nlc7xa050p7rzlrndlc82iy0g29lhpxizs73qhh38j3hj2h")))))

(define-public rust-wayland-protocols-0.31.2
  (let ((name "rust-wayland-protocols")
        (version "0.31.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-protocols" "0.31.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1x310l1p6p3p3l76nl1l2yava9408dy77s605917zadlp1jz70cg")))))
(define-public rust-wayland-protocols-0.32.6
  (let ((name "rust-wayland-protocols")
        (version "0.32.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-protocols" "0.32.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1z0yahh48x8qzdbcallmxn5am5897hkk5d7p51ly6dwvhr3cz087")))))

(define-public rust-wayland-protocols-misc-0.3.6
  (let ((name "rust-wayland-protocols-misc")
        (version "0.3.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-protocols-misc" "0.3.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0m2spzy9diwc3sx0bqq7qx8qdip5lw1ns227bnqinv8220cfxdzy")))))

(define-public rust-wayland-protocols-plasma-0.3.6
  (let ((name "rust-wayland-protocols-plasma")
        (version "0.3.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-protocols-plasma" "0.3.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cs6gpgxybjvr60j6j824blsvz4hnmjwaah2cdkzvzh3cz3srjkw")))))

(define-public rust-wayland-protocols-wlr-0.2.0
  (let ((name "rust-wayland-protocols-wlr")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-protocols-wlr" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1mjww9psk2nc5hm2q4s3qas30rbzfg1sb6qgw518fbbcdfvn27xd")))))
(define-public rust-wayland-protocols-wlr-0.3.6
  (let ((name "rust-wayland-protocols-wlr")
        (version "0.3.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-protocols-wlr" "0.3.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cpqb0d4ryf87x2wgca5n71wilhvc0jjva0zasbdgalmypk052i4")))))

(define-public rust-wayland-scanner-0.31.6
  (let ((name "rust-wayland-scanner")
        (version "0.31.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-scanner" "0.31.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "110ldnyfxjqvjssir1jf3ndlci7xy9lpv4aqg775y518bpyxlvw9")))))
(define-public rust-wayland-server-0.31.7
  (let ((name "rust-wayland-server")
        (version "0.31.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-server" "0.31.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jx410qa59vry55xm40dqgqa7d0cx7xs3a5qaxv8xzwcsrzbvylp")))))

(define-public rust-wayland-sys-0.31.6
  (let ((name "rust-wayland-sys")
        (version "0.31.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wayland-sys" "0.31.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "05b6i4lg2qrrz7l4h2b5fd7blkkvxq34i1yvlngsmmbpkhwvpknv")))))
(define-public rust-web-sys-0.3.77
  (let ((name "rust-web-sys")
        (version "0.3.77"))
    (origin
      (method url-fetch)
      (uri (crate-uri "web-sys" "0.3.77"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lnmc1ffbq34qw91nndklqqm75rasaffj2g4f8h1yvqqz4pdvdik")))))
(define-public rust-web-time-1.1.0
  (let ((name "rust-web-time")
        (version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "web-time" "1.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras")))))
(define-public rust-webpki-roots-0.26.8
  (let ((name "rust-webpki-roots")
        (version "0.26.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "webpki-roots" "0.26.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1jf54brni9lk4ak5pkma2pn18hli22gr7i7wp9zn2lzayy8v4412")))))
(define-public rust-weezl-0.1.8
  (let ((name "rust-weezl")
        (version "0.1.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "weezl" "0.1.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "10lhndjgs6y5djpg3b420xngcr6jkmv70q8rb1qcicbily35pa2k")))))
(define-public rust-whoami-1.5.2
  (let ((name "rust-whoami")
        (version "1.5.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "whoami" "1.5.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vdvm6sga4v9515l6glqqfnmzp246nq66dd09cw5ri4fyn3mnb9p")))))
(define-public rust-winapi-0.3.9
  (let ((name "rust-winapi")
        (version "0.3.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi" "0.3.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw")))))
(define-public rust-winapi-i686-pc-windows-gnu-0.4.0
  (let ((name "rust-winapi-i686-pc-windows-gnu")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi-i686-pc-windows-gnu" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc")))))
(define-public rust-winapi-util-0.1.9
  (let ((name "rust-winapi-util")
        (version "0.1.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi-util" "0.1.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng")))))

(define-public rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (let ((name "rust-winapi-x86-64-pc-windows-gnu")
        (version "0.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "winapi-x86_64-pc-windows-gnu" "0.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki")))))
(define-public rust-windows-0.52.0
  (let ((name "rust-windows")
        (version "0.52.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows" "0.52.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1gnh210qjlprpd1szaq04rjm1zqgdm9j7l9absg0kawi2rwm72p4")))))
(define-public rust-windows-0.58.0
  (let ((name "rust-windows")
        (version "0.58.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows" "0.58.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1dkjj94b0gn91nn1n22cvm4afsj98f5qrhcl3112v6f4jcfx816x")))))

(define-public rust-windows-aarch64-gnullvm-0.42.2
  (let ((name "rust-windows-aarch64-gnullvm")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_aarch64_gnullvm" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr")))))

(define-public rust-windows-aarch64-gnullvm-0.48.5
  (let ((name "rust-windows-aarch64-gnullvm")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_aarch64_gnullvm" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b")))))

(define-public rust-windows-aarch64-gnullvm-0.52.6
  (let ((name "rust-windows-aarch64-gnullvm")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_aarch64_gnullvm" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j")))))

(define-public rust-windows-aarch64-msvc-0.42.2
  (let ((name "rust-windows-aarch64-msvc")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_aarch64_msvc" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0")))))

(define-public rust-windows-aarch64-msvc-0.48.5
  (let ((name "rust-windows-aarch64-msvc")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_aarch64_msvc" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw")))))

(define-public rust-windows-aarch64-msvc-0.52.6
  (let ((name "rust-windows-aarch64-msvc")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_aarch64_msvc" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09")))))

(define-public rust-windows-core-0.52.0
  (let ((name "rust-windows-core")
        (version "0.52.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-core" "0.52.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark")))))
(define-public rust-windows-core-0.58.0
  (let ((name "rust-windows-core")
        (version "0.58.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-core" "0.58.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16czypy425jzmiys4yb3pwsh7cm6grxn9kjp889iqnf2r17d99kb")))))

(define-public rust-windows-i686-gnu-0.42.2
  (let ((name "rust-windows-i686-gnu")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_i686_gnu" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6")))))

(define-public rust-windows-i686-gnu-0.48.5
  (let ((name "rust-windows-i686-gnu")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_i686_gnu" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7")))))

(define-public rust-windows-i686-gnu-0.52.6
  (let ((name "rust-windows-i686-gnu")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_i686_gnu" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf")))))

(define-public rust-windows-i686-gnullvm-0.52.6
  (let ((name "rust-windows-i686-gnullvm")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_i686_gnullvm" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf")))))

(define-public rust-windows-i686-msvc-0.42.2
  (let ((name "rust-windows-i686-msvc")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_i686_msvc" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24")))))

(define-public rust-windows-i686-msvc-0.48.5
  (let ((name "rust-windows-i686-msvc")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_i686_msvc" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg")))))

(define-public rust-windows-i686-msvc-0.52.6
  (let ((name "rust-windows-i686-msvc")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_i686_msvc" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294")))))

(define-public rust-windows-implement-0.58.0
  (let ((name "rust-windows-implement")
        (version "0.58.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-implement" "0.58.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "16spr5z65z21qyv379rv2mb1s5q2i9ibd1p2pkn0dr9qr535pg9b")))))

(define-public rust-windows-interface-0.58.0
  (let ((name "rust-windows-interface")
        (version "0.58.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-interface" "0.58.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "059mxmfvx3x88q74ms0qlxmj2pnidmr5mzn60hakn7f95m34qg05")))))

(define-public rust-windows-link-0.1.0
  (let ((name "rust-windows-link")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-link" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1qr0srnkw148wbrws3726pm640h2vxgcdlxn0cxpbcg27irzvk3d")))))
(define-public rust-windows-registry-0.2.0
  (let ((name "rust-windows-registry")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-registry" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1c04923fq0rbvl3z0h67xr6rh2fgwkizhclhqv0j79i0nwdh0074")))))
(define-public rust-windows-result-0.2.0
  (let ((name "rust-windows-result")
        (version "0.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-result" "0.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x")))))
(define-public rust-windows-strings-0.1.0
  (let ((name "rust-windows-strings")
        (version "0.1.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-strings" "0.1.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac")))))
(define-public rust-windows-sys-0.45.0
  (let ((name "rust-windows-sys")
        (version "0.45.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-sys" "0.45.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m")))))

(define-public rust-windows-sys-0.48.0
  (let ((name "rust-windows-sys")
        (version "0.48.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-sys" "0.48.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7")))))
(define-public rust-windows-sys-0.52.0
  (let ((name "rust-windows-sys")
        (version "0.52.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-sys" "0.52.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8")))))
(define-public rust-windows-sys-0.59.0
  (let ((name "rust-windows-sys")
        (version "0.59.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-sys" "0.59.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y")))))
(define-public rust-windows-targets-0.42.2
  (let ((name "rust-windows-targets")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-targets" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf")))))

(define-public rust-windows-targets-0.48.5
  (let ((name "rust-windows-targets")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-targets" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws")))))
(define-public rust-windows-targets-0.52.6
  (let ((name "rust-windows-targets")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows-targets" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv")))))
(define-public rust-windows-x86-64-gnu-0.42.2
  (let ((name "rust-windows-x86-64-gnu")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_gnu" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd")))))

(define-public rust-windows-x86-64-gnu-0.48.5
  (let ((name "rust-windows-x86-64-gnu")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_gnu" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k")))))
(define-public rust-windows-x86-64-gnu-0.52.6
  (let ((name "rust-windows-x86-64-gnu")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_gnu" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl")))))
(define-public rust-windows-x86-64-gnullvm-0.42.2
  (let ((name "rust-windows-x86-64-gnullvm")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_gnullvm" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16")))))

(define-public rust-windows-x86-64-gnullvm-0.48.5
  (let ((name "rust-windows-x86-64-gnullvm")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_gnullvm" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb")))))
(define-public rust-windows-x86-64-gnullvm-0.52.6
  (let ((name "rust-windows-x86-64-gnullvm")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_gnullvm" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94")))))
(define-public rust-windows-x86-64-msvc-0.42.2
  (let ((name "rust-windows-x86-64-msvc")
        (version "0.42.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_msvc" "0.42.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s")))))

(define-public rust-windows-x86-64-msvc-0.48.5
  (let ((name "rust-windows-x86-64-msvc")
        (version "0.48.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_msvc" "0.48.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d")))))
(define-public rust-windows-x86-64-msvc-0.52.6
  (let ((name "rust-windows-x86-64-msvc")
        (version "0.52.6"))
    (origin
      (method url-fetch)
      (uri (crate-uri "windows_x86_64_msvc" "0.52.6"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq")))))
(define-public rust-winit-0.30.9
  (let ((name "rust-winit")
        (version "0.30.9"))
    (origin
      (method url-fetch)
      (uri (crate-uri "winit" "0.30.9"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h1wmvhfcq2lg6c6715d7pgnv85508zm94ahcfvaiv68337yl2d8")))))

(define-public rust-winnow-0.7.3
  (let ((name "rust-winnow")
        (version "0.7.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "winnow" "0.7.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1c9bmhpdwbdmll6b4l6skabz0296dchnmnxw84hh2y3ggyllwzqf")))))

(define-public rust-winreg-0.50.0
  (let ((name "rust-winreg")
        (version "0.50.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "winreg" "0.50.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj")))))
(define-public rust-wit-bindgen-rt-0.33.0
  (let ((name "rust-wit-bindgen-rt")
        (version "0.33.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wit-bindgen-rt" "0.33.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0g4lwfp9x6a2i1hgjn8k14nr4fsnpd5izxhc75zpi2s5cvcg6s1j")))))
(define-public rust-wl-clipboard-rs-0.8.1
  (let ((name "rust-wl-clipboard-rs")
        (version "0.8.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "wl-clipboard-rs" "0.8.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1nwa0bg6jpq5sd8x94xgkj0yk7zcz2m3sg2mm26b35qlj5rigd0j")))))
(define-public rust-write16-1.0.0
  (let ((name "rust-write16")
        (version "1.0.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "write16" "1.0.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi")))))
(define-public rust-writeable-0.5.5
  (let ((name "rust-writeable")
        (version "0.5.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "writeable" "0.5.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y")))))
(define-public rust-x11-dl-2.21.0
  (let ((name "rust-x11-dl")
        (version "2.21.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "x11-dl" "2.21.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq")))))

(define-public rust-x11rb-0.13.1
  (let ((name "rust-x11rb")
        (version "0.13.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "x11rb" "0.13.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "04jyfm0xmc538v09pzsyr2w801yadsgvyl2p0p76hzzffg5gz4ax")))))
(define-public rust-x11rb-protocol-0.13.1
  (let ((name "rust-x11rb-protocol")
        (version "0.13.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "x11rb-protocol" "0.13.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gfbxf2k7kbk577j3rjhfx7hm70kmwln6da7xyc4l2za0d2pq47c")))))
(define-public rust-xcursor-0.3.8
  (let ((name "rust-xcursor")
        (version "0.3.8"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xcursor" "0.3.8"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0qazsl7h8nrbbzx84qrv39w8m2qc27g0mvrszgdls2v6n6k3vwqf")))))

(define-public rust-xdg-home-1.3.0
  (let ((name "rust-xdg-home")
        (version "1.3.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xdg-home" "1.3.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1xm122zz0wjc8p8cmchij0j9nw34hwncb39jc7dc0mgvb2rdl77c")))))

(define-public rust-xkbcommon-0.8.0
  (let ((name "rust-xkbcommon")
        (version "0.8.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xkbcommon" "0.8.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1j8s1sfwc6bw9phsca65rw3q3b5l2651v1s0pk5yxm6baa9wlrld")))))

(define-public rust-xkbcommon-dl-0.4.2
  (let ((name "rust-xkbcommon-dl")
        (version "0.4.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xkbcommon-dl" "0.4.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1iai0r3b5skd9vbr8z5b0qixiz8jblzfm778ddm8ba596a0dwffh")))))

(define-public rust-xkeysym-0.2.1
  (let ((name "rust-xkeysym")
        (version "0.2.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xkeysym" "0.2.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0mksx670cszyd7jln6s7dhkw11hdfv7blwwr3isq98k22ljh1k5r")))))

(define-public rust-xml-rs-0.8.25
  (let ((name "rust-xml-rs")
        (version "0.8.25"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xml-rs" "0.8.25"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1i73ajf6scni5bi1a51r19xykgrambdx5fkks0fyg5jqqbml1ff5")))))

(define-public rust-xshell-0.2.7
  (let ((name "rust-xshell")
        (version "0.2.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xshell" "0.2.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0g9pd9bfp0f35rzichic55k7p1mn8mqp607y5rimhiq14g390wly")))))

(define-public rust-xshell-macros-0.2.7
  (let ((name "rust-xshell-macros")
        (version "0.2.7"))
    (origin
      (method url-fetch)
      (uri (crate-uri "xshell-macros" "0.2.7"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0irm50jxdc92r0kd6yvl5p28jsfzha59brxk7z9w3jcf7z6h1b1j")))))

(define-public rust-yansi-1.0.1
  (let ((name "rust-yansi")
        (version "1.0.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "yansi" "1.0.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg")))))

(define-public rust-yansi-term-0.1.2
  (let ((name "rust-yansi-term")
        (version "0.1.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "yansi-term" "0.1.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1w8vjlvxba6yvidqdvxddx3crl6z66h39qxj8xi6aqayw2nk0p7y")))))

(define-public rust-yoke-0.7.5
  (let ((name "rust-yoke")
        (version "0.7.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "yoke" "0.7.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj")))))
(define-public rust-yoke-derive-0.7.5
  (let ((name "rust-yoke-derive")
        (version "0.7.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "yoke-derive" "0.7.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013")))))
(define-public rust-zbus-5.5.0
  (let ((name "rust-zbus")
        (version "5.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zbus" "5.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0dmjaih7gi2d0fa37zzylvbmxqn80x4d7haxr5xn86za93v37hsr")))))

(define-public rust-zbus-macros-5.5.0
  (let ((name "rust-zbus-macros")
        (version "5.5.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zbus_macros" "5.5.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1h4zf0wh647fvv97bnsr3ah64cgcnz1r8d10c2q3w2hdxc8as9gk")))))

(define-public rust-zbus-names-4.2.0
  (let ((name "rust-zbus-names")
        (version "4.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zbus_names" "4.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "15ybdd6zic7simi9wjg0ywrxfq4sxg3z0wiyysadps3cpxj8xrkv")))))

(define-public rust-zerocopy-0.7.35
  (let ((name "rust-zerocopy")
        (version "0.7.35"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zerocopy" "0.7.35"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv")))))
(define-public rust-zerocopy-derive-0.7.35
  (let ((name "rust-zerocopy-derive")
        (version "0.7.35"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zerocopy-derive" "0.7.35"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs")))))
(define-public rust-zerofrom-0.1.5
  (let ((name "rust-zerofrom")
        (version "0.1.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zerofrom" "0.1.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bnd8vjcllzrvr3wvn8x14k2hkrpyy1fm3crkn2y3plmr44fxwyg")))))
(define-public rust-zerofrom-derive-0.1.5
  (let ((name "rust-zerofrom-derive")
        (version "0.1.5"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zerofrom-derive" "0.1.5"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "022q55phhb44qbrcfbc48k0b741fl8gnazw3hpmmndbx5ycfspjr")))))
(define-public rust-zeroize-1.8.1
  (let ((name "rust-zeroize")
        (version "1.8.1"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zeroize" "1.8.1"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf")))))
(define-public rust-zeroize-derive-1.4.2
  (let ((name "rust-zeroize-derive")
        (version "1.4.2"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zeroize_derive" "1.4.2"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf")))))
(define-public rust-zerovec-0.10.4
  (let ((name "rust-zerovec")
        (version "0.10.4"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zerovec" "0.10.4"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa")))))
(define-public rust-zerovec-derive-0.10.3
  (let ((name "rust-zerovec-derive")
        (version "0.10.3"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zerovec-derive" "0.10.3"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf")))))
(define-public rust-zvariant-5.4.0
  (let ((name "rust-zvariant")
        (version "5.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zvariant" "5.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "1b53qpb3q7j233512s2684iy7wyydra31pi5vkxwygw98kh9xpxj")))))
(define-public rust-zvariant-derive-5.4.0
  (let ((name "rust-zvariant-derive")
        (version "5.4.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zvariant_derive" "5.4.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0bsbz68dsvkynnnpxpfchmhyl5bsgjjmcbazjg24rf5qhnm0q5vl")))))
(define-public rust-zvariant-utils-3.2.0
  (let ((name "rust-zvariant-utils")
        (version "3.2.0"))
    (origin
      (method url-fetch)
      (uri (crate-uri "zvariant_utils" "3.2.0"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256 (base32 "0d7wllndiv7vwgapmji3q9sxmzbaqfdxjwkqnx9vbmz58gpdyvp1")))))
