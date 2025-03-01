(define-module (gnu packages shell)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module (guix licenses))

(define-public bash-it
  (let ((version "3.0.3"))
    (package
      (name "bash-it")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Bash-it/bash-it")
                      (commit (string-append "v" version))))
                (sha256
                 (base32 "1b8swdqn3z72l0vcf86p4d1vrq4ab1fbwvhyhidb4igzwamj1405"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("." "share/bash-it"))))
      (home-page "https://github.com/Bash-it/bash-it")
      (synopsis "Bash framework for managing aliases, functions, and prompts")
      (description
       "Bash-it is a collection of community-driven Bash scripts that help you 
customize and extend your Bash environment with aliases, functions, and themes.")
      (license expat))))
