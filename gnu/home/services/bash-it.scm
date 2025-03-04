(define-module (gnu home services bash-it)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services files)
  #:use-module (ice-9 match))

(define-record-type* <home-bash-it-configuration>
  home-bash-it-configuration
  make-home-bash-it-configuration
  home-bash-it-configuration?
  (plugins home-bash-it-configuration-plugins)
  (aliases home-bash-it-configuration-aliases)
  (completions home-bash-it-configuration-completions)
  (theme home-bash-it-configuration-theme))

(define (enabled-symlinks-for-category category names suffix)
  (map (lambda (name)
         (let* ((base "share/bash-it")
                (source (string-append base "/" category "/available/" name "." suffix))
                (dest (string-append base "/" category "/enabled/" name "." suffix)))
           (cons dest (symlink-file source))))
       names))

(define home-bash-it-service-type
  (service-type
   (name 'home-bash-it)
   (extensions
    (list
     (service-extension home-profile-service-type
       (lambda (_)
         (list bash-it)))
     (service-extension home-environment-variables-service-type
       (lambda (_)
         '(("BASH_IT" . "$HOME/.guix-home/profile/share/bash-it"))))
     (service-extension home-bash-service-type
       (lambda (config)
         (list (cons "bashrc"
                     (plain-file "bash-it-init.sh"
                                 (string-append
                                  "#!/bin/sh\n"
                                  "export BASH_IT=\"$HOME/.guix-home/profile/share/bash-it\"\n"
                                  "source \"$BASH_IT/bash_it.sh\"\n"
                                  (if (home-bash-it-configuration-theme config)
                                      (string-append "export BASH_IT_THEME=\""
                                                     (home-bash-it-configuration-theme config)
                                                     "\"\n")
                                      ""))))))
     (service-extension home-files-service-type
       (lambda (config)
         (match config
           (($ <home-bash-it-configuration> plugins aliases completions theme)
            (append (enabled-symlinks-for-category "plugins" plugins "plugin.bash")
                    (enabled-symlinks-for-category "aliases" aliases "aliases.bash")
                    (enabled-symlinks-for-category "completions" completions "completion.bash")))))))
   (default-value (make-home-bash-it-configuration
                   '("git" "docker")    ; plugins
                   '("general" "git")   ; aliases
                   '("git" "ssh")       ; completions
                   "powerline"))        ; theme
   (description "Service that installs and configures Bash-it.
It ensures the bash-it package is in the profile, sets the BASH_IT environment variable,
adds an initialization snippet to bashrc, and updates the enabled symlinks for plugins,
aliases, and completions.")))
