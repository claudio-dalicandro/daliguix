(define-module (gnu home services bash-it)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages shell)
  #:use-module (ice-9 match))

(define-record-type* <home-bash-it-configuration>
  home-bash-it-configuration
  make-home-bash-it-configuration
  home-bash-it-configuration?
  (plugins home-bash-it-configuration-plugins
           (default '("git" "docker")))
  (aliases home-bash-it-configuration-aliases
           (default '("general" "git")))
  (completions home-bash-it-configuration-completions
              (default '("git" "ssh")))
  (theme home-bash-it-configuration-theme
         (default "powerline")))

;; Correctly create symlinks using computed-file and activation gexp
(define (make-bash-it-symlinks config)
  (match config
    (($ <home-bash-it-configuration> plugins aliases completions theme)
     #~(begin
         (use-modules (srfi srfi-1) 
                      (ice-9 ftw)
                      (ice-9 format))
         
         (define (ensure-directory dir)
           (unless (file-exists? dir)
             (mkdir-p dir)))
         
         (define (create-symlinks category items suffix)
           (let* ((base (string-append (getenv "HOME") "/.guix-home/profile/share/bash-it"))
                  (available-dir (string-append base "/" category "/available"))
                  (enabled-dir (string-append base "/" category "/enabled")))
             
             ;; Create the enabled directory if it doesn't exist
             (ensure-directory enabled-dir)
             
             (for-each 
              (lambda (item)
                (let ((source (string-append available-dir "/" item "." suffix))
                      (target (string-append enabled-dir "/" item "." suffix)))
                  (when (file-exists? source)
                    (format #t "Creating symlink: ~a -> ~a~%" source target)
                    (false-if-exception (delete-file target))
                    (symlink source target))))
              items)))
         
         ;; Create directories and symlinks for each category
         (create-symlinks "plugins" '#$plugins "plugin.bash")
         (create-symlinks "aliases" '#$aliases "aliases.bash")
         (create-symlinks "completions" '#$completions "completion.bash")
         
         #t))))

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
                                      "")))))))
     ;; The critical part - use activation service for symlink creation
     (service-extension home-activation-service-type make-bash-it-symlinks)))
   (default-value (home-bash-it-configuration))
   (description "Service that installs and configures Bash-it.
It ensures the bash-it package is in the profile, sets the BASH_IT environment variable,
adds an initialization snippet to bashrc, and creates symlinks for enabled plugins,
aliases, and completions.")))
