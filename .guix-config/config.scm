;; This is an operating system configuration template
;; for a "desktop" setup without full-blown desktop
;; environments.

(use-modules (gnu)
             (gnu packages linux)
             (gnu packages tls)
             (gnu system nss)
             (guix build-system trivial)
             (guix download)
             (guix git-download)
             (gnu services xorg)
             (gnu services networking)
             (gnu packages admin)
             (gnu packages xorg)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (srfi srfi-1))

(use-service-modules desktop)
(use-package-modules bootloaders certs suckless ;; xorg
                     )

(define (linux-nonfree-urls version)
  "Return a list of URLs for Linux-Nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/v4.x/"
         "linux-" version ".tar.xz")))

(define-public linux-nonfree
  (package
    (inherit linux-libre)
    (name "linux-nonfree")
    (version "4.14.33")
    (source
     (origin
      (method url-fetch)
      (uri (linux-nonfree-urls
            version
            ;; (package-version linux-libre)
            ))
      (sha256
       (base32
        "0c88p5vly63jsz62ff7971zl6vqzzbv5q519gi8z17ld66sf5063" ; 4.14.33
        ;; "0jwa2r3gpn4ahy38730b7g4xzavfqwxxwgyfhpn5ssvzsc4934gs" ; 4.16.6
        ))))))

(define-public linux-nonfree-firmware
  (let ((version "0.0.0")
        (commit "6d5131107f2ba67a13f469ac770a55f101ba654d"))
    (package
     (name "linux-nonfree-firmware")
     (version version)
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
                    (commit commit)))
              (sha256
               (base32
                "0nql7rqkx064lsw5bh6n29yfdxmp3hl4nqgp1fxdb4ys76awchg3"))))
     (build-system trivial-build-system)
     (arguments
      `(#:modules ((guix build utils))
                #:builder (begin
                          (use-modules (guix build utils))
                          (let ((source (assoc-ref %build-inputs "source"))
                                (destination (string-append %output "/lib/firmware")))
                            (mkdir-p destination)
                            (copy-recursively source destination #:follow-symlinks? #t)
                            #t))))
     (home-page "")
     (synopsis "Non-free firmware for the Linux kernel")
     (description "Non-free firmware for the Linux kernel")
     (license #f))))

;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define my-xorg-modules
  ;; Everything but Nouveau.
  (delete xf86-video-nouveau %default-xorg-modules))

;; Use the "desktop" services, which include the X11
;; log-in service, networking with Wicd, and more.
(define %my-services
  (modify-services
   %desktop-services
   (udev-service-type config =>
                      (udev-configuration
                       (inherit config)
                       (rules (append (udev-configuration-rules config)
                                      (list %backlight-udev-rule)))))
   (slim-service-type config =>
                      (slim-configuration
                       (inherit config)
                       (auto-login? #f)
                       (startx (xorg-start-command #:modules my-xorg-modules))
                       ;; TODO: Can't slim pre-fill the username?
                       (default-user "ambrevar")))))

(operating-system
 (host-name "mimimi")
 (timezone "Europe/Paris")
 (locale "en_US.utf8")

 ;; Use the UEFI variant of GRUB with the EFI System
 ;; Partition mounted on /boot/efi.
 ;; If generating an image to a USB stick, use -bios- instead.
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (timeout 1)
              (target "/boot/efi")))

 (kernel linux-nonfree)
 ;; (kernel-arguments '("modprobe.blacklist=pcspkr"))
 ;; (kernel-arguments '("modprobe.blacklist=nouveau"))
 ;; (kernel-arguments '("pcie_port_pm=off"))
 (firmware (cons* linux-nonfree-firmware %base-firmware))

 (initrd-modules (append (list "shpchp")
                         %base-initrd-modules))

 ;; Assume the target root file system is labelled "guixsd",
 ;; and the EFI System Partition is specified by its UUID.
 (file-systems (cons* (file-system
                       (device "guixsd")
                       (title 'label)   ; TODO: Deprecated?
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device "home")
                       (title 'label)
                       (mount-point "/home")
                       (type "ext4"))
                      (file-system
                       (device (uuid "8AC2-4252" 'fat))
                       (title 'uuid)
                       (mount-point "/boot/efi")
                       (type "vfat"))
                      (file-system
                       (mount-point "/tmp")
                       (device "none")
                       (title 'device)
                       (type "tmpfs")
                       (check? #f))
                      %base-file-systems))

 (users (cons* (user-account
                (name "ambrevar")
                (group "users")
                (supplementary-groups '("wheel" "netdev" ; netdev is needed for networking.
                                        ;; "audio"
                                        "lp" ; for bluetooth
                                        "video"))
                (home-directory "/home/ambrevar"))
               %base-user-accounts))

 (packages (cons* nss-certs             ;for HTTPS access
                  ntfs-3g
                  %base-packages))

 (services (cons*
            ;; TODO: The following service starts too soon and results in a
            ;; kernel panic because /sys/... is not found.
            ;; (simple-service 'my-/sys-tweaks activation-service-type
            ;;                 ;; >> echo '1' > '/sys/module/snd_hda_intel/parameters/power_save';
            ;;                 #~(call-with-output-file "/sys/module/snd_hda_intel/parameters/power_save"
            ;;                     (lambda (port)
            ;;                       (display "1" port)))
            ;;                 ;; >> echo 'auto' > '/sys/bus/usb/devices/1-6/power/control';
            ;;                 ;; >> echo 'auto' > '/sys/bus/usb/devices/1-7/power/control';
            ;;                 ;; >> echo 'auto' > '/sys/bus/i2c/devices/i2c-2/device/power/control';
            ;;                 ;; >> echo 'auto' > '/sys/bus/pci/devices/0000:02:00.0/power/control';
            ;;                 )
            (bluetooth-service)
            %my-services))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
