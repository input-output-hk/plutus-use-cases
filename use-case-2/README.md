## Installing Obelisk

1. [Install Nix](https://nixos.org/nix/).
    If you already have Nix installed, make sure you have version 2.0 or higher.  To check your current version, run `nix-env --version`.
1. Set up nix caches
    1. If you are running NixOS, add this to `/etc/nixos/configuration.nix`:
        ```nix
        nix.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
        nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        ```
        and rebuild your NixOS configuration (e.g. `sudo nixos-rebuild switch`).
    1. If you are using another operating system or Linux distribution, ensure that these lines are present in your Nix configuration file (`/etc/nix/nix.conf` on most systems; [see full list](https://nixos.org/nix/manual/#sec-conf-file)):
        ```nix
        binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
        binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
        binary-caches-parallel-connections = 40
        ```
        * If you're on a Linux distribution other than NixOS, enable sandboxing (see these [issue 172](https://github.com/obsidiansystems/obelisk/issues/172#issuecomment-411507818) or [issue 6](https://github.com/obsidiansystems/obelisk/issues/6) if you run into build problems) by adding the following:
          ```nix
          sandbox = true
          ```
        * If you're on MacOS, disable sandboxing (there are still some impure dependencies for now) by adding the following:
          ```nix
          sandbox = false
          ```
          then restart the nix daemon
          ```bash
          sudo launchctl stop org.nixos.nix-daemon
          sudo launchctl start org.nixos.nix-daemon
          ```
1. Install obelisk: 
   ```bash
   nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command
   ```
##  Running Plutus Application Backend (PAB)

1. [Unpack plutus-starter GitHub Thunk]
  1. After installing Obelisk, use `ob thunk unpack dep/plutus-starter/`
  1. In another terminal `cd dep/plutus-starter` and run `nix-shell`
  1. Then run `cabal new-repl exe:plutus-starter-pab`
  1. And finally `main` to lauch the PAB and have it listen on port 8080

