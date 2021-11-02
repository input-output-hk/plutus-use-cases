{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }, withHoogle ? false
}:
with obelisk;
let
deps = obelisk.nixpkgs.thunkSet ./dep;
rhyolite = (import deps.rhyolite { inherit obelisk; });
cardano-base = import deps.cardano-base {};
p = project ./. ({ pkgs, hackGet, ... }: {
  inherit withHoogle;
  overrides = pkgs.lib.composeExtensions
    rhyolite.haskellOverrides
		(self: super: with pkgs.haskell.lib; {
			canonical-json = doJailbreak (dontCheck (markUnbroken super.canonical-json));
			nothunks =  doJailbreak (dontCheck (self.callCabal2nix "nothunks" "${deps.nothunks}" {}));
			cardano-prelude-test = doJailbreak (dontCheck (self.callCabal2nix "cardano-prelude-test" "${deps.cardano-prelude}/cardano-prelude-test" {}));
			cardano-prelude = doJailbreak (dontCheck (self.callCabal2nix "cardano-prelude" "${deps.cardano-prelude}/cardano-prelude" {}));
			cardano-binary = doJailbreak (self.callCabal2nix "cardano-binary" "${deps.cardano-base}/binary" {});
			quiet = self.callCabal2nix "quiet" "${deps.quiet}" {};
		});
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
});
in builtins.removeAttrs p ["ios" "android"] // {
    plutus-starter = import deps.plutus-starter {};
    statistics = hackGet ./deps/statistics;
  }
