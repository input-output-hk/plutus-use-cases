{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
  }
  , withHoogle ? false
}:
with obelisk;
project ./. ({ ... }: {
  inherit withHoogle;
  android.applicationId = "hsloan.defapp.with.rhyolite";
  android.displayName = "Default Application With Rhyolite";
  ios.bundleIdentifier = "hsloan.defapp.with.rhyolite";
  ios.bundleName = "Default Application With Rhyolite";
})
