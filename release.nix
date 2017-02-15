let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          total = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) total;
  }
