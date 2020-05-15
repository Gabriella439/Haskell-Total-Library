let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = pkgs.haskell.lib.packageSourceOverrides {
          total = ./.;
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) total;
  }
