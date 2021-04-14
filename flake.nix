{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }: {
      nixosModules.kmonad = import ./nix/nixos-module.nix;
      overlay = final: prev: {
        kmonad = prev.haskellPackages.callPackage ./nix/kmonad.nix {};
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.kmonad = pkgs.haskellPackages.callPackage ./nix/kmonad.nix {};
        defaultPackage = packages.kmonad;
      }
    );
}
