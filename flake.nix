{
  nixConfig = {
    extra-substituters = "https://cache.iog.io";
    extra-trusted-public-keys = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
  };  

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      packageName = "calendar-api";
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          ${packageName} =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc925";
              shell = {
                tools = {
                  cabal = "3.8.1.0";
                  hlint = "3.5";
                  # haskell-language-server = {};
                  ormolu = "0.5.1.0";
                };
                buildInputs = with pkgs; [
                  haskellPackages.implicit-hie
                  haskellPackages.cabal-fmt
                  zlib
                  postgresql_14
                ];
                withHoogle = true;
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.${packageName}.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."${packageName}:exe:${packageName}";
      migrationsPackage = flake.packages."${packageName}:exe:${packageName}-migrations";
      inherit pkgs;
    });
}
