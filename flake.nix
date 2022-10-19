{
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
              compiler-nix-name = "ghc924";
              shell = {
                tools = {
                  cabal = "3.8.1.0";
                  hlint = "latest";
                  haskell-language-server = "latest";
                  ormolu = "0.5.0.1";
                };
                buildInputs = with pkgs; [
                  haskellPackages.implicit-hie
                  haskellPackages.cabal-fmt
                  zlib
                  postgresql_14
                ];
                shellHook = ''
                  export PGDIR=".tmp/calendardb"
                  [ ! -d $PGDIR ] && pg_ctl initdb -D $PGDIR
                '';
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
