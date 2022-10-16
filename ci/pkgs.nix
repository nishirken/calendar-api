let
  flake = (builtins.getFlake (toString ../.));
  pkgs = flake.outputs.pkgs."x86_64-linux" // {
    defaultPackage = flake.outputs.defaultPackage."x86_64-linux";
    migrationsPackage = flake.outputs.migrationsPackage."x86_64-linux";
  };
in pkgs
  
