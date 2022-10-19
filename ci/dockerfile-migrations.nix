let pkgs = import ./pkgs.nix; in
import ./mk-dockerfile.nix {
  tagName = "TAG_MIGRATIONS";
  imageName = "webapp_migrations";
  executableName = "calendar-api-migrations";
  executablePackage = pkgs.migrationsPackage;
}
