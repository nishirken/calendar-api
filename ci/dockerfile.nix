let pkgs = import ./pkgs.nix; in
import ./mk-dockerfile.nix {
  tagName = "TAG_APP";
  imageName = "webapp";
  executableName = "calendar-api";
  executablePackage = pkgs.defaultPackage;
}
