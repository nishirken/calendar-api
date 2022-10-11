let
  pkgs = import ./pkgs.nix;
in pkgs.dockerTools.buildImage {
  name = "webapp";
  tag = builtins.getEnv "TAG";
  fromImageName = "docker.io/library/alpine";
  fromImageTag = "latest";
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ pkgs.defaultPackage pkgs.bash ];
    pathsToLink = [ "/bin" ];
  };
  config = {
    Cmd = [ "/bin/calendar-api" ];
    created = builtins.currentTime;
  };
}
