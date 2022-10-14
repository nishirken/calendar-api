let
  pkgs = import ./pkgs.nix;
  bashImage = pkgs.dockerTools.pullImage {
    imageName = "docker.io/library/bash";
    imageDigest = "sha256:0ba55510cdffa76de0d3d8149a3fa7cb62d9725d1a606fc234d18778e7807ac3";
    sha256 = "1ij9sqwg0q4izg415wjg856ly9b34ajkl1k4cv5qr074v5dpsasr";
  };
in pkgs.dockerTools.streamLayeredImage {
  name = "webapp";
  tag = builtins.getEnv "TAG";
  fromImage = bashImage;
  created = "now";
  contents = [ pkgs.defaultPackage ];
  config = {
    Cmd = [ "/bin/calendar-api" ];
  };
}
