let
  pkgs = import ./pkgs.nix;
  alpineImage = pkgs.dockerTools.pullImage {
    imageName = "docker.io/library/alpine";
    imageDigest = "sha256:bc41182d7ef5ffc53a40b044e725193bc10142a1243f395ee852a8d9730fc2ad";
    sha256 = "0cjm69x3pb1sgx4vlr5kfapainbifii6r67hdj848iijqpbdiv6b";
  };
# args: {
#   tagName - TAG_APP or TAG_MIGRATIONS
#   imageName - webapp or webapp_migrations
#   executableName - executable name from cabal file (calendar-api or calendar-api-migrations)
#   executablePackage - executable binary
# }
in { tagName ? "", imageName, executableName, executablePackage }: pkgs.dockerTools.buildImage {
  name = imageName;
  tag = if tagName == "" then "latest" else builtins.getEnv tagName;
  fromImage = alpineImage;
  created = "now";
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    mv /bin/${executableName} /tmp/${executableName}
    mkdir -p /bin/calendar-api
    mv /tmp/${executableName} /bin/calendar-api/${executableName}
  '';
  copyToRoot = pkgs.buildEnv {
    name = "root";
    paths = [
      executablePackage
      pkgs.coreutils
      pkgs.bashInteractive
    ];
    pathsToLink = [ "/bin" ];
  };
  config = {
    Cmd = [ "/bin/calendar-api/${executableName}" ];
    Env = [ "PROD=true" ];
  };
}
