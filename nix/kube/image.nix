{ tag ? "latest"
, name ? "fld-production/dbmigrations-postgresql-service"
, port ? 8080
, pkgs ? import ../. {}
}:
let
  suExec = pkgs.pkgsMusl.su-exec.overrideAttrs(o:{CFLAGS="--static";});
in with pkgs;
dockerTools.buildImage {
  inherit tag name;
  created = "now";

  contents = [ dbmigrations-postgresql-service suExec ];

  config = {
    Cmd = [
      "su-exec"
      "30000:30000" # uid/gid to run the service as
      "dbmigrations-postgresql-service"
      "serve"
      "+RTS"   # the following options are for the RTS...
      "-N"     # use all capabilities (processors)
      "-qn1"   # Only use one capability for parallel GC
    ];
    Env = [
      "PORT=${toString port}"
      "TMPDIR=/"
    ];
    ExposedPorts = {
      "${toString port}/tcp" = {};
    };
  };
}
