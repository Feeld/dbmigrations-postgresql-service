{ pkgsPath ? null
, compiler ? "ghc864"
, user ? null
, pass ? null
}@args:
let nix-build =
  if user != null && pass != null
  then builtins.fetchTarball "https://${user}:${pass}@github.com/Feeld/fld-auth/archive/054e48a8de38710c4448f9d496c176dc38af085d.tar.gz"
  else ../../feeld/fld-auth;
in import "${nix-build}/nix" (args // {
  haskellOverlay = pkgs: self: super:
    with pkgs.haskell.lib;
    let
      inherit (pkgs.lib)
        cleanSource
        withPostgres
        foldl
        flip;

    in
    {
      dbmigrations-postgresql-service =
        let
          drv = self.callCabal2nix "dbmigrations-postgresql-service" (cleanSource ../.) {};
        in foldl (acc: f: f acc) drv
          [ (if (pkgs.stdenv.targetPlatform.isMusl || pkgs.stdenv.isDarwin) then (x: x) else linkWithGold)
            (flip appendConfigureFlag "--ghc-option=-Werror")
            (withPostgres pkgs.pkgsGlibc.postgresql)
          ];
    };

  overlay = self: super:
    {
      dbmigrations-postgresql-service =
        self.haskell.lib.justStaticExecutables
          self.myGlibcHaskellPackages.dbmigrations-postgresql-service;
    };
  })
