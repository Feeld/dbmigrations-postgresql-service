{ pkgsPath ? null
, compiler ? "ghc864"
}@args:
let nix-build = builtins.fetchTarball "https://github.com/Feeld/fld-ks/archive/01e2bdc0d91273eceb540c6e74e3aa6b0dfdb24c.tar.gz";
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
