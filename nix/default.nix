{ pkgsPath ? null
, compiler ? "ghc864"
, fullyStatic ? true
}@args:
let
  nix-build = builtins.fetchTarball "https://github.com/Feeld/nix-build/archive/d692d8fbaa54353d00ee9ece08c53eaad2de8ac8.tar.gz";
  
in import nix-build (args // {
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
