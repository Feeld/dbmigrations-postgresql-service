let
  pkgs = import ./nix {};

  inherit (pkgs)
    myGlibcHaskellPackages
    ;

  shellUtils =
    let
      wrappedCabal = pkgs.writeScript "cabal" ''
        #!${pkgs.stdenv.shell}
        echo Running hpack
        hpack -f
        exec ${pkgs.cabal-install}/bin/cabal "$@"
        '';
    in pkgs.stdenv.mkDerivation {
      name = "hs-shell-utils";
      phases = [ "installPhase" ];
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${wrappedCabal} $out/bin/cabal
        '';
      };
in myGlibcHaskellPackages.dbmigrations-postgresql-service.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or []) ++ (with myGlibcHaskellPackages; [
    ghcid
    hpack
    stylish-haskell
    shellUtils
  ]);
})
