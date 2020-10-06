{
  nixpkgs ? import <nixpkgs> {
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
  }
}:
let
  inherit (nixpkgs) pkgs;
  site = with pkgs;
    stdenv.mkDerivation {
      name = "emacs-site";
      src = lib.cleanSource ./src;
      buildInputs = [ emacs mu offlineimap ];
      buildPhase = ''
        emacs --batch --eval "(byte-recompile-file \"$(pwd)/melpa-mirror-packages.el\" t 0)"
        emacs --batch --eval "(byte-recompile-file \"$(pwd)/project+.el\" t 0)"
        emacs --batch --eval "(byte-recompile-file \"$(pwd)/dashboard.el\" t)"
      '';
      installPhase = ''
        mkdir -p $out/share/emacs/site-lisp
        cp -r . $out/share/emacs/site-lisp/
      '';
    };
  emacs = with pkgs;
    emacsWithPackagesFromUsePackage {
      config = builtins.readFile ./src/melpa-mirror-packages.el;
      alwaysEnsure = true;
    };
in pkgs.symlinkJoin {
  name = "emacs-site-lisp";
  paths = [ emacs site ];
}
