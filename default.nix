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
      buildInputs = [ emacs ];
      buildPhase = ''
        emacs --batch --eval "(byte-recompile-directory \"$(pwd)\" 0)"
      '';
      installPhase = ''
        mkdir -p $out/share/emacs/site-lisp
        cp -r . $out/share/emacs/site-lisp/
      '';
    };
  emacs = with pkgs;
    emacsWithPackagesFromUsePackage {
      config = builtins.readFile ./src/site-packages.el;
      alwaysEnsure = true;
    };
in {
  inherit site;
  inherit emacs;
}
