{
  nixpkgs ? import <nixpkgs> {
    overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay.git";
        # rev = "ef220b4a0990fd0f5dd25c588c24b2c1ad88c8fc";
        rev = "43e04abbb498176bfaac9633cf58870616371c4a";
        ref = "master";
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
      buildInputs = [ emacsWithPackages ];
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
  emacsWithPackages = with pkgs;
    emacsWithPackagesFromUsePackage {
      # Emacs is built from Git each time. Get rid of this once we've finished testing the straight.el issue
      package = pkgs.emacsGit;
      config = builtins.readFile ./src/melpa-mirror-packages.el;
      alwaysEnsure = true;
    };
in pkgs.symlinkJoin {
  name = "emacs-site-lisp";
 paths = [ site emacsWithPackages ];
}
