{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
    name = "purescript-0.15.10";
    # fetchFromGitHub is a build support function that fetches a GitHub
    # repository and extracts into a directory; so we can use it
    # fetchFromGithub is actually a derivation itself :)
    src = pkgs.fetchFromGitHub {
    owner = "purescript";
    repo = "purescript";
    rev = "193977ed819f6cc957a4c253e8a89e3784da0c5b";
    sha256 = "sha256-JfMkpagK/60J/d8qoKrN6+nPkJnCeO/BuLPjMRVG4l4=";
    };
    buildInputs = [pkgs.stack pkgs.llvm pkgs.glib pkgs.which pkgs.haskell.compiler.ghc92];
    installPhase = ''
    mkdir -p $out/ps
    stack --stack-root $out/ps --verbosity info --system-ghc --local-bin-path $out/bin install --fast -j12
    '';
}