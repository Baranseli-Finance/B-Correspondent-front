let
   pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz") {};
   nodejs = pkgs.nodejs-18_x;
   python3 = pkgs.python3;
   python = pkgs.python;
   git = pkgs.git;
   jdk = pkgs.jdk;
   ps = pkgs.purescript;
  #  ps = import ./purescript.nix { inherit pkgs; };
in
pkgs.mkShell {

  buildInputs = [ git nodejs ps jdk python3 ];
  NIX_PATH = "nixpkgs=" + pkgs.path;
  shellHook = ''
    npm install spago
    npm install purs-tidy
    npm install --save-exact esbuild
    npm install @openapitools/openapi-generator-cli -D
    npm install js-beautify
    npm install querystring
    npm install superagent
    npm install crypto-js
    npm install @fingerprintjs/fingerprintjs
    npm install --save string-pixel-width
    export PATH=${ps}:/node_modules/.bin:$PATH
   '';
  }