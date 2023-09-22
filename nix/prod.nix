let
   pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz") {};
   nodejs = pkgs.nodejs-18_x;
in
pkgs.mkShell { 
  buildInputs = [ nodejs ];
  shellHook = ''
    npm install --save-exact esbuild
    export PATH="./node_modules/.bin:$PATH"
   '';
  }