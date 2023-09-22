#!/bin/sh

tmpDir="$(mktemp -d -t bowser-tarball-unpack.XXXXXXXXXX || \
          oops "Can't create temporary directory for downloading the Bowser tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

unpack=$(pwd)/src/lib/Buzgibi/Web

check_for_existence() {
  if [ -d "$unpack/Bowser" ]; then
      ### Take action if $DIR exists ###
     echo "Bowser is installed skip..."
     exit 0
  fi
}

require_util() {
    command -v "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

url="https://github.com/lancedikson/bowser/archive/refs/tags/2.11.0.tar.gz"
version="2.11.0"

tarball="$tmpDir/$(basename "$tmpDir/nix-$version.tar.xz")"

check_for_existence
require_util curl "download the binary tarball"
require_util tar "unpack the binary tarball"
if [ "$(uname -s)" != "Darwin" ]; then
    require_util xz "unpack the binary tarball"
fi

echo "downloading Bowser $version tarball from '$url' to '$tmpDir'..."
curl -L "$url" -o "$tarball" || oops "failed to download '$url'"

mkdir -p "$unpack"
tar -xf "$tarball" -C "$unpack" || oops "failed to unpack '$url'"
mv "$unpack/bowser-$version" "$unpack/Bowser"