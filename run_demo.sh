#!/bin/sh

pushd dist-ghcjs/build/x86_64-linux/ghcjs-8.6.0.1/demo-0.0.1/x/demo/build/demo/demo.jsexe
nix-shell -p python3 --run 'python3 -m http.server'
popd
