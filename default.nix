{ hlsSupport ? true }:
let
  reflex-platform = import ((import <nixpkgs> { }).fetchFromGitHub {
    owner = "Befunctional";
    repo = "reflex-platform";
    rev = "55a855cd0b7547a0e17fffe92650d0203f7bca6e";
    sha256 = "0wnnfbr3nqw5q698iw5ms4dv565d4rdgiww8gvd62wwnqr0b7xrb";
  }) { inherit hlsSupport; };
in reflex-platform.project ({ pkgs, ... }: {
  overrides = self: super: {
    # hspec-wai = self.callHackage "hspec-wai" "0.9.0" {};
  };

  packages = {
    reflex-subtle-crypto = ./reflex-subtle-crypto;
    demo = ./demo;
  };

  shells = {
    ghc = [ "reflex-subtle-crypto" "demo" ];
    ghcjs = [ "reflex-subtle-crypto" "demo" ];
  };
})

