{ nixpkgs ? import <nixpkgs> {}, defaultHspkgs ? nixpkgs.haskell.packages.ghc844 }:
let
  fetchFromGitHub = nixpkgs.fetchFromGitHub;
  dontCheck = nixpkgs.haskell.lib.dontCheck; 
  overrideSrc = nixpkgs.haskell.lib.overrideSrc;
  pipes-cereal-github = fetchFromGitHub {
      owner = "ian-mi";
      repo = "pipes-cereal";
      rev = "b59c5fc59e36d1d5d27136d1656001a49d999827";
      sha256 = "02br2ljh6x8igy6jh6zw8pzviq70zbghlqmzf6p0n5mcfn7m1jy8";
  };
  hspkgs = defaultHspkgs.override {
      overrides = self: super: {
          pipes-cereal = overrideSrc super.pipes-cereal {src = pipes-cereal-github;};
      };
  };
in
dontCheck (hspkgs.callPackage ./levelgen.nix {})
