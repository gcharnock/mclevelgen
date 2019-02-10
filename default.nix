let
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  fetchFromGitHub = pkgs.fetchFromGitHub;
  dontCheck = pkgs.haskell.lib.dontCheck; 
  overrideSrc = pkgs.haskell.lib.overrideSrc;

  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghc844 = pkgs.haskell.packages.ghc844.override {
            overrides = self: super: {
              mkDerivation = args: super.mkDerivation (args // {
                enableLibraryProfiling = true;
              });
              contextual-logger = self.callPackage ../contextual-logger/haskell/contextual-logger.nix {};
              pipes-cereal = overrideSrc super.pipes-cereal {
                src = fetchFromGitHub {
                   owner = "ian-mi";
                   repo = "pipes-cereal";
                   rev = "b59c5fc59e36d1d5d27136d1656001a49d999827";
                   sha256 = "02br2ljh6x8igy6jh6zw8pzviq70zbghlqmzf6p0n5mcfn7m1jy8";
                };
              };
              levelgen = self.callPackage ./levelgen.nix {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
# pkgs.haskell.packages.ghc863.contextual-logger
pkgs.haskell.packages.ghc844.levelgen


# { nixpkgs ? import <nixpkgs> {}, defaultHspkgs ? nixpkgs.haskell.packages.ghc844 }:
# let
#   pipes-cereal-github = ;
#   hspkgs = defaultHspkgs.override {
#       overrides = self: super: {
#           mkDerivation = args: super.mkDerivation (args // {
#             enableLibraryProfiling = true;
#           });
#           pipes-cereal = overrideSrc super.pipes-cereal {src = pipes-cereal-github;};
#       };
#   };
# in
# dontCheck (hspkgs.callPackage ./levelgen.nix { })
