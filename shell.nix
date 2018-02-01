{ nixpkgs ? import <nixpkgs> {}

, compiler ? "default"

, withProfiling ? false
, withHoogle    ? false 

, development   ? true
}:

/* Usage:

  nix-shell
  cabal configure 
  cabal build
  cabal run

*/

########################################
  
let

  inherit (nixpkgs) pkgs;
  inherit (pkgs)    fetchFromGitHub;

  lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };
  hs = pkgs.haskell.lib;

  ########################################

  cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
  } "";

  sources = {

    # This is where to put the output from nix-prefetch-git
    #
    # This is based on the results o
    #   nix-prefetch-git http://github.com/ekmett/mtl
    #
    # For general git fetching:
    #
    # mtl = fetchgit {
    #   url = "http://github.com/ekmett/mtl";
    #   rev = "f75228f7a750a74f2ffd75bfbf7239d1525a87fe";
    #   sha256= "032s8g8j4djx7y3f8ryfmg6rwsmxhzxha2qh1fj15hr8wksvz42a";
    # };
    #
    # Or, more efficient for github repos:
    #
    # mtl = fetchFromGitHub {
    #   owner = "ekmett";
    #   repo = "mtl";
    #   rev = "f75228f7a750a74f2ffd75bfbf7239d1525a87fe";
    #   sha256= "032s8g8j4djx7y3f8ryfmg6rwsmxhzxha2qh1fj15hr8wksvz42a";
    # };
  };

  ########################################

  haskellPackagesWithCompiler = 
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackagesWithProfiling = 
    if withProfiling
    then haskellPackagesWithCompiler.override {
           overrides = self: super: {
             mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
           };
         }
    else haskellPackagesWithCompiler;
                 
  haskellPackagesWithHoogle =
    if withHoogle
    then haskellPackagesWithProfiling.override {
           overrides = self: super: {
             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
             ghcWithPackages = self.ghc.withPackages;
           };
         }
    else haskellPackagesWithProfiling;

  ########################################
  
  modifiedHaskellPackages = haskellPackagesWithHoogle.override {
    overrides = self: super: {

      spiros = self.callPackage ../spiros {};

      # exception-transformers = hs.dontCheck super.exception-transformers;
      #       # Setup: Encountered missing dependencies:
      #       # HUnit >=1.2 && <1.6
      #       # builder for ‘/nix/store/365zv27f15qplgd6gd58fa8v26x2gg5z-exception-transformers-0.4.0.5.drv’ failed with exit code 1

      # Add various dependencies here.
      #
      # Local dependencies:
      # my-dependency = self.callPackage ./deps/my-dependency {};
      #
      # Local dependencies with tests disabled:
      # my-dependency = lib.dontCheck (self.callPackage ./deps/my-dependency {});
      #
      # Git dependencies:
      # mtl = self.callPackage (cabal2nixResult sources.mtl) {};
    };
  };

  ########################################
  
  installationDerivation = modifiedHaskellPackages.callPackage ./. {};

  # development environment
  # for `nix-shell --pure`
  developmentDerivation = hs.linkWithGold 
      (hs.addBuildDepends installationDerivation developmentPackages);

  developmentPackages = developmentHaskellPackages
                     # ++ developmentEmacsPackages 
                     ++ developmentSystemPackages;

  developmentSystemPackages = with pkgs; [
  
      cabal-install

      coreutils
      inotify-tools
  
      emacs
      git

    ];

   developmentHaskellPackages = with modifiedHaskellPackages; [
  
      # ghcid
      # ghc-mod

      stylish-haskell
      hasktags
      present
      hlint
      hoogle
      hindent
  
    ];

   # developmentHaskellPackages = with Packages; [
   #    dante
   #  ];

  env = hs.shellAware developmentDerivation;
        # if pkgs.lib.inNixShell then drv.env else drv;

in

  env

########################################

/*

*/
