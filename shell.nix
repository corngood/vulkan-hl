{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) stdenv lib;
  SDL_vulkan = pkgs.callPackage ./SDL_vulkan {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      sdl2 = self.callPackage ./sdl2 {};
      sdl2-vulkan = self.callPackage ./sdl2-vulkan { inherit SDL_vulkan; };
      vulkan = self.callPackage ./vulkan {};
      vulkan-hl = self.callPackage ./. {};
    };
  };
  drv = haskellPackages.vulkan-hl;
  env = lib.overrideDerivation drv.env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [
      pkgs.cabal-install
      haskellPackages.ghc-mod
      haskellPackages.hindent
      haskellPackages.apply-refact
      haskellPackages.hlint
      haskellPackages.structured-haskell-mode
      haskellPackages.hoogle
      haskellPackages.stylish-haskell
    ];
    vulkan_hl_datadir = "data";
    shellHook = "export LD_LIBRARY_PATH=${pkgs.vulkan-loader}/lib:$LD_LIBRARY_PATH";
  });
in
  if stdenv.lib.inNixShell then env else drv
