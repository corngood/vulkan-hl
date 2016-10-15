{ pkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let
  SDL_vulkan = pkgs.callPackage ~/git/SDL_vulkan {};
  nixpkgs.haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      sdl2 = self.callPackage ./sdl2 {};
      sdl2-vulkan = self.callPackage ./sdl2-vulkan { inherit SDL_vulkan; };
      vulkan = self.callPackage ./vulkan {};
      vulkan-hl = self.callPackage ./. {};
    };
  };
in nixpkgs.haskellPackages.vulkan-hl
