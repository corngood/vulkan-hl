{ mkDerivation, base, sdl2, SDL_vulkan, stdenv, vulkan }:
mkDerivation {
  pname = "sdl2-vulkan";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base sdl2 vulkan ];
  librarySystemDepends = [ SDL_vulkan ];
  homepage = "https://github.com/corngood/sdl2-vulkan";
  description = "A binding for SDL2_vulkan";
  license = stdenv.lib.licenses.mit;
}
