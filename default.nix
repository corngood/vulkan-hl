{ mkDerivation, base, bytestring, data-default, linear, mtl, sdl2
, sdl2-vulkan, stdenv, template-haskell, vector, vector-sized
, vulkan
}:
mkDerivation {
  pname = "vulkan-hl";
  version = "0.1.0.0";
  src = if lib.inNixShell then null else ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring data-default mtl sdl2 sdl2-vulkan template-haskell
    vector vector-sized vulkan
  ];
  executableHaskellDepends = [
    base bytestring data-default linear mtl sdl2 sdl2-vulkan vector
    vector-sized vulkan
  ];
  testHaskellDepends = [ base ];
  homepage = "http://github.com/githubuser/vulkan-hl#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
