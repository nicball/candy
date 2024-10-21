{ mkDerivation, base, bytestring, freetype2, lib, sdl2 }:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring freetype2 sdl2 ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.licenses.agpl3Plus;
  mainProgram = "candy";
}
