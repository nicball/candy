{ mkDerivation, base, bytestring, lib, sdl2, SDL2_Pango, text }:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring sdl2 text ];
  executablePkgconfigDepends = [ SDL2_Pango ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.licenses.agpl3Plus;
  mainProgram = "candy";
}
