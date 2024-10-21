{ mkDerivation, base, bytestring, freetype2, gi-harfbuzz, lib, sdl2
, text
}:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring freetype2 gi-harfbuzz sdl2 text
  ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.licenses.agpl3Plus;
  mainProgram = "candy";
}
