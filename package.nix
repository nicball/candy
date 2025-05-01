{ mkDerivation, base, bytestring, containers, file-embed, freetype2
, gi-harfbuzz, gl, GLFW-b, lib, lrucache, text
}:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers file-embed freetype2 gi-harfbuzz gl
    GLFW-b lrucache text
  ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.licenses.agpl3Plus;
  mainProgram = "candy";
}
