{ mkDerivation, base, bytestring, containers, file-embed, freetype2
, gl, GLFW-b, lib, lrucache, libraqm, string-interpolate, text
, text-icu
}:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers file-embed freetype2 gl GLFW-b lrucache
    string-interpolate text text-icu
  ];
  executablePkgconfigDepends = [ libraqm ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.licenses.agpl3Plus;
  mainProgram = "candy";
}
