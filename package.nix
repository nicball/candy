{ mkDerivation, base, bytestring, containers, freetype2, gi-pango
, gi-pangocairo, gl, GLFW-b, harfbuzz, haskell-gi-base, hslua
, inline-c, lib, lrucache, pango, string-interpolate
, text, text-icu
}:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers freetype2 gi-pango gi-pangocairo gl
    GLFW-b haskell-gi-base hslua inline-c lrucache string-interpolate
    text text-icu
  ];
  executablePkgconfigDepends = [ harfbuzz pango ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.licensesSpdx."AGPL-3.0-or-later";
  mainProgram = "candy";
}
