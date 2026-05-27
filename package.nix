{ mkDerivation, atlas, base, bytestring, containers, gi-pango, gl
, GLFW-b, harfbuzz, harfbuzz-raster, haskell-gi-base, hslua
, inline-c, lib, lrucache, pango, pangoft2, primitive
, string-interpolate, text, text-icu
}:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    atlas base bytestring containers gi-pango gl GLFW-b haskell-gi-base
    hslua inline-c lrucache primitive string-interpolate text text-icu
  ];
  executablePkgconfigDepends = [
    harfbuzz harfbuzz-raster pango pangoft2
  ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.licensesSpdx."AGPL-3.0-or-later";
  mainProgram = "candy";
}
