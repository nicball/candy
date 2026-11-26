{ mkDerivation, atlas, base, bytestring, containers, gi-glib
, gi-pango, gl, GLFW-b, harfbuzz, harfbuzz-raster, hashable
, haskell-gi-base, hslua, inline-c, lib, lrucaching, pango
, pangoft2, primitive, regex-tdfa, string-interpolate, text
, text-icu
}:
mkDerivation {
  pname = "candy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    atlas base bytestring containers gi-glib gi-pango gl GLFW-b
    hashable haskell-gi-base hslua inline-c lrucaching primitive
    regex-tdfa string-interpolate text text-icu
  ];
  executablePkgconfigDepends = [
    harfbuzz harfbuzz-raster pango pangoft2
  ];
  homepage = "https://github.com/nicball/candy";
  description = "Selection-based editor with eye candies";
  license = lib.meta.getLicenseFromSpdxId "AGPL-3.0-or-later";
  mainProgram = "candy";
}
