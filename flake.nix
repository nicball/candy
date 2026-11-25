{
  description = "Selection-based editor with eye candies.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/b4fd65b198c599cbe814fcb9f42d25d021595ec9";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.candy =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ (self: super: { harfbuzz = self.callPackage ./harfbuzz {}; }) ]; };
      haskellPackages.callPackage ./package.nix { pangoft2 = pango; harfbuzz-raster = harfbuzz; };

    packages.x86_64-linux.default = self.packages.x86_64-linux.candy;

    devShells.x86_64-linux.default =
      with nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages.shellFor {
        packages = _: [ self.packages.x86_64-linux.candy ];
        nativeBuildInputs = [ haskell-language-server cabal2nix cabal-install ];
      };

  };
}
