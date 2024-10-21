{
  description = "Selection-based editor with eye candies.";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.candy =
      with nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages.callPackage ./package.nix {};

    packages.x86_64-linux.default = self.packages.x86_64-linux.candy;

    devShells.x86_64-linux.default =
      with nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages.shellFor {
        packages = _: [ self.packages.x86_64-linux.candy ];
        nativeBuildInputs = [ haskell-language-server cabal2nix ];
      };

  };
}
