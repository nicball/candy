{
  description = "Selection-based editor with eye candies.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/4faa5f5321320e49a78ae7848582f684d64783e9";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.candy =
      let overlay = self: super: {
        libraqm = super.libraqm.overrideAttrs {
          version = "2025-08-17";
          src = self.fetchFromGitHub {
            owner = "HOST-Oman";
            repo = "libraqm";
            rev = "e63eab0cc33612d512e80089c3b076b0a0034cf9";
            sha256 = "sha256-LB3or+Wk8NgfiuILstpSDtkthUdSGxYbPL2sWzHydSU=";
          };
        };
      }; in
      with (import nixpkgs { system = "x86_64-linux"; overlays = [ overlay ]; });
      haskellPackages.callPackage ./package.nix {};

    packages.x86_64-linux.default = self.packages.x86_64-linux.candy;

    devShells.x86_64-linux.default =
      with nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages.shellFor {
        packages = _: [ self.packages.x86_64-linux.candy ];
        nativeBuildInputs = [ haskell-language-server cabal2nix cabal-install ];
      };

  };
}
