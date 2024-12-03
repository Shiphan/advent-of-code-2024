{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = with pkgs; [
          ghc
      ];
      shellHook = ''
        echo
        echo "\$ Advent of Code 2024 with Haskell!!!"
        echo "\$ ghc --version"
        ghc --version
        echo
      '';
    };
  };
}