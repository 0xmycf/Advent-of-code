{
  description = "A very basic flake for gleam (I use this to make new projects)";

  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    nixpkgs.url          = "github:nixos/nixpkgs?ref=23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs-unstable, nixpkgs , flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        unstable-pkgs = nixpkgs-unstable.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            unstable-pkgs.gleam
            erlang
            erlang-ls
          ];
        };
      }
   );
}
