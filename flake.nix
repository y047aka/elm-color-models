{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-25.11-darwin";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.elmPackages; [
            elm
            elm-format
            elm-test
            elm-json
          ];

          shellHook = ''
            echo "🎨 elm-color-models - Dev Environment"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo ""
            echo "Nix-managed tools:"
            echo "  • elm: $(elm --version)"
            echo "  • elm-format: $(elm-format -h | head -1 | awk '{print $NF}')"
            echo "  • elm-test: $(elm-test --version)"
            echo "  • elm-json: $(elm-json --version | awk '{print $NF}')"
            echo ""
          '';
        };
      });
}
