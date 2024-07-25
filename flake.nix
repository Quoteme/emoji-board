{
  description = "Emoji Keyboard for Wayland";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        myGHC = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          base
          text
          monomer
        ]);
      in rec
      {
        defaultPackage = packages.emoji-keyboard;
        packages.emoji-keyboard = pkgs.stdenv.mkDerivation (finalAttrs: {
          pname = "emoji-keyboard";
          version = "1.0";

          src = ./src;

          nativeBuildInputs = with pkgs; [
            wayland
            wlroots
            wayland-protocols
            myGHC
          ];
          buildInputs = with pkgs; [
            wayland
            wlroots
            wayland-protocols
            myGHC
          ];

          buildPhase = ''
            mkdir build
            cd build
            cp -r $src/* .
            ghc Main.hs
            mv Main emoji-keyboard
          '';
          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/assets/fonts
            echo "adding fonts..."
            cp $src/assets/fonts/Roboto-Regular.ttf $out/assets/fonts
            cp emoji-keyboard $out/bin
          '';

          meta = {
            description = "An emoji-keyboard";
            homepage = "";
          };
        });

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cmake
            pkg-config
            wayland
            wlroots
            wayland-protocols
            myGHC
            haskell-language-server
            haskellPackages.haskell-dap
            haskellPackages.ghci-dap
            haskellPackages.haskell-debug-adapter
          ];
        };
      }
    );
}
