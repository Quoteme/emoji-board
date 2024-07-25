{
  description = "Emoji Keyboard for Wayland";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    gemoji-database = {
      url = "https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, gemoji-database }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        myGHC = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          base
          text
          string-interpolate
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
            echo "adding fonts..."
            cp -r $src/assets $out/assets
            cp ${gemoji-database} $out/assets/emoji.json
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

          shellHook = ''
            # check if the emoji database has been copied into the assets folder
            if [ ! -f ./assets/emoji.json ]; then
              echo "Copying emoji database..."
              cp "${gemoji-database}" "src/assets/emoji.json"
            fi
            echo "Welcome to the emoji-keyboard dev shell"
          '';
        };
      }
    );
}
