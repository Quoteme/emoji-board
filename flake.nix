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
          aeson
          base
          text
          string-interpolate
          hs-openmoji-data
          fuzzily
          gi-gtk
        ]);
      in
      rec
      {
        defaultPackage = packages.emoji-keyboard;
        packages.emoji-keyboard = pkgs.stdenv.mkDerivation (finalAttrs: {
          pname = "emoji-keyboard";
          version = "1.0";

          src = ./src;

          buildInputs = with pkgs; [
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
            cp emoji-keyboard $out/bin/emoji-keyboard-unwrapped
            echo "wrapping the application to add programs to its PATH..."
            cat > $out/bin/emoji-keyboard <<EOF
              #!/bin/sh
              # Set the necessary environment variables
              export PATH=\$PATH:${pkgs.kdotool}/bin:${pkgs.wl-clipboard}/bin:${pkgs.ydotool}/bin
              # Run the unwrapped application
              exec $out/bin/emoji-keyboard-unwrapped "\$@"
            EOF
            chmod +x $out/bin/emoji-keyboard
          '';

          meta = {
            description = "An emoji-keyboard";
            homepage = "";
          };
        });

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            kdotool
            ydotool
            wl-clipboard
            myGHC
            haskell-language-server
            haskellPackages.haskell-dap
            haskellPackages.ghci-dap
            haskellPackages.haskell-debug-adapter
          ];

          shellHook = ''
            echo "Welcome to the emoji-keyboard dev shell"
          '';
        };
      }
    );
}
