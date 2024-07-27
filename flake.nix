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

          nativeBuildInputs = with pkgs; [
            makeWrapper
          ];

          buildInputs = with pkgs; [
            kdotool
            ydotool
            wl-clipboard
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
            cp emoji-keyboard $out/bin/
            # copy over the .desktop file
            mkdir -p $out/share/applications
            cp assets/emoji-keyboard.desktop $out/share/applications/
            chmod +x $out/bin/emoji-keyboard
          '';

          preFixup = ''
            wrapProgram "$out/bin/emoji-keyboard" \
              --prefix PATH : ${pkgs.kdotool}/bin \
              --prefix PATH : ${pkgs.ydotool}/bin \
              --prefix PATH : ${pkgs.wl-clipboard}/bin
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
