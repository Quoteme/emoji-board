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
        haskellPackages = with pkgs.haskellPackages; {
          ghc = ghcWithPackages (ps: with ps; [ base text ]);
        };
      in
      {
        packages.emoji-keyboard = pkgs.mkDerivation {
          name = "emoji-keyboard";
          src = ./.;
          buildInputs = [ pkgs.wayland pkgs.wlroots pkgs.wlroots pkgs.wayland-protocols pkgs.wayland-protocols pkgs.wayland-protocols ];
          nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config pkgs.wayland-protocols ];
          buildPhase = ''
            mkdir build
            cd build
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp emoji-keyboard $out/bin
          '';
        };

      }
    );
}
