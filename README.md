# emoji-board 🤓👍⌨

A super simple, blazing fast, and lightweight emoji picker for Wayland[^1]

[Example of Emoji-Board running](https://github.com/user-attachments/assets/bff59e56-1eff-4ccc-808d-86b49d452985)

## Installation

If you use NixOS with flakes, you can simply
add this flake to your system configuration like so:

```nix
{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    emoji-board = {
      url = "github:Quoteme/emoji-board";
      # optional, but will reduce the installation size:
      inputs.nixpkgs.follows = "nixpkgs";
    };
  }

  outputs = { self, nixpkgs, emoji-board, ... }@inputs: {
    nixosConfigurations.myConfig = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        ({ nixpkgs, ... }: {
          environment.systemPackages = [
            emoji-board.defaultPackage.x86_64-linux
          ];
        })
      ];
    };
  };
}
```

Now you can run `emoji-board` from the command line 💻.

## Usage

```sh
nix run github:Quoteme/emoji-board
```

---

[^1]: Currently only works on KDE Plasma, because finding the active window requires `kdotool`
