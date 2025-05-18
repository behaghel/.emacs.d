{
  description = "Emacs configuration with strict style enforcement";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default = let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in pkgs.mkShell {
      packages = with pkgs; [ emacs editorconfig-core-c git ];
      shellHook = ''
        export EDITOR=emacs
        git config core.hooksPath .githooks
      '';
    };
  };
}
