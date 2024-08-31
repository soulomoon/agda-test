{
  description = "The HoTT Game Enviroment";
  nixConfig.bash-prompt-prefix = "(agda)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      emacs-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };
        emacsConfig = pkgs.writeTextFile {
          name = "emacs-config";
          text = ''
            ;; Your custom Emacs configuration goes here
            (setq inhibit-startup-message t)
            (load-theme 'tango-dark t)
            ;; Add more configuration as needed
            (load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
          '';
        };
        shell = pkgs.mkShell {
          buildInputs = [
            pkgs.emacs
            pkgs.emacsPackages.agda2-mode
            (pkgs.agda.withPackages (ps: [
              ps.standard-library
              ps.cubical
            ]))
          ];
          shellHook = ''
            export ee="emacs -l ${emacsConfig}"
          '';
        };
      in
      {
        formatter = nixpkgs.legacyPackages.${system}.nixfmt-rfc-style;
        devShell = shell;
      }
    );
}