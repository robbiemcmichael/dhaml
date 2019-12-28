# Used to build a statically linked executable
#
# From https://github.com/nh2/static-haskell-nix
#
# Run using:
#
#     $(nix-build --no-link -A fullBuildScript)
{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
}:
let
  cabalPackageName = "dhaml";
  compiler = "ghc865";
  snapshot = "2019-12-28T00:00:00Z";

  # Pin static-haskell-nix version.
  static-haskell-nix =
    if builtins.pathExists ../.in-static-haskell-nix
      then toString ../. # for the case that we're in static-haskell-nix itself, so that CI always builds the latest version.
      # Update this hash to use a different `static-haskell-nix` version:
      else fetchTarball https://github.com/nh2/static-haskell-nix/archive/b402b38c3af2300e71caeebe51b5e4e1ae2e924c.tar.gz;

  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.; # where stack.yaml is
    hackageSnapshot = snapshot;
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!/usr/bin/env bash
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static_package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

in
  {
    static_package = static-stack2nix-builder.static_package;
    inherit fullBuildScript;
    # For debugging:
    inherit stack2nix-script;
    inherit static-stack2nix-builder;
  }
