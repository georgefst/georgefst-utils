{ pkgs, ... }: {
  name = "georgefst-utils";
  compiler-nix-name = "ghc912";
  shell.tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
