{ haskellPackages ? ( import <nixpkgs> {}).haskellPackages}:
let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_3
    doctest hashable monadsTf parsec tuple
 unorderedContainers utf8String ansiTerminal
 ; # Haskell dependencies here

in cabal.mkDerivation(self: {
  pname = "hlogic";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
   # as imported above
  doctest hashable monadsTf parsec tuple
 unorderedContainers utf8String ansiTerminal
 ];

  buildTools = [ cabalInstall_1_18_0_3 ];
  enableSplitObjs = false;
})
