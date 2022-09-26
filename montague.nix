{ mkDerivation, base, lib, monad-tree, parsec, partial-order
, prettyprinter, row-types, monad-coroutine
}:
mkDerivation {
  pname = "montague";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base monad-tree parsec partial-order prettyprinter row-types monad-coroutine
  ];
  description = "Non-deterministic parser for natural languages";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}