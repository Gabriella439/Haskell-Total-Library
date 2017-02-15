{ mkDerivation, base, stdenv, void }:
mkDerivation {
  pname = "total";
  version = "1.0.4";
  src = ./.;
  libraryHaskellDepends = [ base void ];
  description = "Exhaustive pattern matching using lenses, traversals, and prisms";
  license = stdenv.lib.licenses.bsd3;
}
