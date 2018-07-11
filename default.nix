{ mkDerivation, base, pure, stdenv }:
mkDerivation {
  pname = "pure-paginate";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure ];
  homepage = "github.com/grumply/pure-paginate";
  license = stdenv.lib.licenses.bsd3;
}