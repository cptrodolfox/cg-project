{ mkDerivation, base, bytestring, cassava, GLUT, hashmap, OpenGL
, stdenv, text, vector
}:
mkDerivation {
  pname = "cg-project";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cassava GLUT hashmap OpenGL text vector
  ];
  homepage = "https://github.com/cptrodolfox/cg-project#readme";
  license = stdenv.lib.licenses.bsd3;
}
