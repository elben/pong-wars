{ mkDerivation, base, bytestring, clock, containers, HUnit
, QuickCheck, random, sdl2, sdl2-image, sdl2-mixer, sdl2-ttf
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, unix
}:
mkDerivation {
  pname = "pong-wars";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base bytestring clock containers HUnit QuickCheck random sdl2
    sdl2-image sdl2-mixer sdl2-ttf test-framework test-framework-hunit
    test-framework-quickcheck2 text unix
  ];
  testHaskellDepends = [
    base HUnit test-framework test-framework-hunit
  ];
  homepage = "https://github.com/elben/pong-wars#readme";
  license = stdenv.lib.licenses.bsd3;
}
