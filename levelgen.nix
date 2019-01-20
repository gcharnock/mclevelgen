{ mkDerivation, array, base, bytestring, cereal, containers
, hashable, hashtables, keys, lens, mtl, nbt, pipes
, pipes-bytestring, pipes-cereal, pipes-parse, pipes-zlib, repa
, stdenv, text, text-show, time, vector, zlib
}:
mkDerivation {
  pname = "levelgen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base bytestring cereal containers hashable hashtables keys
    lens mtl nbt pipes pipes-bytestring pipes-cereal pipes-parse
    pipes-zlib repa text text-show time vector zlib
  ];
  license = stdenv.lib.licenses.bsd3;
}
