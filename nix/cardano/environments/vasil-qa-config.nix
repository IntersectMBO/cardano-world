##########################################################
###############         Shelley QA         ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####

  ByronGenesisFile = ./vasil-qa + "/byron-genesis.json";
  ByronGenesisHash = "b9ab777b0082830feec4ba85dcbf142432de53a2e99b417a00a5849355d4094e";
  ShelleyGenesisFile = ./vasil-qa + "/shelley-genesis.json";
  ShelleyGenesisHash = "6e8cb5eca0a845639098fc0b7274b3420d28e1f81d9f0ef4d0017b3fe5d11c94";
  AlonzoGenesisFile = ./vasil-qa + "/alonzo-genesis.json";
  AlonzoGenesisHash = "77b4d1dde534cf42c17d8f403196ce52a0c9eaf2f9b97adb01e1d744f5e263e8";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 0.9;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
  TestAllegraHardForkAtEpoch = 2;
  TestAlonzoHardForkAtEpoch = 4;
  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;
  TestMaryHardForkAtEpoch = 3;
  TestShelleyHardForkAtEpoch = 1;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
