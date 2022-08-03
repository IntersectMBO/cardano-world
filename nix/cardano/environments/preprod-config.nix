##########################################################
###############         Shelley QA         ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####

  # ByronGenesisFile = ./vasil-dev + "/byron-genesis.json";
  # ByronGenesisHash = "c0627a409c0bf0a32b7c9b346809638a149658da81b616b3e14b7df7ce0d87f7";
  # ShelleyGenesisFile = ./vasil-dev + "/shelley-genesis.json";
  # ShelleyGenesisHash = "3c824cd1fa5dda79e3331765960deb3393ab89f97af011b5d3d2ea8b501aaf63";
  # AlonzoGenesisFile = ./vasil-dev + "/alonzo-genesis.json";
  # AlonzoGenesisHash = "8bfa9249601f7174594b0e792a4c5697421df1ec8102a236224b4e0e398dd1d7";

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
