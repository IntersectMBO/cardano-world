##########################################################
###############         Mixed         ####################
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####
  AlonzoGenesisFile = "alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  ByronGenesisFile = "byron-genesis.json";
  ByronGenesisHash = "c29b55983f90f708307b19b2b1b87990c20de18acf86eca55590b481626960a9";
  ShelleyGenesisFile = "shelley-genesis.json";
  ShelleyGenesisHash = "271e0a61c24ce26eb3340d6ab9ab67d6d010dddea604d7856c9b75cf85b2dabf";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 1.1;
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;
  TestMaryHardForkAtEpoch = 0;
  TestShelleyHardForkAtEpoch = 0;

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
