##########################################################
###############         Shelley QA         ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####

  ByronGenesisFile = ./vasil-qa + "/byron-genesis.json";
  ByronGenesisHash = "301f1b4b238645a2823c3cb52ff14c6db7e5e54620213f042bc979a0a1b37a73";
  ShelleyGenesisFile = ./vasil-qa + "/shelley-genesis.json";
  ShelleyGenesisHash = "1f3e40405ac64df7beb7713b16674b8f70934ce910e00a179fa257b6ac0b2caa";
  AlonzoGenesisFile = ./vasil-qa + "/alonzo-genesis.json";
  AlonzoGenesisHash = "fdcb1b56e71318e73aea4042e0ecfdf5ed8bd4ab677d505f5238d47a7672e1ad";

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
