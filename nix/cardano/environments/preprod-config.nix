##########################################################
###############          Preprod           ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####

  ByronGenesisFile = ./preprod + "/byron-genesis.json";
  ByronGenesisHash = "d4b8de7a11d929a323373cbab6c1a9bdc931beffff11db111cf9d57356ee1937";
  ShelleyGenesisFile = ./preprod + "/shelley-genesis.json";
  ShelleyGenesisHash = "90c8bae45d3cb34ef25d84171a74f6e8a4a8f4615fbe289d5a1b21eb6896ac0b";
  AlonzoGenesisFile = ./preprod + "/alonzo-genesis.json";
  AlonzoGenesisHash = "8bfa9249601f7174594b0e792a4c5697421df1ec8102a236224b4e0e398dd1d7";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 2;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
