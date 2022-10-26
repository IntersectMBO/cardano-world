##########################################################
###############          PV8               ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####
  ByronGenesisFile = ./pv8 + "/byron-genesis.json";
  ByronGenesisHash = "420d6bbc7afcd8b6eb18dbf73b384970bc81a501ff86ef54110c29f29d397ae2";
  ShelleyGenesisFile = ./pv8 + "/shelley-genesis.json";
  ShelleyGenesisHash = "5aeaf6d2e0dea694d480eaeeb4797606f96cc051aa52ed488701052a3e1d404e";
  AlonzoGenesisFile = ./pv8 + "/alonzo-genesis.json";
  AlonzoGenesisHash = "8bfa9249601f7174594b0e792a4c5697421df1ec8102a236224b4e0e398dd1d7";

  ### Core protocol parameters #####
  Protocol = "Cardano";
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;

  ##### Update system Parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;

  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
