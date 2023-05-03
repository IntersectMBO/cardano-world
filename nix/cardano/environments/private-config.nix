##########################################################
###############          Private           ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####
  ByronGenesisFile = ./private + "/byron-genesis.json";
  ByronGenesisHash = "9a2d021950a717b48ee26079178ea0740e2b67ee9bcfcf4623f81f7c66d7c583";
  ShelleyGenesisFile = ./private + "/shelley-genesis.json";
  ShelleyGenesisHash = "d9d01796f5cfab8da3833dd98d15b3967fecb52ab75df9f0267be7888d535ead";
  AlonzoGenesisFile = ./private + "/alonzo-genesis.json";
  AlonzoGenesisHash = "1759c0d6cf19d5a4187d0ceff3ae1b9b76fcfff7d5d881963eff6d78575ed184";
  ConwayGenesisFile = ./private + "/conway-genesis.json";
  ConwayGenesisHash = "aba97cfd8f4971983a46dbf4433a40d968cfa97411e7dbfe294c256211de895b";

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
  TestMaryHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;


  ##### Update system Parameters #####
  LastKnownBlockVersion-Major = 6;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
