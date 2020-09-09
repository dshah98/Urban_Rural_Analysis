# C. From the rural data set for any one district perform the following analysis.
#   1.You have to select the most appropriate 05 villages for a ‘rurban’ convergence mission in 
#       the state from this district.
#     d. Choose the top 5 villages of the selected district while comparing their scores from the 
#         two methods.

# Importing Data
DataRural = read.csv("DataRural.csv")

# Segregating District
sub = subset(DataRural, DataRural$DistrictName == "Ahmadabad")

# Linear Normalization
sub1 = data.frame(sub$StateName, sub$DistrictName, sub$TehsilName, sub$AreaName,
                  CensusHouseholds = (sub$CensusHouseholds / max(sub$CensusHouseholds)),
                  Good.CensusHouseholds = (sub$Good.CensusHouseholds / max(sub$Good.CensusHouseholds)),
                  Livable.CenusHouseholds = (sub$Livable.CenusHouseholds / max(sub$Livable.CenusHouseholds)),
                  Dilapidated.CensusHouseholds = (sub$Dilapidated.CensusHouseholds / max(sub$Dilapidated.CensusHouseholds)),
                  CensusResidenceOnly = (sub$CensusResidenceOnly / max(sub$CensusResidenceOnly)),
                  Good.CensusResidenceOnly = (sub$Good.CensusResidenceOnly / max(sub$Good.CensusResidenceOnly)),
                  Livable.CensusResidenceOnly = (sub$Livable.CensusResidenceOnly / max(sub$Livable.CensusResidenceOnly)),
                  Dilapidated.CensusResidenceOnly = (sub$Dilapidated.CensusResidenceOnly / max(sub$Dilapidated.CensusResidenceOnly)),
                  CensusOtherUse = (sub$CensusOtherUse / max(sub$CensusOtherUse)),
                  Good.CensusOtherUse = (sub$Good.CensusOtherUse / max(sub$Good.CensusOtherUse)),
                  Livable.CensusOtherUse = (sub$Livable.CensusOtherUse / max(sub$Livable.CensusOtherUse)),
                  Dilapidated.CensusOtherUse = (sub$Dilapidated.CensusOtherUse / max(sub$Dilapidated.CensusOtherUse)),
                  Grass.Roof = (sub$Grass.Roof / max(sub$Grass.Roof)),
                  Plastic.Roof = (sub$Plastic.Roof / max(sub$Plastic.Roof)),
                  HandTiles.Roof = (sub$HandTiles.Roof / max(sub$HandTiles.Roof)),
                  MachineTiles.Roof = (sub$MachineTiles.Roof / max(sub$MachineTiles.Roof)),
                  Brick.Roof = (sub$Brick.Roof / max(sub$Brick.Roof)),
                  Slate.Roof = (sub$Slate.Roof / max(sub$Slate.Roof)),
                  Metal.Roof = (sub$Metal.Roof / max(sub$Metal.Roof)),
                  Concrete.Roof = (sub$Concrete.Roof / max(sub$Concrete.Roof)),
                  Other.Roof = (sub$Other.Roof / max(sub$Other.Roof)),
                  Mud.Floor = (sub$Mud.Floor / max(sub$Mud.Floor)),
                  Wood.Floor = (sub$Wood.Floor / max(sub$Wood.Floor)),
                  Brick.Floor = (sub$Brick.Floor / max(sub$Brick.Floor)),
                  Stone.Floor = (sub$Stone.Floor / max(sub$Stone.Floor)),
                  Cement.Floor = (sub$Cement.Floor / max(sub$Cement.Floor)),
                  Tile.Floor = (sub$Tile.Floor / max(sub$Tile.Floor)),
                  Other.Floor = (sub$Other.Floor / max(sub$Other.Floor)),
                  No.Rooms = (sub$No.Rooms / max(sub$No.Rooms)),
                  One.Rooms = (sub$One.Rooms / max(sub$One.Rooms)),
                  Two.Rooms = (sub$Two.Rooms / max(sub$Two.Rooms)),
                  Three.Rooms = (sub$Three.Rooms / max(sub$Three.Rooms)),
                  Four.Rooms = (sub$Four.Rooms / max(sub$Four.Rooms)),
                  Five.Rooms = (sub$Five.Rooms / max(sub$Five.Rooms)),
                  SixAndOver.Rooms = (sub$SixAndOver.Rooms / max(sub$SixAndOver.Rooms)),
                  X1.HouseholdSize = (sub$X1.HouseholdSize / max(sub$X1.HouseholdSize)),
                  X2.HouseholdSize = (sub$X2.HouseholdSize / max(sub$X2.HouseholdSize)),
                  X3.HouseholdSize = (sub$X3.HouseholdSize / max(sub$X3.HouseholdSize)),
                  X4.HouseholdSize = (sub$X4.HouseholdSize / max(sub$X4.HouseholdSize)),
                  X5.HouseholdSize = (sub$X5.HouseholdSize / max(sub$X5.HouseholdSize)),
                  X6_8.HouseholdSize = (sub$X6_8.HouseholdSize / max(sub$X6_8.HouseholdSize)),
                  Over9.HouseholdSize = (sub$Over9.HouseholdSize / max(sub$Over9.HouseholdSize)),
                  Owned.Ownership = (sub$Owned.Ownership / max(sub$Owned.Ownership)),
                  Rented.Ownership = (sub$Rented.Ownership / max(sub$Rented.Ownership)),
                  Other.Ownership = (sub$Other.Ownership / max(sub$Other.Ownership)),
                  None.Couples = (sub$None.Couples / max(sub$None.Couples)),
                  One.Couples = (sub$One.Couples / max(sub$One.Couples)),
                  Two.Couples = (sub$Two.Couples / max(sub$Two.Couples)),
                  Three.Couples = (sub$Three.Couples / max(sub$Three.Couples)),
                  Four.Couples = (sub$Four.Couples / max(sub$Four.Couples)),
                  OverFive.Couples = (sub$OverFive.Couples / max(sub$OverFive.Couples)),
                  TreatedTapwater.WaterSource = (sub$TreatedTapwater.WaterSource / max(sub$TreatedTapwater.WaterSource)),
                  UntreatedTapwater.WaterSource = (sub$UntreatedTapwater.WaterSource / max(sub$UntreatedTapwater.WaterSource)),
                  CoveredWell.WaterSource = (sub$CoveredWell.WaterSource / max(sub$CoveredWell.WaterSource)),
                  UncoveredWall.WaterSource = (sub$UncoveredWall.WaterSource / max(sub$UncoveredWall.WaterSource)),
                  Handpump.WaterSource = (sub$Handpump.WaterSource / max(sub$Handpump.WaterSource)),
                  Tubewell.WaterSource = (sub$Tubewell.WaterSource / max(sub$Tubewell.WaterSource)),
                  Spring.WaterSource = (sub$Spring.WaterSource / max(sub$Spring.WaterSource)),
                  River.WaterSource = (sub$River.WaterSource / max(sub$River.WaterSource)),
                  Tank.WaterSource = (sub$Tank.WaterSource / max(sub$Tank.WaterSource)),
                  Other.WaterSource = (sub$Other.WaterSource / max(sub$Other.WaterSource)),
                  WithinPremises.LocationWaterSource = (sub$WithinPremises.LocationWaterSource / max(sub$WithinPremises.LocationWaterSource)),
                  NearPremises.LocationWaterSource = (sub$NearPremises.LocationWaterSource / max(sub$NearPremises.LocationWaterSource)),
                  Away.LocationWaterSource = (sub$Away.LocationWaterSource / max(sub$Away.LocationWaterSource)),
                  Electricty.LightingSource = (sub$Electricty.LightingSource / max(sub$Electricty.LightingSource)),
                  Kerosene.LightingSource = (sub$Kerosene.LightingSource / max(sub$Kerosene.LightingSource)),
                  Solar.LightingSource = (sub$Solar.LightingSource / max(sub$Solar.LightingSource)),
                  Oil.LightingSource = (sub$Oil.LightingSource / max(sub$Oil.LightingSource)),
                  Other.LightingSource = (sub$Other.LightingSource / max(sub$Other.LightingSource)),
                  None.LightingSource = (sub$None.LightingSource / max(sub$None.LightingSource)),
                  HaveLatrine = (sub$HaveLatrine / max(sub$HaveLatrine)),
                  PipedSewer.FlushLatrine = (sub$PipedSewer.FlushLatrine / max(sub$PipedSewer.FlushLatrine)),
                  Septric.FlushLatrine = (sub$Septric.FlushLatrine / max(sub$Septric.FlushLatrine)),
                  Other.FlushLatrine = (sub$Other.FlushLatrine / max(sub$Other.FlushLatrine)),
                  WithSlab.PitLatrine = (sub$WithSlab.PitLatrine / max(sub$WithSlab.PitLatrine)),
                  WithoutSlab.PitLatrine = (sub$WithoutSlab.PitLatrine / max(sub$WithoutSlab.PitLatrine)),
                  NightSoilInOpen = (sub$NightSoilInOpen / max(sub$NightSoilInOpen)),
                  NightSoilbyHuman.ServiceLatrine = (sub$NightSoilbyHuman.ServiceLatrine / max(sub$NightSoil.ServiceLatrine)),
                  NightSoil.ServiceLatrine = (sub$NightSoil.ServiceLatrine / max(sub$NightSoil.ServiceLatrine)),
                  NoLatrine = (sub$NoLatrine / max(sub$NoLatrine)),
                  PublicLatrine = (sub$PublicLatrine / max(sub$PublicLatrine)),
                  OpenLatrine = (sub$OpenLatrine / max(sub$OpenLatrine)),
                  HaveBathroom = (sub$HaveBathroom / max(sub$HaveBathroom)),
                  HaveBathroomNoRoof = (sub$HaveBathroomNoRoof / max(sub$HaveBathroomNoRoof)),
                  NoBathroom = (sub$NoBathroom / max(sub$NoBathroom)),
                  ClosedDrainage.WasteOutlet = (sub$ClosedDrainage.WasteOutlet / max(sub$ClosedDrainage.WasteOutlet)),
                  OpenDrainage.WasteOutlet = (sub$OpenDrainage.WasteOutlet / max(sub$OpenDrainage.WasteOutlet)),
                  NoDrainage.WasteOutlet = (sub$NoDrainage.WasteOutlet / max(sub$NoDrainage.WasteOutlet)),
                  FireWood.Fuel = (sub$FireWood.Fuel / max(sub$FireWood.Fuel)),
                  CropResidue.Fuel = (sub$CropResidue.Fuel / max(sub$CropResidue.Fuel)),
                  Cowdung.Fuel = (sub$Cowdung.Fuel / max(sub$Cowdung.Fuel)),
                  Kerosene.Fuel = (sub$Kerosene.Fuel / max(sub$Kerosene.Fuel)),
                  LPG.Fuel = (sub$LPG.Fuel / max(sub$LPG.Fuel)),
                  Electricity.Fuel = (sub$Electricity.Fuel / max(sub$Electricity.Fuel)),
                  Biogas.Fuel = (sub$Biogas.Fuel / max(sub$Biogas.Fuel)),
                  Other.Fuel = (sub$Other.Fuel / max(sub$Other.Fuel)),
                  None.Fuel = (sub$None.Fuel / max(sub$None.Fuel)),
                  CensusKitchens = (sub$CensusKitchens / max(sub$CensusKitchens)),
                  InsideKitchens = (sub$InsideKitchens / max(sub$InsideKitchens)),
                  Has.InsideKitchens = (sub$Has.InsideKitchens / max(sub$Has.InsideKitchens)),
                  No.InsideKitchens = (sub$No.InsideKitchens / max(sub$No.InsideKitchens)),
                  OutsideKitchens = (sub$OutsideKitchens / max(sub$OutsideKitchens)),
                  Has.OutsideKitchens = (sub$Has.OutsideKitchens / max(sub$Has.OutsideKitchens)),
                  No.OutsideKitchens = (sub$No.OutsideKitchens / max(sub$No.OutsideKitchens)),
                  NoKitchen = (sub$NoKitchen / max(sub$NoKitchen)),
                  BankingHouseholds = (sub$BankingHouseholds / max(sub$BankingHouseholds)),
                  Radio.Assets = (sub$Radio.Assets / max(sub$Radio.Assets)),
                  Television.Assets = (sub$Television.Assets / max(sub$Television.Assets)),
                  InternetComp.Assets = (sub$InternetComp.Assets / max(sub$InternetComp.Assets)),
                  Computer.Assets = (sub$Computer.Assets / max(sub$Computer.Assets)),
                  Landline.Assets = (sub$Landline.Assets / max(sub$Landline.Assets)),
                  Mobile.Assets = (sub$Mobile.Assets / max(sub$Mobile.Assets)),
                  Both.Assets = (sub$Both.Assets / max(sub$Both.Assets)),
                  Bicycle.Assets = (sub$Bicycle.Assets / max(sub$Bicycle.Assets)),
                  Scooter.Assets = (sub$Scooter.Assets / max(sub$Scooter.Assets)),
                  Car.Assets = (sub$Car.Assets / max(sub$Car.Assets)),
                  All.Assets = (sub$All.Assets / max(sub$All.Assets)),
                  None.Assets = (sub$None.Assets / max(sub$None.Assets)),
                  Permanent.Structure = (sub$Permanent.Structure / max(sub$Permanent.Structure)),
                  SemiPermanent.Structure = (sub$SemiPermanent.Structure / max(sub$SemiPermanent.Structure)),
                  Temporary.Structure = (sub$Temporary.Structure / max(sub$Temporary.Structure)),
                  Serviceable.Structure = (sub$Serviceable.Structure / max(sub$Serviceable.Structure)),
                  Unserviceable.Structure = (sub$Unserviceable.Structure / max(sub$Unserviceable.Structure)),
                  Unclassifiable.Structure = (sub$Unclassifiable.Structure / max(sub$Unclassifiable.Structure)),
                  No_HH = (sub$No_HH / max(sub$No_HH)),
                  TOT_P = (sub$TOT_P / max(sub$TOT_P)),
                  TOT_M = (sub$TOT_M / max(sub$TOT_M)),
                  TOT_F = (sub$TOT_F / max(sub$TOT_F)),
                  P_06 = (sub$P_06 / max(sub$P_06)),
                  P_SC = (sub$P_SC / max(sub$P_SC)),
                  P_LIT = (sub$P_LIT / max(sub$P_LIT)),
                  M_LIT = (sub$M_LIT / max(sub$M_LIT)),
                  F_LIT = (sub$F_LIT / max(sub$F_LIT)),
                  TOT_WORK_P = (sub$TOT_WORK_P / max(sub$TOT_WORK_P)),
                  TOT_WORK_M = (sub$TOT_WORK_M / max(sub$TOT_M)),
                  TOT_WORK_F = (sub$TOT_WORK_F / max(sub$TOT_F)),
                  NON_WORK_P = (sub$NON_WORK_P / max(sub$NON_WORK_P)),
                  NON_WORK_M = (sub$NON_WORK_M / max(sub$NON_WORK_M)),
                  NON_WORK_F = (sub$NON_WORK_F / max(sub$NON_WORK_F)))
                
                     

Overall = (sub1$CensusHouseholds + sub1$Good.CensusHouseholds + sub1$Livable.CenusHouseholds + 
             sub1$Dilapidated.CensusHouseholds + sub1$CensusResidenceOnly + 
             sub1$Good.CensusResidenceOnly + sub1$Livable.CensusResidenceOnly + 
             sub1$Dilapidated.CensusResidenceOnly + sub1$CensusOtherUse + sub1$Good.CensusOtherUse +
             sub1$Livable.CensusOtherUse + sub1$Dilapidated.CensusOtherUse + sub1$Grass.Roof + 
             sub1$Plastic.Roof + sub1$HandTiles.Roof + sub1$MachineTiles.Roof + sub1$Brick.Roof + 
             sub1$Slate.Roof + sub1$Metal.Roof + sub1$Concrete.Roof + sub1$Other.Roof + 
             sub1$Mud.Floor + sub1$Wood.Floor + sub1$Brick.Floor + sub1$Stone.Floor + 
             sub1$Cement.Floor + sub1$Tile.Floor + sub1$Other.Floor + sub1$No.Rooms + sub1$One.Rooms +
             sub1$Two.Rooms + sub1$Three.Rooms + sub1$Four.Rooms + sub1$Five.Rooms + 
             sub1$SixAndOver.Rooms + sub1$X1.HouseholdSize + sub1$X2.HouseholdSize + 
             sub1$X3.HouseholdSize + sub1$X4.HouseholdSize + sub1$X5.HouseholdSize + 
             sub1$X6_8.HouseholdSize + sub1$Over9.HouseholdSize + sub1$Owned.Ownership + 
             sub1$Rented.Ownership + sub1$Other.Ownership + sub1$None.Couples + sub1$One.Couples + 
             sub1$Two.Couples + sub1$Three.Couples + sub1$Four.Couples + sub1$OverFive.Couples + 
             sub1$TreatedTapwater.WaterSource + sub1$UntreatedTapwater.WaterSource + 
             sub1$CoveredWell.WaterSource + sub1$UncoveredWall.WaterSource + sub1$Handpump.WaterSource +
             sub1$Tubewell.WaterSource + sub1$Spring.WaterSource + sub1$River.WaterSource + 
             sub1$Tank.WaterSource + sub1$Other.WaterSource + sub1$WithinPremises.LocationWaterSource +
             sub1$NearPremises.LocationWaterSource + sub1$Away.LocationWaterSource + 
             sub1$Electricty.LightingSource + sub1$Kerosene.LightingSource + sub1$Solar.LightingSource +
             sub1$Solar.LightingSource + sub1$Oil.LightingSource + sub1$Other.LightingSource + 
             sub1$None.LightingSource + sub1$None.LightingSource + sub1$HaveLatrine + 
             sub1$PipedSewer.FlushLatrine + sub1$Septric.FlushLatrine + sub1$Other.FlushLatrine +
             sub1$WithSlab.PitLatrine + sub1$WithoutSlab.PitLatrine + sub1$NightSoilInOpen + 
             sub1$NightSoilbyHuman.ServiceLatrine + sub1$NightSoil.ServiceLatrine + sub1$NoLatrine + 
             sub1$PublicLatrine + sub1$PublicLatrine + sub1$HaveBathroom + sub1$HaveBathroomNoRoof +
             sub1$NoBathroom + sub1$ClosedDrainage.WasteOutlet + sub1$OpenDrainage.WasteOutlet + 
             sub1$NoDrainage.WasteOutlet + sub1$FireWood.Fuel + sub1$CropResidue.Fuel + 
             sub1$Cowdung.Fuel + sub1$Kerosene.Fuel + sub1$LPG.Fuel + sub1$Electricity.Fuel + 
             sub1$Biogas.Fuel + sub1$Other.Fuel + sub1$None.Fuel + sub1$CensusKitchens + 
             sub1$InsideKitchens + sub1$Has.InsideKitchens + sub1$Has.OutsideKitchens + 
             sub1$OutsideKitchens + sub1$Has.OutsideKitchens + sub1$No.OutsideKitchens + 
             sub1$NoKitchen + sub1$BankingHouseholds + sub1$Radio.Assets + sub1$Television.Assets + 
             sub1$InternetComp.Assets + sub1$Computer.Assets + sub1$Landline.Assets + 
             sub1$Mobile.Assets + sub1$Both.Assets + sub1$Bicycle.Assets + sub1$Bicycle.Assets + 
             sub1$Scooter.Assets + sub1$Car.Assets + sub1$Car.Assets + sub1$All.Assets + 
             sub1$None.Assets + sub1$Permanent.Structure + sub1$SemiPermanent.Structure + 
             sub1$Temporary.Structure + sub1$Serviceable.Structure + sub1$Unserviceable.Structure + 
             sub1$Unclassifiable.Structure + sub1$No_HH + sub1$TOT_P + sub1$TOT_M +sub1$TOT_F + 
             sub1$P_06 + sub1$P_SC + sub1$P_LIT + sub1$M_LIT + sub1$F_LIT + sub1$TOT_WORK_P + 
             sub1$TOT_WORK_M + sub1$TOT_WORK_F + sub1$NON_WORK_P + sub1$NON_WORK_M + sub1$NON_WORK_F)

sub1 = cbind(sub1, Overall)

# By calulating Liner Normalization we get; Mandal, Dholka, Daskroi, Bavla, Detroj-Rampura, as a
# top 5 village.