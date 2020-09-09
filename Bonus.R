# If you needed to find a way to conclude which, if any, of the following cities/states were 
#   outperforming or under-performing the other states on overall development of sanitation 
#   infrastructure, how would you do that? 
#   Is any area in this data doing exceptional work and if so can you prove it statistically? 


# Importing data
DataUrban = read.csv("DataUrban.csv")
DataRural = read.csv("DataRural.csv")

# Selecting Sanitation Infrastructure
Var_Seg_U = DataUrban[c(7:10, 73:82, 92:109)]
Var_Seg_R = DataRural[c(7:10, 73:82, 92:109)]

# Means of States
Madhya_Pradesh = mean(Var_Seg_U$StateName == "MADHYA PRADESH")
Gujarat = mean(Var_Seg_U$StateName == "GUJARAT")
Rajasthan = mean(Var_Seg_U$StateName == "RAJASTHAN")
Bihar = mean(Var_Seg_U$StateName == "BIHAR")
Maharashtra = mean(Var_Seg_U$StateName == "MAHARASHTRA")
Uttar_Pradesh = mean(Var_Seg_U$StateName == "UTTAR PRADESH")

Combine = rbind(Madhya_Pradesh, Gujarat, Rajasthan, Bihar, Maharashtra, Uttar_Pradesh)

# Plotting 
dotchart(Combine, 
     main = "Means of the States",
     xlab = "Mean Value",
     ylab = "States",
     col = "Black",
     pch = 20)

# Linear Normalization
Var_Seg_U = transform(Var_Seg_U, 
                      New_Treated = (Var_Seg_U$TreatedTapwater.WaterSource/1977162),
                      New_Untreated = (Var_Seg_U$UntreatedTapwater.WaterSource/105386),
                      New_Covered = (Var_Seg_U$CoveredWell.WaterSource/18995),
                      New_Uncovered = (Var_Seg_U$UncoveredWall.WaterSource/20578),
                      New_Handpump = (Var_Seg_U$Handpump.WaterSource/162070),
                      New_Tubewell = (Var_Seg_U$Tubewell.WaterSource/135983),
                      New_Spring = (Var_Seg_U$Spring.WaterSource/2106),
                      New_River = (Var_Seg_U$River.WaterSource/4211),
                      New_Tank = (Var_Seg_U$Tank.WaterSource/12634),
                      New_Other = (Var_Seg_U$Other.WaterSource/33690),
                      New_HaveLattine = (Var_Seg_U$HaveLatrine/1151765),
                      New_PipedSewer = (Var_Seg_U$PipedSewer.FlushLatrine/1015828),
                      New_Septric = (Var_Seg_U$Septric.FlushLatrine/193278), 
                      New_Other = (Var_Seg_U$Other.FlushLatrine/18950),
                      New_WithSlab = (Var_Seg_U$WithSlab.PitLatrine/21056),
                      New_WithoutSlab = (Var_Seg_U$WithoutSlab.PitLatrine/3062),
                      New_NightSoilinOpen = (Var_Seg_U$NightSoilInOpen/27373),
                      New_NightSoilbyHuman = (Var_Seg_U$NightSoilbyHuman.ServiceLatrine/5619),
                      New_NightSoil = (Var_Seg_U$NightSoil.ServiceLatrine/8422),
                      New_NoLatrine = (Var_Seg_U$NoLatrine/953839),
                      New_PublicLatrine = (Var_Seg_U$PublicLatrine/903304),
                      New_OpenLatrine = (Var_Seg_U$OpenLatrine/50534),
                      New_HaveBathroom = (Var_Seg_U$HaveBathroom/1836087),
                      New_HaveBathroomNoroof = (Var_Seg_U$HaveBathroomNoRoof/155815),
                      New_NOBathroom = (Var_Seg_U$NoBathroom/113703),
                      New_ClosedDrainage = (Var_Seg_U$ClosedDrainage.WasteOutlet/1669744),
                      New_OpenDrainage = (Var_Seg_U$OpenDrainage.WasteOutlet/383220),
                      New_NoDrainage = (Var_Seg_U$NoDrainage.WasteOutlet/76688))

Overall = (Var_Seg_U$New_Treated + Var_Seg_U$New_Untreated + Var_Seg_U$New_Covered + 
             Var_Seg_U$New_Uncovered + Var_Seg_U$New_Handpump + Var_Seg_U$New_Tubewell + 
             Var_Seg_U$New_Spring + Var_Seg_U$New_River + Var_Seg_U$New_Tank + 
             Var_Seg_U$New_Other + Var_Seg_U$New_HaveLattine + Var_Seg_U$New_PublicLatrine + 
             Var_Seg_U$New_Septric + Var_Seg_U$New_Other + Var_Seg_U$New_WithSlab +
             Var_Seg_U$New_WithoutSlab + Var_Seg_U$New_NightSoilinOpen + 
             Var_Seg_U$New_NightSoilbyHuman + Var_Seg_U$New_NightSoil + 
             Var_Seg_U$New_NoLatrine + Var_Seg_U$New_PublicLatrine + 
             Var_Seg_U$New_OpenLatrine + Var_Seg_U$New_HaveBathroom + 
             Var_Seg_U$New_HaveBathroom + Var_Seg_U$New_NOBathroom + 
             Var_Seg_U$New_ClosedDrainage + Var_Seg_U$New_OpenDrainage + 
             Var_Seg_U$New_NoDrainage)

Var_Seg_U = cbind(Var_Seg_U, Overall)

# Maharastra is the Out-Performing State for the Overall Development in 
# Sanitation Infrastructure.

# Gujarat is the Under-Performing State for the Overall Development in Sanitation 
# Infrastructure.



# ---------------------------------------------------------------------------------------- #

Var_Seg_R = transform(Var_Seg_R, 
                      New_Treated = (Var_Seg_R$TreatedTapwater.WaterSource/9373),
                      New_Untreated = (Var_Seg_R$UntreatedTapwater.WaterSource/5148),
                      New_Covered = (Var_Seg_R$CoveredWell.WaterSource/1155),
                      New_Uncovered = (Var_Seg_R$UncoveredWall.WaterSource/1731),
                      New_Handpump = (Var_Seg_R$Handpump.WaterSource/8242),
                      New_Tubewell = (Var_Seg_R$Tubewell.WaterSource/3579),
                      New_Spring = (Var_Seg_R$Spring.WaterSource/509),
                      New_River = (Var_Seg_R$River.WaterSource/1005),
                      New_Tank = (Var_Seg_R$Tank.WaterSource/1134),
                      New_Other = (Var_Seg_R$Other.WaterSource/1924),
                      New_HaveLattine = (Var_Seg_R$HaveLatrine/13646),
                      New_PipedSewer = (Var_Seg_R$PipedSewer.FlushLatrine/5770),
                      New_Septric = (Var_Seg_R$Septric.FlushLatrine/7205), 
                      New_Other = (Var_Seg_R$Other.FlushLatrine/1277),
                      New_WithSlab = (Var_Seg_R$WithSlab.PitLatrine/2401),
                      New_WithoutSlab = (Var_Seg_R$WithoutSlab.PitLatrine/1792),
                      New_NightSoilinOpen = (Var_Seg_R$NightSoilInOpen/588),
                      New_NightSoilbyHuman = (Var_Seg_R$NightSoilbyHuman.ServiceLatrine/1220),
                      New_NightSoil = (Var_Seg_R$NightSoil.ServiceLatrine/328),
                      New_NoLatrine = (Var_Seg_R$NoLatrine/7217),
                      New_PublicLatrine = (Var_Seg_R$PublicLatrine/3222),
                      New_OpenLatrine = (Var_Seg_R$OpenLatrine/7201),
                      New_HaveBathroom = (Var_Seg_R$HaveBathroom/13958),
                      New_HaveBathroomNoroof = (Var_Seg_R$HaveBathroomNoRoof/2740),
                      New_NOBathroom = (Var_Seg_R$NoBathroom/5969),
                      New_ClosedDrainage = (Var_Seg_R$ClosedDrainage.WasteOutlet/13131),
                      New_OpenDrainage = (Var_Seg_R$OpenDrainage.WasteOutlet/5580),
                      New_NoDrainage = (Var_Seg_R$NoDrainage.WasteOutlet/7093))

Overall = (Var_Seg_R$New_Treated + Var_Seg_R$New_Untreated + Var_Seg_R$New_Covered + 
             Var_Seg_R$New_Uncovered + Var_Seg_R$New_Handpump + Var_Seg_R$New_Tubewell + 
             Var_Seg_R$New_Spring + Var_Seg_R$New_River + Var_Seg_R$New_Tank + 
             Var_Seg_R$New_Other + Var_Seg_R$New_HaveLattine + Var_Seg_R$New_PublicLatrine + 
             Var_Seg_R$New_Septric + Var_Seg_R$New_Other + Var_Seg_R$New_WithSlab +
             Var_Seg_R$New_WithoutSlab + Var_Seg_R$New_NightSoilinOpen + 
             Var_Seg_R$New_NightSoilbyHuman + Var_Seg_R$New_NightSoil + 
             Var_Seg_R$New_NoLatrine + Var_Seg_R$New_PublicLatrine + 
             Var_Seg_R$New_OpenLatrine + Var_Seg_R$New_HaveBathroom + 
             Var_Seg_R$New_HaveBathroom + Var_Seg_R$New_NOBathroom + 
             Var_Seg_R$New_ClosedDrainage + Var_Seg_R$New_OpenDrainage + 
             Var_Seg_R$New_NoDrainage)

Var_Seg_R = cbind(Var_Seg_R, Overall)

# Maharastra is the Out-Performing State for the Overall Development in Sanitation 
# Infrastructure.

# Gujarat is the Under-Performing State for the Overall Development in Sanitation 
# Infrastructure.