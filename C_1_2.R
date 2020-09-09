# C. From the rural data set for any one district perform the following analysis.
#   1.You have to select the most appropriate 05 villages for a ‘rurban’ convergence mission in 
#       the state from this district.
#     a.What parameters do you think are important as per the mission objectives from the ones
#         given to you? (Main parameters are given on the homepage- https://rurban.gov.in/).
#         Explain your choice or parameters.
#     b.Use at least two methods to give weightages to different criteria and compare the weights
#         obtained.

# Importing data
DataRural = read.csv("DataRural.csv")

# Segregating District and Parameters
District_seg = subset(DataRural, DataRural$DistrictName == "Ahmadabad", 
                      select = c(8:10, 73, 74, 86, 88, 101, 102, 107, 109, 110, 115))

# Selecting Village
Village_seg_1 = subset(District_seg, District_seg$TehsilName == "Mandal" | 
                       District_seg$TehsilName == "Viramgam" | 
                       District_seg$TehsilName == "Sanad" | 
                       District_seg$TehsilName == "Daskroi" |
                       District_seg$TehsilName == "Dholka")

# Unpaired Pairwise Comparision
UPC_Total = data.frame("Weight", "-->", "-->", "20%", "17%", "4%", "6%", "2%", "8%", "11%", "0%",
                       "13%", "19%")

names(UPC_Total) = c("DistrictName", "TehsilName", "AreaName", "TreatedTapwater.WaterSource", 
                     "UntreatedTapwater.WaterSource", "Electricty.LightingSource", 
                     "Solar.LightingSource", "NoLatrine", "PublicLatrine", 
                     "ClosedDrainage.WasteOutlet", "NoDrainage.WasteOutlet", "FireWood.Fuel", 
                     "LPG.Fuel")

Village_seg_1 = rbind(Village_seg_1, UPC_Total)


# ----------------------------------------------------------------------------------------- #


# Segregating Villages
Village_seg_2 = subset(District_seg, District_seg$TehsilName == "Mandal" | 
                         District_seg$TehsilName == "Viramgam" | 
                         District_seg$TehsilName == "Sanad" | 
                         District_seg$TehsilName == "Daskroi" |
                         District_seg$TehsilName == "Dholka")

# Total of Analytical Hierachy Process
AHP_Total = data.frame("Total", "-->", "-->", "3.88", "7.63", "28.22", "23.55", "64.11", "6.14",
                       "28.93", "76", "16.86", "12.19")

# Final Weight of AHP
Final_Weight = data.frame("Addition_Normalization", "-->", "-->", "2.02", "1.43", "0.51", "0.73", 
                       "0.25", "2.01", "0.60", "0.13", "1.03", "1.29")

# Getting Weighted sum in AHP
WS_Total = data.frame("Weighted Sum", "-->", "-->", "2.624", "2.0435", "1.1843", "1.3941", 
                      "0.2919", "3.0275", "1.2196", "0.1309", "1.3294", "1.7586")

# Diving Sum by Weight
SW_Total = data.frame("Sum / Weight", "-->", "-->", "12.99", "14.29", "23.22", "19.09", "11.64", 
                      "15.06", "20.32", "10.06", "12.09", "13.63")

names(AHP_Total) = c("DistrictName", "TehsilName", "AreaName", "TreatedTapwater.WaterSource", 
                     "UntreatedTapwater.WaterSource", "Electricty.LightingSource", 
                     "Solar.LightingSource", "NoLatrine", "PublicLatrine", 
                     "ClosedDrainage.WasteOutlet", "NoDrainage.WasteOutlet", "FireWood.Fuel", 
                     "LPG.Fuel")

names(Final_Weight) = c("DistrictName", "TehsilName", "AreaName", "TreatedTapwater.WaterSource", 
                     "UntreatedTapwater.WaterSource", "Electricty.LightingSource", 
                     "Solar.LightingSource", "NoLatrine", "PublicLatrine", 
                     "ClosedDrainage.WasteOutlet", "NoDrainage.WasteOutlet", "FireWood.Fuel", 
                     "LPG.Fuel")

names(WS_Total) = c("DistrictName", "TehsilName", "AreaName", "TreatedTapwater.WaterSource", 
                   "UntreatedTapwater.WaterSource", "Electricty.LightingSource", 
                   "Solar.LightingSource", "NoLatrine", "PublicLatrine", 
                   "ClosedDrainage.WasteOutlet", "NoDrainage.WasteOutlet", "FireWood.Fuel", 
                   "LPG.Fuel")

names(SW_Total) = c("DistrictName", "TehsilName", "AreaName", "TreatedTapwater.WaterSource", 
                   "UntreatedTapwater.WaterSource", "Electricty.LightingSource", 
                   "Solar.LightingSource", "NoLatrine", "PublicLatrine", 
                   "ClosedDrainage.WasteOutlet", "NoDrainage.WasteOutlet", "FireWood.Fuel", 
                   "LPG.Fuel")

Village_seg_2 = rbind(Village_seg_2, AHP_Total, Final_Weight, WS_Total, SW_Total)

# Finding Lamda Max value
Lamda_max = sum(12.99, 14.29, 23.22, 19.09, 11.64, 15.06, 20.32 ,10.06, 12.09, 13.63)/10

# Finding Consistency index : (lamda - n / (n-1))
Consistency_Index = (Lamda_max - 10)/9

# Finding Consistency Ration
Consistency_Ratio = Consistency_Index / 1.49


# ----------------------------------------------------------------------------------------- #


# Comparision Table of Unpaired and AHP
UPC = c(0.2, 0.17, 0.04, 0.06, 0.02, 0.08, 0.11, 0.000, 0.13, 0.19)

AHP = c(0.202, 0.143, 0.051, 0.073, 0.025, 0.201, 0.06, 0.013, 0.103, 0.129)

names(UPC) = c("TreatedTapwater.WaterSource", 
            "UntreatedTapwater.WaterSource", "Electricty.LightingSource", 
            "Solar.LightingSource", "NoLatrine", "PublicLatrine", 
            "ClosedDrainage.WasteOutlet", "NoDrainage.WasteOutlet", "FireWood.Fuel", 
            "LPG.Fuel")

names(AHP) = c("TreatedTapwater.WaterSource", 
               "UntreatedTapwater.WaterSource", "Electricty.LightingSource", 
               "Solar.LightingSource", "NoLatrine", "PublicLatrine", 
               "ClosedDrainage.WasteOutlet", "NoDrainage.WasteOutlet", "FireWood.Fuel", 
               "LPG.Fuel")

# Comparision Table 
Comparision = rbind(UPC, AHP)
Comparision