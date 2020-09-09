# C. From the rural data set for any one district perform the following analysis.
#   1.You have to select the most appropriate 05 villages for a ‘rurban’ convergence mission 
#       in the state from this district.
#     c.Estimate the score using the weighted sum and weighted product method.

# Importing data
DataRural = read.csv("DataRural.csv")

# Segregating District and Parameters
District_seg = subset(DataRural, DataRural$DistrictName == "Ahmadabad", 
                      select = c(8:10, 73, 74, 86, 87,92, 101, 103, 109, 110, 115))

# Selecting Village_1
Village_seg_1 = subset(District_seg, District_seg$TehsilName == "Mandal" | 
                         District_seg$TehsilName == "Viramgam" | 
                         District_seg$TehsilName == "Sanand" | 
                         District_seg$TehsilName == "Daskroi" |
                         District_seg$TehsilName == "Dholka")


# Calculating Weighted Sum
# Linear_Normalization = Variable_Value / Max_of_Variable_Value
# Weighted_Sum = sum(Linear_Normalization * Final_Weight_of_AHP)
Village_seg_1 = transform(Village_seg_1, 
                      New_Treated = (Village_seg_1$TreatedTapwater.WaterSource/692)*0.202,
                      New_Untreated = (Village_seg_1$UntreatedTapwater.WaterSource/1871)*0.143,
                      New_Electricity = (Village_seg_1$Electricty.LightingSource/2724)*0.051, 
                      New_Kerosene = (Village_seg_1$Kerosene.LightingSource/18)*0.073,
                      New_HaveLatrine = (Village_seg_1$HaveLatrine/1475)*0.025,
                      New_NoLatrine = (Village_seg_1$NoLatrine/1475)*0.201,
                      New_PublicLatrine = (Village_seg_1$OpenLatrine/229)*0.06,
                      New_NoDrainage = (Village_seg_1$NoDrainage.WasteOutlet/2767)*0.013,
                      New_FireWood = (Village_seg_1$FireWood.Fuel/1610)*0.103,
                      New_LPG = (Village_seg_1$LPG.Fuel/1048)*0.129)

Weighted_Sum = (Village_seg_1$New_Treated + Village_seg_1$New_Untreated +
                  Village_seg_1$New_Electricity + Village_seg_1$New_Kerosene + 
                  Village_seg_1$New_HaveLatrine + Village_seg_1$New_NoLatrine +
                  Village_seg_1$New_PublicLatrine + Village_seg_1$New_NoDrainage +
                  Village_seg_1$New_FireWood + Village_seg_1$New_LPG)

Village_seg_1 = cbind(Village_seg_1, Weighted_Sum)

summary(Village_seg_1$Weighted_Sum)
# By calulating Weighted Sum, we can do our Rurban Project in Mandal at Ahmedabad, Gujarat


# ---------------------------------------------------------------------------------------- #


# Selecting Village_2
Village_seg_2 = subset(District_seg, District_seg$TehsilName == "Mandal" | 
                         District_seg$TehsilName == "Viramgam" | 
                         District_seg$TehsilName == "Sanand" | 
                         District_seg$TehsilName == "Daskroi" |
                         District_seg$TehsilName == "Dholka")


# Calculating Weighted Product
# Linear_Normalization = Variable_Value / Max_of_Variable_Value
# Weighted_Product = product(Linear_Normalization ^ Final_Weight_of_AHP)
Village_seg_2 = transform(Village_seg_2, 
                  New_Treated = (Village_seg_1$TreatedTapwater.WaterSource/692)^0.202,
                  New_Untreated = (Village_seg_1$UntreatedTapwater.WaterSource/1871)^0.143,
                  New_Electricity = (Village_seg_1$Electricty.LightingSource/2724)^0.051, 
                  New_Kerosene = (Village_seg_1$Kerosene.LightingSource/18)^0.073,
                  New_HaveLatrine = (Village_seg_1$HaveLatrine/1475)^0.025,
                  New_NoLatrine = (Village_seg_1$NoLatrine/1475)^0.201,
                  New_PublicLatrine = (Village_seg_1$OpenLatrine/229)^0.06,
                  New_NoDrainage = (Village_seg_1$NoDrainage.WasteOutlet/2767)^0.013,
                  New_FireWood = (Village_seg_1$FireWood.Fuel/1610)^0.103,
                  New_LPG = (Village_seg_1$LPG.Fuel/1048)^0.129)

Weighted_Product = (Village_seg_1$New_Treated * Village_seg_1$New_Untreated *
                      Village_seg_1$New_Electricity * Village_seg_1$New_Kerosene * 
                      Village_seg_1$New_HaveLatrine * Village_seg_1$New_NoLatrine *
                      Village_seg_1$New_PublicLatrine * Village_seg_1$New_NoDrainage *
                      Village_seg_1$New_FireWood * Village_seg_1$New_LPG)

Village_seg_2 = cbind(Village_seg_2, Weighted_Product)

summary(Village_seg_2$Weighted_Product)
