# B. Segregate the Data for Bihar, Rajasthan and Uttar Pradesh.
#   1.For the material of roof in these states, can it be said that there is a pattern in 
#     these states? 
#     Does this pattern differ with respect to any of these states. 
#     Discuss the test statistic that would be used here.


# Importing data
DataUrban = read.csv("DataUrban.csv")

# Segregating Materials of Roof
Glass_Roof = tapply(DataUrban$Grass.Roof, DataUrban$StateName, sum, na.rm = TRUE)
Plastic_Roof = tapply(DataUrban$Plastic.Roof, DataUrban$StateName, sum, na.rm = TRUE)
HandTiles_Roof = tapply(DataUrban$HandTiles.Roof, DataUrban$StateName, sum, na.rm = TRUE)
MachineTile_Roof = tapply(DataUrban$MachineTiles.Roof, DataUrban$StateName, sum, na.rm = TRUE)
Brick_Roof = tapply(DataUrban$Brick.Roof, DataUrban$StateName, sum, na.rm = TRUE)
Slate_Roof = tapply(DataUrban$Slate.Roof, DataUrban$StateName, sum, na.rm = TRUE)
Metal_Roof = tapply(DataUrban$Metal.Roof, DataUrban$StateName, sum, na.rm = TRUE)
Concrete_Roof = tapply(DataUrban$Concrete.Roof, DataUrban$StateName, sum, na.rm = TRUE)
Other_Roof = tapply(DataUrban$Other.Roof, DataUrban$StateName, sum, na.rm = TRUE)

# Combining Data
RoofData = cbind(Glass_Roof, Plastic_Roof, HandTiles_Roof, MachineTile_Roof, Brick_Roof,
                 Slate_Roof, Metal_Roof, Concrete_Roof, Other_Roof)

# Selecting particular 3 states
Sel_RoofData = RoofData[c(1,5,6),]

# Plotting
barplot(Sel_RoofData, beside=T, 
        main = "Roof Material in Different States",
        xlab = "Different Roof Materials",
        ylab = "Values",
        col=c("#0E6251","#17A589","#76D7C4"))
legend(x = "topleft", 
       legend = c("BIhar", "Rajasthan", "Uttar Pradesh"), 
       cex =.8, 
       fill= c("#0E6251","#17A589", "#76D7C4"))


boxplot(Sel_RoofData, beside=T, 
        main = "Roof Material in Different States",
        xlab = "Different Roof Materials",
        ylab = "Values")

# T-Test
chisq.test(Sel_RoofData)
