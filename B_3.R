# B. Segregate the Data for Bihar, Rajasthan and Uttar Pradesh.
#   3.Is there a variable in this data set that is correlated with whether an area will have 
#     a high number of households where the Census house will be in poor/Dilapidated condition?
#     How strong/reliable would a relation of these variables be?
#     Discuss your intuition with the results.(Hint: You could start with a correlation matrix) 


# Importing data
DataUrban = read.csv("DataUrban.csv")

# Segregating the data for Gujarat & Maharashtra
seg_RUB = subset(DataUrban, DataUrban$StateName == "RAJASTHAN" |
                   DataUrban$StateName == "UTTAR PRADESH" |
                   DataUrban$StateName == "BIHAR", select = c(7,15))

seg_R = subset(DataUrban, DataUrban$StateName == "RAJASTHAN", select = c(7,15))
seg_U = subset(DataUrban, DataUrban$StateName == "UTTAR PRADESH", select = c(7,15))
seg_B = subset(DataUrban, DataUrban$StateName == "BIHAR", select = c(7,15))

sel_R = seg_R$Dilapidated.CensusHouseholds[1:196]
sel_U = seg_U$Dilapidated.CensusHouseholds[1:196]
sel_B = seg_B$Dilapidated.CensusHouseholds[1:196]

combine = data.frame(sel_R, sel_U, sel_B)

# Correlation
correlation = round(cor(combine),4)
correlation

library(Hmisc)
cor_hmisc =  rcorr(as.matrix(combine), type = "spearman")

# P-value
cor_p = round(cor_hmisc$P, 4)
cor_p

boxplot(sel_R, sel_U, sel_B,
        names = c("Rajasthan", "Uttar Pradesh", "Bihar"),
        horizontal = TRUE,
        notch = TRUE,
        col = c("red", "green", "yellow"),
        ylim = c(0,1000))


# ------------------------------------------------------------------------------------- #


plot(DataUrban$CensusHouseholds/1000, DataUrban$Dilapidated.CensusHouseholds/1000, 
     main = "CensusHousehold vs DilapidatedHouseholds",
     col ="green" , 
     xlab = "Census Households", 
     ylab = "Dilapidated Census Households", 
     cex = 0.75, 
     las = 1, 
     pch = 20,
     xlim = c(0,1500))

cor(DataUrban$CensusHouseholds, DataUrban$Dilapidated.CensusHouseholds)

TA = aggregate(cbind(DataUrban$CensusHouseholds, DataUrban$Dilapidated.CensusHouseholds) ~ 
                 DataUrban$StateName, DataUrban, sum, na.rm=T)


HHCor = subset(DataUrban, select = c(12, 15))

str(HHCor)

library(corrplot)
corrplot(cor(HHCor), method = "number")

# correlation does not imply causation [principle]