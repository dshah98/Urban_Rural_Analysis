# B. Segregate the Data for Bihar, Rajasthan and Uttar Pradesh.
#   2.Are the means of households connected to electricity equal to these three states? 
#     Please discuss the test statistic and the results of the analysis.


# Importing data
DataUrban = read.csv("DataUrban.csv")

# Segregating the data for Rajasthan, Uttar Pradesh & Bihar.
seg_RUB = subset(DataUrban, DataUrban$StateName == "RAJASTHAN" |
                 DataUrban$StateName == "UTTAR PRADESH" |
                 DataUrban$StateName == "BIHAR")

Elec_HH = seg_RUB[c(7,86)]

# Mean 
Mean_Elec = mean(DataUrban$Electricty.LightingSource)
Mean = cbind(tapply(DataUrban$Electricty.LightingSource, DataUrban$StateName, mean, na.rm = T))
Seg_Mean = (Mean[c(1,5,6),])

# Are Mean of Household Electricity equals to 3 State
Mean_Elec
sum(Seg_Mean)
# Ans : No, the mean of Household Electicity is not equal to 3 states. 

# Plotting
barchart(Seg_Mean, 
         main = "Mean of 3 States",
         xlab = "Mean Value",
         ylab = "States")

# Anova Test 
anova = aov(Elec_HH$StateName == "RAJASTHAN" ~ Elec_HH$StateName == "BIHAR")
summary(anova)

anova1 = aov(Elec_HH$StateName == "RAJASTHAN" ~ Elec_HH$StateName == "UTTAR PRADESH")
summary(anova1)

anova2 = aov(Elec_HH$StateName == "BIHAR" ~ Elec_HH$StateName == "UTTAR PRADESH")
summary(anova2)
