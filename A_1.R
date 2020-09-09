# A.Segregate the Data for Gujarat and Maharashtra and Answer the following.
#   1.Calculate the mean of households with electricity. 
#     Is there a significant difference in mean of the electrified houses among the states 
#     of Gujarat and Maharashtra? What test is most suitable for this analysis? 
#     Discuss your test result.


# Importing data
DataUrban = read.csv("DataUrban.csv")

# Segregating the data for Gujarat & Maharashtra
seg_G = subset(DataUrban, DataUrban$StateName == "GUJARAT")
seg_M = subset(DataUrban, DataUrban$StateName == "MAHARASHTRA")

sel_G = seg_G$Electricty.LightingSource
sel_M = seg_M$Electricty.LightingSource

# Getting mean of electricity
summary(sel_G)
summary(sel_M)

# Significant Difference
mean(seg_M$Electricty.LightingSource - seg_G$Electricty.LightingSource)

boxplot(seg_G$Electricty.LightingSource, seg_M$Electricty.LightingSource,
        ylim = c(0, 20000),
        notch = TRUE,
        main = "Significant Difference of Mean ",
        names = c("GUJARAT", "MAHARASHTRA"),
        col = c("Lightblue", "Lightgreen"),
        xlab = "States",
        ylab = "Values")

# T-test
t.test(sel_G, sel_M)
