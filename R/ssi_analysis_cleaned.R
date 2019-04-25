#Whole Alberta, SSI and CSI analyses - HF gradient - Plants####
#v1.0

#Load package and functions
f <- function(x)length(unique(x))
library(ggplot2)
#may need some other packages, I did not verified this as they are already load in my R envir.

#Load data####
#Load vegetation dataset
veg_alberta <- read.csv("data/ABMI_veg_cleaned.csv", stringsAsFactors = F)

#Retrieve ABMI naming with "W" for wetlands true sites
veg_alberta$WSite <- veg_alberta$Site
veg_alberta[veg_alberta$Protocol == "Wetland",]$WSite <- gsub("WOG","OGW",paste0("W",veg_alberta[veg_alberta$Protocol == "Wetland",]$WSite))

#Create the unique ID = Protocol + Year + Site (Protocol is include in WSite column with the "W")
veg_alberta$ID <- paste(veg_alberta$WSite, veg_alberta$Year, sep = "_")

#Load HF data
HF_alberta <- read.csv("data/landscape/Alberta_HF.csv", stringsAsFactors = F)

#Create the unique ID = Protocol + Year + Site
HF_alberta$ID <- paste(HF_alberta$WSite, HF_alberta$Year, sep = "_")

#Data set-up####
#Calculate the sum of % of HF for each ID
HF_alberta_sum <- aggregate(Area_percent ~ ID + WSite + Year + Protocol + NRNAME, data = HF_alberta, sum)

#Merging PA vegetation data and HF value for each ID and make data corrections
data_ssi <- merge(veg_alberta, HF_alberta_sum[,c("ID","Area_percent")], by = "ID", all.x = T)
colnames(data_ssi)[colnames(data_ssi) == "Area_percent"] <- "HF"
colnames(data_ssi)[colnames(data_ssi) == "Species"] <- "SCIENTIFIC_NAME"
data_ssi$HF[is.na(data_ssi$HF)] <- 0 #Assuming all vegetation ID without HF data (NA) are IDs without HF

#Plot the gradient distribution: uneven --> need of the randomization process (see below)
hist(data_ssi[!duplicated(data_ssi$ID),]$HF,
     main = "Human Footprint gradient, #ID = 2054 - Plants", xlab = "% HF")

#Analyses####
#IDM: Intermediate Disturbance Hypothesis?
#Plot SR~HF gradient
dplot <- data.frame(ID = names(tapply(data_ssi$SCIENTIFIC_NAME, data_ssi$ID,f)),
                    SR = tapply(data_ssi$SCIENTIFIC_NAME, data_ssi$ID,f),
                    HF = tapply(data_ssi$HF, data_ssi$ID,unique))
dplot <- cbind(dplot, predict(lm(SR ~ HF + I(HF^2), data = dplot), interval = 'confidence'))

scatter_plot <- ggplot(dplot, aes(HF, SR)) + xlab("Human Footprint gradient")
scatter_plot <- scatter_plot + geom_point(col = "grey")
scatter_plot <- scatter_plot + geom_line(aes(HF, fit), col = "red")
scatter_plot <- scatter_plot + geom_ribbon(aes(ymin=lwr,ymax=upr), fill = "blue", alpha = 0.3)
scatter_plot

#Randomly split the dataset in different even categories (10 bins with same number of IDs) based on the HF gradient
#Sort the dataframe based on gradient values
data_ssi <- data_ssi[order(data_ssi$HF),]

#Pick-up randomly ID with low HF values
#Creating 100 independent random dataset and corresponding calculated SSI for each of them
x <- hist(data_ssi[!duplicated(data_ssi$ID),]$HF, plot = FALSE)$counts
t <- data_ssi[data_ssi$HF >= 10,]
d <- data_ssi[data_ssi$HF < 10,]
data_ssi_random<-list()
for(i in 1:100){
  a <- sample(unique(d$ID), sort(x, TRUE)[2])
  data_ssi_random[[i]] <- rbind(t,d[d$ID %in% a,])
}
rm(t,d,a,x)

rm(all_ssi)
summary_cat <- list()
for(i in 1:100){
  #Cut the random dataset into 10 even categories, i.e. same number of IDs
  d <- data_ssi_random[[i]][!duplicated(data_ssi_random[[i]]$ID),c("ID","HF")]
  d <- d[order(d$HF),]
  d$cat <- ceiling(seq_along(d$HF)/
                     round(length(d$HF)/10))
  if(f(d$cat) == 11){d[d$cat == 11,]$cat <- 10} #Sometimes, number of IDs per bin cannot be equal, add the "supplementary" IDs to the last bin
  data_ssi_random[[i]] <- merge(data_ssi_random[[i]], d[,c("ID","cat")], by = "ID")
  data_ssi_calc <- split(data_ssi_random[[i]], data_ssi_random[[i]]$cat)
  
  #For each new random gradient dataset, create a summary
  summary_cat[[i]] <- data.frame()
  for(x in 1:10){
    results <- data.frame(nbre_cat = 10,
                          cat = paste0("cat_",x),
                          range_cat = range(data_ssi_calc[[x]]$HF)[2]-range(data_ssi_calc[[x]]$HF)[1],
                          min_range = range(data_ssi_calc[[x]]$HF)[1],
                          max_range = range(data_ssi_calc[[x]]$HF)[2],
                          nbre_ID = f(data_ssi_calc[[x]]$ID),
                          nbre_bio_data = f(data_ssi_calc[[x]]$SCIENTIFIC_NAME))
    summary_cat[[i]] <- rbind(summary_cat[[i]], results)
  }

  #SSI calclulation
  freq_cat <- vector()
  ssi_HF <- data.frame()
  for(y in unique(data_ssi_random[[i]]$SCIENTIFIC_NAME)){
    
    for(x in 1:10){
      freq_cat[x] <- sum(data_ssi_calc[[x]][data_ssi_calc[[x]]$SCIENTIFIC_NAME == y,]$PA > 0)
    }
    
    results <- data.frame(SCIENTIFIC_NAME = y,
                          ssi_HF = sd(freq_cat)/mean(freq_cat))
    ssi_HF <- rbind(ssi_HF, results)
  }
  
  if(exists("all_ssi")){all_ssi <- merge(all_ssi,ssi_HF, by = "SCIENTIFIC_NAME")} else {all_ssi <- ssi_HF}
  
}
rm(data_ssi_calc, freq_cat, results)
#Warnings just due to the last merging, i.e. automatic renaming colums

#Example of the HF gradient from a particular random dataset
hist(data_ssi_random[[1]][!duplicated(data_ssi_random[[1]]$ID),]$HF,
     main = "HF gradient for SSI calculation #100", xlab = "HF %")

#Verify correlation of calculated SSI (expected high)
mean(cor(all_ssi[,-1])) #Mean of correlation values between all SSI calculated
sd(cor(all_ssi[,-1])) #SD of correlation values between all SSI calculated

#Exploring linear relationship between two random SSI calculation
d <- lm(all_ssi$ssi_HF.x ~ all_ssi$ssi_HF.y)
par(mfrow = c(2,2))
plot(d)
par(mfrow = c(1,1))
summary(d)

#Plot the relationship
plot(all_ssi$ssi_HF.x ~ all_ssi$ssi_HF.y,
     xlab = "SSI random #1", ylab = "SSI random #2", main = "Plants species")
abline(0, 1, col = "red")
abline(d, col = "blue")

#Issue with high value of SSI --> i.e. heteroscedascticity in the model --> delete corresponding species

#Delete the high SSI values (> 3) of ssi_HF object = last SSI values calculated in the random process
#ssi_HF values are taken as reference SSI values for next analyses

ssi_HF <- ssi_HF[ssi_HF$ssi_HF < 3,]
#ssi_HF values are taken as reference SSI values for next analyses

#Distribution of SSI values
hist(ssi_HF$ssi_HF, xlab = "SSI values - HF gradient", main = "")

#Relationship between SSI values and species frequency
dplot <- data.frame(frequency = tapply(veg_alberta$PA, veg_alberta$Species,
                                       function(x)sum(x > 0)))
dplot$SCIENTIFIC_NAME <- row.names(dplot)
dplot <- merge(dplot, ssi_HF)

plot(dplot$frequency ~ dplot$ssi_HF,
     xlab = "SSI", ylab = "Frequency", main = "Plants species")

#CSI calculation, i.e. mean of SSI values of communuties
CSI <- data.frame()
for(x in unique(data_ssi$ID)){
  results <- data.frame(ID = x,
                        CSI = mean(ssi_HF[ssi_HF$SCIENTIFIC_NAME %in% data_ssi[data_ssi$ID == x,]$SCIENTIFIC_NAME,]$ssi_HF),
                        HF = unique(data_ssi[data_ssi$ID == x,]$HF))
  CSI <- rbind(CSI,results)
}

#Calculate proportion of exotic species in each community
data_ssi <- merge(data_ssi,invasive_plants_ab, by.x = "SCIENTIFIC_NAME", by.y = "SPECIES")

d <- data.frame(ID = names(tapply(data_ssi$ORIGIN,data_ssi$ID,function(x)100*table(x)["Exotic"]/sum(table(x)))),
                invasive_prop = tapply(data_ssi$ORIGIN,data_ssi$ID,function(x)100*table(x)["Exotic"]/sum(table(x))))

#Plot CSI values ~ HF disturbance gradient:
dplot <- cbind(CSI, predict(lm(CSI ~ HF + I(HF^2), data = CSI), interval = 'confidence'))
dplot <- merge(dplot, d, by = "ID")

scatter_plot <- ggplot(dplot, aes(HF,CSI)) + xlab("Human Footprint gradient")
scatter_plot <- scatter_plot + geom_point(aes(colour = invasive_prop))
scatter_plot <- scatter_plot + geom_line(aes(HF, fit), col = "red")
scatter_plot <- scatter_plot + geom_ribbon(aes(ymin=lwr,ymax=upr), fill = "blue", alpha = 0.3)
scatter_plot <- scatter_plot + scale_colour_gradient(name = "% of exotic species",
                                                     low = "#ffffb2", high = "#bd0026")
scatter_plot
