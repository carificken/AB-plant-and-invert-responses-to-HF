# This script cleans and combines the veg, hf, and wetland classification dfs

library(tidyverse)
rm(list=ls())

# vascular plants - wetland sites 
{
    setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Wetland data")
    wet_vascplant_pa <- read.csv("A_W05_Vascular_Plants.csv", row.names = NULL)
    
    # clean plant data set
    names(wet_vascplant_pa)
    wet_vascplant_pa <- wet_vascplant_pa %>% select(Site=ABMI.Site,
                                                    Year=Year,
                                                    Old.Zone,
                                                    Species=Scientific.Name)
    
    # keep sp only for open water and emergent zones
    wet_vascplant_pa <- wet_vascplant_pa %>% filter(Old.Zone=="Open Water" |
                                                      Old.Zone=="Emergent" ) %>% 
      select(-Old.Zone)
    
    names(wet_vascplant_pa)
    levels(wet_vascplant_pa$Species)
    
    wet_vascplant_pa <- wet_vascplant_pa %>% filter(Species!="NONE" &
                                                      Species!="VNA" &
                                                      Species!="SNI" &
                                                      Species!="DNC")
    wet_vascplant_pa %>% summarize(NSites = length(unique(Site))) # 878 wetland sites with plant p/a
    
}

# wet_vascplant_pa %>% distinct(Site,Year) %>% View()
wet_vascplant_pa$Site <- wet_vascplant_pa$Site %>% str_replace(pattern="OG-", replace="OGW-") 

# vascular plants - terrestrial sites
{
  setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data")
  vascplant_pa <- read.csv("A_T15_Vascular_Plants.csv", row.names = NULL)
  names(vascplant_pa) <- colnames(vascplant_pa[,2:ncol(vascplant_pa)])
  vascplant_pa <- vascplant_pa[2:ncol(vascplant_pa)-1]
  names(vascplant_pa)
  
  vascplant_pa <- vascplant_pa %>% 
    select(Site="ABMI.Site",Year,Species="Scientific.Name")
  vascplant_pa <- filter(vascplant_pa, Year!=2017)
  
  # # extract proper site names
  # vascplant_pa <- vascplant_pa %>% 
  #   mutate(newid=paste(str_extract(tmpSite, "OG-"),
  #                      str_extract(tmpSite, "[[:digit:]]+"),
  #                      str_extract(tmpSite,"-[[:digit:]]+$"), sep="")) %>%
  #   mutate(Site = gsub('NA', '', newid)) %>%
  #   select(Site,Year,Species)
  
  head(vascplant_pa)
  levels(vascplant_pa$Species)
  vascplant_pa <- vascplant_pa %>% filter(Species!="NONE" &
                                            Species!="VNA" &
                                            Species!="SNI" &
                                            Species!="DNC")
  vascplant_pa <- droplevels(vascplant_pa)
  vascplant_pa %>% summarize(NSites = length(unique(Site))) 
} 

# load and clean wetland classification - terrestrial sites
{
  setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data")
  siteclass <- read.csv("A_T01C_Site_Capability.csv")
  # remove missing classificaiton values, and only retain the dominant site classification
  levels(siteclass$Percent.Area.of.Ecological.Site.Classification)
  siteclass <- siteclass %>% filter(Percent.Area.of.Ecological.Site.Classification!="VNA" & 
                                      Percent.Area.of.Ecological.Site.Classification!="DNC",
                                    Percent.Area.of.Ecological.Site.Classification!="PNA")
  siteclass <- droplevels(siteclass)
  siteclass$Percent.Area.of.Ecological.Site.Classification <- as.numeric(levels(siteclass$Percent.Area.of.Ecological.Site.Classification))[siteclass$Percent.Area.of.Ecological.Site.Classification]
  
  # separate sites by wetland type
  names(siteclass)
  bogsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(08) PD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  fensites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(09) MD" |
                                     Ecosite...Nutrient.Moisture.Code == "(10) RDp" |
                                     Ecosite...Nutrient.Moisture.Code == "(13) AD") %>% 
    select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  wetmeadowsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(10.5) RDm") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  marshsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(11) VD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  swampsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(12) SD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  
  # add wetland type ID and combine datasets
  bogsites$WetlandType <- "Bog"
  fensites$WetlandType <- "Fen"
  wetmeadowsites$WetlandType <- "Wet Meadow"
  marshsites$WetlandType <- "Marsh"
  
  wetlandclassification <- rbind(bogsites,
                                 fensites,
                                 wetmeadowsites,
                                 marshsites)
  # # extract proper site names
  # wetlandclassification <- wetlandclassification %>% 
  #   mutate(newid=paste(str_extract(tmpSite, "OG-"),
  #                      str_extract(tmpSite, "[[:digit:]]+"),
  #                      str_extract(tmpSite,"-[[:digit:]]+$"), sep="")) %>%
  #   mutate(Site = gsub('NA', '', newid)) %>% 
  #   select(WetlandType,Site,Cover)
  
  # how many repeated site classifications are there
  wetlandclassification <- wetlandclassification %>% group_by(Site, WetlandType) %>% summarize(meanCover=mean(Cover))
  wetlandclassification %>% mutate(NWetlandClasses=length(WetlandType)) %>% filter(NWetlandClasses>1) %>% arrange(desc(NWetlandClasses))
  
  # note: many sites have multiple ecosite classifications; must take the DOMINANT classification
  wetlandclassification <- wetlandclassification %>% group_by(Site) %>% filter(meanCover==max(meanCover)) %>% select(Site, WetlandType)
  head(wetlandclassification)
  # we still have 20 sites w/ 50:50 split classifications
  wetlandclassification %>% mutate(NWetlandClasses=length(WetlandType)) %>% filter(NWetlandClasses>1) %>% select(Site) %>% unique()
  
  
  # amend manually
  # Fen x Marsh >> marsh
  wetlandclassification[wetlandclassification$Site=="1393","WetlandType"] <- "Marsh"
  wetlandclassification[wetlandclassification$Site==791,"WetlandType"] <- "Marsh"
  wetlandclassification[wetlandclassification$Site=="OG-SRD-1239-1","WetlandType"] <- "Marsh"
  
  # Bog x Marsh >> marsh
  wetlandclassification[wetlandclassification$Site=="OG-ABMI-1175-1","WetlandType"] <- "Marsh"
  
  # Bog x Fen >> Fen
  wetlandclassification[wetlandclassification$Site==292,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==295,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==352,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==418,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==442,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==497,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==498,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==561,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==626,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==627,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==631,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==728,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site==759,"WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site=="OG-ABMI-571-21","WetlandType"] <- "Fen"
  wetlandclassification[wetlandclassification$Site=="OG-ABMI-510-21","WetlandType"] <- "Fen"
  
  # Fen x WetMeandow
  wetlandclassification[wetlandclassification$Site==63,"WetlandType"] <- "Wet Meadow"
  
  # delete repeated Site classificaitons
  wetlandclassification <- wetlandclassification %>% distinct(Site, WetlandType)
  
  # check to make sure each site has only 1 wetland classification
  wetlandclassification %>% select(Site) %>% unique() %>% nrow() # 579 unique sites
  wetlandclassification %>% select(Site, WetlandType) %>% unique() %>% nrow() # 579 unique site x wetlandtypes
  
}

# add wetland class to plant veg df
{
  plant_pa <- left_join(vascplant_pa, wetlandclassification, by="Site") %>% select(WetlandType,Site,Year,everything())
  plant_pa <- plant_pa %>% filter(!is.na(WetlandType))
  veg_pa <- plant_pa
}



# filter plant data set by sites in parkland and grassland
setwd("/Users/cari/Desktop/Waterloo/Martin")
load("ID_grassland.Rdata")
head(ID_grassland)
ID_grassland <- ID_grassland %>% select("Site"=SITE,"Year"="YEAR")
wet_vascplant_pa <- wet_vascplant_pa %>% filter(Site %in% ID_grassland$Site)
head(wet_vascplant_pa)  

# wet_vascplant_pa %>% distinct(Site,Year) %>% View()
wet_vascplant_pa %>% distinct(Site,Year) %>% nrow() # 391
  
wet_vascplant_pa_after2010 <- wet_vascplant_pa %>% filter(Year>2010)
wet_vascplant_pa_after2010 %>% distinct(Site,Year) %>% nrow() # 331
# veg_pa <- wet_vascplant_pa_after2010
veg_pa <- inner_join(wet_vascplant_pa_after2010, ID_grassland, by=c("Site", "Year"))
head(veg_pa)
veg_pa$PA <- 1 
veg_pa <- distinct(veg_pa)
veg_pa <- veg_pa %>% spread(key=Species,value=PA)
veg_pa <- veg_pa %>% gather(key=Species, value=PA, 3:ncol(veg_pa)) %>% mutate(PA=replace_na(PA,0))

# sites to keep 
sites <- veg_pa %>% select(Site,Year) %>% distinct()

# C SCORES
{
# load and combine C-score datasets
{
    setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Indicator Species Analysis")
    Cscores <- read.csv("USGS C scores.csv")
    # Cscores$ScientificName <- str_replace(Cscores$ScientificName, pattern = " " , replacement=".")
    head(Cscores)
    
    unique(Cscores$ScientificName) %>% length() # 1522 sp w/ CC scores
 
    Cscores2 <- read.csv("Wilson et al 2013 Alb sp cc scores.csv")
    # Cscores2$Species.name <- str_replace(Cscores2$Species.name, " " , ".")
    head(Cscores2)
    
    length(Cscores2$Species.name) # 188 sp in smaller C scores dataset
    intersect(Cscores2$Species.name, Cscores$ScientificName) %>% length() # 69 sp have scores in both datasets
    
    # most sp are similar, but some have veeeery diff Cscores
    bothCCdf <- inner_join(Cscores2, Cscores, by=c("Species.name"= "ScientificName")) %>% 
      select("Sp"=Species.name,
             "CC2"= Boreal.Plains, 
             "CC1"=Cscore) %>% 
      mutate(diff=as.numeric(CC1)-as.numeric(CC2)) 
    
    bothCCdf %>% filter(diff>=5 | diff<=-2)
    
    # combine add the smaller C scores dataset to the larger USGS dataset
    head(Cscores)
    head(Cscores2)
    tmp <- Cscores2 %>% select("ScientificName"="Species.name", 
                               "CommonName"="Common.name", 
                               "Cscore"="Boreal.Plains")
    tmp$Cscore <- as.factor(tmp$Cscore)
    tmp2 <- bind_rows(Cscores,tmp) # different num of cols but binds properly b/c col headings match
    
    head(tmp2)
    
    tmp2 %>% dim()
    tmp2$ScientificName <- tolower(tmp2$ScientificName)
    Cscores <- tmp2 %>% distinct(ScientificName, .keep_all=T) # this uses Cscores from the USGS dataset 
    
    unique(Cscores$Cscore)
    
    Cscores <- filter(Cscores, !is.na(Cscore))
    
    Cscores$Cscore <- recode(Cscores$Cscore, nn="0") # convert nn levels to 0
    Cscores <- droplevels(Cscores)
    Cscores$Cscore <- as.numeric(Cscores$Cscore)
    
    # Convert Scientific Name to sentence case
    proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
    Cscores$ScientificName <- proper(Cscores$ScientificName)
  }   
  
# add C scores to veg data set
{
    head(Cscores)
    head(veg_pa)
    
    Veg_Cscores <- left_join(veg_pa,Cscores, by=c("Species"="ScientificName")) 
    
    Veg_Cscores <- Veg_Cscores %>% select(-Acronym,-CommonName,-Physiognomy,-Family)
  }

# dist of median C scores
# save(Veg_Cscores, file="Veg_Cscores.Rdata")
Veg_Cscores %>% filter(PA==1) %>% group_by(Site,Year) %>% 
    summarize(medCscore=median(Cscore, na.rm=T),
              meanCscore=mean(Cscore,na.rm=T),
              varCscore=var(Cscore, na.rm=T)) %>% 
    ggplot(aes(x=meanCscore)) + geom_histogram(bins = 30)

}
  
# Distribution of Env Gradients - wetland sites
{  
setwd("/Users/cari/Desktop/Waterloo/Martin")
load("climate_covar.Rdata")
head(covar) # water quality
covar$Year <- as.numeric(str_sub(string=as.character(covar$ID),start=-4, end=-1))

covar2 <- left_join(sites,select(covar, -ID,-ID_ABMI, -NATURALREGIONS), by=c("Site"="SITE", "Year"))
covar2 <-   covar2 %>% select(Site,Year, LATTITUDE, "DO"="Dissolved Oxygen", "TN"="Total Nitrogen", "TP"="Total Phosphorous", "DOC"="Dissolved Organic Carbon", everything())

covar2 %>% filter(TN<20000) %>% 
  gather(key=WQVar, value=Value, 3:10) %>% 
  ggplot(aes(x=Value, fill=WQVar)) + geom_histogram(bins=30) +
  facet_wrap(~WQVar, scales="free") + theme(legend.position="none")

head(climate) # climatic data
climate2 <- left_join(sites,select(climate, -ID,-ID_ABMI, -ASPEN), by=c("Site"="SITE", "Year"="YEAR"))
head(climate2)  
  
climate2 %>% mutate(TempRange=MWMT-MCMT) %>% gather(key=ClimateVar, value=Value, 3:ncol(climate2)) %>% 
  ggplot(aes(x=Value, fill=ClimateVar)) + 
  geom_histogram(bins=30) + 
  facet_wrap(~ClimateVar, scales="free") + theme(legend.position="none")
}

# Distribution of Env Gradients - wetland sites
{  
  setwd("/Users/cari/Desktop/Waterloo/Martin")
  load("climate_covar.Rdata")
  head(covar) # water quality
  covar$Year <- as.numeric(str_sub(string=as.character(covar$ID),start=-4, end=-1))
  
  covar2 <- left_join(sites,select(covar, -ID,-ID_ABMI, -NATURALREGIONS), by=c("Site"="SITE", "Year"))
  covar2 <-   covar2 %>% select(Site,Year, LATTITUDE, "DO"="Dissolved Oxygen", "TN"="Total Nitrogen", "TP"="Total Phosphorous", "DOC"="Dissolved Organic Carbon", everything())
  
  covar2 %>% filter(TN<20000) %>% 
    gather(key=WQVar, value=Value, 3:10) %>% 
    ggplot(aes(x=Value, fill=WQVar)) + geom_histogram(bins=30) +
    facet_wrap(~WQVar, scales="free") + theme(legend.position="none")
  
  head(climate) # climatic data
  climate2 <- left_join(sites,select(climate, -ID,-ID_ABMI, -ASPEN), by=c("Site"="SITE", "Year"="YEAR"))
  head(climate2)  
  
  climate2 %>% mutate(TempRange=MWMT-MCMT) %>% gather(key=ClimateVar, value=Value, 3:ncol(climate2)) %>% 
    ggplot(aes(x=Value, fill=ClimateVar)) + 
    geom_histogram(bins=30) + 
    facet_wrap(~ClimateVar, scales="free") + theme(legend.position="none")
}
  
# bivariate relationships between env grad and median C-score
{
  medCscore <- Veg_Cscores %>% filter(PA==1) %>% group_by(Site,Year) %>% 
    summarize(medCscore=median(Cscore, na.rm=T))
  
  meanCscore <- Veg_Cscores %>% filter(PA==1) %>% group_by(Site,Year) %>% 
    summarize(meanCscore=mean(Cscore, na.rm=T))
  
  covar2 <- covar2 %>% filter(TN < 20000)
  left_join(medCscore, 
            select(covar2, -COMPLEXITY_INDEX, -HYDROPHYTES_PERCENT, -'Wetland Elevation', -MAX_DEPTH, -FISH), 
            by=c("Site", "Year")) %>% 
    gather(key=WQVar, value=Value, 4:ncol(.)) %>% 
    ggplot(aes(x=(Value), y=(medCscore), color=WQVar)) +
    geom_point() +
    facet_wrap(~WQVar, scales="free_x") +
    theme(legend.position = "none")
  
  left_join(meanCscore, 
            select(covar2, -COMPLEXITY_INDEX, -HYDROPHYTES_PERCENT, -'Wetland Elevation', -MAX_DEPTH, -FISH), 
            by=c("Site", "Year")) %>% 
    gather(key=WQVar, value=Value, 4:ncol(.)) %>% 
    ggplot(aes(x=log(Value), y=(meanCscore), color=WQVar)) +
    geom_point() +
    geom_smooth(method="lm", se=F) +
    facet_wrap(~WQVar, scales="free_x") +
    theme(legend.position = "none")
  
  # stats
  {
    tmp <- left_join(meanCscore, 
                     select(covar2, -COMPLEXITY_INDEX, -HYDROPHYTES_PERCENT, -'Wetland Elevation', -MAX_DEPTH, -FISH), 
                     by=c("Site", "Year"))
    summary(lm(meanCscore ~ LATTITUDE, data=tmp))
    
    }
  
  head(climate2)
  left_join(medCscore, climate2, by=c("Site", "Year")) %>% 
    gather(ClimateVar, Value, 4:ncol(.)) %>% 
    ggplot(aes(x=Value, y=medCscore, color=ClimateVar)) +
    geom_point() +
    facet_wrap(~ClimateVar, scales="free_x") + 
    theme(legend.position = "none")
  
  left_join(meanCscore, climate2, by=c("Site", "Year")) %>% 
    gather(ClimateVar, Value, 4:ncol(.)) %>% 
    ggplot(aes(x=(Value), y=meanCscore, color=ClimateVar)) +
    geom_point() +
    facet_wrap(~ClimateVar, scales="free_x") + 
    theme(legend.position = "none")
}  

# Species and community sensitivity indices  
# lat, ph, conductivity
{
  # tally number of sp occurrences across gradients
  head(covar2)
  
  covar3 <- covar2 %>% select(Site,Year,Lat=LATTITUDE,pH,Conductivity)
  covar3$latbin <- ntile(covar3$Lat, n=10)
  covar3$phbin <- ntile(covar3$pH, n=5)
  covar3$condbin <- ntile(covar3$Conductivity, n=5)
  head(covar3)
  
  covar3 %>% 
    ggplot(aes(x=condbin)) + 
    geom_histogram(bins=5, color=1, fill="white") # how many sites per bin
  
  veg_pa <- veg_pa %>% filter(PA==1)
  head(veg_pa)
  length(unique(veg_pa$Species))
  head(covar3)
  
  # plot the occurrence frequency of example species 
  # latbin
  left_join(veg_pa, select(covar3, Site,Year,latbin,phbin,condbin), by=c("Site", "Year")) %>% 
    group_by(latbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=latbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  # phbin
  left_join(veg_pa, select(covar3, Site,Year,latbin,phbin,condbin), by=c("Site", "Year")) %>% 
    group_by(phbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=phbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  # condbin
  left_join(veg_pa, select(covar3, Site,Year,latbin,phbin,condbin), by=c("Site", "Year")) %>% 
    group_by(condbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=condbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  
  # occurrence freq of each sp in each bin
  occfreq_latbin <- left_join(veg_pa, select(covar3, Site,Year,latbin), by=c("Site", "Year")) %>% 
    group_by(latbin,Species) %>% summarize(occ_freq_lat=sum(PA))
  occfreq_phbin <- left_join(veg_pa, select(covar3, Site,Year,phbin), by=c("Site", "Year")) %>% 
    group_by(phbin,Species) %>% summarize(occ_freq_ph=sum(PA))
  occfreq_condbin <- left_join(veg_pa, select(covar3, Site,Year,condbin), by=c("Site", "Year")) %>% 
    group_by(condbin,Species) %>% summarize(occ_freq_cond=sum(PA))
  occfreq_latbin
  occfreq_phbin
  occfreq_condbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_latbin)
  unique(occfreq_latbin$Species) %>% length() # 270 species
  occfreq_latbin <- occfreq_latbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_lat)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  occfreq_phbin <- occfreq_phbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_ph)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  occfreq_condbin <- occfreq_condbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_cond)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_latbin$Species) %>% length() # 167 species
  unique(occfreq_phbin$Species) %>% length() # 167 species
  unique(occfreq_condbin$Species) %>% length() # 167 species
  
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_latbin <- occfreq_latbin %>% 
    spread(key=latbin, value=occ_freq_lat) %>% 
    gather(key=latbin, value=occ_freq_lat, 2:ncol(.)) %>% 
    mutate(occ_freq_lat=replace_na(occ_freq_lat,0))
  occfreq_phbin <- occfreq_phbin %>% 
    spread(key=phbin, value=occ_freq_ph) %>% 
    gather(key=phbin, value=occ_freq_ph, 2:ncol(.)) %>% 
    mutate(occ_freq_ph=replace_na(occ_freq_ph,0))
  occfreq_condbin <- occfreq_condbin %>% 
    spread(key=condbin, value=occ_freq_cond) %>% 
    gather(key=condbin, value=occ_freq_cond, 2:ncol(.)) %>% 
    mutate(occ_freq_cond=replace_na(occ_freq_cond,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each latbin bin gradient
  sp_SSI <- occfreq_latbin %>% 
    group_by(Species) %>%
    summarize(CV_lat=sd(occ_freq_lat)/mean(occ_freq_lat))
  sp_SSI2 <- occfreq_phbin %>% 
    group_by(Species) %>%
    summarize(CV_ph=sd(occ_freq_ph)/mean(occ_freq_ph))
  sp_SSI3 <- occfreq_condbin %>% 
    group_by(Species) %>%
    summarize(CV_cond=sd(occ_freq_cond)/mean(occ_freq_cond))
  # combine
  sp_SSI <- left_join(sp_SSI, sp_SSI2)
  sp_SSI <- left_join(sp_SSI, sp_SSI3)
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  top_n(sp_SSI, n=5, wt=CV_lat)
  top_n(sp_SSI, n=5, wt=CV_ph)
  top_n(sp_SSI, n=5, wt=CV_cond)
  
  
  # distribution of CV for each species
  sp_SSI %>% 
    gather(key=variable, value=CV, 2:4) %>% 
    ggplot(aes(x=CV, fill=variable)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_lat), occfreq_latbin) %>% 
    group_by(Species, CV_lat) %>% summarize(cum_occ_freq = sum(occ_freq_lat)) %>% 
    ggplot(aes(x=CV_lat, y=cum_occ_freq)) + 
    geom_point()
  left_join(select(sp_SSI, Species, CV_ph), occfreq_phbin) %>% 
    group_by(Species, CV_ph) %>% summarize(cum_occ_freq = sum(occ_freq_ph)) %>% 
    ggplot(aes(x=CV_ph, y=cum_occ_freq)) + 
    geom_point()
  left_join(select(sp_SSI, Species, CV_cond), occfreq_condbin) %>% 
    group_by(Species, CV_cond) %>% summarize(cum_occ_freq = sum(occ_freq_cond)) %>% 
    ggplot(aes(x=CV_cond, y=cum_occ_freq)) + 
    geom_point()
  
  
  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_lat))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_lat=sum(CV_lat)/length(CV_lat),
              CSI_ph=sum(CV_ph)/length(CV_ph),
              CSI_cond=sum(CV_cond)/length(CV_cond))
  
  veg_CSI_covar <- left_join(veg_CSI_covar,covar3) %>% select(-latbin, -phbin, -condbin)
  
  # distribution of site-level CSI
  veg_CSI_covar %>% 
    gather(key=variable, value=CSI, 3:5) %>% 
    ggplot(aes(x=CSI, fill=variable)) +
    geom_histogram(bins=30, alpha=0.5) + facet_wrap(~variable)
  
  ggplot(left_join(veg_CSI_covar, wetlandclassification),aes(x=Lat,y=CSI_lat)) +
    geom_point(aes(color=WetlandType))
  ggplot(veg_CSI_covar,aes(x=pH,y=CSI_ph)) +
    geom_point()
  ggplot(veg_CSI_covar,aes(x=log(Conductivity),y=CSI_cond)) +
    geom_point()
}

# FFP, MAP, MAT 
{
  # tally number of sp occurrences across gradients
  head(climate2)
  
  climate3 <- climate2 %>% select(Site,Year,FFP,MAP,MAT)
  climate3$FFPbin <- ntile(climate3$FFP, n=10) # not a good distributio of site
  climate3$MAPbin <- ntile(climate3$MAP, n=5)
  climate3$MATbin <- ntile(climate3$MAT, n=5)
  head(climate3)
  
  climate3 %>% 
    ggplot(aes(x=FFPbin)) + 
    geom_histogram(bins=10, color=1, fill="white") # how many sites per bin
  
  veg_pa <- veg_pa %>% filter(PA==1)
  head(veg_pa)
  length(unique(veg_pa$Species))
  head(climate3)
  
  # plot the occurrence frequency of example species 
  # ffp
  left_join(veg_pa, select(climate3, Site,Year,FFPbin, MAPbin, MATbin), by=c("Site", "Year")) %>% 
    group_by(FFPbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=FFPbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  # MAP
  left_join(veg_pa, select(climate3, Site,Year,FFPbin, MAPbin, MATbin), by=c("Site", "Year")) %>% 
    group_by(MAPbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=MAPbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  # MAT
  left_join(veg_pa, select(climate3, Site,Year,FFPbin, MAPbin, MATbin), by=c("Site", "Year")) %>% 
    group_by(MATbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=MATbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  
  # occurrence freq of each sp in each bin
  occfreq_FFPbin <- left_join(veg_pa, select(climate3, Site,Year,FFPbin), by=c("Site", "Year")) %>% 
    group_by(FFPbin,Species) %>% summarize(occ_freq_FFP=sum(PA))
  occfreq_MAPbin <- left_join(veg_pa, select(climate3, Site,Year,MAPbin), by=c("Site", "Year")) %>% 
    group_by(MAPbin,Species) %>% summarize(occ_freq_MAP=sum(PA))
  occfreq_MATbin <- left_join(veg_pa, select(climate3, Site,Year,MATbin), by=c("Site", "Year")) %>% 
    group_by(MATbin,Species) %>% summarize(occ_freq_MAT=sum(PA))
  occfreq_FFPbin
  occfreq_MAPbin
  occfreq_MATbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_FFPbin)
  unique(occfreq_FFPbin$Species) %>% length() # 270 species
  occfreq_FFPbin <- occfreq_FFPbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_FFP)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  occfreq_MAPbin <- occfreq_MAPbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_MAP)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  occfreq_MATbin <- occfreq_MATbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_MAT)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_FFPbin$Species) %>% length() # 167 species
  unique(occfreq_MAPbin$Species) %>% length() # 167 species
  unique(occfreq_MATbin$Species) %>% length() # 167 species
  
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_FFPbin <- occfreq_FFPbin %>% 
    spread(key=FFPbin, value=occ_freq_FFP) %>% 
    gather(key=FFPbin, value=occ_freq_FFP, 2:ncol(.)) %>% 
    mutate(occ_freq_FFP=replace_na(occ_freq_FFP,0))
  occfreq_MAPbin <- occfreq_MAPbin %>% 
    spread(key=MAPbin, value=occ_freq_MAP) %>% 
    gather(key=MAPbin, value=occ_freq_MAP, 2:ncol(.)) %>% 
    mutate(occ_freq_MAP=replace_na(occ_freq_MAP,0))
  occfreq_MATbin <- occfreq_MATbin %>% 
    spread(key=MATbin, value=occ_freq_MAT) %>% 
    gather(key=MATbin, value=occ_freq_MAT, 2:ncol(.)) %>% 
    mutate(occ_freq_MAT=replace_na(occ_freq_MAT,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each FFPbin bin gradient
  head(occfreq_MATbin)
  sp_SSI <- occfreq_FFPbin %>% 
    group_by(Species) %>%
    summarize(CV_FFP=sd(occ_freq_FFP)/mean(occ_freq_FFP))
  sp_SSI2 <- occfreq_MAPbin %>% 
    group_by(Species) %>%
    summarize(CV_MAP=sd(occ_freq_MAP)/mean(occ_freq_MAP))
  sp_SSI3 <- occfreq_MATbin %>% 
    group_by(Species) %>%
    summarize(CV_MAT=sd(occ_freq_MAT)/mean(occ_freq_MAT))
  # combine
  sp_SSI <- left_join(sp_SSI, sp_SSI2)
  sp_SSI <- left_join(sp_SSI, sp_SSI3)
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  
  # distribution of CV for each species
  sp_SSI %>% 
    gather(key=variable, value=CV, 2:4) %>% 
    ggplot(aes(x=CV, fill=variable)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_FFP), occfreq_FFPbin) %>% 
    group_by(Species, CV_FFP) %>% summarize(cum_occ_freq = sum(occ_freq_FFP)) %>% 
    ggplot(aes(x=CV_FFP, y=cum_occ_freq)) + 
    geom_point()
  left_join(select(sp_SSI, Species, CV_MAP), occfreq_MAPbin) %>% 
    group_by(Species, CV_MAP) %>% summarize(cum_occ_freq = sum(occ_freq_MAP)) %>% 
    ggplot(aes(x=CV_MAP, y=cum_occ_freq)) + 
    geom_point()
  left_join(select(sp_SSI, Species, CV_MAT), occfreq_MATbin) %>% 
    group_by(Species, CV_MAT) %>% summarize(cum_occ_freq = sum(occ_freq_MAT)) %>% 
    ggplot(aes(x=CV_MAT, y=cum_occ_freq)) + 
    geom_point()
  
  
  head(veg_pa)
  veg_CSI_climate <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_FFP))
  head(veg_CSI_climate)
  unique(veg_CSI_climate$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  veg_CSI_climate <- veg_CSI_climate %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_FFP=sum(CV_FFP)/length(CV_FFP),
              CSI_MAP=sum(CV_MAP)/length(CV_MAP),
              CSI_MAT=sum(CV_MAT)/length(CV_MAT))
  
  veg_CSI_climate <- left_join(veg_CSI_climate,climate3) %>% select(-FFPbin, -MAPbin, -MATbin)
  
  # distribution of site-level CSI
  veg_CSI_climate %>% 
    gather(key=variable, value=CSI, 3:5) %>% 
    ggplot(aes(x=CSI, fill=variable)) +
    geom_histogram(bins=30, alpha=0.5) + facet_wrap(~variable)
  
  ggplot(veg_CSI_climate,aes(x=FFP,y=CSI_FFP)) +
    geom_point()
  ggplot(veg_CSI_climate,aes(x=MAP,y=CSI_MAP)) +
    geom_point()
  ggplot(left_join(veg_CSI_climate, wetlandclassification),aes(x=MAT,y=CSI_MAT)) +
    geom_point(aes(color=WetlandType))
  
  # ggplot(veg_CSI_climate,aes(x=CSI_FFP,y=CSI_MAT)) +
  #   geom_point()
}

# lat, ph, conductivity
{
  # tally number of sp occurrences across gradients
  head(covar2)
  head(climate2)
  
  covar3 <- covar2 %>% select(Site,Year,Lat=LATTITUDE,pH,Conductivity)
  covar3$latbin <- ntile(covar3$Lat, n=10)
  covar3$phbin <- ntile(covar3$pH, n=5)
  covar3$condbin <- ntile(covar3$Conductivity, n=5)
  head(covar3)
  
  covar3 %>% 
    ggplot(aes(x=condbin)) + 
    geom_histogram(bins=5, color=1, fill="white") # how many sites per bin
  
  veg_pa <- veg_pa %>% filter(PA==1)
  head(veg_pa)
  length(unique(veg_pa$Species))
  head(covar3)
  
  # plot the occurrence frequency of example species 
  # latbin
  left_join(veg_pa, select(covar3, Site,Year,latbin,phbin,condbin), by=c("Site", "Year")) %>% 
    group_by(latbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=latbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  # phbin
  left_join(veg_pa, select(covar3, Site,Year,latbin,phbin,condbin), by=c("Site", "Year")) %>% 
    group_by(phbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=phbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  # condbin
  left_join(veg_pa, select(covar3, Site,Year,latbin,phbin,condbin), by=c("Site", "Year")) %>% 
    group_by(condbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=condbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  
  # occurrence freq of each sp in each bin
  occfreq_latbin <- left_join(veg_pa, select(covar3, Site,Year,latbin), by=c("Site", "Year")) %>% 
    group_by(latbin,Species) %>% summarize(occ_freq_lat=sum(PA))
  occfreq_phbin <- left_join(veg_pa, select(covar3, Site,Year,phbin), by=c("Site", "Year")) %>% 
    group_by(phbin,Species) %>% summarize(occ_freq_ph=sum(PA))
  occfreq_condbin <- left_join(veg_pa, select(covar3, Site,Year,condbin), by=c("Site", "Year")) %>% 
    group_by(condbin,Species) %>% summarize(occ_freq_cond=sum(PA))
  occfreq_latbin
  occfreq_phbin
  occfreq_condbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_latbin)
  unique(occfreq_latbin$Species) %>% length() # 270 species
  occfreq_latbin <- occfreq_latbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_lat)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  occfreq_phbin <- occfreq_phbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_ph)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  occfreq_condbin <- occfreq_condbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_cond)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_latbin$Species) %>% length() # 167 species
  unique(occfreq_phbin$Species) %>% length() # 167 species
  unique(occfreq_condbin$Species) %>% length() # 167 species
  
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_latbin <- occfreq_latbin %>% 
    spread(key=latbin, value=occ_freq_lat) %>% 
    gather(key=latbin, value=occ_freq_lat, 2:ncol(.)) %>% 
    mutate(occ_freq_lat=replace_na(occ_freq_lat,0))
  occfreq_phbin <- occfreq_phbin %>% 
    spread(key=phbin, value=occ_freq_ph) %>% 
    gather(key=phbin, value=occ_freq_ph, 2:ncol(.)) %>% 
    mutate(occ_freq_ph=replace_na(occ_freq_ph,0))
  occfreq_condbin <- occfreq_condbin %>% 
    spread(key=condbin, value=occ_freq_cond) %>% 
    gather(key=condbin, value=occ_freq_cond, 2:ncol(.)) %>% 
    mutate(occ_freq_cond=replace_na(occ_freq_cond,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each latbin bin gradient
  sp_SSI <- occfreq_latbin %>% 
    group_by(Species) %>%
    summarize(CV_lat=sd(occ_freq_lat)/mean(occ_freq_lat))
  sp_SSI2 <- occfreq_phbin %>% 
    group_by(Species) %>%
    summarize(CV_ph=sd(occ_freq_ph)/mean(occ_freq_ph))
  sp_SSI3 <- occfreq_condbin %>% 
    group_by(Species) %>%
    summarize(CV_cond=sd(occ_freq_cond)/mean(occ_freq_cond))
  # combine
  sp_SSI <- left_join(sp_SSI, sp_SSI2)
  sp_SSI <- left_join(sp_SSI, sp_SSI3)
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  
  # distribution of CV for each species
  sp_SSI %>% 
    gather(key=variable, value=CV, 2:4) %>% 
    ggplot(aes(x=CV, fill=variable)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_lat), occfreq_latbin) %>% 
    group_by(Species, CV_lat) %>% summarize(cum_occ_freq = sum(occ_freq_lat)) %>% 
    ggplot(aes(x=CV_lat, y=cum_occ_freq)) + 
    geom_point()
  left_join(select(sp_SSI, Species, CV_ph), occfreq_phbin) %>% 
    group_by(Species, CV_ph) %>% summarize(cum_occ_freq = sum(occ_freq_ph)) %>% 
    ggplot(aes(x=CV_ph, y=cum_occ_freq)) + 
    geom_point()
  left_join(select(sp_SSI, Species, CV_cond), occfreq_condbin) %>% 
    group_by(Species, CV_cond) %>% summarize(cum_occ_freq = sum(occ_freq_cond)) %>% 
    ggplot(aes(x=CV_cond, y=cum_occ_freq)) + 
    geom_point()
  
  
  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_lat))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_lat=sum(CV_lat)/length(CV_lat),
              CSI_ph=sum(CV_ph)/length(CV_ph),
              CSI_cond=sum(CV_cond)/length(CV_cond))
  
  filter(veg_CSI_covar, Site=="1532")
  mean(tmp$CV_lat)
  sum(tmp$CV_lat)/length(tmp$CV_lat)
  
  veg_CSI_covar <- left_join(veg_CSI_covar,covar3) %>% select(-latbin, -phbin, -condbin)
  
  # distribution of site-level CSI
  veg_CSI_covar %>% 
    gather(key=variable, value=CSI, 3:5) %>% 
    ggplot(aes(x=CSI, fill=variable)) +
    geom_histogram(bins=30, alpha=0.5) + facet_wrap(~variable)
  
  ggplot(veg_CSI_covar,aes(x=Lat,y=CSI_lat)) +
    geom_point()
  ggplot(veg_CSI_covar,aes(x=pH,y=CSI_ph)) +
    geom_point()
  ggplot(veg_CSI_covar,aes(x=log(Conductivity),y=CSI_cond)) +
    geom_point()
}

# TN - water
{
  # tally number of sp occurrences across gradients
  head(covar2)
  
  covar3 <- covar2 %>% select(Site,Year,TN)
  covar3$TNbin <- ntile(covar3$TN, n=10)

  covar3 %>% 
    ggplot(aes(x=TNbin)) + 
    geom_histogram(bins=10, color=1, fill="white") # how many sites per bin
  
  veg_pa <- veg_pa %>% filter(PA==1)
  head(veg_pa)
  length(unique(veg_pa$Species))
  head(covar3)
  
  # plot the occurrence frequency of example species 
  left_join(veg_pa, select(covar3, Site,Year,TNbin), by=c("Site", "Year")) %>% 
    group_by(TNbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=TNbin,y=occ_freq)) + 
    geom_bar(stat="identity")

  
  # occurrence freq of each sp in each bin
  occfreq_TNbin <- left_join(veg_pa, select(covar3, Site,Year,TNbin), by=c("Site", "Year")) %>% 
    group_by(TNbin,Species) %>% summarize(occ_freq_TN=sum(PA))
  occfreq_TNbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_TNbin)
  unique(occfreq_TNbin$Species) %>% length() # 270 species
  occfreq_TNbin <- occfreq_TNbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_TN)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)

  unique(occfreq_TNbin$Species) %>% length() # 167 species
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_TNbin <- occfreq_TNbin %>% 
    spread(key=TNbin, value=occ_freq_TN) %>% 
    gather(key=TNbin, value=occ_freq_TN, 2:ncol(.)) %>% 
    mutate(occ_freq_TN=replace_na(occ_freq_TN,0))
 
  
  # now calculate species sensitivitiy index (=CV) for each species across each latbin bin gradient
  sp_SSI <- occfreq_TNbin %>% 
    group_by(Species) %>%
    summarize(CV_TN=sd(occ_freq_TN)/mean(occ_freq_TN))
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  top_n(sp_SSI, n=5, wt=CV_TN) # sp which are the most specialized to some N level
  top_n(sp_SSI, n=-5, wt=CV_TN) # sp which are N-generalists
  
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV_TN)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_TN), occfreq_TNbin) %>% 
    group_by(Species, CV_TN) %>% summarize(cum_occ_freq = sum(occ_freq_TN)) %>% 
    ggplot(aes(x=CV_TN, y=cum_occ_freq)) + 
    geom_point()

  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_TN))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  Nsp <- length(unique(veg_CSI_covar$Species))
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_TN=sum(CV_TN)/length(CV_TN),
              CSI_TN2=(sum(CV_TN)/length(CV_TN))/Nsp )
  
  veg_CSI_covar <- left_join(veg_CSI_covar,covar3) %>% select(-TNbin)
  
  # distribution of site-level CSI
    ggplot(veg_CSI_covar, aes(x=CSI_TN2)) +
    geom_histogram(bins=30, alpha=0.5)
  
  ggplot(veg_CSI_covar,aes(x=log(TN),y=CSI_TN2)) +
    geom_point()

}

# TP - water
{
  # tally number of sp occurrences across gradients
  head(covar2)
  
  covar3 <- covar2 %>% select(Site,Year,TP)
  covar3$TPbin <- ntile(covar3$TP, n=10)
  
  covar3 %>% 
    ggplot(aes(x=TPbin)) + 
    geom_histogram(bins=10, color=1, fill="white") # how many sites per bin
  
  veg_pa <- veg_pa %>% filter(PA==1)
  head(veg_pa)
  length(unique(veg_pa$Species))
  head(covar3)
  
  # plot the occurrence frequency of example species 
  left_join(veg_pa, select(covar3, Site,Year,TPbin), by=c("Site", "Year")) %>% 
    group_by(TPbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=TPbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  
  # occurrence freq of each sp in each bin
  occfreq_TPbin <- left_join(veg_pa, select(covar3, Site,Year,TPbin), by=c("Site", "Year")) %>% 
    group_by(TPbin,Species) %>% summarize(occ_freq_TP=sum(PA))
  occfreq_TPbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_TPbin)
  unique(occfreq_TPbin$Species) %>% length() # 270 species
  occfreq_TPbin <- occfreq_TPbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_TP)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_TPbin$Species) %>% length() # 167 species
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_TPbin <- occfreq_TPbin %>% 
    spread(key=TPbin, value=occ_freq_TP) %>% 
    gather(key=TPbin, value=occ_freq_TP, 2:ncol(.)) %>% 
    mutate(occ_freq_TP=replace_na(occ_freq_TP,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each latbin bin gradient
  sp_SSI <- occfreq_TPbin %>% 
    group_by(Species) %>%
    summarize(CV_TP=sd(occ_freq_TP)/mean(occ_freq_TP))
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  top_n(sp_SSI, n=5, wt=CV_TP) # sp which are the most specialized to some N level
  top_n(sp_SSI, n=-5, wt=CV_TP) # sp which are N-generalists
  
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV_TP)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_TP), occfreq_TPbin) %>% 
    group_by(Species, CV_TP) %>% summarize(cum_occ_freq = sum(occ_freq_TP)) %>% 
    ggplot(aes(x=CV_TP, y=cum_occ_freq)) + 
    geom_point()
  
  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_TP))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  Nsp <- length(unique(veg_CSI_covar$Species))
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_TP=sum(CV_TP)/length(CV_TP),
              CSI_TP2=(sum(CV_TP)/length(CV_TP))/Nsp )
  
  veg_CSI_covar <- left_join(veg_CSI_covar,covar3) %>% select(-TPbin)
  
  # distribution of site-level CSI
  ggplot(veg_CSI_covar, aes(x=CSI_TP)) +
    geom_histogram(bins=30, alpha=0.5)
  
  ggplot(veg_CSI_covar,aes(x=log(TP),y=CSI_TP2)) +
    geom_point()
  
}

# AREA
{
  # tally number of sp occurrences across gradients
  head(covar2)
  
  covar3 <- covar2 %>% select(Site,Year,AREA)
  covar3$AREAbin <- ntile(covar3$AREA, n=10)
  
  covar3 %>% 
    ggplot(aes(x=AREAbin)) + 
    geom_histogram(bins=10, color=1, fill="white") # how many sites per bin
  
  veg_pa <- veg_pa %>% filter(PA==1)
  head(veg_pa)
  length(unique(veg_pa$Species))
  head(covar3)
  
  # plot the occurrence frequency of example species 
  left_join(veg_pa, select(covar3, Site,Year,AREAbin), by=c("Site", "Year")) %>% 
    group_by(AREAbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=AREAbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  
  # occurrence freq of each sp in each bin
  occfreq_AREAbin <- left_join(veg_pa, select(covar3, Site,Year,AREAbin), by=c("Site", "Year")) %>% 
    group_by(AREAbin,Species) %>% summarize(occ_freq_AREA=sum(PA))
  occfreq_AREAbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_AREAbin)
  unique(occfreq_AREAbin$Species) %>% length() # 270 species
  occfreq_AREAbin <- occfreq_AREAbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_AREA)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_AREAbin$Species) %>% length() # 167 species
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_AREAbin <- occfreq_AREAbin %>% 
    spread(key=AREAbin, value=occ_freq_AREA) %>% 
    gather(key=AREAbin, value=occ_freq_AREA, 2:ncol(.)) %>% 
    mutate(occ_freq_AREA=replace_na(occ_freq_AREA,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each latbin bin gradient
  sp_SSI <- occfreq_AREAbin %>% 
    group_by(Species) %>%
    summarize(CV_AREA=sd(occ_freq_AREA)/mean(occ_freq_AREA))
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  top_n(sp_SSI, n=5, wt=CV_AREA) # sp which are the most specialized to some N level
  top_n(sp_SSI, n=-5, wt=CV_AREA) # sp which are N-generalists
  
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV_AREA)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_AREA), occfreq_AREAbin) %>% 
    group_by(Species, CV_AREA) %>% summarize(cum_occ_freq = sum(occ_freq_AREA)) %>% 
    ggplot(aes(x=CV_AREA, y=cum_occ_freq)) + 
    geom_point()
  
  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_AREA))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  Nsp <- length(unique(veg_CSI_covar$Species))
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_AREA=sum(CV_AREA)/length(CV_AREA),
              CSI_AREA2=(sum(CV_AREA)/length(CV_AREA))/Nsp )
  
  veg_CSI_covar <- left_join(veg_CSI_covar,covar3) %>% select(-AREAbin)
  
  # distribution of site-level CSI
  ggplot(veg_CSI_covar, aes(x=CSI_AREA)) +
    geom_histogram(bins=30, alpha=0.5)
  
  ggplot(veg_CSI_covar,aes(x=log(AREA),y=log(CSI_AREA2))) +
    geom_point()
  
}

# MAX_DEPTH
{
  # tally number of sp occurrences across gradients
  head(covar2)
  
  covar3 <- covar2 %>% select(Site,Year,MAX_DEPTH)
  covar3$MAX_DEPTHbin <- ntile(covar3$MAX_DEPTH, n=10)
  
  covar3 %>% 
    ggplot(aes(x=MAX_DEPTHbin)) + 
    geom_histogram(bins=10, color=1, fill="white") # how many sites per bin
  
  veg_pa <- veg_pa %>% filter(PA==1)
  head(veg_pa)
  length(unique(veg_pa$Species))
  head(covar3)
  
  # plot the occurrence frequency of example species 
  left_join(veg_pa, select(covar3, Site,Year,MAX_DEPTHbin), by=c("Site", "Year")) %>% 
    group_by(MAX_DEPTHbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=MAX_DEPTHbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  
  # occurrence freq of each sp in each bin
  occfreq_MAX_DEPTHbin <- left_join(veg_pa, select(covar3, Site,Year,MAX_DEPTHbin), by=c("Site", "Year")) %>% 
    group_by(MAX_DEPTHbin,Species) %>% summarize(occ_freq_MAX_DEPTH=sum(PA))
  occfreq_MAX_DEPTHbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_MAX_DEPTHbin)
  unique(occfreq_MAX_DEPTHbin$Species) %>% length() # 270 species
  occfreq_MAX_DEPTHbin <- occfreq_MAX_DEPTHbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_MAX_DEPTH)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_MAX_DEPTHbin$Species) %>% length() # 167 species
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_MAX_DEPTHbin <- occfreq_MAX_DEPTHbin %>% 
    spread(key=MAX_DEPTHbin, value=occ_freq_MAX_DEPTH) %>% 
    gather(key=MAX_DEPTHbin, value=occ_freq_MAX_DEPTH, 2:ncol(.)) %>% 
    mutate(occ_freq_MAX_DEPTH=replace_na(occ_freq_MAX_DEPTH,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each latbin bin gradient
  sp_SSI <- occfreq_MAX_DEPTHbin %>% 
    group_by(Species) %>%
    summarize(CV_MAX_DEPTH=sd(occ_freq_MAX_DEPTH)/mean(occ_freq_MAX_DEPTH))
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  top_n(sp_SSI, n=5, wt=CV_MAX_DEPTH) # sp which are the most specialized to some N level
  top_n(sp_SSI, n=-5, wt=CV_MAX_DEPTH) # sp which are N-generalists
  
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV_MAX_DEPTH)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_MAX_DEPTH), occfreq_MAX_DEPTHbin) %>% 
    group_by(Species, CV_MAX_DEPTH) %>% summarize(cum_occ_freq = sum(occ_freq_MAX_DEPTH)) %>% 
    ggplot(aes(x=CV_MAX_DEPTH, y=cum_occ_freq)) + 
    geom_point()
  
  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_MAX_DEPTH))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  Nsp <- length(unique(veg_CSI_covar$Species))
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_MAX_DEPTH=sum(CV_MAX_DEPTH)/length(CV_MAX_DEPTH),
              CSI_MAX_DEPTH2=(sum(CV_MAX_DEPTH)/length(CV_MAX_DEPTH))/Nsp )
  
  veg_CSI_covar <- left_join(veg_CSI_covar,covar3) %>% select(-MAX_DEPTHbin)
  
  # distribution of site-level CSI
  ggplot(veg_CSI_covar, aes(x=CSI_MAX_DEPTH)) +
    geom_histogram(bins=30, alpha=0.5)
  
  ggplot(veg_CSI_covar,aes(x=log(MAX_DEPTH),y=(CSI_MAX_DEPTH))) +
    geom_point()
  
}

# Soil C 
{
  setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data")
  soilC <- read.csv("A_T25_Mineral_Soil.csv", row.names = NULL)
  colnames(soilC) <- colnames(soilC)[2:ncol(soilC)]
  soilC <- soilC[,1:(ncol(soilC)-1)]
  soilC <- soilC %>% select(Site="ABMI.Site", "Year", TC="Total.Carbon..Percent.of.Dry.Weight.")
  head(soilC, 10)
  soilC <- soilC %>%  filter(TC!="VNA" & TC!="DNC") %>% droplevels()
  unique(soilC$TC) 
  soilC$TC <- as.numeric(levels(soilC$TC))[soilC$TC]
  soilC <- soilC %>% replace_na(list(TC=0))
  
  soilC <- soilC %>% group_by(Site,Year) %>% summarize(TC=mean(TC,na.rm = T))
  
  # keep only sites in veg_pa
 soilC <- inner_join(soilC, select(veg_pa, WetlandType,Site,Year), by=c("Site", "Year")) %>% distinct()
  
 # tally number of sp occurrences across gradients
  ggplot(soilC, aes(x=TC)) + geom_histogram()
  soilC$TCbin <- ntile(soilC$TC, n=10)
  
  soilC %>% 
    ggplot(aes(x=TCbin)) + 
    geom_histogram(bins=10, color=1, fill="white") # how many sites per bin
  
  # veg_pa <- veg_pa %>% filter(PA==1)
  # head(veg_pa)
  # length(unique(veg_pa$Species))
  # head(soilC)
  
  # plot the occurrence frequency of example species 
  left_join(veg_pa, select(soilC, Site,Year,TCbin), by=c("Site", "Year")) %>% 
    group_by(TCbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=TCbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  
  # occurrence freq of each sp in each bin
  occfreq_TCbin <- left_join(veg_pa, select(soilC, Site,Year,TCbin), by=c("Site", "Year")) %>% 
    group_by(TCbin,Species) %>% summarize(occ_freq_TC=sum(PA))
  occfreq_TCbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_TCbin)
  unique(occfreq_TCbin$Species) %>% length() # 270 species
  occfreq_TCbin <- occfreq_TCbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_TC)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_TCbin$Species) %>% length() # 167 species
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_TCbin <- occfreq_TCbin %>% 
    spread(key=TCbin, value=occ_freq_TC) %>% 
    gather(key=TCbin, value=occ_freq_TC, 2:ncol(.)) %>% 
    mutate(occ_freq_TC=replace_na(occ_freq_TC,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each bin gradient
  sp_SSI <- occfreq_TCbin %>% 
    group_by(Species) %>%
    summarize(CV_TC=sd(occ_freq_TC)/mean(occ_freq_TC))
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  top_n(sp_SSI, n=5, wt=CV_TC) # sp which are the most specialized to some N level
  top_n(sp_SSI, n=-5, wt=CV_TC) # sp which are N-generalists
  
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV_TC)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_TC), occfreq_TCbin) %>% 
    group_by(Species, CV_TC) %>% summarize(cum_occ_freq = sum(occ_freq_TC)) %>% 
    ggplot(aes(x=CV_TC, y=cum_occ_freq)) + 
    geom_point()
  
  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_TC))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  Nsp <- length(unique(veg_CSI_covar$Species))
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_TC=sum(CV_TC)/length(CV_TC),
              CSI_TC2=(sum(CV_TC)/length(CV_TC))/Nsp )
  
  veg_CSI_covar <- left_join(veg_CSI_covar,soilC) %>% select(-TCbin)
  
  # distribution of site-level CSI
  ggplot(veg_CSI_covar, aes(x=CSI_TC)) +
    geom_histogram(bins=30, alpha=0.5)
  
  ggplot(veg_CSI_covar,aes(x=log(TC),y=CSI_TC)) +
    geom_point()
  
}

# Soil pH
{
  setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data")
  soilpH <- read.csv("A_T25_Mineral_Soil.csv", row.names = NULL)
  colnames(soilpH) <- colnames(soilpH)[2:ncol(soilpH)]
  soilpH <- soilpH[,1:(ncol(soilpH)-1)]
  soilpH <- soilpH %>% select(Site="ABMI.Site", "Year", pH="Soil.Acidity..pH.")
  head(soilpH, 10)
  soilpH <- soilpH %>%  filter(pH!="VNA" & pH!="DNC") %>% droplevels()
  unique(soilpH$pH) 
  soilpH$pH <- as.numeric(levels(soilpH$pH))[soilpH$pH]

  soilpH <- soilpH %>% group_by(Site,Year) %>% summarize(pH=mean(pH,na.rm = T))
  
  # keep only sites in veg_pa
  soilpH <- inner_join(soilpH, select(veg_pa, WetlandType,Site,Year), by=c("Site", "Year")) %>% distinct()
  
  # tally number of sp occurrences across gradients
  ggplot(soilpH, aes(x=pH)) + geom_histogram()
  soilpH$pHbin <- ntile(soilpH$pH, n=5)
  
  soilpH %>% 
    ggplot(aes(x=pHbin)) + 
    geom_histogram(bins=5, color=1, fill="white") # how many sites per bin
  
  # veg_pa <- veg_pa %>% filter(PA==1)
  # head(veg_pa)
  # length(unique(veg_pa$Species))
  # head(soilpH)
  
  # plot the occurrence frequency of example species 
  left_join(veg_pa, select(soilpH, Site,Year,pHbin), by=c("Site", "Year")) %>% 
    group_by(pHbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=pHbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  # occurrence freq of each sp in each bin
  occfreq_pHbin <- left_join(veg_pa, select(soilpH, Site,Year,pHbin), by=c("Site", "Year")) %>% 
    group_by(pHbin,Species) %>% summarize(occ_freq_pH=sum(PA))
  occfreq_pHbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  dim(occfreq_pHbin)
  unique(occfreq_pHbin$Species) %>% length() # 270 species
  occfreq_pHbin <- occfreq_pHbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_pH)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_pHbin$Species) %>% length() # 167 species
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_pHbin <- occfreq_pHbin %>% 
    spread(key=pHbin, value=occ_freq_pH) %>% 
    gather(key=pHbin, value=occ_freq_pH, 2:ncol(.)) %>% 
    mutate(occ_freq_pH=replace_na(occ_freq_pH,0))
  
  
  # now calculate species sensitivitiy index (=CV) for each species across each bin gradient
  sp_SSI <- occfreq_pHbin %>% 
    group_by(Species) %>%
    summarize(CV_pH=sd(occ_freq_pH)/mean(occ_freq_pH))
  
  sp_SSI
  dim(sp_SSI)
  unique(sp_SSI$Species) %>% length() # 167 species
  
  top_n(sp_SSI, n=5, wt=CV_pH) # sp which are the most specialized to some N level
  top_n(sp_SSI, n=-5, wt=CV_pH) # sp which are N-generalists
  
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV_pH)) + geom_histogram(bins=50, alpha=0.5)
  
  # relationship between cum occ freq and CV scores
  left_join(select(sp_SSI, Species, CV_pH), occfreq_pHbin) %>% 
    group_by(Species, CV_pH) %>% summarize(cum_occ_freq = sum(occ_freq_pH)) %>% 
    ggplot(aes(x=CV_pH, y=cum_occ_freq)) + 
    geom_point()
  
  head(veg_pa)
  veg_CSI_covar <- left_join(veg_pa, sp_SSI) %>% filter(!is.na(CV_pH))
  head(veg_CSI_covar)
  unique(veg_CSI_covar$Species) %>% length() # 167 species
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  Nsp <- length(unique(veg_CSI_covar$Species))
  veg_CSI_covar <- veg_CSI_covar %>% 
    group_by(Site,Year) %>% 
    summarize(CSI_pH=sum(CV_pH)/length(CV_pH),
              CSI_pH2=(sum(CV_pH)/length(CV_pH))/Nsp )
  
  veg_CSI_covar <- left_join(veg_CSI_covar,soilpH) %>% select(-pHbin)
  
  # distribution of site-level CSI
  ggplot(veg_CSI_covar, aes(x=CSI_pH)) +
    geom_histogram(bins=30, alpha=0.5)
  
  ggplot(veg_CSI_covar,aes(x=log(pH),y=log(CSI_pH))) +
    geom_point()
  
}