# this file computes plant & invert sensitivity to surrounding human pop density

rm(list=ls())
library(tidyverse)
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
  
  head(vascplant_pa)
  levels(vascplant_pa$Species)
  vascplant_pa <- vascplant_pa %>% filter(Species!="NONE" &
                                            Species!="VNA" &
                                            Species!="SNI" &
                                            Species!="DNC")
  vascplant_pa <- droplevels(vascplant_pa)
  vascplant_pa %>% summarize(NSites = length(unique(Site))) #1207 terrestrial sites
  vascplant_pa$Protocol <- "Terrestrial"
  vascplant_pa <- vascplant_pa %>% select(Protocol, everything())
} 

# vascular plants - wetland sites 
{
  setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Wetland data")
  wet_vascplant_pa <- read.csv("A_W05_Vascular_Plants.csv", row.names = NULL)
  
  # clean plant data set
  names(wet_vascplant_pa)
  wet_vascplant_pa <- wet_vascplant_pa %>% select(Site=ABMI.Site,
                                                  Year=Year,
                                                  Species=Scientific.Name)
  
  names(wet_vascplant_pa)
  levels(wet_vascplant_pa$Species)
  
  wet_vascplant_pa <- wet_vascplant_pa %>% filter(Species!="NONE" &
                                                    Species!="VNA" &
                                                    Species!="SNI" &
                                                    Species!="DNC")
  wet_vascplant_pa %>% summarize(NSites = length(unique(Site))) # 1044 wetland sites with plant p/a
  wet_vascplant_pa$Protocol <- "Wetland"
  wet_vascplant_pa <- wet_vascplant_pa %>% select(Protocol, everything())
  
}

# # wet_vascplant_pa %>% distinct(Site,Year) %>% View()
# wet_vascplant_pa$Site <- paste("wet_",wet_vascplant_pa$Site, sep="")
# wet_vascplant_pa$Site <- wet_vascplant_pa$Site %>% str_replace(pattern="wet_OG-", replace="OGW-") 

# combine both veg datasets
{
  plant_pa <- bind_rows(vascplant_pa,wet_vascplant_pa)
  
  head(plant_pa)
  
  # round every taxa to sp; keep only taxa ID'd to species
  plant_pa$Species <- plant_pa$Species %>% word(start=1, end=2)
  
  # make site x species matrix
  # first create a full sitexspecies matrix for moss dataset
  plant_pa$PA <- 1
  
  # remove species that are duplicated b/c they were present in multiple zones, or b/c there taxa ID'd past species
  plant_pa <- plant_pa %>% distinct()
  plant_pa <- plant_pa %>% filter(!is.na(Species))
  
  # exclude singletons
  sptokeep <- plant_pa %>% group_by(Species) %>% tally() %>% filter(n>1) %>% select(Species)
  plant_pa <- plant_pa %>% filter(Species %in% sptokeep$Species)    
  
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
  poorfensites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(09) MD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  richfensites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(10) RDp" ) %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  wetmeadowsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(10.5) RDm") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  marshsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(11) VD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  swampsites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(12) SD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  alkalifensites <- siteclass %>% filter(Ecosite...Nutrient.Moisture.Code == "(13) AD") %>% select(Site="ABMI.Site", Cover=Percent.Area.of.Ecological.Site.Classification)
  
  # add wetland type ID and combine datasets
  bogsites$WetlandType <- "Bog"
  poorfensites$WetlandType <- "Poor Fen"
  richfensites$WetlandType <- "Rich Fen"
  alkalifensites$WetlandType <- "Alkali Fen"
  wetmeadowsites$WetlandType <- "Wet Meadow"
  marshsites$WetlandType <- "Marsh"
  swampsites$WetlandType <- "Swamp"
  
  
  wetlandclassification <- rbind(bogsites,
                                 poorfensites,
                                 richfensites,
                                 alkalifensites,
                                 wetmeadowsites,
                                 marshsites,
                                 swampsites)
  
  # how many repeated site classifications are there
  wetlandclassification <- wetlandclassification %>% group_by(Site, WetlandType) %>% summarize(meanCover=mean(Cover))
  wetlandclassification %>% mutate(NWetlandClasses=length(WetlandType)) %>% filter(NWetlandClasses>1) %>% arrange(desc(NWetlandClasses))
  
  # note: many sites have multiple ecosite classifications; must take the DOMINANT classification
  wetlandclassification <- wetlandclassification %>% group_by(Site) %>% filter(meanCover==max(meanCover)) %>% select(Site, WetlandType)
  head(wetlandclassification)
  # we still have 20 sites w/ 50:50 split classifications
  wetlandclassification %>% mutate(NWetlandClasses=length(WetlandType)) %>% filter(NWetlandClasses>1) %>% select(Site) %>% unique()
  
  # amend manually
  wetlandclassification[wetlandclassification$Site=="120","WetlandType"] <- "Swamp"
  wetlandclassification[wetlandclassification$Site=="122","WetlandType"]  <- "Rich Fen"
  wetlandclassification[wetlandclassification$Site=="1313","WetlandType"]  <- "Swamp"
  wetlandclassification[wetlandclassification$Site=="270","WetlandType"] <- "Rich Fen"
  wetlandclassification[wetlandclassification$Site=="508","WetlandType"] <- "Rich Fen"
  wetlandclassification[wetlandclassification$Site=="529","WetlandType"] <- "Rich Fen"
  wetlandclassification[wetlandclassification$Site=="599","WetlandType"] <- "Swamp"
  wetlandclassification[wetlandclassification$Site=="63","WetlandType"] <- "Wet Meadow"
  wetlandclassification[wetlandclassification$Site=="652","WetlandType"] <- "Rich Fen"
  wetlandclassification[wetlandclassification$Site=="729","WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site=="992","WetlandType"] <- "Swamp"
  wetlandclassification[wetlandclassification$Site=="OG-ABMI-653-1","WetlandType"]  <- "Swamp"
  
  wetlandclassification[wetlandclassification$Site=="625","WetlandType"] <- "Bog"
  
  # Fen x Marsh >> marsh
  wetlandclassification[wetlandclassification$Site=="1393","WetlandType"] <- "Marsh"
  wetlandclassification[wetlandclassification$Site==791,"WetlandType"] <- "Marsh"
  wetlandclassification[wetlandclassification$Site=="OG-SRD-1239-1","WetlandType"] <- "Marsh"
  
  # Bog x Marsh >> marsh
  wetlandclassification[wetlandclassification$Site=="OG-ABMI-1175-1","WetlandType"] <- "Marsh"
  
  # Bog x Fen >> Fen
  wetlandclassification[wetlandclassification$Site==292,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==295,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==352,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==418,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==442,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==497,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==498,"WetlandType"] <- "Rich Fen"
  wetlandclassification[wetlandclassification$Site==561,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==627,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==631,"WetlandType"] <- "Rich Fen"
  wetlandclassification[wetlandclassification$Site==728,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site==759,"WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site=="OG-ABMI-571-21","WetlandType"] <- "Poor Fen"
  wetlandclassification[wetlandclassification$Site=="OG-ABMI-510-21","WetlandType"] <- "Poor Fen"
  
  # Fen x WetMeandow
  wetlandclassification[wetlandclassification$Site==63,"WetlandType"] <- "Wet Meadow"
  
  # Bog x Swamp 
  wetlandclassification[wetlandclassification$Site=="120","WetlandType"] <- "Swamp"
  
  
  # delete repeated Site classificaitons
  wetlandclassification <- wetlandclassification %>% distinct(Site, WetlandType)
  
  # check to make sure each site has only 1 wetland classification
  wetlandclassification %>% select(Site) %>% unique() %>% nrow() # 586 unique sites
  wetlandclassification %>% select(Site, WetlandType) %>% unique() %>% nrow() # 586 unique site x wetlandtypes
  wetlandclassification$Protocol <- "Terrestrial"
}

# load and clean wetland classification - wetland sites
{
  setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Wetland data")
  siteclass_wet <- read.csv("A_W02B_Site_Capability.csv")
  # remove missing classificaiton values, and only retain the dominant site classification
  levels(siteclass_wet$Ecosite...Nutrient.Moisture.Code)
  siteclass_wet <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code!="DNC" & 
                                              Ecosite...Nutrient.Moisture.Code!="NONE",
                                            Ecosite...Nutrient.Moisture.Code!="VNA")
  siteclass_wet <- droplevels(siteclass_wet)
  # siteclass_wet$Percent.Area.of.Ecological.Site.Classification <- as.numeric(levels(siteclass_wet$Percent.Area.of.Ecological.Site.Classification))[siteclass_wet$Percent.Area.of.Ecological.Site.Classification]
  
  # separate sites by wetland type
  names(siteclass_wet)
  # separate sites by wetland type
  names(siteclass_wet)
  bogsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(08) PD") %>% select(Site="ABMI.Site")
  poorfensites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(09) MD") %>% select(Site="ABMI.Site")
  richfensites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(10) RDp" ) %>% select(Site="ABMI.Site")
  wetmeadowsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(10.5) RDm") %>% select(Site="ABMI.Site")
  marshsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(11) VD") %>% select(Site="ABMI.Site")
  swampsites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(12) SD") %>% select(Site="ABMI.Site")
  alkalifensites_wetland <- siteclass_wet %>% filter(Ecosite...Nutrient.Moisture.Code == "(13) AD") %>% select(Site="ABMI.Site")
  
  # add wetland type ID and combine datasets
  bogsites_wetland$WetlandType <- "Bog"
  poorfensites_wetland$WetlandType <- "Poor Fen"
  richfensites_wetland$WetlandType <- "Rich Fen"
  alkalifensites_wetland$WetlandType <- "Alkali Fen"
  wetmeadowsites_wetland$WetlandType <- "Wet Meadow"
  marshsites_wetland$WetlandType <- "Marsh"
  swampsites_wetland$WetlandType <- "Swamp"
  
  
  wetlandclassification_wetland <- rbind(bogsites_wetland,
                                         poorfensites_wetland,
                                         richfensites_wetland,
                                         alkalifensites_wetland,
                                         wetmeadowsites_wetland,
                                         marshsites_wetland,
                                         swampsites_wetland)
  
  # # extract proper site names add "wet" before site id
  # wetlandclassification_wetland$Site <- paste("wet_",wetlandclassification_wetland$Site, sep="")
  # wetlandclassification_wetland$Site <- wetlandclassification_wetland$Site %>% str_replace(pattern="OG-", replace="OGW-") 
  
  # MANY sites have repeated site classifications; 
  # wetlandclassification_wetland <- wetlandclassification_wetland %>% distinct()
  wetlandclassification_wetland %>% group_by(Site,WetlandType) %>% tally() %>% filter(n>1) %>% arrange(desc(n))
  
  # note: many sites have multiple ecosite classifications; must take the DOMINANT classification ie the one which occurs most often
  wetlandclassification_wetland <- wetlandclassification_wetland %>% group_by(Site,WetlandType) %>% tally() %>% ungroup() %>% group_by(Site) %>% filter(n==max(n)) %>% select(Site,WetlandType)
  
  # we still have 33 sites w/ double split classifications; ammend manually
  wetlandclassification_wetland %>% group_by(Site) %>% tally() %>% filter(n>1) %>% arrange(desc(n)) %>% select(Site) %>% data.frame()
  
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="207","WetlandType"] <- "Swamp"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="210","WetlandType"] <- "Swamp"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="296","WetlandType"] <- "Rich Fen"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="390","WetlandType"] <- "Poor Fen"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="595","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="924","WetlandType"] <- "Wet Meadow"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="OGW-ABMI-628-21","WetlandType"] <- "Rich Fen"
  
  # marsh + wet meadow >> marsh
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1057","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1333","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1363","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1370","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1429","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1435","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1611","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1651","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="236","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="OGW-ABMI-511-21","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="826","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="956","WetlandType"] <- "Marsh"
  
  # bog + marsh >> "bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="154","WetlandType"] <- "Bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="20","WetlandType"] <- "Bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="362","WetlandType"] <- "Bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="416","WetlandType"] <- "Bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="OGW-ABMI-469-1","WetlandType"] <- "Bog"
  
  # fen + marsh >> marsh
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="157","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="1570","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="237","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="247","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="387","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="418","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="467","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="553","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="754","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="829","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="851","WetlandType"] <- "Marsh"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="899","WetlandType"] <- "Marsh"
  
  # bog + fen >> Fen
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="16","WetlandType"] <- "Bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="295","WetlandType"] <- "Rich Fen"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="298","WetlandType"] <- "Bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="417","WetlandType"] <- "Bog"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="560","WetlandType"] <- "Rich Fen"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="621","WetlandType"] <- "Poor Fen"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="726","WetlandType"] <- "Poor Fen"
  wetlandclassification_wetland[wetlandclassification_wetland$Site=="842","WetlandType"] <- "Poor Fen"
  
  
  # delete repeated Site classificaitons
  wetlandclassification_wetland <- wetlandclassification_wetland %>% distinct()
  
  # check to make sure each site has only 1 wetland classification
  wetlandclassification_wetland %>% select(Site) %>% unique() %>% nrow() # 863 unique sites
  wetlandclassification_wetland %>% select(Site, WetlandType) %>% unique() %>% nrow() # 863 unique site x wetlandtypes
  wetlandclassification_wetland$Protocol <- "Wetland"
}

# combine wetland classifications
wetlandclassification_all <- bind_rows(wetlandclassification, wetlandclassification_wetland)
wetlandclassification_all %>% distinct(Site) %>% dim()


# add wetland class to plant veg df
{
  plant_pa <- left_join(plant_pa, wetlandclassification_all, by=c("Protocol", "Site")) %>% select(Protocol, WetlandType,Site,Year,everything())
  tmp <- filter(plant_pa, is.na(WetlandType) & Protocol=="Wetland") # wetland sites w/o classification are SOWWs
  head(tmp)
  tmp$WetlandType <- "Shallow Lake"
  plant_pa <- plant_pa %>% filter(!is.na(WetlandType)) # excludes wetland and terrestrial sites without classification
  # exclude species which only occur 1x
  plant_pa <- plant_pa %>% group_by(Species) %>% mutate(n=sum(PA)) %>% filter(n>1) %>% select(-n) %>% ungroup()
  
  plant_pa2 <- bind_rows(plant_pa,tmp) # adds SOWWs back to dataset
  plant_pa2 <- plant_pa2 %>% group_by(Species) %>% mutate(n=sum(PA)) %>% filter(n>1) %>% select(-n) %>% ungroup()
  
}

veg_pa <- plant_pa2 # plant_pa excludes SOWWs; plant_pa2 includes SOWWs
head(veg_pa)
veg_pa %>% ungroup() %>% distinct(Protocol,Site,Year) %>% dim()
veg_pa <- veg_pa %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
  mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
  mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-"))


#  load natural region site designation
setwd("/Users/cari/Desktop/Waterloo/ABMI Data/ABMI Site locations/")
nr <- read.csv("Terrestrial_Wetland_Sites_all_NRs.csv")
nr %>% distinct(Protocol, ABMI.Site) %>% tail(50) %>% data.frame() # note: must exclude the "W-" in wetland sites
nr$ABMI.Site <- nr$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")

veg_pa$tmpSiteMatch <- paste(str_extract(string=veg_pa$Site, pattern="OG-"), str_extract(string=veg_pa$Site, pattern="\\d+"), sep="") 
veg_pa$tmpSiteMatch <- veg_pa$tmpSiteMatch %>% str_remove(pattern="NA")

veg_pa <- left_join(veg_pa, select(nr, Protocol, ABMI.Site, NSRNAME, NRNAME), by=c("Protocol", "tmpSiteMatch"="ABMI.Site")) %>% select(-tmpSiteMatch) %>% distinct()

veg_pa %>% filter(is.na(NRNAME)) %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) #19 sites w/o NR

veg_pa <- veg_pa %>% filter(WetlandType!="Alkali Fen") %>% droplevels()
veg_pa <- veg_pa %>% filter(!is.na(NRNAME)) %>% droplevels()

veg_pa %>% distinct(Protocol,WetlandType,Site, NRNAME) %>% group_by(Protocol) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site, NRNAME) %>% group_by(WetlandType) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site, NRNAME) %>% group_by(NRNAME) %>% tally()


# human population density around each wetland
{
  setwd("/Users/cari/Desktop/Waterloo/Martin")
  pop <- read.csv("CD and CSD of ABMI sites.csv")
  tmp <- read.csv("Alberta Population 2011 and 2016 censuses by CSD.csv") %>% select(Geo_name, Pop_2016, Pop_2011, Pop_percent_change, "PopDens_km2"=Population.density.per.square.kilometre_2016)
  pop <- left_join(pop, tmp, by=c("CSDNAME"="Geo_name")) %>% select(-Year)
  
  tail(pop$ABMI.Site,100)
  veg_pa %>% select(Protocol, Site) %>% tail(100)
  # remove "W-" from pop sites
  pop$ABMI.Site <- pop$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")
  
  # create ABMI main site matching ID in veg_pa (veg pa has sites w/ extra numbers at end)
  veg_pa$tmpSiteMatch <- paste(str_extract(string=veg_pa$Site, pattern="OG-"), str_extract(string=veg_pa$Site, pattern="\\d+"), sep="") 
  veg_pa$tmpSiteMatch <- veg_pa$tmpSiteMatch %>% str_remove(pattern="NA")
  
  # sites to keep
  sites <- veg_pa %>% ungroup() %>% select(Protocol, Site, tmpSiteMatch,Year) %>% distinct()
  
  pop <- left_join(sites,pop, by=c("tmpSiteMatch"="ABMI.Site")) %>% select(-tmpSiteMatch) %>% distinct()
  
  # get rid of matching site variable in veg_pa
  veg_pa <- veg_pa %>% select(-tmpSiteMatch)
}

# Species sensitivity to human development (based on pop)
{
  # first create bins
  pop$PopDensbin <- ntile(pop$PopDens_km2, n=10) 
  head(pop)
  ggplot(pop, aes(x=PopDensbin)) +
    geom_histogram(bins=10, color=1, fill="white")
  
  # occurrence freq of each sp in each bin
  # must exclude species which were found in sits w/o climate data, and therefore were not assigned a bin 
  occfreq_PopDensbin <- left_join(veg_pa, select(pop, Site,Year,PopDensbin), by=c("Site", "Year")) %>% 
    group_by(PopDensbin,Species) %>% 
    summarize(occ_freq_PopDens=sum(PA)) %>% 
    filter(!is.na(PopDensbin)) 
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_PopDensbin <- occfreq_PopDensbin %>% 
    spread(key=PopDensbin, value=occ_freq_PopDens) %>% 
    gather(key=PopDensbin, value=occ_freq_PopDens, 2:ncol(.)) %>% 
    mutate(occ_freq_PopDens=replace_na(occ_freq_PopDens,0))
  
  # cumulate occurrence frequency of example species across binned gradient
  occfreq_PopDensbin %>% 
    filter(Species=="Antennaria pulcherrima") %>% 
    ggplot(aes(x=PopDensbin,y=occ_freq_PopDens)) + 
    geom_bar(stat="identity")
  
  # aside: how many species increase/decrease in abundance across gradient
  class(occfreq_PopDensbin$PopDensbin)
  occfreq_PopDensbin$PopDensbin <- factor(occfreq_PopDensbin$PopDensbin, levels=1:10, ordered=T)
  head(occfreq_PopDensbin)
  rel_occfreq <- occfreq_PopDensbin %>% 
    group_by(Species) %>% 
    mutate(tot_occ=sum(occ_freq_PopDens), 
           rel_occ_freq=occ_freq_PopDens/tot_occ) %>% select(-occ_freq_PopDens, -tot_occ)
  
  #loop lm over species
  rel_occfreq$Species <- rel_occfreq$Species %>% str_replace(" ", "_")
  rel_occfreq$Species <- rel_occfreq$Species %>% str_replace("-", "_")
  rel_occfreq_wide <- rel_occfreq %>% spread(key=Species, value=rel_occ_freq)
  
  responses <- names(rel_occfreq_wide)[-1]
  slopes2 <- data.frame(Species=responses, 
                        slope=rep(NA, length(responses)), 
                        pval=rep(NA, length(responses)))
  slopes2
  
  for(i in 1:length(responses)){
    myformula <- paste(responses[i],'~as.numeric(PopDensbin)')
    mod <- lm(as.formula(myformula), data=rel_occfreq_wide)
    slopes2[i,"slope"]<-summary(mod)$coefficients[2,1]
    slopes2[i, "pval"]<-summary(mod)$coefficients[2,4]
  }
  
  slopes2 %>% dim() # 905 sp in total
  slopes2 %>% filter(pval<0.05) %>% dim() # 144 with sig slopes
  slopes2 %>% 
    filter(pval<0.05) %>%
    ggplot(aes(x=slope)) + geom_histogram()
  
  tmp <- left_join(rel_occfreq, slopes2, by="Species") %>% arrange(Species)
  
  tmp$color <- round(ifelse(tmp$pval<=0.05, tmp$slope, 0),3)
  tmp$color2 <- ifelse(tmp$pval<=0.05 & tmp$slope<0, "Neg.", 
                       ifelse(tmp$pval<=0.05 & tmp$slope>0, "Pos.", "ns"))
  head(tmp)
  tmp %>% ungroup() %>% distinct(color, color2) %>% filter(color2=="ns")
  
  tmp %>% group_by(color2) %>% summarize(
    minslope=min(slope),
    meanslope=mean(slope),
    maxslope=max(slope),
    Nslope=length(slope)/10)
  
  tmp$color2 <- factor(tmp$color2, levels=c("ns", "Neg.", "Pos."))
  
  # continuous color
  tmp %>% 
    filter(pval<=0.05) %>% 
    ggplot(aes(x=PopDensbin, y=rel_occ_freq, group=Species, 
               col=color)) +
    # geom_smooth(method="lm", se=F, size=0.2) +
    geom_smooth(method="lm", formula=y~exp(x), size=0.2, se=F) +
    scale_color_gradient2(low="red", high="blue") +
    theme_classic()
  
  # color based on significant pos/neg slopes 
  color2 <- c("Pos."="blue", "Neg."="red", "ns"="grey80")
  
  tmp %>% 
    ggplot(aes(x=PopDensbin, y=rel_occ_freq, group=Species, 
               col=factor(color2))) +
    geom_smooth(method="lm", se=F, size=0.2, alpha=0.5) +
    # geom_smooth(method="lm", formula=y~exp(x), size=0.2, se=F) +
    # geom_smooth(method="lm", formula=y~poly(x,2), size=0.2, se=F) +
    scale_color_manual(values=color2, name="Slope") +
    theme_classic() + theme(legend.position="top")
  
  # now calculate species sensitivitiy index (=CV) for each species across each FFPbin bin gradient
  head(occfreq_PopDensbin)
  sp_SSI <- occfreq_PopDensbin %>% 
    group_by(Species) %>%
    summarize(CV_PopDens=sd(occ_freq_PopDens)/mean(occ_freq_PopDens))
  
  unique(sp_SSI$Species) %>% length() # 905 species
  
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV_PopDens)) + geom_histogram(bins=100, alpha=0.5)
  
  # relationship between cum occ freq (i.e. total occurrence) and CV scores
  left_join(select(sp_SSI, Species, CV_PopDens), occfreq_PopDensbin) %>% 
    group_by(Species, CV_PopDens) %>% summarize(cum_occ_freq = sum(occ_freq_PopDens)) %>% 
    ggplot(aes(x=CV_PopDens, y=cum_occ_freq)) + 
    geom_point()
  
  # calculate mean CV of each community (also compare the summed CV of each community)
  veg_CSI_popdens <- left_join(veg_pa, sp_SSI) 
  veg_CSI_popdens <- veg_CSI_popdens %>% 
    group_by(Protocol,NRNAME, WetlandType,Site,Year) %>% 
    summarize(CSI_PopDens=mean(CV_PopDens)) 
  
  veg_CSI_popdens <- left_join(veg_CSI_popdens,pop) %>% select(-Pop_2016, -Pop_2011, -PopDensbin)
  
  # now examine relationships
  # distribution of site-level CSI
  ggplot(veg_CSI_popdens, aes(x=CSI_PopDens)) +
    geom_histogram(bins=30, alpha=0.5, position="identity") 
  
  # relationship between veg community specialization and Pop Density
  ggplot(veg_CSI_popdens,aes(x=PopDens_km2,y=CSI_PopDens, color=Protocol)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", se=F, formula=y~poly(x,2)) +
    scale_x_log10() +
    theme_classic() +
    theme(legend.position = "top")
  ggplot(veg_CSI_popdens,aes(x=PopDens_km2,y=CSI_PopDens, color=WetlandType, shape=WetlandType)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", se=F) +
    theme_classic() +
    scale_x_log10() +
    facet_wrap(~WetlandType) +
    scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "top")
  ggplot(veg_CSI_popdens,aes(x=PopDens_km2,y=CSI_PopDens, color=NRNAME, shape=NRNAME)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", se=F, formula=y~poly(x,2)) +
    theme_classic() +
    scale_x_log10() +
    facet_wrap(~NRNAME) +
    # scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "top")
  
  # relationship between veg community specialization to pop density vs clim vars
  veg_CSI_popdenclim <- left_join(veg_CSI_popdens, clim2, by=c("Protocol", "Site", "Year")) %>% select(-Pop_percent_change)
  ggplot(veg_CSI_popdenclim, aes(x=climatic_moisture_defecit, 
                                 y=CSI_PopDens)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", se=F, formula=y~poly(x,2)) +
    theme_classic() +
    theme(legend.position = "top")
  
  veg_CSI_popdenclim %>% gather(key=ClimVar, value=Value, 10:ncol(veg_CSI_popdenclim)) %>% 
    ggplot(aes(x=Value, y=CSI_PopDens, color=ClimVar)) +
    geom_point(alpha=0.5,size=0.1) + 
    geom_smooth(method="lm", se=F, formula=y~poly(x,2)) +
    geom_smooth(method="lm", se=F, linetype="dashed") +
    facet_wrap(~ClimVar, scales="free_x") +
    theme_classic() +
    theme(legend.position = "top")
}

