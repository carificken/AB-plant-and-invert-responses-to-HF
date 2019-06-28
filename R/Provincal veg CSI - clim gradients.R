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


# env vars - MARTIN'S WETLAND ONLY CLIM DATA
{
  # sites to keep
  sites <- veg_pa %>% ungroup() %>% select(Protocol, Site,Year) %>% distinct()
  dim(sites)
  
  setwd("/Users/cari/Desktop/Waterloo/Martin")
load("climate_covar.Rdata")
head(climate)
climate <- climate %>% mutate(SITE = str_replace(SITE, pattern="-ABMI-", replacement = "-"))  
  
# duplicate climate df so each terrestrial and wetland site has data
climate_wet <- climate
climate_ter <- climate

climate_wet$Protocol <- "Wetland"
climate_ter$Protocol <- "Terrestrial"

climate_ter$SITE <- climate_ter$SITE %>% str_replace(pattern="OGW-", replace="OG-") 
head(climate_ter)

climate2 <- bind_rows(climate_ter,climate_wet)

climate3 <- left_join(sites,select(climate2, -ID,-ID_ABMI, -ASPEN), by=c("Protocol", "Site"="SITE", "Year"="YEAR"))
climate3 <- climate3  %>% mutate(TempRange=MWMT-MCMT) 

# climate3 %>% gather(key=ClimateVar, value=Value, 4:ncol(climate3)) %>% 
#   ggplot(aes(x=Value, fill=ClimateVar)) + 
#   geom_histogram(bins=30) + 
#   facet_wrap(~ClimateVar, scales="free") + theme(legend.position="none")

# which sites do not have clim data
tmp <- filter(climate3, is.na(MAT))
tmp %>% group_by(Protocol) %>% tally() # 155 terrestrial sites have no climate data

# exclude sites wo climate data
climate3 <- climate3 %>% filter(!is.na(MAP))
}

# clim vars - extracted from ABMI data; locations inaccurate
{
  setwd("/Users/cari/Desktop/Waterloo/Martin")
  clim <- read.csv("ABMI terrestrial + wetland sites climate.csv") %>% select(-Year, -Latitude, -Longitude)
  tail(clim,100)
  veg_pa %>% distinct(Protocol,Site) %>% tail(150)
  
  # remove "W-" from climate sites
  clim$ABMI.Site <- clim$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")
  
  # create ABMI main site matching ID in veg_pa (veg pa has sites w/ extra numbers at end)
  veg_pa$tmpSiteMatch <- paste(str_extract(string=veg_pa$Site, pattern="OG-"), str_extract(string=veg_pa$Site, pattern="\\d+"), sep="") 
  veg_pa$tmpSiteMatch <- veg_pa$tmpSiteMatch %>% str_remove(pattern="NA")
  
  # sites to keep
  sites <- veg_pa %>% ungroup() %>% select(Protocol, Site, tmpSiteMatch,Year) %>% distinct()
  
  tail(clim,20)
  tail(sites, 20)

  clim2 <- left_join(sites,clim, by=c("Protocol", "tmpSiteMatch"="ABMI.Site")) %>% select(-tmpSiteMatch)
  tail(clim2)

  clim2 %>% gather(key=ClimateVar, value=Value, 4:ncol(clim2)) %>%
    ggplot(aes(x=Value, fill=ClimateVar)) +
    geom_histogram(bins=30) +
    facet_wrap(~ClimateVar, scales="free") + theme_classic() + theme(legend.position="none")
  
  # which sites do not have clim data
  tmp <- filter(clim2, is.na(MAT))
  tmp %>% group_by(Protocol) %>% tally() # 25 sites have no climate data
  
  # exclude sites wo climate data
  clim2 <- clim2 %>% filter(!is.na(MAP))
  
  }

# population density
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
# Species sensitivity to env gradients
# first create bins
{
  # tally number of sp occurrences across gradients
  clim2 <- clim2 %>% select(Protocol, Site,Year,"CMD"=climatic_moisture_defecit,FFP,MAP,MAT, "SumPrecip"=summer_precip)
  head(clim2)
  clim2$CMDbin <- ntile(clim2$FFP, n=10) 
  clim2$FFPbin <- ntile(clim2$FFP, n=10) 
  clim2$MAPbin <- ntile(clim2$MAP, n=10)
  clim2$MATbin <- ntile(clim2$MAT, n=10)
  clim2$SumPrecipbin <- ntile(clim2$SumPrecip, n=10)
  
  head(clim2)
  
  # clim2 %>%
  #   ggplot(aes(x=MATbin)) +
  #   geom_histogram(bins=10, color=1, fill="white") # how many sites per bin
}  

# plot the occurrence frequency of example species 
{  
  # CMD
  left_join(veg_pa, clim2, by=c("Site", "Year")) %>% 
    group_by(CMDbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=CMDbin,y=occ_freq)) + 
    geom_bar(stat="identity")

    # ffp
  left_join(veg_pa, clim2, by=c("Site", "Year")) %>% 
    group_by(FFPbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=FFPbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  # MAP
  left_join(veg_pa, clim2, by=c("Site", "Year")) %>% 
    group_by(MAPbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=MAPbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  # MAT
  left_join(veg_pa, clim2, by=c("Site", "Year")) %>% 
    group_by(MATbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=MATbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  
  # Summer Precip
  left_join(veg_pa, clim2, by=c("Site", "Year")) %>% 
    group_by(SumPrecipbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=SumPrecipbin,y=occ_freq)) + 
    geom_bar(stat="identity")
  }
 
# occurrence freq of each sp in each bin
{
# must exclude species which were found in sits w/o climate data, and therefore were not assigned a bin 
  occfreq_CMDbin <- left_join(veg_pa, select(clim2, Protocol, Site,Year,CMDbin), by=c("Protocol", "Site", "Year")) %>% 
    group_by(CMDbin,Species) %>% 
    summarize(occ_freq_CMD=sum(PA)) %>% 
    filter(!is.na(CMDbin)) 
occfreq_FFPbin <- left_join(veg_pa, select(clim2, Protocol, Site,Year,FFPbin), by=c("Protocol", "Site", "Year")) %>% 
    group_by(FFPbin,Species) %>% 
    summarize(occ_freq_FFP=sum(PA)) %>% 
    filter(!is.na(FFPbin)) 
occfreq_MAPbin <- left_join(veg_pa, select(clim2, Protocol, Site,Year, MAPbin), by=c("Protocol", "Site", "Year")) %>% 
    group_by(MAPbin,Species) %>% 
    summarize(occ_freq_MAP=sum(PA)) %>% 
    filter(!is.na(MAPbin)) 
occfreq_MATbin <-left_join(veg_pa, select(clim2, Protocol, Site,Year, MATbin), by=c("Protocol", "Site", "Year")) %>% 
    group_by(MATbin,Species) %>% 
    summarize(occ_freq_MAT=sum(PA)) %>% 
    filter(!is.na(MATbin)) 
occfreq_SumPrecipbin <- left_join(veg_pa, select(clim2, Protocol, Site,Year, SumPrecipbin), by=c("Protocol", "Site", "Year")) %>% 
    group_by(SumPrecipbin,Species) %>% 
    summarize(occ_freq_SumPrecip=sum(PA)) %>% 
    filter(!is.na(SumPrecipbin)) 

occfreq_CMDbin  
occfreq_FFPbin
  occfreq_MAPbin
  occfreq_MATbin
  occfreq_SumPrecipbin
  
  # exclude species which occur only 1x - they will have high sensitivity
  unique(occfreq_FFPbin$Species) %>% length() # 1057 species

  occfreq_CMDbin <- occfreq_CMDbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_CMD)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
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
  occfreq_SumPrecipbin <- occfreq_SumPrecipbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq_SumPrecip)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  unique(occfreq_CMDbin$Species) %>% length() # 1056 species
  unique(occfreq_FFPbin$Species) %>% length() # 1056 species
  unique(occfreq_MAPbin$Species) %>% length() # 1056 species
  unique(occfreq_MATbin$Species) %>% length() # 1056 species
  unique(occfreq_SumPrecipbin$Species) %>% length() # 1056 species
  
  
  # expand the bin categories so each species has an occ freq value for bins 1:10
  occfreq_CMDbin <- occfreq_CMDbin %>% 
    spread(key=CMDbin, value=occ_freq_CMD) %>% 
    gather(key=CMDbin, value=occ_freq_CMD, 2:ncol(.)) %>% 
    mutate(occ_freq_CMD=replace_na(occ_freq_CMD,0))
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
  occfreq_SumPrecipbin <- occfreq_SumPrecipbin %>% 
    spread(key=SumPrecipbin, value=occ_freq_SumPrecip) %>% 
    gather(key=SumPrecipbin, value=occ_freq_SumPrecip, 2:ncol(.)) %>% 
    mutate(occ_freq_SumPrecip=replace_na(occ_freq_SumPrecip,0))
}  
  
# now calculate species sensitivitiy index (=CV) for each species across each FFPbin bin gradient
{
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
sp_SSI4 <- occfreq_SumPrecipbin %>% 
  group_by(Species) %>%
  summarize(CV_SumPrecip=sd(occ_freq_SumPrecip)/mean(occ_freq_SumPrecip))
sp_SSI5 <- occfreq_CMDbin %>% 
  group_by(Species) %>%
  summarize(CV_CMD=sd(occ_freq_CMD)/mean(occ_freq_CMD))
  
# combine
sp_SSI <- left_join(sp_SSI, sp_SSI2)
sp_SSI <- left_join(sp_SSI, sp_SSI3)
sp_SSI <- left_join(sp_SSI, sp_SSI4)
sp_SSI <- left_join(sp_SSI, sp_SSI5)
} 

sp_SSI
dim(sp_SSI)
unique(sp_SSI$Species) %>% length() # 1056 species
  
# distribution of CV for each species
sp_SSI %>% 
  gather(key=variable, value=CV, 2:6) %>% 
  ggplot(aes(x=CV, fill=variable)) + geom_histogram(bins=50, alpha=0.5)
  
# relationship between cum occ freq and CV scores
{
  p0 <- left_join(select(sp_SSI, Species, CV_CMD), occfreq_CMDbin) %>% 
    group_by(Species, CV_CMD) %>% summarize(cum_occ_freq = sum(occ_freq_CMD)) %>% 
    ggplot(aes(x=CV_CMD, y=cum_occ_freq)) + 
    geom_point()
  
  p1 <- left_join(select(sp_SSI, Species, CV_FFP), occfreq_FFPbin) %>% 
    group_by(Species, CV_FFP) %>% summarize(cum_occ_freq = sum(occ_freq_FFP)) %>% 
    ggplot(aes(x=CV_FFP, y=cum_occ_freq)) + 
    geom_point()
  
  p2 <- left_join(select(sp_SSI, Species, CV_MAP), occfreq_MAPbin) %>% 
    group_by(Species, CV_MAP) %>% summarize(cum_occ_freq = sum(occ_freq_MAP)) %>% 
    ggplot(aes(x=CV_MAP, y=cum_occ_freq)) + 
    geom_point()
  
  p3 <- left_join(select(sp_SSI, Species, CV_MAT), occfreq_MATbin) %>% 
    group_by(Species, CV_MAT) %>% summarize(cum_occ_freq = sum(occ_freq_MAT)) %>% 
    ggplot(aes(x=CV_MAT, y=cum_occ_freq)) + 
    geom_point()
  
  p4 <- left_join(select(sp_SSI, Species, CV_SumPrecip), occfreq_SumPrecipbin) %>% 
    group_by(Species, CV_SumPrecip) %>% summarize(cum_occ_freq = sum(occ_freq_SumPrecip)) %>% 
    ggplot(aes(x=CV_SumPrecip, y=cum_occ_freq)) + 
    geom_point()
  
  cowplot::plot_grid(p0, p1,p2,p3,p4, ncol=2, nrow=3)
  
  head(veg_pa)
  head(sp_SSI)
  } 
  
# calculate mean CV of each community (also compare the summed CV of each community)
{
  veg_CSI_climate <- left_join(veg_pa, sp_SSI) 
  veg_CSI_climate <- veg_CSI_climate %>% 
    group_by(Protocol,NRNAME, WetlandType,Site,Year) %>% 
    summarize(CSI_CMD=mean(CV_FFP),
              CSI_FFP=mean(CV_FFP),
              CSI_MAP=mean(CV_MAP),
              CSI_MAT=mean(CV_MAT),
              CSI_SumPrecip=mean(CV_SumPrecip)) %>% 
  filter(!is.na(CSI_FFP))
  
  veg_CSI_climate <- left_join(veg_CSI_climate,clim2) %>% select(-CMDbin, -FFPbin, -MAPbin, -MATbin, -SumPrecipbin)

  }  
  
  head(veg_CSI_climate)
  filter(veg_CSI_climate, is.na(WetlandType))

# plotting relatinoships bw CSI and clim grads
{
  # distribution of site-level CSI
  veg_CSI_climate %>% 
    gather(key=variable, value=CSI, 6:10) %>% 
    ggplot(aes(x=CSI, fill=variable)) +
    geom_histogram(bins=30, alpha=0.5, position="identity") 
  
  # relationship between veg community specialization and CMD
  ggplot(veg_CSI_climate,aes(x=CMD,y=CSI_CMD, color=Protocol)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2)) +
    theme_classic() +
    theme(legend.position = "top")
  ggplot(veg_CSI_climate,aes(x=CMD,y=CSI_CMD, color=WetlandType, shape=WetlandType)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", se=F) +
    theme_classic() +
    facet_wrap(~WetlandType) +
    scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "top")

  # relationship between veg community specialization and FFP
  ggplot(veg_CSI_climate,aes(x=FFP,y=CSI_FFP, color=Protocol)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2)) +
    theme_classic() +
    theme(legend.position = "top")
  ggplot(veg_CSI_climate,aes(x=FFP,y=CSI_FFP, color=WetlandType, shape=WetlandType)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", se=F) +
    theme_classic() +
    facet_wrap(~WetlandType) +
    scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "top")
  
  # relationship between veg community specialization and MAT
  ggplot(veg_CSI_climate,aes(x=MAT,y=CSI_MAT, color=Protocol)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2)) +
    theme_classic() +
    theme(legend.position = "top")
  ggplot(veg_CSI_climate,aes(x=MAT,y=CSI_MAT, color=WetlandType, shape=WetlandType)) +
    geom_point(alpha=0.5) + 
    # geom_smooth(method="lm", se=F) +
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    facet_wrap(~WetlandType) +
    scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "none")
  ggplot(veg_CSI_climate,aes(x=MAT,y=CSI_MAT)) +
    geom_point(alpha=0.5, aes(color=NRNAME, shape=NRNAME)) + 
    # geom_smooth(method="lm", se=F, aes(color=NRNAME, shape=NRNAME)) +
    geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
    theme_classic() +
    scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "top")
  
  # relationship between veg community specialization and MAP
  ggplot(veg_CSI_climate,aes(x=MAP,y=CSI_MAP, color=Protocol)) +
    # coord_trans(x="log10",y="log10") +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=T) +
    theme_classic() +
    theme(legend.position = "top")
  ggplot(filter(veg_CSI_climate, Site!="1261"), # marsh outliers
         aes(x=MAP,y=CSI_MAP, color=WetlandType, shape=WetlandType)) +
    # coord_trans(x="log10",y="log10") +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", se=F) +
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    facet_wrap(~WetlandType) +
    scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "top")
  
  # relationship between veg community specialization and SumPrecip
  ggplot(veg_CSI_climate,aes(x=SumPrecip,y=CSI_SumPrecip, color=Protocol)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm") +
    theme_classic() +
    theme(legend.position = "top")
  ggplot(veg_CSI_climate,aes(x=SumPrecip,y=CSI_SumPrecip, color=WetlandType, shape=WetlandType)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    # geom_smooth(method="lm", linetype="dashed", se=F) +
    theme_classic() +
    facet_wrap(~WetlandType) +
    scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
    theme(legend.position = "top")
}

dim(veg_pa)
head(veg_pa)

ggplot(veg_CSI_climate,aes(x=MAT,y=CSI_MAT)) +
  geom_point(alpha=0.5, aes(color=NRNAME, shape=NRNAME)) + 
  # geom_smooth(method="lm", se=F, aes(color=NRNAME, shape=NRNAME)) +
  geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
  theme_classic() +
  scale_shape_manual(values=c(15:17,8, 10,14, 3)) +
  theme(legend.position = "top")

# compare sp richness between protocols 
{
# calculate sp richness

veg_rich <- veg_pa %>% group_by(Protocol, NRNAME, WetlandType, Site,  Year) %>% summarize(spR=sum(PA))
veg_rich <- left_join(veg_rich,clim2) %>% select(-CMDbin, -FFPbin, -MAPbin, -MATbin, -SumPrecipbin) 

# compare sp richness between wetland vs terr protocols
ggplot(veg_rich, aes(x=CMD, y=spR, color=Protocol, shape=Protocol)) +
  geom_point( alpha=0.5, size=0.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm", se=F) +
  theme_classic() +
  theme(legend.position = "top") +
  guides(shape = guide_legend(override.aes = list(size = 2, alpha=1)))

ggplot(veg_rich, aes(x=FFP, y=spR, color=Protocol, shape=Protocol)) +
  geom_point( alpha=0.5, size=0.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm", se=F) +
  theme_classic() +
  theme(legend.position = "top") +
  guides(shape = guide_legend(override.aes = list(size = 2, alpha=1)))

ggplot(veg_rich, aes(x=MAP, y=spR, color=Protocol)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  geom_smooth(method="lm", se=F) +
  labs(y="Sp. Richness (N)") +
  theme_classic() +
  theme(legend.position = "top")

ggplot(veg_rich, aes(x=MAT, y=spR, color=Protocol)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  geom_smooth(method="lm", se=F) +
  labs(y="Sp. Richness (N)") +
  theme_classic() +
  theme(legend.position = "top")

ggplot(veg_rich, aes(x=SumPrecip, y=spR, color=Protocol)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  geom_smooth(method="lm", se=F) +
  labs(y="Sp. Richness (N)") +
  theme_classic() +
  theme(legend.position = "top")

# compare sp richness among wetland types
p0 <- ggplot(veg_rich, aes(x=CMD, y=spR, color=WetlandType)) +
  geom_point(pch=20, alpha=0.5, size=0.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p1 <- ggplot(veg_rich, aes(x=FFP, y=spR, color=WetlandType)) +
  geom_point(pch=20, alpha=0.5, size=0.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p2 <- ggplot(veg_rich, aes(x=MAP, y=spR, color=WetlandType)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p3 <- ggplot(veg_rich, aes(x=MAT, y=spR, color=WetlandType)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p4 <- ggplot(veg_rich, aes(x=SumPrecip, y=spR, color=WetlandType)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

richness.climgrad_wetlandtype <- cowplot::plot_grid(p0 + theme(legend.position = "none"), 
                                                    p1 + theme(legend.position = "none"),
                                                    p2 + theme(legend.position = "none"), 
                                                    p3 + theme(legend.position = "none"),  
                                                    p4 + theme(legend.position = "none"), ncol=2, nrow=3)
myleg <- cowplot::get_legend(p1)
richness.climgrad_wetlandtype <- cowplot::plot_grid(myleg,
                                                    richness.climgrad_wetlandtype,
                                                    ncol=1,
                                                    rel_heights = c(0.2,2))
richness.climgrad_wetlandtype

# compare sp richness among natural regions
p0 <- ggplot(veg_rich, aes(x=CMD, y=spR, color=NRNAME)) +
  geom_point(pch=20, alpha=0.5, size=0.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p1 <- ggplot(veg_rich, aes(x=FFP, y=spR, color=NRNAME)) +
  geom_point(pch=20, alpha=0.5, size=0.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p2 <- ggplot(veg_rich, aes(x=MAP, y=spR, color=NRNAME)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p3 <- ggplot(veg_rich, aes(x=MAT, y=spR, color=NRNAME)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

p4 <- ggplot(veg_rich, aes(x=SumPrecip, y=spR, color=NRNAME)) +
  geom_point(pch=20, alpha=0.5, size=.5) +
  labs(y="Sp. Richness (N)") +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  theme(legend.position = "top")

richness.climgrad_nr <- cowplot::plot_grid(p0 + theme(legend.position = "none"), 
                                                    p1 + theme(legend.position = "none"),
                                                    p2 + theme(legend.position = "none"), 
                                                    p3 + theme(legend.position = "none"),  
                                                    p4 + theme(legend.position = "none"), ncol=2, nrow=3)
myleg <- cowplot::get_legend(p1)
richness.climgrad_nr <- cowplot::plot_grid(myleg,
                                           richness.climgrad_nr,
                                           ncol=1,
                                           rel_heights = c(0.2,2))
richness.climgrad_nr

}

# check that SSI isn't dependent on protocol
{
# calculate SSI only with terrestrial data
{
  vegt <- filter(veg_pa, Protocol=="Terrestrial") %>% select(Species) %>% distinct()
  vegw <- filter(veg_pa, Protocol=="Wetland") %>% select(Species) %>% distinct()
  
  sptokeep <- inner_join(vegt, vegw, by=c("Species"))
  head(sptokeep)
  
  veg_pa_sharedsp <- filter(veg_pa, Species %in% sptokeep$Species)
  head(veg_pa_sharedsp)
  
  occfreq_CMDbin_t <- left_join(filter(veg_pa_sharedsp, Protocol=="Terrestrial"),
                                select(clim2, Protocol, Site,Year,CMDbin),
                                by=c("Protocol", "Site", "Year")) %>%
    group_by(CMDbin,Species) %>% 
    summarize(occ_freq_CMD=sum(PA)) %>% 
    filter(!is.na(CMDbin)) 
  occfreq_FFPbin_t <- left_join(filter(veg_pa_sharedsp, Protocol=="Terrestrial"),
                              select(clim2, Protocol, Site,Year,FFPbin),
                              by=c("Protocol", "Site", "Year")) %>%
  group_by(FFPbin,Species) %>% 
  summarize(occ_freq_FFP=sum(PA)) %>% 
  filter(!is.na(FFPbin)) 
occfreq_MAPbin_t <- left_join(filter(veg_pa_sharedsp, Protocol=="Terrestrial"),
                              select(clim2, Protocol, Site,Year, MAPbin),
                              by=c("Protocol", "Site", "Year")) %>% 
  group_by(MAPbin,Species) %>% 
  summarize(occ_freq_MAP=sum(PA)) %>% 
  filter(!is.na(MAPbin)) 
occfreq_MATbin_t <-left_join(filter(veg_pa_sharedsp, Protocol=="Terrestrial"),
                             select(clim2, Protocol, Site,Year, MATbin),
                             by=c("Protocol", "Site", "Year")) %>%
  group_by(MATbin,Species) %>% 
  summarize(occ_freq_MAT=sum(PA)) %>% 
  filter(!is.na(MATbin)) 
occfreq_SumPrecipbin_t <- left_join(filter(veg_pa_sharedsp, Protocol=="Terrestrial"),
                                    select(clim2, Protocol, Site,Year, SumPrecipbin), 
                                    by=c("Protocol", "Site", "Year")) %>% 
  group_by(SumPrecipbin,Species) %>%
  summarize(occ_freq_SumPrecip=sum(PA)) %>% 
  filter(!is.na(SumPrecipbin)) 

occfreq_CMDbin_t <- occfreq_CMDbin_t %>% 
  spread(key=CMDbin, value=occ_freq_CMD) %>% 
  gather(key=CMDbin, value=occ_freq_CMD, 2:ncol(.)) %>% 
  mutate(occ_freq_CMD=replace_na(occ_freq_CMD,0))
occfreq_FFPbin_t <- occfreq_FFPbin_t %>% 
  spread(key=FFPbin, value=occ_freq_FFP) %>% 
  gather(key=FFPbin, value=occ_freq_FFP, 2:ncol(.)) %>% 
  mutate(occ_freq_FFP=replace_na(occ_freq_FFP,0))
occfreq_MAPbin_t <- occfreq_MAPbin_t %>% 
  spread(key=MAPbin, value=occ_freq_MAP) %>% 
  gather(key=MAPbin, value=occ_freq_MAP, 2:ncol(.)) %>% 
  mutate(occ_freq_MAP=replace_na(occ_freq_MAP,0))
occfreq_MATbin_t <- occfreq_MATbin_t %>% 
  spread(key=MATbin, value=occ_freq_MAT) %>% 
  gather(key=MATbin, value=occ_freq_MAT, 2:ncol(.)) %>% 
  mutate(occ_freq_MAT=replace_na(occ_freq_MAT,0))
occfreq_SumPrecipbin_t <- occfreq_SumPrecipbin_t %>% 
  spread(key=SumPrecipbin, value=occ_freq_SumPrecip) %>% 
  gather(key=SumPrecipbin, value=occ_freq_SumPrecip, 2:ncol(.)) %>% 
  mutate(occ_freq_SumPrecip=replace_na(occ_freq_SumPrecip,0))

sp_SSI_t <- occfreq_CMDbin_t %>% 
  group_by(Species) %>%
  summarize(CV_CMD=sd(occ_freq_CMD)/mean(occ_freq_CMD))
sp_SSI1_t <- occfreq_FFPbin_t %>% 
  group_by(Species) %>%
  summarize(CV_FFP=sd(occ_freq_FFP)/mean(occ_freq_FFP))
sp_SSI2_t <- occfreq_MAPbin_t %>% 
  group_by(Species) %>%
  summarize(CV_MAP=sd(occ_freq_MAP)/mean(occ_freq_MAP))
sp_SSI3_t <- occfreq_MATbin_t %>% 
  group_by(Species) %>%
  summarize(CV_MAT=sd(occ_freq_MAT)/mean(occ_freq_MAT))
sp_SSI4_t <- occfreq_SumPrecipbin_t %>% 
  group_by(Species) %>%
  summarize(CV_SumPrecip=sd(occ_freq_SumPrecip)/mean(occ_freq_SumPrecip))

sp_SSI_t <- left_join(sp_SSI_t, sp_SSI1_t)
sp_SSI_t <- left_join(sp_SSI_t, sp_SSI2_t)
sp_SSI_t <- left_join(sp_SSI_t, sp_SSI3_t)
sp_SSI_t <- left_join(sp_SSI_t, sp_SSI4_t)

head(sp_SSI_t)
}

# calculate csi based on terrestrial SSI
{

veg_CSI_climate_t <- left_join(veg_pa_sharedsp, sp_SSI_t)   
veg_CSI_climate_t <- veg_CSI_climate_t %>% 
  group_by(Protocol,WetlandType,Site,Year) %>% 
  summarize(CSI_CMD=mean(CV_CMD),
            CSI_FFP=mean(CV_FFP),
            CSI_MAP=mean(CV_MAP),
            CSI_MAT=mean(CV_MAT),
            CSI_SumPrecip=mean(CV_SumPrecip)) %>% 
  filter(!is.na(CSI_FFP))

veg_CSI_climate_t <- left_join(veg_CSI_climate_t,clim2) %>% 
  select(-CMDbin, -FFPbin, -MAPbin, -MATbin, -SumPrecipbin)
veg_CSI_climate_t <- left_join(veg_CSI_climate_t, wetlandclassification) %>% 
  select(Protocol, WetlandType, everything()) 
}

# calculate SSI only with wetland data

{
  occfreq_CMDbin_w <- left_join(filter(veg_pa_sharedsp, Protocol=="Wetland"),
                                select(clim2, Protocol, Site,Year,CMDbin),
                                by=c("Protocol", "Site", "Year")) %>%
    group_by(CMDbin,Species) %>% 
    summarize(occ_freq_CMD=sum(PA)) %>% 
    filter(!is.na(CMDbin)) 
  occfreq_FFPbin_w <- left_join(filter(veg_pa_sharedsp, Protocol=="Wetland"),
                                select(clim2, Protocol, Site,Year,FFPbin),
                                by=c("Protocol", "Site", "Year")) %>%
    group_by(FFPbin,Species) %>% 
    summarize(occ_freq_FFP=sum(PA)) %>% 
    filter(!is.na(FFPbin)) 
  occfreq_MAPbin_w <- left_join(filter(veg_pa_sharedsp, Protocol=="Wetland"),
                                select(clim2, Protocol, Site,Year, MAPbin),
                                by=c("Protocol", "Site", "Year")) %>% 
    group_by(MAPbin,Species) %>% 
    summarize(occ_freq_MAP=sum(PA)) %>% 
    filter(!is.na(MAPbin)) 
  occfreq_MATbin_w <-left_join(filter(veg_pa_sharedsp, Protocol=="Wetland"),
                               select(clim2, Protocol, Site,Year, MATbin),
                               by=c("Protocol", "Site", "Year")) %>%
    group_by(MATbin,Species) %>% 
    summarize(occ_freq_MAT=sum(PA)) %>% 
    filter(!is.na(MATbin)) 
  occfreq_SumPrecipbin_w <- left_join(filter(veg_pa_sharedsp, Protocol=="Wetland"),
                                      select(clim2, Protocol, Site,Year, SumPrecipbin), 
                                      by=c("Protocol", "Site", "Year")) %>% 
    group_by(SumPrecipbin,Species) %>%
    summarize(occ_freq_SumPrecip=sum(PA)) %>% 
    filter(!is.na(SumPrecipbin)) 
  
  occfreq_CMDbin_w <- occfreq_CMDbin_w %>% 
    spread(key=CMDbin, value=occ_freq_CMD) %>% 
    gather(key=CMDbin, value=occ_freq_CMD, 2:ncol(.)) %>% 
    mutate(occ_freq_CMD=replace_na(occ_freq_CMD,0))
  occfreq_FFPbin_w <- occfreq_FFPbin_w %>% 
    spread(key=FFPbin, value=occ_freq_FFP) %>% 
    gather(key=FFPbin, value=occ_freq_FFP, 2:ncol(.)) %>% 
    mutate(occ_freq_FFP=replace_na(occ_freq_FFP,0))
  occfreq_MAPbin_w <- occfreq_MAPbin_w %>% 
    spread(key=MAPbin, value=occ_freq_MAP) %>% 
    gather(key=MAPbin, value=occ_freq_MAP, 2:ncol(.)) %>% 
    mutate(occ_freq_MAP=replace_na(occ_freq_MAP,0))
  occfreq_MATbin_w <- occfreq_MATbin_w %>% 
    spread(key=MATbin, value=occ_freq_MAT) %>% 
    gather(key=MATbin, value=occ_freq_MAT, 2:ncol(.)) %>% 
    mutate(occ_freq_MAT=replace_na(occ_freq_MAT,0))
  occfreq_SumPrecipbin_w <- occfreq_SumPrecipbin_w %>% 
    spread(key=SumPrecipbin, value=occ_freq_SumPrecip) %>% 
    gather(key=SumPrecipbin, value=occ_freq_SumPrecip, 2:ncol(.)) %>% 
    mutate(occ_freq_SumPrecip=replace_na(occ_freq_SumPrecip,0))
  
  sp_SSI_w <- occfreq_CMDbin_w %>% 
    group_by(Species) %>%
    summarize(CV_CMD=sd(occ_freq_CMD)/mean(occ_freq_CMD))
  sp_SSI1_w <- occfreq_FFPbin_w %>% 
    group_by(Species) %>%
    summarize(CV_FFP=sd(occ_freq_FFP)/mean(occ_freq_FFP))
  sp_SSI2_w <- occfreq_MAPbin_w %>% 
    group_by(Species) %>%
    summarize(CV_MAP=sd(occ_freq_MAP)/mean(occ_freq_MAP))
  sp_SSI3_w <- occfreq_MATbin_w %>% 
    group_by(Species) %>%
    summarize(CV_MAT=sd(occ_freq_MAT)/mean(occ_freq_MAT))
  sp_SSI4_w <- occfreq_SumPrecipbin_w %>% 
    group_by(Species) %>%
    summarize(CV_SumPrecip=sd(occ_freq_SumPrecip)/mean(occ_freq_SumPrecip))
  
  sp_SSI_w <- left_join(sp_SSI_w, sp_SSI1_w)
  sp_SSI_w <- left_join(sp_SSI_w, sp_SSI2_w)
  sp_SSI_w <- left_join(sp_SSI_w, sp_SSI3_w)
  sp_SSI_w <- left_join(sp_SSI_w, sp_SSI4_w)
  
  head(sp_SSI_w)
  }
  
  # calculate csi based on wetland SSI
  {
    veg_CSI_climate_w <- left_join(veg_pa_sharedsp, sp_SSI_w)   
    veg_CSI_climate_w <- veg_CSI_climate_w %>% 
      group_by(Protocol,WetlandType,Site,Year) %>% 
      summarize(CSI_CMD=mean(CV_CMD),
                CSI_FFP=mean(CV_FFP),
                CSI_MAP=mean(CV_MAP),
                CSI_MAT=mean(CV_MAT),
                CSI_SumPrecip=mean(CV_SumPrecip)) %>% 
      filter(!is.na(CSI_FFP))
    
    veg_CSI_climate_w <- left_join(veg_CSI_climate_w,clim2) %>% 
      select(-CMDbin,-FFPbin, -MAPbin, -MATbin, -SumPrecipbin)
    veg_CSI_climate_w <- left_join(veg_CSI_climate_w, wetlandclassification) %>% 
      select(Protocol, WetlandType, everything()) 
  }
}  
  


# cor b/w SSI calculated with both wet and terr protocols vs only terr
head(sp_SSI)
head(sp_SSI_t)
head(sp_SSI_w)

sp_SSI_compare <- sp_SSI_t
colnames(sp_SSI_compare) <- c("Species", "CV_CMD_t", "CV_FFP_t", "CV_MAP_t", "CV_MAT_t", "CV_SumPrecip_t")

sp_SSI_compare <- left_join(sp_SSI, sp_SSI_compare)

tmp <- sp_SSI_w
colnames(tmp) <- c("Species", "CV_CMD_w", "CV_FFP_w", "CV_MAP_w", "CV_MAT_w", "CV_SumPrecip_w")

sp_SSI_compare <- left_join(sp_SSI_compare, tmp)
colnames(sp_SSI_compare)

coef(lm(CV_MAT ~ CV_MAT_t, data=sp_SSI_compare))[2]
coef(lm(CV_MAT ~ CV_MAT_w, data=sp_SSI_compare))[2]

ggplot(sp_SSI_compare, aes(x=CV_MAT_t, y=CV_MAT)) +
  geom_point() +
  labs(x="SSI w/ Terrestrial protocol", y="SSI w/ both protocols") +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(method=lm, formula=y~x-1, color="red", fill="red", linetype="dashed") +
  annotate("text", x=.6,y=3, label="1:1 Line") +
  annotate("text", x=.6,y=2.9, color="red",label=paste("Slope=",round(coef(lm(CV_MAT ~ CV_MAT_t, data=sp_SSI_compare))[2],2), sep="")) +
  theme_classic()

ggplot(sp_SSI_compare, aes(x=CV_MAT_w, y=CV_MAT)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x="SSI w/ Wetland protocol", y="SSI w/ both protocols") +
  geom_smooth(method=lm, formula=y~x-1, color="red", fill="red", linetype="dashed") +
  annotate("text", x=.6,y=3, label="1:1") +
  annotate("text", x=.6,y=2.9, color="red", label=paste("Slope=",round(coef(lm(CV_MAT ~ CV_MAT_w, data=sp_SSI_compare))[2],2), sep="")) +
  theme_classic()

# how to chose the most appropriate predictors
# focus on MAT
predvars <- left_join(veg_CSI_climate, select(veg_rich, Protocol, WetlandType, Site, Year, spR))
predvars <- predvars %>% select(Protocol, NRNAME, WetlandType, Site, Year, spR, everything()) %>% distinct()
head(predvars)
predvars$Protocol <- as.factor(predvars$Protocol)
predvars$NRNAME <- as.factor(predvars$NRNAME)
predvars$WetlandType <- as.factor(predvars$WetlandType)

library(rpart); library(rpart.plot)
# grow tree
predvar_fit <- rpart(CSI_MAT ~ Protocol + NRNAME + WetlandType + spR + MAT + MAP + FFP + SumPrecip + CMD, 
                     data=predvars, method="anova")

# pick tree that minimizes miss-classification rate (prediction error); 
# we want cp values that minimizes xerror
printcp(predvar_fit) # display the results 
bestcp <- predvar_fit$cptable[which.min(predvar_fit$cptable[,"xerror"]),"CP"]
# prune tree usinging bestcp
predvar_fit.pruned <- prune(predvar_fit, cp = bestcp)
printcp(predvar_fit.pruned) # miscalculation rate of 0.04 * 0.25632 *100 = 1%

plotcp(predvar_fit.pruned) # visualize cross-validation results 
summary(predvar_fit.pruned) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(predvar_fit.pruned) # visualize cross-validation results   

# plot tree 
prp(predvar_fit.pruned, faclen = 0, cex = 0.8, extra = 1)


# random forest
library(randomForest)
fit <- randomForest(CSI_MAT ~ Protocol + NRNAME + WetlandType + spR + MAT + MAP + FFP + SumPrecip + CMD, 
                    data=predvars,
                    ntree=1000, replace=T)
print(fit) # view results 
importance(fit) # importance of each predictor
mean(fit$rsq)
library(randomForestSRC); library(ggrandomforest)

fit2 <- rfsrc(CSI_MAT ~ Protocol + NRNAME + WetlandType + spR + MAT, 
              data=as.data.frame(predvars))
plot.variable(fit2, partial=T, smooth.lines=T)
print(fit2)
print(vimp(fit2)$importance)
