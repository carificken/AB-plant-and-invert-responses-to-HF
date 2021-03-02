# this file cleans existing ABMI vegetation data, HF data, and climate data for use in the AB invert + plant project
rm(list=ls())
library(tidyverse)

##### Vegetation Data Cleaning #####
{
# load plant data
{
  # vascular plants - terrestrial sites
  {
    vascplant_pa <- read.csv("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data/A_T15_Vascular_Plants.csv", row.names = NULL)
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
    wet_vascplant_pa <- read.csv("/Users/cari/Desktop/Waterloo/ABMI Data/Wetland data/A_W05_Vascular_Plants.csv", row.names = NULL)
    
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
    siteclass <- read.csv("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data/A_T01C_Site_Capability.csv")
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
    siteclass_wet <- read.csv("/Users/cari/Desktop/Waterloo/ABMI Data/Wetland data/A_W02B_Site_Capability.csv")
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
  
  #  load natural region site designation and add to veg_pa
  {
    nr <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/Site IDs/Terrestrial_Wetland_Sites_all_NRs.csv")
    nr %>% distinct(Protocol, ABMI.Site) %>% tail(50) %>% data.frame() # note: must exclude the "W-" in wetland sites
    nr$ABMI.Site <- nr$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")
    
    veg_pa$tmpSiteMatch <- paste(str_extract(string=veg_pa$Site, pattern="OG-"), str_extract(string=veg_pa$Site, pattern="\\d+"), sep="") 
    veg_pa$tmpSiteMatch <- veg_pa$tmpSiteMatch %>% str_remove(pattern="NA")
    
    veg_pa <- left_join(veg_pa, select(nr, Protocol, ABMI.Site, NSRNAME, NRNAME), by=c("Protocol", "tmpSiteMatch"="ABMI.Site")) %>% select(-tmpSiteMatch) %>% distinct()
    
    veg_pa %>% filter(is.na(NRNAME)) %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) #19 sites w/o NR
    
    # veg_pa <- veg_pa %>% filter(WetlandType!="Alkali Fen") %>% droplevels()
    veg_pa <- veg_pa %>% filter(!is.na(NRNAME)) %>% droplevels()
    }
}

# keep taxa id'd to species
{
  veg_pa$Species <- veg_pa$Species %>% word(start=1, end=2)
  veg_pa$Species <- veg_pa$Species %>% recode("Betula X" = "Betula X sargentii")
  veg_pa <- veg_pa %>% filter(!is.na(Species))
  # veg_pa %>% filter(Species=="Hesperostipa comata ssp. comata")
  
}

veg_pa <- veg_pa %>% select(NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
head(veg_pa)

}

##### HF Data Cleaning #####
{
# HF data from ABMI excluding boreal
{
  terhf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/non-Boreal HF 250 m buffer/Terrestrial250HFData.csv") %>% 
    select("Site"="ABMI_Assigned_Site_ID", "Year"="Survey_Year", "FEATURE_TY", "Area_m2"="Shape_Area")
  terhf <- terhf %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
  terhf$Protocol <- "Terrestrial"
  terhf$Area_km2 <- terhf$Area_m2/1000000
  terhf <- terhf %>% select(-Area_m2)
  head(terhf)
  
  wethf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/non-Boreal HF 250 m buffer/Wetland250HFData.csv") 
  wethf <- wethf %>% gather(key="FEATURE_TY", value="Area_percent", 7:ncol(wethf))
  wethf$Area_km2 <- wethf$Area_percent*(pi*0.25^2)
  wethf <- wethf  %>% select("Site", "Year", "FEATURE_TY", "Area_km2")
  wethf <- wethf %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="W", replacement = ""))
  wethf$Protocol <- "Wetland"
  wethf <- wethf %>% select(Site, Year, FEATURE_TY, Protocol, Area_km2)
  
  hf_nonboreal <- bind_rows(terhf, wethf)
  hf_nonboreal <- hf_nonboreal %>% filter(!is.na(FEATURE_TY)) # Feature_ty = NA assigned for undeveloped land in ter sites, exclude and then add back in
  # check site IDs: excludes W-, -ABMI-; includes trailing -1 etc
  hf_nonboreal %>% distinct(Site) %>% filter(str_detect(Site, "OG-"))
}

# HF data from ABMI boreal only
{
  # For every site-year of veg data, I want to have hf data
  # for MATCHING sites, the HF data were collected on the same years as the veg data
  # but for MISMATCHING sites, i have veg data collected on offset years; for these i need to take average of HF data from 2 closest years
  library(readxl); library(data.table)
  
  # Boreal Terrestrial Sites
  {
    # first load HF datasets
    ter_hf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/Boreal HF 250 m buffer/Terrestrial - All_HF_buff250m_OfficialNR_BorealRegion_2003_2016.csv")
    
    # average HF for sites with replicate measures (i.e. HF measured on different transects?)
    # extract proper site names
    ter_hf <- ter_hf %>% select(Site=ABMI_Assigned_Site_ID,Year=survey_year, FEATURE_TY, Shape_Area) 
    # ter_hf <- ter_hf %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-CITSCI-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
    
    # some sites appear to have duplicated and/or questionable HF data provided; delete them 
    ter_hf <- ter_hf %>% filter(Site!="OG-ABMI-583-21") %>% droplevels()
    
    # for other sites there are a number of area estimates for each FEATURE_TY; sum them and convert to km2
    ter_hf <- ter_hf %>% 
      group_by(Site,Year,FEATURE_TY) %>% 
      summarize(Shape_Area=sum(Shape_Area),
                Area=Shape_Area/1000000) %>% 
      select(-Shape_Area)
    
    # spread, then re-gather to replace NAs with 0s
    ter_hf <- ter_hf %>% select(Site,Year,FEATURE_TY,Area) %>% 
      spread(., key=FEATURE_TY, value=Area) %>% 
      gather(., FEATURE_TY,Area, 3:ncol(.)) %>% 
      replace_na(list(Area=0)) 
    
    
    # final data frame needs to be wide
    ter_hf <- ter_hf %>% spread(FEATURE_TY,Area)
    head(ter_hf)
    
    # we will use a rolling join to identify the nearest years for which HF data was collected PRIOR TO and FOLLOWING veg collection
    # vascular plants - terrestrial sites
    {
      vascplant_pa <- read.csv("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data/A_T15_Vascular_Plants.csv", row.names = NULL)
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
      vascplant_pa <- vascplant_pa %>% select(Site, Year) %>% distinct()
    } 
    
    head(vascplant_pa)
    head(ter_hf)
    unique(vascplant_pa$Site) # we can keep the inner -ABMI- etc at first
    unique(ter_hf$Site) # we can keep the inner -ABMI- etc at first
    
    # first we have to create DTs with just the ID variables
    # remove data columns from HF
    hf <- select(ungroup(ter_hf), Site, Year)
    
    head(hf)
    
    # make HF a data table and specify grouping variables
    setDT(hf)
    setkey(hf)
    # Add a duplicate Year variable so it doesn't get lost when joining DTs
    hf$HFYear  <- hf$Year # HFYear is the year for which we have HF data
    
    # We have already removed the data columns from the veg df. 
    # Create a duplicate Year variable, convert to data.table, and set grouping variables
    setDT(vascplant_pa)
    setkey(vascplant_pa)
    vascplant_pa$VegYear <- vascplant_pa$Year #VegYear is the year for which we have veg data and for which we want HF data
    
    # now do the first rolling join - 
    # which disturbance-year occurs BEFORE each veg year?
    pairs_before <- hf[vascplant_pa,roll=Inf] %>% select(-Year) # remove duplicate Year variable
    
    # now re-add the HF data variables
    hf_before <- left_join(pairs_before, ter_hf, by=c("Site", "HFYear"="Year"))
    # Convert to long-wise df
    hf_before <- hf_before %>% gather(., key="DistType", value="Area", 4:ncol(hf_before))
    hf_before <- rename(hf_before, "BeforeYear"="HFYear", "BeforeArea"="Area")
    
    #  now do the second rolling join
    # which disturbance-year occurs AFTER each veg year?
    pairs_after <- hf[vascplant_pa,roll=-Inf] %>% select(-Year)
    
    # add back hf data variables, convert to long-wise, and rename variables
    hf_after <- left_join(pairs_after, ter_hf, by=c("Site", "HFYear"="Year"))
    hf_after <- hf_after %>% gather(., key="DistType", value="Area", 4:ncol(hf_after))
    hf_after <- rename(hf_after, "AfterYear"="HFYear", "AfterArea"="Area")
    
    # now COMBINE the before and after data sets to prepare for interpolating
    # recall that "VegYear" has not changed - this is the target year for which veg has been sampled and for which we ideally want the hf data
    hf_beforeafter <- full_join(hf_before, hf_after, by=c("Site","VegYear", "DistType")) %>% select(Site,VegYear,DistType, everything())
    
    # interpolate Areas between Before and After years
    # if the years match, then that is the best
    # if either BeforeYear or AfterYear is missing, just use the other one - dont' interpolate
    hf_beforeafter <- hf_beforeafter %>%
      rowwise() %>% 
      mutate(InterpArea=
               if(is.na(BeforeYear) & is.na(AfterYear)){
                 #If there's no disturbance data.
                 return(NA) } 
             else if((is.na(BeforeYear) | is.na(AfterYear)) | BeforeYear==AfterYear){  
               #If the before or after years are the same, or if one of them is missing...
               return(max(c(BeforeArea,AfterArea), na.rm=TRUE))} 
             else {  
               #If there are two disturbance measurements taken on different dates. 
               approx(x=c(BeforeYear,AfterYear), 
                      y=c(BeforeArea,AfterArea), 
                      xout=VegYear)$y }) 
    
    # keep only interp area
    hf_beforeafter_ter <- hf_beforeafter %>% select(Site,"Year"=VegYear, DistType,"Area_km2"="InterpArea")
    hf_beforeafter_ter %>% filter(!is.na(Area_km2)) %>% distinct(Site,Year) %>% dim() # note that hf data and years can be missing if site is not in boreal region and therefore didnt have HF supplied
    
    unique(hf_beforeafter_ter$Site) # keep -ABMI- at first
  }
  
  # Boreal Wetland Sites
  {
    # first load HF datasets
    wet_hf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/Boreal HF 250 m buffer/Wetland-ABMI-all-sites-HF-250mBuffer-BorealNR.csv")
    
    # average HF for sites with replicate measures (i.e. HF measured on different transects?)
    # extract proper site names
    wet_hf <- wet_hf %>% 
      # mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
      # mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
      # mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>% 
      # mutate(Site = str_replace(Site, pattern="-CITSCI-", replacement = "-")) %>% 
      # mutate(Site = str_replace(Site, pattern="-SRD-", replacement = "-")) %>% 
      mutate(Site = str_remove(Site, pattern="W"))
  
    # consolidate the FEATURE_TYs into DistTypes
    # Area is now in % (*******I THINK******); convert to km2
    wet_hf <- wet_hf %>% 
      gather(FEATURE_TY,Shape_Area,3:ncol(wet_hf)) %>% 
      mutate(Area=Shape_Area*(pi*0.25^2)) %>% 
      select(-Shape_Area) %>% 
      spread(key=FEATURE_TY, value=Area)
  
    # we will use a rolling join to identify the nearest years for which HF data was collected PRIOR TO and FOLLOWING veg collection
    # vascular plants - wetland sites 
    {
      setwd("/Users/cari/Desktop/Waterloo/ABMI Data/Wetland data")
      wet_vascplant_pa <- read.csv("A_W05_Vascular_Plants.csv", row.names = NULL)
      
      # clean plant data set
      wet_vascplant_pa <- wet_vascplant_pa %>% select(Site=ABMI.Site,
                                                      Year=Year) %>% distinct()
      # wet_vascplant_pa <- wet_vascplant_pa %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
      #   mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
      #   mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>% 
      #   mutate(Site = str_replace(Site, pattern="-CITSCI-", replacement = "-")) %>% 
      #   mutate(Site = str_replace(Site, pattern="-SRD-", replacement = "-")) %>% distinct()
      
      head(wet_vascplant_pa)
      wet_vascplant_pa %>% summarize(NSites = length(unique(Site))) # 1044 wetland sites with plant p/a
  }
    
    # first we have to create DTs with just the ID variables
    # remove data columns from HF
    hf <- select(ungroup(wet_hf), Site, Year)
    
    head(hf)
    
    # make HF a data table and specify grouping variables
    setDT(hf)
    setkey(hf)
    # Add a duplicate Year variable so it doesn't get lost when joining DTs
    hf$HFYear  <- hf$Year # HFYear is the year for which we have HF data
    
    # We have already removed the data columns from the veg df. 
    # Create a duplicate Year variable, convert to data.table, and set grouping variables
    setDT(wet_vascplant_pa)
    setkey(wet_vascplant_pa)
    wet_vascplant_pa$VegYear <- wet_vascplant_pa$Year #VegYear is the year for which we have veg data and for which we want HF data
    
    # now do the first rolling join - 
    # which disturbance-year occurs BEFORE each veg year?
    pairs_before <- hf[wet_vascplant_pa,roll=Inf] %>% select(-Year) # remove duplicate Year variable
    
    # now re-add the HF data variables
    hf_before <- left_join(pairs_before, wet_hf, by=c("Site", "HFYear"="Year"))
    # Convert to long-wise df
    hf_before <- hf_before %>% gather(., key="DistType", value="Area", 4:ncol(hf_before))
    hf_before <- rename(hf_before, "BeforeYear"="HFYear", "BeforeArea"="Area")
    
    #  now do the second rolling join
    # which disturbance-year occurs AFTER each veg year?
    pairs_after <- hf[wet_vascplant_pa,roll=-Inf] %>% select(-Year)
    
    # add back hf data variables, convert to long-wise, and rename variables
    hf_after <- left_join(pairs_after, wet_hf, by=c("Site", "HFYear"="Year"))
    hf_after <- hf_after %>% gather(., key="DistType", value="Area", 4:ncol(hf_after))
    hf_after <- rename(hf_after, "AfterYear"="HFYear", "AfterArea"="Area")
    
    # now COMBINE the before and after data sets to prepare for interpolating
    # recall that "VegYear" has not changed - this is the target year for which veg has been sampled and for which we ideally want the hf data
    dim(hf_before)
    head(hf_after)
    
    hf_beforeafter <- full_join(hf_before, hf_after, by=c("Site","VegYear", "DistType")) %>% select(Site,VegYear,DistType, everything())
    
    # interpolate Areas between Before and After years
    # if the years match, then that is the best
    # if either BeforeYear or AfterYear is missing, just use the other one - dont' interpolate
    hf_beforeafter <- hf_beforeafter %>%
      rowwise() %>% 
      mutate(InterpArea=
               if(is.na(BeforeYear) & is.na(AfterYear)){
                 #If there's no disturbance data.
                 return(NA) } 
             else if((is.na(BeforeYear) | is.na(AfterYear)) | BeforeYear==AfterYear){  
               #If the before or after years are the same, or if one of them is missing...
               return(max(c(BeforeArea,AfterArea), na.rm=TRUE))} 
             else {  
               #If there are two disturbance measurements taken on different dates. 
               approx(x=c(BeforeYear,AfterYear), 
                      y=c(BeforeArea,AfterArea), 
                      xout=VegYear)$y }) 
    
    # keep only interp area
    hf_beforeafter_wet <- hf_beforeafter %>% select(Site,"Year"=VegYear, DistType,"Area_km2"="InterpArea")
    hf_beforeafter_wet %>% filter(!is.na(Area_km2)) %>% distinct(Site,Year) %>% dim() # note that hf data and years can be missing if site is not in boreal region and therefore didnt have HF supplied
    
    unique(hf_beforeafter_wet$Site) # keep -ABMI- at first
  }
  
  # combine terrestrial and wetland interpolated hf dfs
  {  
    head(hf_beforeafter_ter)
    hf_beforeafter_ter$Protocol <- "Terrestrial"
    tail(hf_beforeafter_wet)
    hf_beforeafter_wet$Protocol <- "Wetland"
    
    hf_interp <- bind_rows(hf_beforeafter_ter, hf_beforeafter_wet)
    
    # now remove the -ABMI- in site IDs
    hf_interp <- hf_interp %>% 
      mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-CITSCI-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
    
    # check that site IDs exclue -ABMI-
    unique(hf_interp$Site) %>% tail(100)
    
    # in order to join with NR, must also temporarily exclude trailing "-1's" 
    hf_interp$tmpSite <- hf_interp$Site
    hf_interp <- hf_interp %>% 
      mutate(tmpSite = str_remove(string=tmpSite, pattern="-1$")) %>% 
      mutate(tmpSite = str_remove(string=tmpSite, pattern="-2$")) %>% 
      mutate(tmpSite = str_remove(string=tmpSite, pattern="-21$"))
    
    # check that trailing #s were removed
    hf_interp %>% distinct(tmpSite) %>% filter(str_detect(tmpSite, "OG-")) %>% data.frame()
    
  }
  
  # keep only boreal sites; must join by the tmpSite ID which doesnt have trailing #s
    # load boreal sites
    {
      boreal_sites <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/Site IDs/Terrestrial_Wetland_Sites_all_NRs.csv") %>% 
        filter(NRNAME=="Boreal") %>% select(Protocol, Site=ABMI.Site) %>% distinct()
      head(boreal_sites)
      boreal_sites <- boreal_sites %>% 
        mutate(Site=str_replace(string=Site, pattern="OGW-", replacement = "OG-")) %>% 
        mutate(Site=str_remove(string=Site, pattern="W-"))
      
      unique(boreal_sites$Site) # site IDs exclude "W-", "-ABMI-", and trailing "-1"
    }

  # filter HF data based on boreal wetland sites; join based on tmpSite in hf_interp, but then remove tmpSite column
    boreal_wetland_hf <- inner_join(hf_interp, boreal_sites, by=c("Protocol", "tmpSite"="Site")) %>% filter(!is.na(Area_km2)) %>% select(-tmpSite)
    dim(boreal_wetland_hf)  
    boreal_wetland_hf %>% distinct(Site,Year) %>% nrow() #808 boreal sites  
    }

# now combine with rest of alberta HF data
{
  boreal_wetland_hf <- boreal_wetland_hf %>% select(Protocol, Site, Year, FEATURE_TY=DistType, Area_km2)
  boreal_wetland_hf$FEATURE_TY <- tolower(boreal_wetland_hf$FEATURE_TY)
  head(boreal_wetland_hf)
  hf_nonboreal <- hf_nonboreal %>% select(Protocol, Site, Year, FEATURE_TY, Area_km2)
  hf_nonboreal$FEATURE_TY <- tolower(hf_nonboreal$FEATURE_TY)
  head(hf_nonboreal)

}

# combine HF dfs and add in HF Category
{
hf_alb <- bind_rows(hf_nonboreal, boreal_wetland_hf)

hf_groupingvars <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/HF grouping vars.csv")
hf_groupingvars$FEATURE_TY <- tolower(hf_groupingvars$FEATURE_TY)
hf_alb <- left_join(hf_alb, hf_groupingvars, by="FEATURE_TY")

head(hf_alb)
hf_alb %>% dim()
hf_alb %>% distinct() %>% dim()

unique(hf_alb$Site) # again, site IDs exclude "W-" and "-ABMI-" but INCLUDE trailing #s (eg. -1)

}

# keep only wetland sites; HF files include both wetland and non-wetland ABMI sites, use wetland classification to keep only wetland ABMI sites
{
  # load and clean wetland classification - terrestrial sites
  {
    siteclass <- read.csv("/Users/cari/Desktop/Waterloo/ABMI Data/Terrestrial data/A_T01C_Site_Capability.csv")
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
    
    wetlandclassification <- wetlandclassification %>% ungroup() %>%      
      mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-CITSCI-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
    
    unique(wetlandclassification$Site)
    head(wetlandclassification)
    
    # how many repeated site classifications are there
    wetlandclassification <- wetlandclassification %>% group_by(Site, WetlandType) %>% summarize(meanCover=mean(Cover))
    wetlandclassification %>% mutate(NWetlandClasses=length(WetlandType)) %>% filter(NWetlandClasses>1) %>% arrange(desc(NWetlandClasses))
    
    # note: many sites have multiple ecosite classifications; must take the DOMINANT classification
    wetlandclassification <- wetlandclassification %>% group_by(Site) %>% filter(meanCover==max(meanCover)) %>% select(Site, WetlandType)
    head(wetlandclassification)
    # we still have 29 sites w/ 50:50 split classifications
    wetlandclassification %>% mutate(NWetlandClasses=length(WetlandType)) %>% filter(NWetlandClasses>1) %>% select(Site) %>% unique()
    
    # amend manually
    {
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
      wetlandclassification[wetlandclassification$Site=="OG-653-1","WetlandType"]  <- "Swamp"
      
      wetlandclassification[wetlandclassification$Site=="625","WetlandType"] <- "Bog"
      
      # Fen x Marsh >> marsh
      wetlandclassification[wetlandclassification$Site=="1393","WetlandType"] <- "Marsh"
      wetlandclassification[wetlandclassification$Site==791,"WetlandType"] <- "Marsh"
      wetlandclassification[wetlandclassification$Site=="OG-1239-1","WetlandType"] <- "Marsh"
      
      # Bog x Marsh >> marsh
      wetlandclassification[wetlandclassification$Site=="OG-1175-1","WetlandType"] <- "Marsh"
      
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
      wetlandclassification[wetlandclassification$Site=="OG-571-21","WetlandType"] <- "Poor Fen"
      wetlandclassification[wetlandclassification$Site=="OG-510-21","WetlandType"] <- "Poor Fen"
      
      # Fen x WetMeandow
      wetlandclassification[wetlandclassification$Site==63,"WetlandType"] <- "Wet Meadow"
      
      # Bog x Swamp 
      wetlandclassification[wetlandclassification$Site=="120","WetlandType"] <- "Swamp"
    }    
      
      # delete repeated Site classificaitons
      wetlandclassification <- wetlandclassification %>% distinct(Site, WetlandType)
      
      # check to make sure each site has only 1 wetland classification
      wetlandclassification %>% select(Site) %>% unique() %>% nrow() # 602 unique sites
      wetlandclassification %>% select(Site, WetlandType) %>% unique() %>% nrow() # 602 unique site x wetlandtypes
      wetlandclassification$Protocol <- "Terrestrial"
   
}
  
  # load and clean wetland classification - wetland sites
  {
    siteclass_wet <- read.csv("/Users/cari/Desktop/Waterloo/ABMI Data/Wetland data/A_W02B_Site_Capability.csv")
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
    unique(wetlandclassification_wetland$Site)
    
    wetlandclassification_wetland <- wetlandclassification_wetland %>% ungroup() %>%
      mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>%
      mutate(Site = str_replace(Site, pattern="OGW-", replacement = "OG-")) 
    
    
    # MANY sites have repeated site classifications; 
    wetlandclassification_wetland %>% group_by(Site,WetlandType) %>% tally() %>% filter(n>1) %>% arrange(desc(n))
    
    # note: many sites have multiple ecosite classifications; must take the DOMINANT classification ie the one which occurs most often
    wetlandclassification_wetland <- wetlandclassification_wetland %>% 
      group_by(Site,WetlandType) %>% 
      tally() %>% ungroup() %>% group_by(Site) %>% filter(n==max(n)) %>% select(Site,WetlandType)
    
    # we still have 38 sites w/ double split classifications; ammend manually
    wetlandclassification_wetland %>% group_by(Site) %>% tally() %>% filter(n>1) %>% arrange(desc(n)) %>% select(Site) %>% data.frame()
    
    {    
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="207","WetlandType"] <- "Swamp"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="210","WetlandType"] <- "Swamp"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="296","WetlandType"] <- "Rich Fen"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="390","WetlandType"] <- "Poor Fen"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="595","WetlandType"] <- "Marsh"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="924","WetlandType"] <- "Wet Meadow"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="OG-628-21","WetlandType"] <- "Rich Fen"
      
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
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="OG-511-21","WetlandType"] <- "Marsh"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="826","WetlandType"] <- "Marsh"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="956","WetlandType"] <- "Marsh"
      
      # bog + marsh >> "bog"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="154","WetlandType"] <- "Bog"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="20","WetlandType"] <- "Bog"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="362","WetlandType"] <- "Bog"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="416","WetlandType"] <- "Bog"
      wetlandclassification_wetland[wetlandclassification_wetland$Site=="OG-469-1","WetlandType"] <- "Bog"
      
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
  } 
    
    # delete repeated Site classificaitons
    wetlandclassification_wetland <- wetlandclassification_wetland %>% distinct()
    
    # check to make sure each site has only 1 wetland classification
    wetlandclassification_wetland %>% select(Site) %>% unique() %>% nrow() # 868 unique sites
    wetlandclassification_wetland %>% select(Site, WetlandType) %>% unique() %>% nrow() # 868 unique site x wetlandtypes
    wetlandclassification_wetland$Protocol <- "Wetland"
  }
  
  # combine wetland classifications
  wetlandclassification_all <- bind_rows(wetlandclassification, wetlandclassification_wetland)
  head(wetlandclassification_all)

  # add wetland classification to hf df
  head(hf_alb)
  hf_alb %>% distinct(Protocol, Site) %>% dim() # 1969 sites w/ HF data
  wetlandclassification_all %>% distinct(Protocol, Site) %>% dim() # 1470 wetlands
  left_join(hf_alb, wetlandclassification_all, by=c("Protocol", "Site")) %>% 
    filter(!is.na(WetlandType)) %>% 
    distinct(Protocol, Site) %>% dim() # 1253 wetlands with HF data
  
  # many sites classified as wetland do not have HF data; why not?
  anti_join(wetlandclassification_all, hf_alb, by=c("Protocol", "Site")) %>% group_by(Protocol, WetlandType) %>% tally() 
  anti_join(wetlandclassification_all, hf_alb, by=c("Protocol", "Site"))
  hf_alb %>% filter(str_detect(Site, "122")) %>% distinct(Protocol, Site, Year)
  
  hf_alb_wetlands <- left_join(hf_alb, wetlandclassification_all, by=c("Protocol", "Site")) 
  tmp <- filter(hf_alb_wetlands, is.na(WetlandType) & Protocol=="Wetland") # wetland sites w/o classification are SOWWs
  tmp$WetlandType <- "Shallow Lake"
  hf_alb_wetlands <- hf_alb_wetlands %>% filter(!is.na(WetlandType)) # excludes wetland and terrestrial sites without classification

  hf_alb_wetlands <- bind_rows(hf_alb_wetlands,tmp) # adds SOWWs back to dataset
  head(hf_alb_wetlands)
  unique(hf_alb_wetlands$Site) # site IDs exclude "W-" and "-ABMI-" but include trailing #s
}

# add NR names to df
{
  nr <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/Site IDs/Terrestrial_Wetland_Sites_all_NRs.csv")
  nr %>% distinct(Protocol, ABMI.Site) %>% tail(50) %>% data.frame() # note: must exclude the "W-" in wetland sites
  nr$ABMI.Site <- nr$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")
  
  unique(nr$ABMI.Site) # NR excludes "W-", "-ABMI-" AND trailing #s; 
  # must creating temporary matching ID which excludes trailing #s to match with nr
  hf_alb_wetlands$tmpSiteMatch <- paste(str_extract(string=hf_alb_wetlands$Site, pattern="OG-"), str_extract(string=hf_alb_wetlands$Site, pattern="\\d+"), sep="") 
  hf_alb_wetlands$tmpSiteMatch <- hf_alb_wetlands$tmpSiteMatch %>% str_remove(pattern="NA")
  unique(hf_alb_wetlands$tmpSiteMatch)
  
  # join nr and hf dfs based on matching site, then remove that colum
  hf_alb_wetlands <- left_join(hf_alb_wetlands, 
                               select(nr, Protocol, ABMI.Site, NSRNAME, NRNAME), 
                               by=c("Protocol", "tmpSiteMatch"="ABMI.Site")) %>% 
    select(-tmpSiteMatch) %>% distinct()
  
  hf_alb_wetlands %>% filter(is.na(NRNAME)) %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) # 70 sites w/o NR and do not have coordinates
  hf_alb_wetlands <- hf_alb_wetlands  %>% filter(!is.na(NRNAME))
  hf_alb_wetlands %>% distinct(Protocol, Site, Year, WetlandType, NRNAME) %>% group_by(Protocol) %>% tally()
  hf_alb_wetlands %>% distinct(Protocol, Site, Year, WetlandType, NRNAME) %>% group_by(WetlandType) %>% tally()
  hf_alb_wetlands %>% distinct(Protocol, Site, Year, WetlandType, NRNAME) %>% group_by(NRNAME) %>% tally()

}
dim(hf_alb_wetlands)
head(hf_alb_wetlands)


# convert area to precent; organize order of columns
plotarea <- pi*0.25^2
plotarea
hf_alb_wetlands <- hf_alb_wetlands %>% rowwise() %>% 
  mutate(Area_percent = 100*Area_km2/plotarea) %>% 
  select(NRNAME, NSRNAME, Protocol, WetlandType, Site, Year, HFCategory, FEATURE_TY, Area_percent) 
}

# make veg and HF data sets compatible; add in sites with 0 hf from veg df ####
{
  vegsites <- select(veg_pa, -Species, -PA) %>% distinct()
  hfsites <- select(hf_alb_wetlands, -FEATURE_TY, -Area_percent, -HFCategory, -NSRNAME) %>% distinct()
  bothsites <- inner_join(vegsites, hfsites) %>% distinct()
  
  dim(vegsites) # 2054
  dim(hfsites) # 1866
  dim(bothsites) # 1711
  
  anti_join(vegsites, hfsites) %>% distinct() %>% group_by(Protocol) %>% tally() # 343 veg sites that don't have HF data
  missingsites <- anti_join(vegsites, hfsites) %>% distinct()
  dim(missingsites) 
  anti_join(hfsites, vegsites) %>% distinct() %>% group_by(Protocol) %>% tally() # 155 hf sites that don't have veg data - why not?
  
  hf_alb_wetlands %>% 
    group_by(Protocol, Site, Year) %>% 
    summarize(totdist=sum(Area_percent)) %>% 
    group_by(Protocol) %>% top_n(n=-3, wt=totdist) # note: terrestrial sites need to have 0s added back in!
  
  # sum all features into HF cats
  hf_alb_wetlands <- hf_alb_wetlands %>% 
    group_by(NRNAME, Protocol, WetlandType, Site, Year, HFCategory) %>% 
    summarize(Area_percent=sum(Area_percent)) 
  
  hf_alb_wetlands %>% distinct(NRNAME, Protocol, WetlandType, Site, Year) %>% dim() # 1866
  
  # add in 0% hf for sites with veg data but missing hf data
  hf_alb_wetlands <- left_join(vegsites, hf_alb_wetlands, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year")) 
  hf_alb_wetlands <- hf_alb_wetlands %>% replace_na(list(HFCategory="Agriculture")) # add placeholder HF Category name
  hf_alb_wetlands %>% distinct(NRNAME, Protocol, WetlandType, Site, Year) %>% dim() # 2054
  
  hf_alb_wetlands <- hf_alb_wetlands %>% 
    spread(key=HFCategory, value=Area_percent) %>% 
    gather(key=HFCategory, value=Area_percent, 6:ncol(.)) %>%
    replace_na(list(Area_percent=0))
  hf_alb_wetlands %>% distinct(NRNAME, Protocol, WetlandType, Site, Year) %>% dim() # 2054
  
  # add extra ID column for martin's matching, and re-order columns
  hf_alb_wetlands <- hf_alb_wetlands %>% 
    mutate(WSite=ifelse(Protocol=="Wetland", paste("W",Site,sep=""), Site)) %>% 
    mutate(WSite=str_replace(WSite, "WOG", "OGW")) 
  hf_alb_wetlands <- hf_alb_wetlands %>% select(NRNAME, Protocol, WetlandType, WSite, Site, Year, HFCategory, Area_percent)
  veg_pa <- veg_pa %>% 
    mutate(WSite=ifelse(Protocol=="Wetland", paste("W",Site,sep=""), Site)) %>% 
    mutate(WSite=str_replace(WSite, "WOG", "OGW")) 
  veg_pa <- veg_pa %>% select(NRNAME, Protocol, WetlandType, WSite, Site, Year, Species, PA)


# want 0 non-overlapping rows for both anti-joins
anti_join(select(hf_alb_wetlands, NRNAME, Protocol, WetlandType, WSite, Site, Year), 
          select(veg_pa, NRNAME, Protocol, WetlandType, WSite, Site, Year)) 
anti_join(select(veg_pa, NRNAME, Protocol, WetlandType, WSite, Site, Year), 
          select(hf_alb_wetlands, NRNAME, Protocol, WetlandType, WSite, Site, Year)) 

# export
## veg
# write.csv(x=veg_pa, file="/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned.csv", row.names = F)
# hf
## write.csv(x=hf_alb_wetlands, file="/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/Alb wetlands HF.csv", row.names=F)
}

# add in lat/long coordinates ####
{
# create temp site ID
# NOTE: removing some site ID info - is this valid?

siteids <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/Site IDs/Terrestrial_Wetland_Sites_all_NRs.csv")
veg_pa <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned.csv")

siteids <- siteids %>% 
  select(-Year) %>% 
  distinct() %>% 
  mutate(ABMI.Site=str_replace(ABMI.Site, "W-", "W")) %>% 
  mutate(ABMI.Site=str_replace(ABMI.Site, "OGW", "OGW-"))

veg_pa <- veg_pa %>% 
  mutate(tmpSite = str_remove(WSite, "B"))

vegog <- veg_pa %>%   
  filter(str_detect(WSite, "OG")) %>% 
  mutate(tmpSite = str_remove(WSite, "-1$")) %>%
  mutate(tmpSite = str_remove(tmpSite, "-21$")) %>%
  mutate(tmpSite = str_remove(tmpSite, "-2$")) %>%
  mutate(tmpSite = str_remove(tmpSite, "-3$")) %>%
  mutate(tmpSite = str_remove(tmpSite, "-4$")) %>%
  mutate(tmpSite = str_remove(tmpSite, "-5$")) %>%
  mutate(tmpSite = str_remove(tmpSite, "-6$"))

veg_pa <- bind_rows(filter(veg_pa, str_detect(WSite, "OG")==FALSE),
                 vegog)
veg_pa <- left_join(veg_pa,
          select(siteids, NRNAME, Protocol, ABMI.Site, Latitude, Longitude),
          by=c("NRNAME", "Protocol", "tmpSite"="ABMI.Site")) %>% 
  select(-tmpSite) %>% 
  select(Latitude, Longitude, NRNAME, Protocol, WetlandType, WSite, Site, Year, Species, PA)

veg_pa %>% distinct(Latitude, Longitude, Protocol, Site, WSite) %>%  
  group_by(Latitude, Longitude) %>% 
  tally() %>% 
  filter(n>1) %>% 
  arrange(desc(n)) %>% 
  head() %>% 
  View()

veg_pa %>% distinct(Latitude, Longitude, Protocol, Site, WSite, Year) %>% 
  filter(Latitude<55 &
           Latitude>54.976) # note 3-6 diff sites must be assigned same coords
# write.csv(x=veg_pa,
#           file="data/cleaned/ABMI veg cleaned_latlong.csv",
#           row.names = F)

hf <- read.csv("data/cleaned/Alb wetlands HF.csv")
hf <- left_join(hf,
          distinct(veg_pa, Protocol, Site, Year, Latitude, Longitude)) %>% 
  select(Latitude, Longitude, everything())
# write.csv(x = hf,
#           file="data/cleaned/Alb wetlands HF_latlong.csv",
#           row.names = F)
}

# combine all files into final dfs ####
{
  # df with richness, CSI, and prop exotic species 
  { 
    # plant data ####
    veg_pa <- read.csv("data/cleaned/ABMI veg cleaned_latlong.csv")
    
    # select only 1 sampling event for each site
    {
      # median sampling year
      median(veg_pa$Year) # 2013
      # select sampling event closest to 2013 for each site
      
      sitestokeep <- veg_pa %>% 
        mutate(Years_from_med = abs(Year-2013)) %>% # create column w/ num of yrs from 2013
        group_by(Protocol, Site) %>% 
        slice_min(order_by=Years_from_med, with_ties=F) %>% # select the lowest Years_from_med value for each group
        select(Protocol, Site, Year) # 
      # note: there were still a few sites that were classified as diff wetland types depending on yr; this has removed them
      sitestokeep$uniquesite <- paste(sitestokeep$Protocol, sitestokeep$Site, sitestokeep$Year, sep="_")
      
      veg_pa$tmpuniqueid <- paste(veg_pa$Protocol, veg_pa$Site, veg_pa$Year, sep="_")
      veg_pa <- veg_pa %>% 
        filter(tmpuniqueid %in% sitestokeep$uniquesite) %>% 
        select(-tmpuniqueid) 
      
      # check 
      veg_pa %>% 
        distinct(Protocol, WetlandType, Site, Year) %>% 
        group_by(Protocol, Site) %>% 
        tally() %>% 
        arrange(desc(n))
      
      # export for Martin's updated SSI calculates
      # write.csv(veg_pa,
      #           file="data/cleaned/ABMI veg cleaned latlong - 1 sample per site.csv",
      #           row.names = F)
    }
    
    # calculate chao estimated richness####
    {
      # make separate dfs of terrestrial and wetland sites; create new UniqueID with Site_Year
      sites_ter <- veg_pa %>% filter(Protocol=="Terrestrial") %>% 
        select(Site, Year) %>% 
        mutate(UniqueID = paste(Site, Year, sep="_"))
      sites_wet <- veg_pa %>% filter(Protocol=="Wetland")%>% 
        select(Site, Year) %>% 
        mutate(UniqueID = paste(Site, Year, sep="_"))
      
      # terrestrial protocol sites
      {
        #load and prep data on raw veg presence-absence per plot within sites
        {
          ter_vascplant_pa <- read.csv("/Users/carif/Dropbox/Desktop/Waterloo/ABMI data/Terrestrial data/A_T15_Vascular_Plants.csv", 
                                       row.names = NULL, 
                                       na.strings = c("NONE", "VNA", "SNI", "DNC", "SNR"))
          names(ter_vascplant_pa) <- colnames(ter_vascplant_pa[,2:ncol(ter_vascplant_pa)])
          ter_vascplant_pa <- ter_vascplant_pa[2:ncol(ter_vascplant_pa)-1]
          ter_vascplant_pa <- ter_vascplant_pa %>% 
            select(Site=ABMI.Site, Year, Quadrant, Species = Scientific.Name) %>% 
            mutate(UniqueID = paste(Site, Year, sep="_")) %>% # create new unique ID
            select(-Site, -Year) %>% # remove columns captured in UniqueID
            select(UniqueID, everything()) %>% # rearrange for clarity
            filter(!is.na(Species)) # exclude missing species
          
          # ter_vascplant_pa %>%  # was "Year" mislabeled in some sites?
          #   filter(ABMI.Site=="693" & Year == "2013" & Quadrant=="NE" & Scientific.Name=="Abies balsamea")
          # 
          # ter_vascplant_pa %>% 
          #   filter(Field.Date=="10-Jul-17") %>% # sites with this field date were marked as sampled in as 2013 or 2017; which is wrong??
          #   distinct(ABMI.Site, Year)
          # # let's just move forward assuming "Year" is correct. There is a mistake somewhere but Im opting to move forward with brute force   
          
          ter_vascplant_pa <- ter_vascplant_pa %>% 
            mutate(PA=1) %>% 
            group_by(UniqueID, Quadrant) %>% 
            distinct() %>%  # this is my brute force fix for the errors with unmatching "Year" vs "Field Date"
            spread(., key=Species, value=PA) %>% # spread out into site x sp matrix
            gather(., key=Species, value=PA, 3:ncol(.)) %>% # regather to replace NAs with 0 (=absent)
            replace_na(., list(PA=0)) %>% 
            spread(., Species, PA) # now respread for site x sp matrix including both PA=1 and PA=0
          
          # UGHHH remove -ABMI- etc from interior of sites names
          ter_vascplant_pa <- ter_vascplant_pa %>% 
            mutate(UniqueID = str_replace(UniqueID, "-DH-", "-"),
                   UniqueID = str_replace(UniqueID, "-ABMI-", "-"),
                   UniqueID = str_replace(UniqueID, "-ALPAC-", "-"),
                   UniqueID = str_replace(UniqueID, "-SRD-", "-"))
          
          # keep only sites we used in full analyses
          sites_ter %>% head() # this is the list of sites
          ter_vascplant_pa <- ter_vascplant_pa %>% 
            filter(UniqueID %in% sites_ter$UniqueID)
          
        }
        
        # estimate true diversity
        div.out.ter <- vegan::specpool(ter_vascplant_pa[3:ncol(ter_vascplant_pa)],pool=ter_vascplant_pa$UniqueID)
        div.out.ter$UniqueID <- rownames(div.out.ter)
        }
      
      # wetland protocol sites
      {
        # load and prep data
        {
          wet_vascplant_pa <- read.csv("/Users/carif/Dropbox/Desktop/Waterloo/ABMI data/Wetland data/A_W05_Vascular_Plants.csv", 
                                       row.names = NULL, 
                                       na.strings = c("NONE", "VNA", "SNI", "DNC", "SNR")) %>% 
            select(Site=ABMI.Site, Year, Old.Zone, New.Zone, Transect, Species = Scientific.Name) %>% 
            mutate(UniqueID = paste(Site, Year, sep="_")) %>% # create new unique ID
            select(-Site, -Year) %>% # remove columns captured in UniqueID
            select(UniqueID, everything()) # rearrange for clarity
          
          wet_vascplant_pa <- wet_vascplant_pa %>% filter(!is.na(Species)) # exclude sp with missing IDs
          # what are replicates? transects? each transect crosses multiple zones; replicate sp recorded in diff zones
          wet_vascplant_pa %>% 
            distinct(UniqueID, Transect) %>% 
            group_by(UniqueID) %>% 
            tally() #  num of transects per site/year
          wet_vascplant_pa <- wet_vascplant_pa %>% 
            mutate(PA=1) %>% 
            group_by(UniqueID, Transect) %>% 
            spread(., key=Species, value=PA) %>%  # spread out into site x sp matrix
            gather(., key=Species, value=PA, 5:ncol(.)) %>% # regather to replace NAs with 0 (=absent)
            replace_na(., list(PA=0)) %>% 
            spread(., Species, PA) # now respread for site x sp matrix including both PA=1 and PA=0
          
          # UGHHH remove -ABMI- etc from interior of sites names
          wet_vascplant_pa <- wet_vascplant_pa %>% 
            mutate(UniqueID = str_replace(UniqueID, "-DH-", "-"),
                   UniqueID = str_replace(UniqueID, "-ABMI-", "-"),
                   UniqueID = str_replace(UniqueID, "-ALPAC-", "-"),
                   UniqueID = str_replace(UniqueID, "-SRD-", "-")) 
          
          # keep only sites we used in full analyses
          head(sites_wet) # these are the sites to keep
          wet_vascplant_pa <- wet_vascplant_pa %>% 
            filter(UniqueID %in% sites_wet$UniqueID) 
          
          # how many transects per site
          wet_vascplant_pa %>% 
            distinct(UniqueID, Transect) %>% 
            group_by(UniqueID) %>% 
            tally() %>%
            filter(n==5) %>% nrow()
          
        }
        
        # estimates of true diversity
        div.out <- vegan::specpool(wet_vascplant_pa[5:ncol(wet_vascplant_pa)],pool=wet_vascplant_pa$UniqueID)
        div.out$UniqueID <- rownames(div.out)
      }
      
      # create df of true richness for terrestrial and wetland protocols
      div.out$Protocol <- "Wetland"
      div.out.ter$Protocol <- "Terrestrial"
      truerich <- bind_rows(div.out, div.out.ter) 
      truerich$UniqueID <- paste(truerich$Protocol, truerich$UniqueID, sep="_")
      
    }
    
    # compare underestimation for wetland and terrestrial protocol sites
    # this chunk has extra code to quantify and visualize the difference between true and measured richness
    {
      # terrestrial protocol
      median(div.out.ter$Species)  # median observed richness
      median(div.out.ter$chao-div.out.ter$chao.se) # median lower bound for true richness
      
      # compute the average % of underestimation
      div.out.ter %>% 
        ungroup() %>% 
        summarize(percent_underestimated = 100*((chao - Species)/chao))  %>% 
        summarize(mean_under = mean(percent_underestimated),
                  se_under = sd(percent_underestimated)/sqrt(length(percent_underestimated)))
      
      # compute the average num of missing sp
      div.out.ter %>% 
        ungroup() %>% 
        summarize(num_underestimated = chao - Species)  %>% 
        summarize(mean_under = mean(num_underestimated),
                  se_under = sd(num_underestimated)/sqrt(length(num_underestimated)))
      
      mean(div.out.ter$chao.se)
      ter.chao <- ggplot(div.out.ter) +
        geom_point(aes(x=reorder(as.factor(UniqueID), Species, min), y=chao), color="red", alpha=0.5) +
        geom_linerange(aes(x = reorder(as.factor(UniqueID), Species, min), 
                           ymin=chao-chao.se,
                           ymax=chao+chao.se), color="red", alpha=0.5) +
        # geom_point(aes(x=reorder(as.factor(UniqueID), Species, min), y=Species)) +
        geom_line(aes(x=reorder(as.factor(UniqueID), Species, min), 
                      y=Species, group=1)) +
        labs(x="Terrestrial Protocol Sites", y="Species (Num.)") +
        ggtitle("Extrapolated Richness (Chao)") +
        theme(axis.text.x = element_blank()) +
        lims(y=c(0,150))
      
      # wetland protocol
      median(div.out$Species) # median observed richness
      median(div.out$chao-div.out$chao.se) # median lower bound for true richness
      
      # compute the average % of underestimation
      div.out %>% 
        ungroup() %>% 
        summarize(percent_underestimated = 100*((chao - Species)/chao))  %>% 
        summarize(mean_under = mean(percent_underestimated),
                  se_under = sd(percent_underestimated)/sqrt(length(percent_underestimated)))
      
      # compute the average num of missing sp
      div.out %>% 
        ungroup() %>% 
        summarize(num_underestimated = chao - Species)  %>% 
        summarize(mean_under = mean(num_underestimated),
                  se_under = sd(num_underestimated)/sqrt(length(num_underestimated)))
      
      wet.chao <- ggplot(div.out) +
        geom_point(aes(x=reorder(as.factor(UniqueID), Species, min), y=chao), color="red", alpha=0.5) +
        geom_linerange(aes(x = reorder(as.factor(UniqueID), Species, min), 
                           ymin=chao-chao.se,
                           ymax=chao+chao.se), color="red", alpha=0.5) +
        # geom_point(aes(x=reorder(as.factor(UniqueID), Species, min), y=Species)) +
        geom_line(aes(x=reorder(as.factor(UniqueID), Species, min), 
                      y=Species, group=1)) +
        labs(x="Wetland Protocol Sites", y="Species (Num.)") +
        ggtitle("Extrapolated Richness (Chao)") +
        theme(axis.text.x = element_blank()) +
        lims(y=c(0,100))
      
      
      # compare underestimation for two protocols
      ter_underestimation <- div.out.ter %>% 
        ungroup() %>% 
        summarize(
          num_sp_underestimated = chao - Species,
          percent_underestimated = 100*((chao - Species)/chao))
      mean(ter_underestimation$num_sp_underestimated) # average num of missed species
      mean(ter_underestimation$percent_underestimated) # average percent of missed species
      
      wet_underestimation <- div.out %>% 
        ungroup() %>% 
        summarize(
          num_sp_underestimated = chao - Species,
          percent_underestimated = 100*((chao - Species)/chao))
      mean(wet_underestimation$num_sp_underestimated) # average num of missed species
      mean(wet_underestimation$percent_underestimated) # average percent of missed species
      
      # significant difference in the number of missing species 
      t.test(ter_underestimation$num_sp_underestimated, 
             wet_underestimation$num_sp_underestimated)
      
      # significant difference in the % of underestimation
      t.test(ter_underestimation$percent_underestimated, 
             wet_underestimation$percent_underestimated)
    }
    
    # load HF data ####
    hf <- read.csv("data/cleaned/Alb wetlands HF_latlong.csv") 
    # calculate total disturbance ####
    hf_tot <- hf %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(totdist_percent=sum(Area_percent))
    
    # SSI from 1000 randomizations excluding 170 rare sp (<= 3 occurrences)
    {
      sp_SSI <- read.csv("data/cleaned/ssi_mean_one rotation.csv", sep=",") %>% 
        select(-X) # first column is just row nums and can be deleted
      colnames(sp_SSI) <- c("Species", "CV")
      
      # 37 species have poor correlation among randomization runs
      outliers <- read.csv("data/cleaned/species_high_range_SSI_one rotation.csv")
      sp_SSI_no_outliers <- sp_SSI %>% filter(Species %in% outliers$SCIENTIFIC_NAME == F)
      # sp_SSI <- sp_SSI_no_outliers
    }
    
    # calculate CSI : mean CV of each community (also compare the summed CV of each community) ####
    {
      veg_CSI_HF <- left_join(veg_pa, sp_SSI)
      veg_CSI_HF <- veg_CSI_HF %>% 
        group_by(Protocol,NRNAME, WetlandType,Site,Year) %>% 
        summarize(CSI=mean(CV, na.rm = T)) # 211 sites have na value for at least 1 sp
      
      veg_CSI_HF <- left_join(veg_CSI_HF,hf_tot, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year")) 
      veg_CSI_HF$UniqueID <- paste(veg_CSI_HF$Protocol, veg_CSI_HF$Site, sep="_")
      
    }
    
    # make sp richness df
    {
     spR <- veg_pa %>% 
        filter(Species %in% sp_SSI$Species) %>% # keep only non-rare species
        group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% 
        summarize(rich=sum(PA))
      spR <- inner_join(spR, hf_tot, by=c("Latitude", "Longitude", "Protocol", "NRNAME", "WetlandType", "Site", "Year"))
      spR$UniqueID <- paste(spR$Protocol, spR$Site, sep="_")
      
    }
    
    # exotic species
    {
      exotics <- read.csv("data/cleaned/exotic_plants_ab.csv", sep=";")
      veg_exot <- left_join(veg_pa, exotics, by=c("Species"="SPECIES")) %>% select(-TYPE)
      veg_exot <- veg_exot %>% 
        filter(Species %in% sp_SSI$Species) %>% # keep only non-rare species
        group_by(Latitude, Longitude, Protocol, Site,Year, ORIGIN) %>% 
        tally() %>% 
        spread(key=ORIGIN, value=n) %>% 
        replace_na(list(Exotic=0, Native=0,`Unknown/Undetermined`=0)) %>% 
        rowwise() %>% 
        mutate(rich=sum(Exotic, Native, `Unknown/Undetermined`),
               propexotic=100*(Exotic/rich))
      
      veg_exot <- left_join(veg_CSI_HF, 
                            veg_exot, 
                            by=c("Latitude", "Longitude", "Protocol", "Site", "Year")) %>% 
        select(-Exotic, -Native, -`Unknown/Undetermined`)
    }
    
    veg_df <- left_join(spR, veg_CSI_HF,
              by=c("Protocol", "NRNAME", "Latitude", "Longitude", "WetlandType", "Site", "Year", "UniqueID", "totdist_percent"))
    veg_df <- left_join(veg_df, veg_exot,
              by=c("Protocol", "NRNAME", "Latitude", "Longitude", "WetlandType", "Site", "Year", "UniqueID", "totdist_percent", "rich", "CSI"))
    
    # add estimated (i.e. true richness) to df
    {
      head(veg_df)
      # rename "rich" column to "rich_observed"
      colnames(veg_df)[8] <- "rich_observed"
      
      head(truerich)
      veg_df$tmpUniqueID <- paste(veg_df$UniqueID, veg_df$Year, sep="_") # create unique ID that is protocol_site_year to match that of truerich
      veg_df <- left_join(veg_df,
                select(truerich, UniqueID, "rich_chao" = chao),
                by=c("tmpUniqueID" = "UniqueID")) %>% 
        select(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year, rich_chao, rich_observed, totdist_percent, CSI, propexotic)
       
    }
    
    # write.csv(veg_df,
    #           file="data/cleaned/veg_rich_CSI_exot.csv", row.names = F)
  }
  
  # prep df for NMDS with HD levels based on hd %; calculate distance measures ####
  {
    # create veg PA df with only sp used in SSI calculations 
    veg_hfa <- veg_pa %>% 
      filter(Species %in% sp_SSI$Species) %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
    # spread site x sp matrix and replace NAs with 0s for absent species 
    veg_hfa <- veg_hfa %>% 
      spread(key=Species, value=PA) %>% 
      gather(key=Species, value=PA, 8:ncol(.)) %>% 
      replace_na(list(PA=0)) %>% 
      spread(key=Species, value=PA) 
    
    # add tot disturbance to df
    veg_hfa <- left_join(veg_hfa, 
                         hf_tot,  
                         by=c("Latitude","Longitude", "NRNAME", "Protocol", "WetlandType", "Site", "Year"))
    
    # veg_hf_2groups has most & least dist communities
    veg_hf_2groups <- veg_hfa %>% 
      filter(totdist_percent==0 | totdist_percent>=90) %>% # keep only sites with HF == 0% or HF>= 90%
      mutate(HFbin = ifelse(totdist_percent==0, "Low", "High")) %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything()) %>% 
      select(-totdist_percent)
    # veg_hf_2groups %>% group_by(HFbin) %>% tally()# n=125 sites in high bin; n=435 sites in low bin
    
    # exclude species that were only detected 1x
    sptokeep2 <- colSums(veg_hf_2groups[,9:ncol(veg_hf_2groups)]) %>% data.frame() 
    sptokeep2$Species <- rownames(sptokeep2)
    colnames(sptokeep2) <- c("Obs", "Species")
    sptokeep2 <-  filter(sptokeep2, Obs > 1)
    veg_hf_2groups <- veg_hf_2groups %>% 
      gather(Species, PA, 9:ncol(.)) %>% 
      filter(Species %in% sptokeep2$Species) %>% # keep only species that were detected >1x
      spread(Species, PA)
    
    # veg_hf_3groups has most, least, and intermed dist communities
    veg_hf_3groups <- veg_hfa %>% filter(totdist_percent==0 | totdist_percent>=90 | totdist_percent>=45 & totdist_percent<=55)  %>% 
      mutate(HFbin = ifelse(totdist_percent==0, "Low", ifelse(totdist_percent>=90, "High", "Int."))) %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything()) %>% 
      select(-totdist_percent)
    
    # veg_hf_3groups %>% group_by(HFbin) %>% tally() # n= 125 sites in high grp; n=53 in intermediate; n=435 in low grp
    
    sptokeep3 <- colSums(veg_hf_3groups[,9:ncol(veg_hf_3groups)]) %>% data.frame() 
    sptokeep3$Species <- rownames(sptokeep3)
    colnames(sptokeep3) <- c("Obs", "Species")
    sptokeep3 <-  filter(sptokeep3, Obs > 1)
    veg_hf_3groups <- veg_hf_3groups %>% 
      gather(Species, PA, 9:ncol(.)) %>% 
      filter(Species %in% sptokeep3$Species) %>% 
      spread(Species, PA)

    # export files
    # write.csv(veg_hf_2groups,
    #           file="data/cleaned/ordination data/veg site x sp mat - 2 HF groups.csv", row.names = F)
    # write.csv(veg_hf_3groups,
    #           file="data/cleaned/ordination data/veg site x sp mat - 3 HF groups.csv", row.names = F)

  }
}
