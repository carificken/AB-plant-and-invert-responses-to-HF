# this file cleans existing ABMI vegetation data, HF data, and climate data for use in the AB invert + plant project
rm(list=ls())
library(tidyverse)

##### Vegetation Data Cleaning #####

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

head(veg_pa)
veg_pa <- veg_pa %>% select(NRNAME, Protocol, WetlandType, Site, Year, Species, PA)

# export
# write.csv(x=veg_pa, file="/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned.csv", row.names = F)


##### HF Data Cleaning #####

# HF data from ABMI excluding boreal
{
  terhf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/non-Boreal HF 250 m buffer/Terrestrial250HFData.csv") %>% 
    select("Site"="ABMI_Assigned_Site_ID", "Year"="Survey_Year", "FEATURE_TY", "Area_m2"="Shape_Area")
  terhf <- terhf %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-"))
  terhf$Protocol <- "Terrestrial"
  terhf$Area_km2 <- terhf$Area_m2/1000000
  terhf <- terhf %>% select(-Area_m2)
  head(terhf)
  
  wethf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/non-Boreal HF 250 m buffer/Wetland250HFData.csv") 
  wethf <- wethf %>% gather(key="FEATURE_TY", value="Area_percent", 7:ncol(wethf))
  wethf$Area_km2 <- wethf$Area_percent*(pi*0.25^2)
  wethf <- wethf  %>% select("Site"="ABMI_Site", "Year", "FEATURE_TY", "Area_km2")
  wethf <- wethf %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>% 
    mutate(Site = str_replace(Site, pattern="W", replacement = ""))
  wethf$Protocol <- "Wetland"
  wethf <- wethf %>% select(Site, Year, FEATURE_TY, Protocol, Area_km2)
  
  hf_nonboreal <- bind_rows(terhf, wethf)
  hf_nonboreal <- hf_nonboreal %>% filter(!is.na(FEATURE_TY)) # Feature_ty = NA assigned for undeveloped land in ter sites, exclude and then add back in
}

# HF data from ABMI boreal only
{
  # For every site-year of veg data, I want to have hf data
  # for MATCHING sites, the HF data were collected on the same years as the veg data
  # but for MISMATCHING sites, i have veg data collected on offset years; for these i need to take average of HF data from 2 closest years
  library(readxl); library(data.table)
  
  # Terrestrial Sites
  {
    # first load HF datasets
    ter_hf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/hf/Boreal HF 250 m buffer/Terrestrial - All_HF_buff250m_OfficialNR_BorealRegion_2003_2016.csv.csv")
    
    # average HF for sites with replicate measures (i.e. HF measured on different transects?)
    # extract proper site names
    ter_hf <- ter_hf %>% select(Site=ABMI_Assigned_Site_ID,Year=survey_year, FEATURE_TY, Shape_Area) # rename site to tmpsite
    # ter_hf <- ter_hf %>% mutate(Site = str_replace(Site, pattern="-ABMI-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-ALPAC-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-DH-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-CITSCI-", replacement = "-")) %>% 
    #   mutate(Site = str_replace(Site, pattern="-SRD-", replacement = "-"))
    
    # some sites appear to have duplicated and/or questionable HF data provided; delete them 
    ter_hf <- ter_hf %>% filter(Site!="OG-583-21") %>% droplevels()
    
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
    unique(vascplant_pa$Site)
    unique(ter_hf$Site)
    
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
    hf_beforeafter_ter %>% filter(!is.na(Area_km2)) %>% distinct(Site,Year) %>% dim()
    hf_beforeafter_ter$Protocol <- "Terrestrial"
    
    # note that hf data and years can be missing if site is not in boreal region and therefore didnt have HF supplied
  }
  
  # Wetland Sites
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
    hf_beforeafter_wet %>% filter(!is.na(Area_km2)) %>% distinct(Site,Year) %>% dim()
    # note that hf data and years can be missing if site is not in boreal region and therefore didnt have HF supplied
  }
  
  # combine terrestrial and wetland interpolated hf dfs
  {  
    head(hf_beforeafter_ter)
    hf_beforeafter_ter$Protocol <- "Terrestrial"
    tail(hf_beforeafter_wet)
    hf_beforeafter_wet$Protocol <- "Wetland"
    
    hf_interp <- bind_rows(hf_beforeafter_ter, hf_beforeafter_wet)
  }
  
  # keep only boreal sites
    # load boreal sites
    {
      boreal_sites <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/Site IDs/Terrestrial_Wetland_Sites_all_NRs.csv") %>% filter(NRNAME=="Boreal") %>% select(Protocol, Site=ABMI.Site) %>% distinct()
      head(boreal_sites)
      boreal_sites <- boreal_sites %>% 
        mutate(Site=str_replace(string=Site, pattern="OGW-", replacement = "OG-")) %>% 
        mutate(Site=str_remove(string=Site, pattern="W-"))
    }

  # filter HF data based on boreal wetland sites
    boreal_wetland_hf <- inner_join(boreal_sites, hf_interp, by=c("Protocol", "Site")) %>% filter(!is.na(Area_km2))
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

}

# keep only wetland sites
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
    
    # how many repeated site classifications are there
    wetlandclassification <- wetlandclassification %>% group_by(Site, WetlandType) %>% summarize(meanCover=mean(Cover))
    wetlandclassification %>% mutate(NWetlandClasses=length(WetlandType)) %>% filter(NWetlandClasses>1) %>% arrange(desc(NWetlandClasses))
    
    # note: many sites have multiple ecosite classifications; must take the DOMINANT classification
    wetlandclassification <- wetlandclassification %>% group_by(Site) %>% filter(meanCover==max(meanCover)) %>% select(Site, WetlandType)
    head(wetlandclassification)
    # we still have 29 sites w/ 50:50 split classifications
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
    wetlandclassification %>% select(Site) %>% unique() %>% nrow() # 609 unique sites
    wetlandclassification %>% select(Site, WetlandType) %>% unique() %>% nrow() # 609 unique site x wetlandtypes
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
    wetlandclassification_wetland %>% select(Site) %>% unique() %>% nrow() # 868 unique sites
    wetlandclassification_wetland %>% select(Site, WetlandType) %>% unique() %>% nrow() # 868 unique site x wetlandtypes
    wetlandclassification_wetland$Protocol <- "Wetland"
  }
  
  # combine wetland classifications
  wetlandclassification_all <- bind_rows(wetlandclassification, wetlandclassification_wetland)
  head(wetlandclassification_all)

  # add wetland classification to hf df
  head(hf_alb)
  hf_alb %>% distinct(Protocol, Site) %>% dim() #1849 sites w/ HF data
  wetlandclassification_all %>% distinct(Protocol, Site) %>% dim() # 1477 wetlands
  left_join(hf_alb, wetlandclassification_all, by=c("Protocol", "Site")) %>% 
    filter(!is.na(WetlandType)) %>% 
    distinct(Protocol, Site) %>% dim() # 1126 wetlands with HF data
  
  # combine wetland classifications
  wetlandclassification_all <- bind_rows(wetlandclassification, wetlandclassification_wetland)
  wetlandclassification_all %>% distinct(Site) %>% dim()
  
  hf_alb_wetlands <- left_join(hf_alb, wetlandclassification_all, by=c("Protocol", "Site")) 
  tmp <- filter(hf_alb_wetlands, is.na(WetlandType) & Protocol=="Wetland") # wetland sites w/o classification are SOWWs
  tmp$WetlandType <- "Shallow Lake"
  hf_alb_wetlands <- hf_alb_wetlands %>% filter(!is.na(WetlandType)) # excludes wetland and terrestrial sites without classification

  hf_alb_wetlands <- bind_rows(hf_alb_wetlands,tmp) # adds SOWWs back to dataset

}

# add NR names to df
{
  nr <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/Site IDs/Terrestrial_Wetland_Sites_all_NRs.csv")
  nr %>% distinct(Protocol, ABMI.Site) %>% tail(50) %>% data.frame() # note: must exclude the "W-" in wetland sites
  nr$ABMI.Site <- nr$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")
  
  head(nr)
  
  hf_alb_wetlands$tmpSiteMatch <- paste(str_extract(string=hf_alb_wetlands$Site, pattern="OG-"), str_extract(string=hf_alb_wetlands$Site, pattern="\\d+"), sep="") 
  hf_alb_wetlands$tmpSiteMatch <- hf_alb_wetlands$tmpSiteMatch %>% str_remove(pattern="NA")
  
  hf_alb_wetlands <- left_join(hf_alb_wetlands, select(nr, Protocol, ABMI.Site, NSRNAME, NRNAME), by=c("Protocol", "tmpSiteMatch"="ABMI.Site")) %>% select(-tmpSiteMatch) %>% distinct()
  
  hf_alb_wetlands %>% filter(is.na(NRNAME)) %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) #18 sites w/o NR; not listed on coordinates
  hf_alb_wetlands <- hf_alb_wetlands  %>% filter(!is.na(NRNAME))
  hf_alb_wetlands %>% distinct(Protocol, Site, Year, WetlandType, NRNAME) %>% group_by(Protocol) %>% tally()
  hf_alb_wetlands %>% distinct(Protocol, Site, Year, WetlandType, NRNAME) %>% group_by(WetlandType) %>% tally()
  hf_alb_wetlands %>% distinct(Protocol, Site, Year, WetlandType, NRNAME) %>% group_by(NRNAME) %>% tally()
  dim(hf_alb_wetlands)
}

# export
{
  setwd("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/")
  write.csv(x=hf_alb_wetlands, file="Alb wetlands HF.csv", row.names=F)
}