# This file runs the stats

library(tidyverse); library(cowplot); library(ggrepel)
library(lme4)
library(ape)
library(lmerTest)

rm(list=ls())

# prep CSI, species richness, and exotic species data frames ####
{
  # plant data ####
  veg_pa <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned_latlong.csv")
  
  # load HF data ####
  hf <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/Alb wetlands HF_latlong.csv") 
  # calculate total disturbance ####
  hf_tot <- hf %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(totdist_percent=sum(Area_percent))
  
  # SSI from 1000 randomizations
  {
    sp_SSI <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ssi_final.csv", sep=";")
    colnames(sp_SSI) <- c("Species", "CV")
  }
  
  # calculate CSI : mean CV of each community (also compare the summed CV of each community) ####
  {
    veg_CSI_HF <- left_join(veg_pa, sp_SSI)
    veg_CSI_HF <- veg_CSI_HF %>% 
      group_by(Protocol,NRNAME, WetlandType,Site,Year) %>% 
      summarize(CSI=mean(CV, na.rm = T)) # 32 sites have na value for at least 1 sp
    
    veg_CSI_HF <- left_join(veg_CSI_HF,hf_tot, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year")) 
    veg_CSI_HF$UniqueID <- paste(veg_CSI_HF$Protocol, veg_CSI_HF$Site, sep="_")
    
  }
  
  # make sp richness df
  {
    spR <- veg_pa %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% 
      summarize(rich=sum(PA))
    spR <- inner_join(spR, hf_tot, by=c("Latitude", "Longitude", "Protocol", "NRNAME", "WetlandType", "Site", "Year"))
    spR$UniqueID <- paste(spR$Protocol, spR$Site, sep="_")
    
  }
  
  # exotic species
  {
    exotics <- read.csv("data/cleaned/exotic_plants_ab.csv", sep=";")
    veg_exot <- left_join(veg_pa, exotics, by=c("Species"="SPECIES")) %>% select(-TYPE)
    veg_exot <- veg_exot %>% 
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
  
}

# prep data frames with 2 or 3 bins; calc distance matrices
{
  library(vegan)
  veg_hf <- veg_pa %>% 
    select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
  veg_hf <- veg_hf %>% 
    spread(key=Species, value=PA) %>% 
    gather(key=Species, value=PA, 8:ncol(.)) %>% 
    replace_na(list(PA=0)) %>% 
    spread(key=Species, value=PA) 
  hf_bin <- hf_tot
  hf_bin$HFbin <- ntile(hf_bin$totdist_percent, n=10) 
  
  # disturbance levels of each bin
  hf_bin %>% group_by(as.factor(HFbin)) %>% summarize(meandist=mean(totdist_percent),
                                                      meddist=median(totdist_percent))
  veg_hf <- left_join(veg_hf, 
                      select(hf_bin, -totdist_percent), 
                      by=c("Latitude","Longitude", "NRNAME", "Protocol", "WetlandType", "Site", "Year"))
  
  
  # veg_hf2 has most & least dist communities
  veg_hf2 <- veg_hf %>% filter(HFbin==1 | HFbin==10) %>% 
    select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
  sptokeep2 <- colSums(veg_hf2[,9:ncol(veg_hf2)]) %>% data.frame() 
  sptokeep2$Species <- rownames(sptokeep2)
  colnames(sptokeep2) <- c("Obs", "Species")
  sptokeep2 <-  filter(sptokeep2, Obs > 1)
  veg_hf2 <- veg_hf2 %>% 
    gather(Species, PA, 9:ncol(.)) %>% 
    filter(Species %in% sptokeep2$Species) %>% 
    spread(Species, PA)
  
  # veg_hf3 has most, least, and intermed dist communities
  veg_hf3 <- veg_hf %>% filter(HFbin==1 | HFbin==8 | HFbin==10) %>% 
    select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
  sptokeep3 <- colSums(veg_hf3[,9:ncol(veg_hf3)]) %>% data.frame() 
  sptokeep3$Species <- rownames(sptokeep3)
  colnames(sptokeep3) <- c("Obs", "Species")
  sptokeep3 <-  filter(sptokeep3, Obs > 1)
  veg_hf3 <- veg_hf3 %>% 
    gather(Species, PA, 9:ncol(.)) %>% 
    filter(Species %in% sptokeep3$Species) %>% 
    spread(Species, PA)
  
  veg_d2 <- vegdist(veg_hf2[,9:ncol(veg_hf2)], method="jaccard", binary=T)
  veg_d3 <- vegdist(veg_hf3[,9:ncol(veg_hf3)], method="jaccard", binary=T)
  
}

# 0. test for spatial autocorrelation - Moran's I
{
  # richness w/ ape::Moran.I
  {
    head(spR)
    # create inverse distance matrix
    rich.d <- as.matrix(dist(cbind(spR$Longitude, spR$Latitude)))
    rich.d.inv <- 1/rich.d
    diag(rich.d.inv) <- 0
    rich.d.inv[1:5, 1:5] # inf values have same coords
    rich.d.inv[is.infinite(rich.d.inv)] <- 0
    
    Moran.I(spR$rich, rich.d.inv) # yes spatial autocorr
  }
  
  # CSI
  {
    head(veg_CSI_HF)
    csi.d <- as.matrix(dist(cbind(veg_CSI_HF$Longitude, veg_CSI_HF$Latitude)))
    csi.d.inv <- 1/csi.d
    diag(csi.d.inv) <- 0
    csi.d.inv[1:5, 1:5] # inf values have same coords
    csi.d.inv[is.infinite(csi.d.inv)] <- 0
    
    Moran.I(veg_CSI_HF$CSI, csi.d.inv) # yes spatial autocorr
    
  }

}

# 1. how does sp richness vary across HF gradient
{
  spR$Year <- as.factor(spR$Year)
  # should HF be linear or quad? and does ran ef variance differ from zero?
  rich.linear <- lmer(rich ~ totdist_percent + 
                        Protocol + 
                        (1|Year) + (1|UniqueID), 
                      data=spR, REML=F)
  rich.poly <- lmer(rich ~ poly(totdist_percent,2) + 
                      Protocol + 
                      (1|Year) + (1|UniqueID), 
                    data=spR, REML=F)
  summary(rich.linear) # RE of unique site ID should be kept
  summary(rich.poly) # RE of unique site ID should be kept
  AIC(rich.linear, rich.poly) # poly is better
  
  piecewiseSEM::rsquared(rich.poly) # best model
  
  Moran.I(residuals(rich.poly), rich.d.inv) # I = 0.011
  
  # old analyses - ignore
  {
    # anova(lme(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
    #           random=~1|UniqueID,
    #           data=spR), type="marginal")
    # 
    # rich.poly1 <- lmer(rich ~ poly(totdist_percent,2) + 
    #                      Protocol + 
    #                      Latitude + Longitude +
    #                      (1|Year) + (1|UniqueID), 
    #                    data=spR, REML=F)
    # rich.poly2 <- lmer(rich ~ poly(totdist_percent,2) + 
    #                      Protocol +
    #                      (1|Year) + (1|UniqueID), 
    #                    data=spR, REML=F)
    # rich.poly3 <- lmer(rich ~ poly(totdist_percent,2) + 
    #                      (1|Year) + (1|UniqueID), 
    #                    data=spR, REML=F)
    # 
    # AIC(rich.poly, rich.poly1, rich.poly2, rich.poly3)  
    # summary(rich.poly2) # latitudinal climate variables not important for richness
    # piecewiseSEM::rsquared(rich.poly2)
    # 
    # Moran.I(residuals(rich.poly2), rich.d.inv)
  }

}

# 2. How does CSI vary with HF
{
  csi.linear <- lmer(CSI ~ totdist_percent + 
                       Protocol + 
                       (1|Year) + (1|UniqueID), 
                     data=veg_CSI_HF,
                     REML = F)
  csi.poly <- lmer(CSI ~ poly(totdist_percent,2) + 
                     Protocol + 
                     (1|Year) + (1|UniqueID), 
                   data=veg_CSI_HF,
                   REML = F)
  summary(csi.linear) # variance on group RE indistinguishable from zero
  summary(csi.poly) # variance on group RE indistinguishable from zero
  AIC(csi.linear, csi.poly) # csi poly
  
  piecewiseSEM::rsquared(csi.poly) # best model
  
  Moran.I(residuals(csi.poly), rich.d.inv) # I = 0.006
  
  # old analyses - ignore
  {
    # library(nlme)
    # anova(lme(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
    #           random=~1|UniqueID,
    #           data=veg_CSI_HF), type="marginal")
    # 
    # csi.poly1 <- lmer(CSI ~ poly(totdist_percent,2) + 
    #                     Protocol + 
    #                     Latitude + Longitude +
    #                     (1|Year) + (1|UniqueID), 
    #                   data=veg_CSI_HF,
    #                   REML = F)
    # csi.poly2 <- lmer(CSI ~ poly(totdist_percent,2) + 
    #                     Protocol +
    #                     (1|Year) + (1|UniqueID), 
    #                   data=veg_CSI_HF,
    #                   REML = F)
    # csi.poly3 <- lmer(CSI ~ poly(totdist_percent,2) +
    #                     (1|Year) + (1|UniqueID), 
    #                   data=veg_CSI_HF,
    #                   REML = F)
    # 
    # AIC(csi.poly, csi.poly1, csi.poly2, csi.poly3)
    # 
    # summary(csi.poly1) # lat/long coords are important
    # piecewiseSEM::rsquared(csi.poly1)
    
  }
  

}

# 3. Does including % exotics improve fit of richness model (from 1 above)
{
  rich.poly # previous best
  # refit previous best w/ updated df that includes exotic
  rich.polya <- lmer(rich ~ poly(totdist_percent,2) + 
                        Protocol +
                        (1|Year) + (1|UniqueID), 
                      data=veg_exot,
                      REML=F)
  rich.exot.only <- lmer(rich ~ propexotic + 
                           Protocol +
                           (1|Year) + (1|UniqueID), 
                         data=veg_exot,
                         REML=F)
  rich.poly.exot <- lmer(rich ~ poly(totdist_percent,2) + 
                           propexotic + 
                           Protocol +
                           (1|Year) + (1|UniqueID), 
                         data=veg_exot,
                         REML=F)
  rich.poly.exot.interaction <- lmer(rich ~ poly(totdist_percent,2) * propexotic + 
                                       Protocol +
                                       (1|Year) + (1|UniqueID), 
                                     data=veg_exot,
                                     REML=F)
  
  AIC(rich.polya, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  anova(rich.polya, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  summary(rich.poly.exot.interaction)
  piecewiseSEM::rsquared(rich.polya)

  piecewiseSEM::rsquared(rich.poly.exot.interaction) # best model
  
  Moran.I(residuals(rich.poly.exot.interaction), rich.d.inv) # I=0.003
}

# 4. does including % exotics improve fit of CSI model (from 2 above)
{
  csi.poly # previous best
  # refit previous best w/ updated df
  csi.polya <- lmer(CSI ~ poly(totdist_percent,2) + 
                       Protocol + 
                       # Latitude + Longitude +
                       (1|Year) + (1|UniqueID), 
                     data=veg_exot,
                     REML=F)
  csi.exot.only <- lmer(CSI ~ propexotic +
                          Protocol +
                          # Latitude + Longitude +
                          (1|Year) + (1|UniqueID), 
                        data=veg_exot,
                        REML=F)
  csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + 
                          propexotic + 
                          Protocol + 
                          # Latitude + Longitude +
                          (1|Year) + (1|UniqueID), 
                        data=veg_exot,
                        REML=F)
  csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + 
                                      Protocol + 
                                      # Latitude + Longitude +
                                      (1|Year) + (1|UniqueID), 
                                    data=veg_exot,
                                    REML=F)
  
  AIC(csi.polya, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  anova(csi.polya, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  summary(csi.poly.exot.interaction)
  
  piecewiseSEM::rsquared(csi.poly1a)
  
  piecewiseSEM::rsquared(csi.poly.exot.interaction) # best model
  anova(csi.poly.exot.interaction, type="chisq")  
  
  Moran.I(residuals(csi.poly.exot.interaction), rich.d.inv) # I=0.006 
}

# 5. permanovas of multivariate NMDS's
{
  # permanova
  {
    # check for sig diffs among 2 groups
    mrpp(veg_d2, grouping=as.factor(veg_hf2$HFbin)) # sig diff between groups based on mean score
    
    adonis2(veg_d2 ~ as.factor(veg_hf2$HFbin) + as.factor(veg_hf2$Protocol),
            by="margin") # both significant
    
    # check for sig diffs among 3 groups
    library(RVAideMemoire)
    library(ecodist)
    
    adonis2(veg_d3 ~ as.factor(veg_hf3$HFbin) + as.factor(veg_hf3$Protocol),
            by="margin") # both significant
    
    pairwise.perm.manova(resp=veg_d3, fact=veg_hf3$HFbin, p.method="holm")
    mrpp(veg_d3, grouping=as.factor(veg_hf3$HFbin)) # sig diff between groups based on mean score
    
  }
}

# 6. comparison of median disturbance across low, med, high bins
{
  
  # disturbance levels of each bin
  hf_bin %>% 
    group_by(as.factor(HFbin)) %>% 
    summarize(meandist=mean(totdist_percent),
              meddist=median(totdist_percent))
  
  hf_bin2 <- hf_bin %>% filter(HFbin==1 | HFbin==10)
  hf_bin2$UniqueID <- paste(hf_bin2$Protocol, hf_bin2$Site, sep="_")
  summary(lmer(totdist_percent ~ as.factor(HFbin) + 
         Protocol + (1|Year) + (1|UniqueID), 
       data=hf_bin2, REML=F)) # sig effect of HF bin
  
  hf_bin3 <- hf_bin %>% filter(HFbin==1 | HFbin==8 | HFbin==10)
  hf_bin3$UniqueID <- paste(hf_bin3$Protocol, hf_bin3$Site, sep="_")
  hf_bin3$HFbin <- recode(hf_bin3$HFbin, "1"="Low", "8"="Int.", "10"="High")
  summary(lmer(totdist_percent ~ HFbin + 
                 Protocol + (1|Year) + (1|UniqueID), 
               data=hf_bin3, REML=F)) # both bins 8 and 1 differ from 1
  
  ggplot(hf_bin3, aes(x=HFbin, y=totdist_percent)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Human Development (%)")
  
  
}

# 7. comparison of median prop exotic across low/med/high bins
{
  exot_bin <- left_join(select(ungroup(hf_bin),Protocol, WetlandType, Year, Site, HFbin ), 
                        veg_exot, 
                        by=c("Protocol", "WetlandType", "Year", "Site"))
  
  exot_bin %>% 
    group_by(as.factor(HFbin)) %>% 
    summarize(medexot=median(propexotic),
              IQRexot=IQR(propexotic))
  
  
  # exot_bin2 <- exot_bin %>% filter(HFbin==1 | HFbin==10)
  # exot_bin2$UniqueID <- paste(exot_bin2$Protocol, exot_bin2$Site, sep="_")
  
  
  exot_bin3 <- exot_bin %>% filter(HFbin==1 | HFbin==8 | HFbin==10)
  exot_bin3$UniqueID <- paste(exot_bin3$Protocol, exot_bin3$Site, sep="_")
  exot_bin3$HFbin <- recode(exot_bin3$HFbin, "1"="Low", "8"="Int.", "10"="High")
  exot_bin3$HFbin <- factor(exot_bin3$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
}
