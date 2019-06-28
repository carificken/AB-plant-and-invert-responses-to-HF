# This file runs the stats

library(tidyverse); library(cowplot); library(ggrepel)
library(lme4)

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

# 0. test for spatial autocorrelation
{
  
}

# 1. how does sp richness vary across HF gradient
{
  # should HF be linear or quad? and does ran ef variance differ from zero?
  rich.linear <- lmer(rich ~ totdist_percent + Protocol + Latitude + Longitude +  Year +
                        (1|UniqueID), 
                      data=spR, REML=F)
  rich.poly <- lmer(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +  Year +
                      (1|UniqueID), 
                    data=spR, REML=F)
  summary(rich.linear) # RE of unique site ID should be kept
  summary(rich.poly) # RE of unique site ID should be kept
  AIC(rich.linear, rich.poly) # poly is better
  
  # anova(lme(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
  #           random=~1|UniqueID,
  #           data=spR), type="marginal")
  
  rich.poly1 <- lmer(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +
                       (1|UniqueID), 
                     data=spR, REML=F)
  rich.poly2 <- lmer(rich ~ poly(totdist_percent,2) + Protocol +
                       (1|UniqueID), 
                     data=spR, REML=F)
  rich.poly3 <- lmer(rich ~ poly(totdist_percent,2) + 
                       (1|UniqueID), 
                     data=spR, REML=F)
  
  AIC(rich.poly, rich.poly1, rich.poly2, rich.poly3)  
  summary(rich.poly2) # latitudinal climate variables not important for richness
  piecewiseSEM::rsquared(rich.poly2)
}

# 2. How does CSI vary with HF
{
  csi.linear <- lmer(CSI ~ totdist_percent + Protocol + Latitude + Longitude + Year +
                       (1|UniqueID), 
                     data=veg_CSI_HF,
                     REML = F)
  csi.poly <- lmer(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year +
                     (1|UniqueID), 
                   data=veg_CSI_HF,
                   REML = F)
  summary(csi.linear) # variance on group RE indistinguishable from zero
  summary(csi.poly) # variance on group RE indistinguishable from zero
  AIC(csi.linear, csi.poly) # csi poly
  
  # library(nlme)
  # anova(lme(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
  #           random=~1|UniqueID,
  #           data=veg_CSI_HF), type="marginal")
  
  csi.poly1 <- lmer(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +
                      (1|UniqueID), 
                    data=veg_CSI_HF,
                    REML = F)
  csi.poly2 <- lmer(CSI ~ poly(totdist_percent,2) + Protocol +
                      (1|UniqueID), 
                    data=veg_CSI_HF,
                    REML = F)
  csi.poly3 <- lmer(CSI ~ poly(totdist_percent,2) +
                      (1|UniqueID), 
                    data=veg_CSI_HF,
                    REML = F)
  
  AIC(csi.poly, csi.poly1, csi.poly2, csi.poly3)
  
  summary(csi.poly1)
  piecewiseSEM::rsquared(csi.poly1)
  
}

# 3. Does including % exotics improve fit of richness model (from 1 above)
{
  rich.poly2 # previous best
  # refit previous best w/ updated df
  rich.poly2a <- lmer(rich ~ poly(totdist_percent,2) + Protocol +
                        (1|UniqueID),
                      data=veg_exot,
                      REML=F)
  rich.exot.only <- lmer(rich ~ propexotic + Protocol +
                           (1|UniqueID),
                         data=veg_exot,
                         REML=F)
  rich.poly.exot <- lmer(rich ~ poly(totdist_percent,2) + propexotic + Protocol +
                           (1|UniqueID),
                         data=veg_exot,
                         REML=F)
  rich.poly.exot.interaction <- lmer(rich ~ poly(totdist_percent,2) * propexotic + Protocol +
                                       (1|UniqueID),
                                     data=veg_exot,
                                     REML=F)
  
  AIC(rich.poly2a, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  anova(rich.poly2a, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  summary(rich.poly.exot.interaction)
  piecewiseSEM::rsquared(rich.poly.exot.interaction)
  piecewiseSEM::rsquared(rich.poly2a)
  anova(rich.poly.exot.interaction, type="chisq")
}

# 4. does including % exotics improve fit of CSI model (from 2 above)
{
  csi.poly1 # previous best
  # refit previous best w/ updated df
  csi.poly1a <- lmer(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +
                       (1|UniqueID),
                     data=veg_exot,
                     REML=F)
  csi.exot.only <- lmer(CSI ~ propexotic + Protocol + Latitude + Longitude +
                          (1|UniqueID),
                        data=veg_exot,
                        REML=F)
  csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + propexotic + Protocol + Latitude + Longitude +
                          (1|UniqueID),
                        data=veg_exot,
                        REML=F)
  csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + Protocol + Latitude + Longitude +
                                      (1|UniqueID),
                                    data=veg_exot,
                                    REML=F)
  
  AIC(csi.poly1a, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  anova(csi.poly1a, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  summary(csi.poly.exot.interaction)
  piecewiseSEM::rsquared(csi.poly.exot.interaction)
  piecewiseSEM::rsquared(csi.poly1a)
  anova(csi.poly.exot.interaction, type="chisq")  
}

# 5. permanovas of multivariate NMDS's
{
  # prep ordination data
  {
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
    
    # permanova
    {
      # check for sig diffs among 2 groups
      mrpp(veg_d2, grouping=as.factor(veg_hf2$HFbin)) # sig diff between groups based on mean score
      adonis2(veg_d2 ~ as.factor(veg_hf2$HFbin))
      
      # check for sig diffs among 3 groups
      library(RVAideMemoire)
      library(ecodist)
      
      pairwise.perm.manova(resp=veg_d3, fact=veg_hf3$HFbin, p.method="holm")
      mrpp(veg_d3, grouping=as.factor(veg_hf3$HFbin)) # sig diff between groups based on mean score
      
    }
  }
}