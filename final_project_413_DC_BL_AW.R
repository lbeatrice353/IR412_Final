#########################################################################################
# Title: Final Project
# Project: IR 413
# By: Domenic Camicia, Bebe Lin, Aren Wright
# Original script: 04/19/2023
# Last modified: 04/19/2023 by Aren Wright
# Based on "Regression Analysis - Complex Interdependence" by Yeiyoung Choo
#########################################################################################

#setting working directory, loading packages
setwd("/Users/aren.wright/Library/CloudStorage/GoogleDrive-arencwright@gmail.com/My Drive/USC (Spring 2023)/IR 413/IR412_413_shared_F22/")
library(tidyverse)
library(ggplot2)
library(broom)
library(dotwhisker)
library(texreg)

#loading data
merged1 <- readRDS("final_project_materials_413/COPY_master_complexinterdep_04132023.rds")

#variables to identify dyads with at least 1 rich country  
merged1 <- merged1 %>% mutate(rich = ifelse(gdppc_rep_GDP >4000 | gdppc_par_GDP > 4000, 1, 0))
# and dyads with both sides rich 
merged1 <- merged1 %>% mutate(rich2= ifelse(gdppc_rep_GDP >4000 & gdppc_par_GDP > 4000, 1, 0))
# and dyads with at least 1 poor country  
merged1 <- merged1 %>% mutate(poor = ifelse(gdppc_rep_GDP <1050 | gdppc_par_GDP <1050, 1, 0))

##DV - war 
#control for dyads with at least 1 rich country 
a1 <- glm(war_lead_MID ~ tradeshare_less_BACI, data=merged1, family=binomial(link="probit"))
a2 <- glm(war_lead_MID ~ tradeshare_less_BACI + jointEU_MEM + common_peg_IRK + cow_defense, data=merged1, family = binomial(link="probit"))
# using gdppc>40000 for rich countries 
a3 <- glm(war_lead_MID ~ tradeshare_less_BACI + jointEU_MEM + common_peg_IRK + cow_defense + rich, data=merged1,family = binomial(link="probit")) 
a4 <- glm(war_lead_MID ~ tradeshare_less_BACI + jointEU_MEM + common_peg_IRK + cow_defense + rich + gdpdis_GDP, data=merged1,family = binomial(link="probit")) 
a5 <- glm(war_lead_MID ~ tradeshare_less_BACI + jointEU_MEM + common_peg_IRK + cow_defense + rich + gdpdis_GDP+ exports_CK + contiguity + distcap_CEPII, data=merged1,family = binomial(link="probit")) 
a6 <- glm(war_lead_MID ~ tradeshare_less_BACI + jointEU_MEM + common_peg_IRK + cow_defense + rich + gdpdis_GDP+ exports_CK + contiguity + distcap_CEPII+ colony_CEPII + comcol_CEPII, data=merged1,family = binomial(link="probit")) 

htmlreg(list(a1, a2, a3, a4, a5, a6),
        file = "final_project_materials_413/final_project_tables.html",
        custom.coef.names = c("Intercept", "Trade Share", "Joint EU Membership", "Common Peg",
                              "Joint Alliance", "Rich Country", "GDP Dissimilarity", "Export Similarity",
                              "Contiguity", "Distance", "Colonial Tie", "Common Colonizer"),
        reorder.coef = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1))