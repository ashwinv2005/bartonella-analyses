require(tidyverse)
require(Rcapture)
require(ggthemes)

a = read.csv("capture_data_combined.csv")

## Rattus satarae

sp1 = a %>% filter(species == "Rattus satarae", effort == "Grid")

sp1_kad_for_2016 = sp1 %>% filter(area == "Kadumane", habitat == "Forest", year == 2016)
sp1_kad_pla_2016 = sp1 %>% filter(area == "Kadumane", habitat == "Plantation", year == 2016)

sp1_fit_kad_for_2016 = closedp.0(sp1_kad_for_2016[,8:11])
sp1_fit_kad_for_2016CI = closedpCI.0(sp1_kad_for_2016[,8:11])

sp1_fit_kad_pla_2016 = closedp.0(sp1_kad_pla_2016[,8:10])
sp1_fit_kad_pla_2016CI = closedpCI.0(sp1_kad_pla_2016[,8:10])

sp1_kad_comb_2016 = sp1 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2016)

sp1_kad_gra_2016 = sp1 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2016)

sp1_fit_kad_comb_2016 = closedp.0(sp1_kad_comb_2016[,8:10])
sp1_fit_kad_comb_2016CI = closedpCI.0(sp1_kad_comb_2016[,8:10])

sp1_kad_for_2017 = sp1 %>% filter(area == "Kadumane", habitat == "Forest", year == 2017)
sp1_kad_pla_2017 = sp1 %>% filter(area == "Kadumane", habitat == "Plantation", year == 2017)

sp1_fit_kad_for_2017 = closedp.0(sp1_kad_for_2017[,8:11])
sp1_fit_kad_for_2017CI = closedpCI.0(sp1_kad_for_2017[,8:11])

sp1_fit_kad_pla_2017 = closedp.0(sp1_kad_pla_2017[,8:10])
sp1_fit_kad_pla_2017CI = closedpCI.0(sp1_kad_pla_2017[,8:10])

sp1_kad_comb_2017 = sp1 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2017)

sp1_kad_gra_2017 = sp1 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2017)

sp1_fit_kad_comb_2017 = closedp.0(sp1_kad_comb_2017[,8:10])
sp1_fit_kad_comb_2017CI = closedpCI.0(sp1_kad_comb_2017[,8:10])

sp1_kud_comb_2021 = sp1 %>% 
  filter(area == "Kudremukh", habitat %in% c("Forest","Plantation"), year == 2021)

sp1_kud_gra_2021 = sp1 %>% 
  filter(area == "Kudremukh", habitat %in% c("Grassland"), year == 2021)

sp1_fit_kud_comb_2021 = closedp.0(sp1_kud_comb_2021[,8:11])
sp1_fit_kud_comb_2021CI = closedpCI.0(sp1_kud_comb_2021[,8:11])

# 2018
sp1_kad_comb_2018 = sp1 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2018)
sp1_kad_gra_2018 = sp1 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2018)




## Mus cf. famulus

sp2 = a %>% filter(species == "Mus cf famulus", effort == "Grid")

sp2_kad_gra_2016 = sp2 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2016)

sp2_fit_kad_gra_2016 = closedp.0(sp2_kad_gra_2016[,8:11])
sp2_fit_kad_gra_2016CI = closedpCI.0(sp2_kad_gra_2016[,8:11])

sp2_kad_for_2016 = sp2 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2016)

sp2_kad_gra_2017 = sp2 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2017)

sp2_fit_kad_gra_2017 = closedp.0(sp2_kad_gra_2017[,8:11])
sp2_fit_kad_gra_2017CI = closedpCI.0(sp2_kad_gra_2017[,8:11])

sp2_kad_for_2017 = sp2 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2017)

sp2_kud_gra_2021 = sp2 %>% 
  filter(area == "Kudremukh", habitat %in% c("Grassland"), year == 2021)

sp2_fit_kud_gra_2021 = closedp.0(sp2_kud_gra_2021[,8:11])
sp2_fit_kud_gra_2021CI = closedpCI.0(sp2_kud_gra_2021[,8:11])

sp2_kud_for_2021 = sp2 %>% 
  filter(area == "Kudremukh", habitat %in% c("Forest","Plantation"), year == 2021)

#2018
sp2_kad_gra_2018 = sp2 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2018)

sp2_kad_for_2018 = sp2 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2018)







## Mus cf fernandoni

sp3 = a %>% filter(species == "Mus cf fernandoni", effort == "Grid")

sp3_kad_for_2016 = sp3 %>% filter(area == "Kadumane", habitat == "Forest", year == 2016)
sp3_kad_gra_2016 = sp3 %>% filter(area == "Kadumane", habitat == "Grassland", year == 2016)

sp3_fit_kad_for_2016 = closedp.0(sp3_kad_for_2016[,8:11])
sp3_fit_kad_for_2016CI = closedpCI.0(sp3_kad_for_2016[,8:11])

sp3_fit_kad_gra_2016 = closedp.0(sp3_kad_gra_2016[,8:11])
sp3_fit_kad_gra_2016CI = closedpCI.0(sp3_kad_gra_2016[,8:11])

sp3_kad_for_2017 = sp3 %>% filter(area == "Kadumane", habitat == "Forest", year == 2017)
sp3_kad_gra_2017 = sp3 %>% filter(area == "Kadumane", habitat == "Grassland", year == 2017)

sp3_fit_kad_for_2017 = closedp.0(sp3_kad_for_2017[,8:11])
sp3_fit_kad_for_2017CI = closedpCI.0(sp3_kad_for_2017[,8:11])

#sp3_fit_kad_gra_2017 = closedp.0(sp3_kad_gra_2017[,8:11])
#sp3_fit_kad_gra_2017CI = closedpCI.0(sp3_kad_gra_2017[,8:11])

sp3_kud_gra_2021 = sp3 %>% 
  filter(area == "Kudremukh", habitat %in% c("Grassland"), year == 2021)

sp3_fit_kud_gra_2021 = closedp.0(sp3_kud_gra_2021[,8:11])
sp3_fit_kud_gra_2021CI = closedpCI.0(sp3_kud_gra_2021[,8:11])

sp3_kud_for_2021 = sp3 %>% 
  filter(area == "Kudremukh", habitat %in% c("Forest"), year == 2021)

#2018
sp3_kad_for_2018 = sp3 %>% filter(area == "Kadumane", habitat == "Forest", year == 2018)
sp3_kad_gra_2018 = sp3 %>% filter(area == "Kadumane", habitat == "Grassland", year == 2018)







## Mus cf. terricolor

sp4 = a %>% filter(species == "Mus cf terricolor", effort == "Grid")

sp4_kad_gra_2016 = sp4 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2016)

sp4_fit_kad_gra_2016 = closedp.0(sp4_kad_gra_2016[,8:11])
sp4_fit_kad_gra_2016CI = closedpCI.0(sp4_kad_gra_2016[,8:11])

sp4_kad_for_2016 = sp4 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2016)

sp4_kad_gra_2017 = sp4 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2017)

sp4_fit_kad_gra_2017 = closedp.0(sp4_kad_gra_2017[,8:11])
sp4_fit_kad_gra_2017CI = closedpCI.0(sp4_kad_gra_2017[,8:11])

sp4_kad_for_2017 = sp4 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2017)

sp4_kud_gra_2021 = sp4 %>% 
  filter(area == "Kudremukh", habitat %in% c("Grassland"), year == 2021)

sp4_fit_kud_gra_2021 = closedp.0(sp4_kud_gra_2021[,8:11])
sp4_fit_kud_gra_2021CI = closedpCI.0(sp4_kud_gra_2021[,8:11])

sp4_kud_for_2021 = sp4 %>% 
  filter(area == "Kudremukh", habitat %in% c("Forest","Plantation"), year == 2021)

#2018
sp4_kad_gra_2018 = sp4 %>% 
  filter(area == "Kadumane", habitat %in% c("Grassland"), year == 2018)

sp4_kad_for_2018 = sp4 %>% 
  filter(area == "Kadumane", habitat %in% c("Forest","Plantation"), year == 2018)








#########################################


sp = rbind(sp1,sp2,sp3,sp4)
abund1 = data.frame(species = rep(unique(sp$species),each = 2), habitat = rep(c("Grassland","Forest"),4))
abund1$site = "Kadumane"
abund1$year = 2016
abund2 = abund1
abund2$year = 2017
abund3 = abund1
abund3$year = 2018
abund4 = abund1
abund4$site = "Kudremukh"
abund4$year = 2021
abund = rbind(abund1,abund2,abund3,abund4)
abund$type = "Estimated"
abund$cir = abund$mean = abund$cil = NA




# satarae

sp1_fit_kad_comb_2016$parameters$M0
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$mean = sp1_fit_kad_comb_2016CI$CI[1]/5
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$cil = sp1_fit_kad_comb_2016CI$CI[2]/5
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$cir = sp1_fit_kad_comb_2016CI$CI[3]/5
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$mean = length(sp1_kad_gra_2016$ind_id)/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$type = "Detected"
sp1_fit_kad_comb_2017$parameters$M0
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$mean = sp1_fit_kad_comb_2017CI$CI[1]/5
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$cil = sp1_fit_kad_comb_2017CI$CI[2]/5
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$cir = sp1_fit_kad_comb_2017CI$CI[3]/5
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$mean = length(sp1_kad_gra_2017$ind_id)/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$type = "Detected"
sp1_fit_kud_comb_2021$parameters$M0
sp1_fit_kud_comb_2021CI$CI[2] = length(sp1_kud_comb_2021$ind_id)
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$mean = sp1_fit_kud_comb_2021CI$CI[1]/5
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$cil = sp1_fit_kud_comb_2021CI$CI[2]/5
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$cir = sp1_fit_kud_comb_2021CI$CI[3]/5
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$mean = length(sp1_kud_gra_2021$ind_id)/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$type = "Detected"
#2018
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$mean = length(sp1_kad_comb_2018$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Rattus satarae",]$type = "Detected"
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$mean = length(sp1_kad_gra_2018$ind_id)/2
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Rattus satarae",]$type = "Detected"






# famulus

sp2_fit_kad_gra_2016$parameters$M0
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$mean = sp2_fit_kad_gra_2016CI$CI[1]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$cil = sp2_fit_kad_gra_2016CI$CI[2]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$cir = sp2_fit_kad_gra_2016CI$CI[3]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$mean = length(sp2_kad_for_2016$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$type = "Detected"
sp2_fit_kad_gra_2017$parameters$M0
sp2_fit_kad_gra_2017CI$CI[2] = length(sp2_kad_gra_2017$ind_id)
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$mean = sp2_fit_kad_gra_2017CI$CI[1]/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$cil = sp2_fit_kad_gra_2017CI$CI[2]/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$cir = sp2_fit_kad_gra_2017CI$CI[3]/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$mean = length(sp2_kad_for_2017$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$type = "Detected"
#sp2_fit_kud_gra_2021$parameters$M0
#sp2_fit_kud_gra_2021CI$CI/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$mean = length(sp2_kud_gra_2021$ind_id)/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$type = "Detected"
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$mean = length(sp2_kud_for_2021$ind_id)/3
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$type = "Detected"
#2018
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$mean = length(sp2_kad_gra_2018$ind_id)/2
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Mus cf famulus",]$type = "Detected"
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$mean = length(sp2_kad_for_2018$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Mus cf famulus",]$type = "Detected"





# fernandoni

sp3_fit_kad_gra_2016$parameters$M0
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$mean = sp3_fit_kad_gra_2016CI$CI[1]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$cil = sp3_fit_kad_gra_2016CI$CI[2]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$cir = sp3_fit_kad_gra_2016CI$CI[3]/2
sp3_fit_kad_for_2016$parameters$M0
sp3_fit_kad_for_2016CI$CI[2] = length(sp3_kad_for_2016$ind_id)
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$mean = sp3_fit_kad_for_2016CI$CI[1]/5
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$cil = sp3_fit_kad_for_2016CI$CI[2]/5
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$cir = sp3_fit_kad_for_2016CI$CI[3]/5
#sp3_fit_kad_gra_2017$parameters$M0
#sp3_fit_kad_gra_2017CI$CI/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$mean = length(sp3_kad_gra_2017$ind_id)/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$type = "Detected"
sp3_fit_kad_for_2017$parameters$M0
sp3_fit_kad_for_2017CI$CI[2] = length(sp3_kad_for_2017$ind_id)
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$mean = sp3_fit_kad_for_2017CI$CI[1]/5
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$cil = sp3_fit_kad_for_2017CI$CI[2]/5
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$cir = sp3_fit_kad_for_2017CI$CI[3]/5
#sp3_fit_kud_gra_2021$parameters$M0
#sp3_fit_kud_gra_2021CI$CI/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$mean = length(sp3_kud_gra_2021$ind_id)/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$mean = length(sp3_kud_for_2021$ind_id)/3
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$type = "Detected"
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$type = "Detected"
#2018
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$mean = length(sp3_kad_gra_2018$ind_id)/2
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$mean = length(sp3_kad_for_2018$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Mus cf fernandoni",]$type = "Detected"
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Mus cf fernandoni",]$type = "Detected"






# terricolor

sp4_fit_kad_gra_2016$parameters$M0
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$mean = sp4_fit_kad_gra_2016CI$CI[1]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$cil = sp4_fit_kad_gra_2016CI$CI[2]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$cir = sp4_fit_kad_gra_2016CI$CI[3]/2
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$mean = length(sp4_kad_for_2016$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2016 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$type = "Detected"
sp4_fit_kad_gra_2017$parameters$M0
sp4_fit_kad_gra_2017CI$CI[2] = length(sp4_kad_gra_2017$ind_id)
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$mean = sp4_fit_kad_gra_2017CI$CI[1]/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$cil = sp4_fit_kad_gra_2017CI$CI[2]/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$cir = sp4_fit_kad_gra_2017CI$CI[3]/2
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$mean = length(sp4_kad_for_2017$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2017 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$type = "Detected"
sp4_fit_kud_gra_2021$parameters$M0
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$mean = sp4_fit_kud_gra_2021CI$CI[1]/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$cil = sp4_fit_kud_gra_2021CI$CI[2]/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$cir = sp4_fit_kud_gra_2021CI$CI[3]/2
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$mean = length(sp4_kud_for_2021$ind_id)/3
abund[abund$site == "Kudremukh" & abund$year == 2021 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$type = "Detected"
#2018
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$mean = length(sp4_kad_gra_2018$ind_id)/2
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Grassland" & abund$species == "Mus cf terricolor",]$type = "Detected"
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$mean = length(sp4_kad_for_2018$ind_id)/5
abund[abund$site == "Kadumane" & abund$year == 2018 & abund$habitat == "Forest" & abund$species == "Mus cf terricolor",]$type = "Detected"

abund  = abund %>% unite(site_year, c("site", "year"))

abund$species = factor(abund$species, levels = c("Rattus satarae", "Mus cf fernandoni", "Mus cf famulus", "Mus cf terricolor"))
abund$type = factor(abund$type, levels = c("Estimated", "Detected"))



theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

clrs = c("#0072B2", "#E69F00")
clrs = c("#de853a","#376a94")

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 


b = abund %>% select(species,habitat,site_year,mean)
b$site = "Kadumane"
b[25:32,]$site = "Kudremukh"
b = b %>% group_by(species,habitat,site) %>% 
  arrange(desc(mean), .by_group = TRUE) %>% slice(1) %>% ungroup %>%
  select(-site_year)

temp = read.csv("capture_data_combined.csv")
names(temp)[7] = "site"
temp$habitat[temp$habitat == "Plantation"] = "Forest"
temp = temp %>%
  filter(species %in% c("Crocidura horsfieldii","Golunda ellioti","Vandeleuria nilagirica",
                        "Funambulus tristriatus","Platacanthomys lasiurus","Suncus niger",
                        "Rattus rattus") | habitat %in% c("Built-up","Tea")) %>%
  group_by(species,habitat,site,year) %>% summarize(mean = n()) %>%
  arrange(desc(mean), .by_group = TRUE) %>% slice(1) %>% ungroup %>%
  select(-year)

div = c(2,1,5,2,1,1,5,3,1,1,1,1,3,2,5)
temp$mean = temp$mean/div

temp = temp %>% select(species,habitat,mean,site)
b = rbind(b,temp)
b = b %>% distinct(species,habitat,mean,site)

write.csv(b, "max_abundance.csv", row.names = F)

require(extrafont)

pd = position_dodge(0.5)
ggp = ggplot(data = abund, aes(x = site_year, y = mean, col = habitat)) +
  facet_wrap(.~species, nrow = 2, ncol = 2, scales = "free_y") +
  geom_point(size = 3, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.2, position = pd) +
  xlab("Site & Year") +
  ylab("Rodent density (per ha)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  scale_x_discrete(breaks = c("Kadumane_2016","Kadumane_2017","Kadumane_2018","Kudremukh_2021"), 
                   labels = c("Kadumane\n2016","Kadumane\n2017","Kadumane\n2018","Kudremukh\n2021")) +
  theme(legend.position = "bottom")

png('Fig. 2.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


