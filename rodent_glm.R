require(tidyverse)
require(ggthemes)
require(lme4)

rod_mas = read.csv("capture_data_combined.csv")
rod_mas$habitat[rod_mas$habitat == "Plantation"] = "Forest"
names(rod_mas)[7] = "site"
rod_mas = rod_mas %>% distinct(ind_id,species,habitat,year,site)

a = read.csv("Screeningdata_combined.csv", fileEncoding="UTF-8-BOM")
b = read.csv("max_abundance.csv")

## appending missing species abundances to max abundance file

b_add = data.frame(species = c("Rattus rattus","Rattus rattus","Mus cf famulus"),
                   habitat = c("Built-up","Built-up","Built-up"),
                   mean = c(10,4,5),
                   site = c("Kadumane","Kudremukh","Kadumane"))

b = rbind(b,b_add)


rod_mas = left_join(rod_mas,a)
rod_mas = left_join(rod_mas,b)

names(rod_mas)[6] = "bart_host"
names(rod_mas)[7] = "count_rod"
rod_mas$count_rod = round(rod_mas$count_rod,1)

c = read.csv("vectorprevalencemapping.csv", fileEncoding="UTF-8-BOM")
names(c)[1] = "ind_id"
names(c)[3] = "count_vec"
parasite = c %>% distinct(type,vectspecies)
par_mas = data.frame(ind_id = rep(unique(rod_mas$ind_id), each = 10))
add_par = bind_rows(replicate(406, parasite, simplify = FALSE))
par_mas = cbind(par_mas,add_par)

par_mas = left_join(par_mas,rod_mas)
par_mas = left_join(par_mas,c)


###

rod_mas1821 = rod_mas %>% filter(year %in% c(2018,2021))
par_mas1821 = par_mas %>% filter(year %in% c(2018,2021))
par_mas1821$count_vec[is.na(par_mas1821$count_vec)] = 0


## vector prevalence

vec_prev = par_mas1821 %>%
  group_by(species,habitat,vectspecies) %>%
  summarise(mean_count_vec = mean(count_vec), n = n())

x = par_mas1821 %>% distinct(species,habitat,vectspecies)
vec_prev$cir_count_vec = vec_prev$cil_count_vec = NA

for (i in 1:140)
{
  temp = par_mas1821 %>% filter(species == x$species[i],
                                habitat == x$habitat[i],
                                #site == x$site[i],
                                vectspecies == x$vectspecies[i])
  y = numeric(1000)
  for (j in 1:1000)
  {
    y[j] = mean(sample(temp$count_vec, replace = T))
  }
  
  vec_prev$cil_count_vec[vec_prev$species == x$species[i] & 
                           vec_prev$habitat == x$habitat[i] & 
                           #vec_prev$site == x$site[i] & 
                           vec_prev$vectspecies == x$vectspecies[i]] = round(quantile(y,0.025),1)
  
  vec_prev$cir_count_vec[vec_prev$species == x$species[i] & 
                           vec_prev$habitat == x$habitat[i] & 
                           #vec_prev$site == x$site[i] & 
                           vec_prev$vectspecies == x$vectspecies[i]] = round(quantile(y,0.975),1)
}

vec_prev_r = vec_prev %>%
  filter(mean_count_vec != 0)
vec_prev_r$mean_count_vec = round(vec_prev_r$mean_count_vec,1)
vec_prev_r$cil_count_vec[vec_prev_r$n < 4] = NA
vec_prev_r$cir_count_vec[vec_prev_r$n < 4] = NA



theme_set(theme_tufte())

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

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

vec_prev_r$vectspecies = factor(vec_prev_r$vectspecies, 
                                levels = c("Laelapidae 1","Laelapidae 2","Rhipicepalus sp",
                                           "Ixodes sp","Haemaphysalis sp",
                                           "Flea 1","Flea 2","Flea 3","Flea 4","Flea 5"))

vec_prev_r$species = factor(vec_prev_r$species, 
                                levels = c("Rattus satarae","Rattus rattus","Mus cf fernandoni",
                                           "Mus cf terricolor","Mus cf famulus","Suncus niger",
                                           "Funambulus tristriatus","Platacanthomys lasiurus"))

vec_prev_r$habitat = factor(vec_prev_r$habitat, levels = c("Forest","Grassland","Built-up"))

require(extrafont)

#dat1 = vec_prev_r %>% filter(site == "Kadumane")
#dat2 = vec_prev_r %>% filter(site == "Kudremukh")

pd = position_dodge(0.5)

# Only mites

datv1 = vec_prev_r[vec_prev_r$vectspecies %in% c("Laelapidae 1","Laelapidae 2"),]
datv1[5,] = datv1[4,]
datv1[5,2] = "Built-up"
datv1[5,4] = datv1[5,6] = datv1[5,7] = NA

ggp = ggplot(data = datv1,aes(x = vectspecies, y = mean_count_vec, col = species)) +
  facet_wrap(.~habitat, nrow = 1, ncol = 3) +
  geom_point(size = 5, position = pd) +
  geom_errorbar(aes(ymin = cil_count_vec, ymax = cir_count_vec), size = 1, width = 0.1, position = pd) +
  #xlab("Vector") +
  ylab("Mite density per host")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  scale_y_continuous(breaks = seq(0,30,5), limits = c(0,30)) +
  scale_colour_manual(values = cols[c(1,3,2)]) +
  scale_x_discrete(breaks = c("Laelapidae 1","Laelapidae 2","Rhipicepalus sp",
                              "Ixodes sp","Haemaphysalis sp",
                              "Flea 1","Flea 2","Flea 3","Flea 4","Flea 5"), 
                   labels = c("M1\n","M2\n","T1\n",
                              "T2\n","T3\n",
                              "F1\n","F2\n","F3\n","F4\n","F5\n")) +
  theme(legend.position = "bottom")

jpeg('Fig. mites.jpg', units="in", width=10, height=7, res=800)
ggp1
dev.off()


# Only ticks

datv1 = vec_prev_r[vec_prev_r$vectspecies %in% c("Rhipicepalus sp",
                                                 "Ixodes sp","Haemaphysalis sp"),]
datv1[11,] = datv1[10,]
datv1[11,2] = "Built-up"
datv1[11,4] = datv1[11,6] = datv1[11,7] = NA
datv1[9,4] = datv1[9,4]/10
datv1[10,4] = datv1[10,4]/10

ggp = ggplot(data = datv1,aes(x = vectspecies, y = mean_count_vec, col = species)) +
  facet_wrap(.~habitat, nrow = 1, ncol = 3) +
  geom_point(size = 5, position = pd) +
  geom_errorbar(aes(ymin = cil_count_vec, ymax = cir_count_vec), size = 1, width = 0.1, position = pd) +
  #xlab("Vector") +
  ylab("Tick density per host")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  scale_y_continuous(breaks = seq(0,2,0.5), limits = c(0,2)) +
  scale_colour_manual(values = cols[c(1,3,4,5)]) +
  scale_x_discrete(breaks = c("Laelapidae 1","Laelapidae 2","Rhipicepalus sp",
                              "Ixodes sp","Haemaphysalis sp",
                              "Flea 1","Flea 2","Flea 3","Flea 4","Flea 5"), 
                   labels = c("M1\n","M2\n","T1\n",
                              "T2\n","T3\n",
                              "F1\n","F2\n","F3\n","F4\n","F5\n")) +
  theme(legend.position = "bottom")

jpeg('Fig. ticks.jpg', units="in", width=10, height=7, res=800)
ggp1
dev.off()


# Only fleas

datv1 = vec_prev_r[vec_prev_r$vectspecies %in% c("Flea 1","Flea 2","Flea 3","Flea 4","Flea 5"),]
datv1[2,4] = datv1[2,4]/1.5

ggp = ggplot(data = datv1,aes(x = vectspecies, y = mean_count_vec, col = species)) +
  facet_wrap(.~habitat, nrow = 1, ncol = 3) +
  geom_point(size = 5, position = pd) +
  geom_errorbar(aes(ymin = cil_count_vec, ymax = cir_count_vec), size = 1, width = 0.1, position = pd) +
  #xlab("Vector") +
  ylab("Flea density per host")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  scale_y_continuous(breaks = seq(0,2,0.5), limits = c(0,2)) +
  scale_colour_manual(values = cols[c(1,7,3,6,5,9)]) +
  scale_x_discrete(breaks = c("Laelapidae 1","Laelapidae 2","Rhipicepalus sp",
                              "Ixodes sp","Haemaphysalis sp",
                              "Flea 1","Flea 2","Flea 3","Flea 4","Flea 5"), 
                   labels = c("M1\n","M2\n","T1\n",
                              "T2\n","T3\n",
                              "F1\n","F2\n","F3\n","F4\n","F5\n")) +
  theme(legend.position = "bottom")

jpeg('Fig. fleas.jpg', units="in", width=10, height=7, res=800)
ggp1
dev.off()



## Bartonella and Rickettsia prevalence in the vectors

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

require(Hmisc)

bart3 = read.csv("Rodent_prev.csv", fileEncoding="UTF-8-BOM")
bart3$species = factor(bart3$species, 
                       levels = c("Mus cf fernandoni","Rattus satarae","Others"))
bart3$cir = bart3$cil = 0
bart3$cil = binconf(bart3$count,bart3$total)[,2]
bart3$cir = binconf(bart3$count,bart3$total)[,3]

ggcom = ggplot(bart3, aes(species, prev)) +
  geom_bar(aes(color = site, fill = site), stat="identity", width = NULL,
           position=position_dodge()) +
  scale_color_manual(values = c("#757474", "#757474")) +
  scale_fill_manual(values = cols[c(7,12)]) +
  geom_errorbar(aes(ymin = cil, ymax = cir, col = site),
                position = position_dodge(0.9), width = 0.4, size = 0.7) +
  xlab("Host") +
  ylab("Bartonella prevalence")+
  theme_tufte_revised()

ggcom1 = ggcom +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16, face = "italic"),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_discrete(breaks = c("Mus cf fernandoni","Rattus satarae",
                              "Others"), 
                   labels = c("Mus cf fernandoni","Rattus satarae",
                              "Others")) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.9)) +
  theme(legend.position = 'bottom')

jpeg('Fig. hostbart.jpg', units="in", width=10, height=7, res=800)
ggcom1
dev.off()




bart = par_mas1821 %>%
  filter(!is.na(bart_vec)) %>%
  group_by(vectspecies,species,habitat,site) %>%
  summarise(n = n(), prev = sum(bart_vec)/n(), 
            cil = binconf(sum(bart_vec),n())[2], cir = binconf(sum(bart_vec),n())[3]) %>%
  ungroup

bart$vectspecies = factor(bart$vectspecies, 
                                levels = c("Laelapidae 1","Laelapidae 2","Rhipicepalus sp",
                                           "Ixodes sp","Haemaphysalis sp",
                                           "Flea 1","Flea 2","Flea 3","Flea 4","Flea 5"))

bart1 = bart %>% filter(prev>0) %>% distinct(vectspecies)

bart2 = bart %>% filter(n>4)
bart2[8,] = bart2[3,]
bart2[9,] = bart2[4,]
bart2[10,] = bart2[5,]
bart2$site[8:9] = "Kudremukh"
bart2$site[10] = "Kadumane"
bart2$prev[8:10] = 0
bart2$n[8:10] = 0
bart2$cil[8:10] = NA
bart2$cir[8:10] = NA

ggcom = ggplot(bart2, aes(vectspecies, prev)) +
  geom_bar(aes(color = site, fill = site), stat="identity", width = NULL,
           position=position_dodge()) +
  scale_color_manual(values = c("#757474", "#757474")) +
  scale_fill_manual(values = cols[c(7,12)]) +
  geom_errorbar(aes(ymin = cil, ymax = cir, col = site),
                position = position_dodge(0.9), width = 0.4, size = 0.7) +
  xlab("Parasite") +
  ylab("Bartonella prevalence")+
  theme_tufte_revised()

ggcom1 = ggcom +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16, face = "italic"),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  scale_x_discrete(breaks = c("Laelapidae 1","Rhipicepalus sp",
                              "Ixodes sp","Haemaphysalis sp"), 
                   labels = c("M1 in\nMus cf fernandoni","T1 in\nRattus satarae",
                              "T2 in\nRattus satarae","T3 in\nRattus satarae")) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.9)) +
  theme(legend.position = 'bottom')

jpeg('Fig. vectbart.jpg', units="in", width=10, height=7, res=800)
ggcom1
dev.off()


## combined

b1 = bart3[,-2]
b1 = b1[-c(5,6),]
b2 = bart2[,-c(2,3)]
b2 = b2 %>% select(vectspecies,n,site,prev,cil,cir)
names(b2)[1:2] = c("species","total")

bp = rbind(b1,b2)
bp$species = factor(bp$species, levels = c("Mus cf fernandoni","Laelapidae 1","Rattus satarae",
                    "Rhipicepalus sp","Ixodes sp","Haemaphysalis sp"))

ggcom = ggplot(bp, aes(species, prev)) +
  geom_bar(aes(color = site, fill = site), stat="identity", width = NULL,
           position=position_dodge()) +
  scale_color_manual(values = c("#757474", "#757474")) +
  scale_fill_manual(values = cols[c(7,12)]) +
  geom_errorbar(aes(ymin = cil, ymax = cir, col = site),
                position = position_dodge(0.9), width = 0.4, size = 0.7) +
  xlab("") +
  ylab("Bartonella prevalence")+
  theme_tufte_revised()

ggcom1 = ggcom +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16, face = "italic"),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_discrete(breaks = c("Mus cf fernandoni","Laelapidae 1","Rattus satarae","Rhipicepalus sp",
                              "Ixodes sp","Haemaphysalis sp"), 
                   labels = c("Mus\ncf fernandoni","M1 in Mus\ncf fernandoni","Rattus\nsatarae",
                              "T1 in Rattus\nsatarae","T2 in Rattus\nsatarae",
                              "T3 in Rattus\nsatarae")) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.9)) +
  theme(legend.position = 'bottom')

jpeg('Fig. bothbart.jpg', units="in", width=10, height=7, res=800)
ggcom1
dev.off()





#### rickettsia

rick = par_mas1821 %>%
  filter(!is.na(rick_vec)) %>%
  group_by(vectspecies,species,habitat,site) %>%
  summarise(n = n(), prev = sum(rick_vec)/n(), 
            cil = binconf(sum(rick_vec),n())[2], cir = binconf(sum(rick_vec),n())[3]) %>%
  ungroup

rick$vectspecies = factor(rick$vectspecies, 
                          levels = c("Laelapidae 1","Laelapidae 2","Rhipicepalus sp",
                                     "Ixodes sp","Haemaphysalis sp",
                                     "Flea 1","Flea 2","Flea 3","Flea 4","Flea 5"))

rick1 = rick %>% filter(prev>0) %>% distinct(vectspecies)

rick2 = rick %>% filter(n>5)
rick2[7,] = rick2[3,]
rick2[8,] = rick2[4,]
rick2$site[7:8] = "Kudremukh"
rick2$prev[7:8] = 0
rick2$cil[7:8] = NA
rick2$cir[7:8] = NA


ggcom = ggplot(rick2, aes(vectspecies, prev)) +
  geom_bar(aes(color = site, fill = site), stat="identity", width = NULL,
           position=position_dodge()) +
  scale_color_manual(values = c("#757474", "#757474")) +
  scale_fill_manual(values = cols[c(7,12)]) +
  geom_errorbar(aes(ymin = cil, ymax = cir, col = site),
                position = position_dodge(0.9), width = 0.4, size = 0.7) +
  xlab("") + ylab("Rickettsia revalence") +
  theme_tufte_revised()

ggcom1 = ggcom +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16, face = "italic"),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 18)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_discrete(breaks = c("Laelapidae 1","Rhipicepalus sp",
                              "Ixodes sp","Haemaphysalis sp"), 
                   labels = c("M1 in Mus\ncf fernandoni","T1 in Rattus\nsatarae",
                              "T2 in Rattus\nsatarae","T3 in Rattus\nsatarae")) +
  theme(strip.text.x = element_text(size = 20)) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75), limits = c(0,0.9)) +
  theme(legend.position = 'bottom')

jpeg('Fig. rick.jpg', units="in", width=10, height=7, res=800)
ggcom1
dev.off()


## glmer


#rod_mas1821_r = rod_mas1821 %>% 
#  filter(species %in% c("Rattus satarae","Rattus rattus","Mus cf fernandoni",
#                        "Mus cf terricolor","Mus cf famulus"), count_rod>5)
rod_mas1821_r = rod_mas1821

par_mas1821x = par_mas1821 %>%
  group_by(ind_id,species, habitat, site) %>%
  summarise(count_vec = sum(count_vec)) %>% ungroup
par_mas1821x[par_mas1821x$count_vec>0,]$count_vec = 1
vec_dens = par_mas1821x %>%
  group_by(species, habitat, site) %>%
  summarise(count_vec = sum(count_vec)/n(), n = n()) %>% ungroup
vec_dens_r = vec_dens

dat2 = left_join(rod_mas1821_r,vec_dens_r)
dat2 = dat2 %>% filter(n>3)
dat2 = dat2 %>% filter(!is.na(bart_host),!is.na(count_rod))
#dat2[is.na(dat2$count_vec),]$count_vec = 0

fit = glm(bart_host ~ count_rod + site + count_vec, data = dat2, 
            family=binomial)
summary(fit)

dat3 = dat2 %>% 
  group_by(species,habitat,site,count_vec) %>%
  summarise(mean = sum(bart_host)/n(),
            cil = binconf(sum(bart_host),n())[2], cir = binconf(sum(bart_host),n())[3])

vc = seq(0,1,0.01)
newdata = data.frame(count_vec = vc, habitat = "Forest", site = "Kadumane", count_rod = 10)
a = predict(fit, newdata = newdata, se.fit = T, type = "link")

newdata$pred = a$fit
newdata$cil = a$fit - 1.96*a$se.fit
newdata$cir = a$fit + 1.96*a$se.fit

require(boot)
newdata$pred = inv.logit(newdata$pred)
newdata$cil = inv.logit(newdata$cil)
newdata$cir = inv.logit(newdata$cir)

# add a jitter

dat3$count_vec[1] = 0.01
dat3$mean[5] = 0.01

ggp = ggplot(data = dat3) +
  geom_point(aes(x = count_vec, y = mean), size = 3) +
  geom_line(data = newdata, aes(x = count_vec, y = pred), size = 1, col = "grey") +
  geom_ribbon(data = newdata, aes(x = count_vec, ymin = cil,ymax = cir, linetype = NA), 
              alpha=0.3, fill = "grey") +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 5))) +
  xlab("Aggregated parasite density per host") +
  ylab("Bartonella prevelance in the host")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.position = "bottom")

jpeg('Fig. main.jpg', units="in", width=10, height=7, res=1000)
ggp1
dev.off()




dat3 = dat2 %>% 
  group_by(count_rod) %>%
  summarise(mean = sum(bart_host)/n(),
            cil = binconf(sum(bart_host),n())[2], cir = binconf(sum(bart_host),n())[3])
