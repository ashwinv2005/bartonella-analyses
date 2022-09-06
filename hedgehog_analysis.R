require(tidyverse)
require(ggthemes)

data = read.csv("hedgehog_data.csv")

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

require(extrafont)

data$month = factor(data$month,levels = c("April","May","June",
                                             "July","August","September","October","November",
                                             "December","January","February","March"))
data = data %>%
  mutate(region = "Southern TN")

data$region[!data$district %in% c("Tenkasi","Kanyakumari")] = "Central/Northern TN"

data$district = factor(data$district, levels = c("Salem","Coimbatore","Pudukkottai",
                                                 "Tenkasi","Kanyakumari"))

season = data %>%
  filter(!is.na(month)) %>%
  group_by(region,month) %>% summarize(count = n()) %>% ungroup()



ggp = ggplot(data = season, aes(x = month, y = count)) +
  facet_wrap(.~region, nrow = 2, ncol = 1) +
  geom_bar(stat = "identity", fill = clrs[1]) +
  xlab("Month") +
  ylab("Hedgehog encounters")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  #scale_colour_manual(values = clrs) +
  scale_x_discrete(breaks = c("January","February","March","April","May","June",
                              "July","August","September","October","November",
                              "December"), 
                   labels = c("Jan","Feb","Mar","Apr","May","Jun",
                              "Jul","Aug","Sep","Oct","Nov",
                              "Dec")) +
  #scale_shape_manual(values = c(19,17)) +
  theme(legend.position = "bottom")

png('Hedgehog_seasonality.jpg', units="in", width=7, height=7, res=1000)
ggp1
dev.off()




##################33

years = data %>%
  filter(!is.na(year), !district %in% c("Salem","Pudukkottai"), year >= 1980)


ggp = ggplot(data = years) +
  facet_wrap(.~district, nrow = 3, ncol = 1, scales = "free_y") +
  geom_histogram(aes(x = year,y = 100*(..count..)/sum(..count..)), fill = clrs[1], bins = 40) +
  xlab("Year") +
  ylab("Last hedgehog encounter (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  #scale_colour_manual(values = clrs) +
  #scale_shape_manual(values = c(19,17)) +
  theme(legend.position = "bottom")

png('Hedgehog_last_seen.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()




###################

datax = data %>%
  filter(!is.na(habitation) | !is.na(farmland) | !is.na(scrub) | !is.na(forest) | !is.na(road)) %>%
  filter(habitation != 0 | farmland != 0 | scrub != 0 | forest != 0 | road != 0) %>%
  filter(!district %in% c("Salem","Pudukkottai")) %>%
  group_by(district) %>% mutate(total = n()) %>% ungroup() 

datax$habitat = NA
datax$stat = NA
data1 = data2 = data3 = data4 = data5 = datax
data1$habitat = "Habitation"
data1$stat = datax$habitation
data2$habitat = "Farmland"
data2$stat = datax$farmland
data3$habitat = "Scrub"
data3$stat = datax$scrub
data4$habitat = "Forest"
data4$stat = datax$forest
data5$habitat = "Road"
data5$stat = datax$road

datay = rbind(data1,data2,data3,data4,data5)
datay$habitat = factor(datay$habitat, levels = c("Habitation","Farmland","Road","Scrub","Forest"))


habitat = datay %>%
  group_by(district,habitat) %>% summarize(perc = round(sum(stat)/max(total)*100)) %>% ungroup()
  
  

ggp = ggplot(data = habitat) +
  facet_wrap(.~district, nrow = 5, ncol = 1) +
  geom_bar(stat = "identity", aes(x = habitat, y = perc, fill = district)) +
  xlab("Habitat") +
  ylab("Percentage encounters (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = cols[1:5]) +
  #scale_shape_manual(values = c(19,17)) +
  theme(legend.position = "bottom")

png('Hedgehog_habitat_pref.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()





############# Probability of sighting

detprobvil = data %>%
  filter(!is.na(invil)) %>%
  group_by(village) %>% mutate(tot = length(na.omit(invil))) %>%
  ungroup() %>%
  filter(tot >= 10) %>%
  group_by(village,tot) %>% 
  summarize(prop = sum(na.omit(invil)))


detprob = data %>%
  group_by(district) %>% mutate(totvil = length(na.omit(invil)), totdis = length(na.omit(indis))) %>%
  ungroup() %>%
  group_by(district,totvil,totdis) %>% 
  summarize(vil = sum(na.omit(invil)), dis = sum(na.omit(indis)))

det = detprob[,1]
det1 = det
det1$type = "Within district"
det2 = det
det2$type = "Around village"

a = binconf(detprob$dis,detprob$totdis)
det1$prop = round(a[,1]*100)
det1$cil = round(a[,2]*100)
det1$cir = round(a[,3]*100)

a = binconf(detprob$vil,detprob$totvil)
det2$prop = round(a[,1]*100)
det2$cil = round(a[,2]*100)
det2$cir = round(a[,3]*100)

det = rbind(det1,det2)
detx = det
detx$period = "All time"

data$invil1 = data$invil
data$invil1[data$invil == 1 & data$year < 2015] = 0
data$indis1 = data$indis
data$indis1[data$indis == 1 & data$year < 2015] = 0

detprob = data %>%
  group_by(district) %>% mutate(totvil = length(na.omit(invil1)), totdis = length(na.omit(indis1))) %>%
  ungroup() %>%
  group_by(district,totvil,totdis) %>% 
  summarize(vil = sum(na.omit(invil1)), dis = sum(na.omit(indis1)))

det = detprob[,1]
det1 = det
det1$type = "Within district"
det2 = det
det2$type = "Around village"

a = binconf(detprob$dis,detprob$totdis)
det1$prop = round(a[,1]*100)
det1$cil = round(a[,2]*100)
det1$cir = round(a[,3]*100)

a = binconf(detprob$vil,detprob$totvil)
det2$prop = round(a[,1]*100)
det2$cil = round(a[,2]*100)
det2$cir = round(a[,3]*100)

det = rbind(det1,det2)
dety = det
dety$period = "Last 5 years"

det = rbind(detx,dety)



pd = position_dodge(0.5)
ggp = ggplot(data = det, aes(x = district, y = prop, col = type)) +
  facet_wrap(.~period, nrow = 2, ncol = 1) +
  geom_point(size = 3, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 1, width = 0.2, position = pd) +
  xlab("District") +
  ylab("Percentage of people with encounters (%)")+
  theme_tufte_revised()


ggp1 = ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 20), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(strip.text.x = element_text(size = 16)) +
  scale_colour_manual(values = clrs) +
  theme(legend.position = "bottom")

png('Hedgehog_abundance.jpg', units="in", width=7, height=10, res=1000)
ggp1
dev.off()
