# For Ansil's paper

require(tidyverse)
scr = read.csv("Screening result.csv")

scr = scr %>%
  group_by(species,result) %>% dplyr::summarize(count = n())

all = data.frame(species = rep(unique(scr$species),each = 2), 
                 result = rep(c("Positive","Negative"),length(unique(scr$species))))
all = left_join(all,scr)
all$count[is.na(all$count)] = 0

tot = all %>%
  group_by(species) %>% dplyr::summarize(total = sum(count))

all = all %>%
  filter(result == "Positive")

all = left_join(all,tot)

require(Hmisc)

conf = binconf(all$count,all$total)

all = cbind(all,conf)
all$species = factor(all$species, levels = c("Mus booduga","Mus musculus","Rattus rattus","Rattus satarae",
                                             "Gollunda ellioti","Platacanthomys lasiurus","Suncus murinus",
                                             "Funambulus tristriatus"))

require(ggthemes)
theme_set(theme_tufte())

ggp = ggplot(all, aes(x=species, y=PointEst)) + 
  geom_point(
    #position = pd,
    #col = "dark green"
    size = 4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), size = 0.5,
                #col = "dark green",
                width = 0.1) +
  xlab("species") +
  ylab(expression(paste("frequency of ",italic("Bartonella")," incidence")))

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12, face = 'italic'),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
                     limits = c(0,1)) +
  scale_x_discrete(breaks = c("Mus booduga","Mus musculus","Rattus rattus","Rattus satarae",
                              "Gollunda ellioti","Platacanthomys lasiurus","Suncus murinus",
                              "Funambulus tristriatus"),
                   labels = c("Mus\nbooduga","Mus\nmusculus","Rattus\nrattus","Rattus\nsatarae",
                              "Gollunda\nellioti","Platacanthomys\nlasiurus","Suncus\nmurinus",
                              "Funambulus\ntristriatus"))

png('plot1a.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()
