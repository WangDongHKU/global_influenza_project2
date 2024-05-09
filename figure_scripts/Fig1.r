#==load packages==
library(stringr)
library(Biostrings)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggpubr)
library(rworldmap)
library(ape)
library(ggtree)
library(phangorn)
library(phytools)
library(treeio)
library(zoo)
library(patchwork)
library(ggsci)
library(scales)

#==define date==
date_match <- data.frame(date = seq(as.Date("1995-01-05"),as.Date("2024-04-18"),7))
date_match$ISO_YEAR <- isoyear(date_match$date)
date_match$ISO_WEEK <- isoweek(date_match$date)

#==define color==
colors <- c(pal_npg("nrc", alpha =1)(10)[c(1:7,9:10)],"darkred","#FADDA9","grey80")
colors1 <- c(pal_aaas("default", alpha =0.7)(10))
show_col(colors)
show_col(colors1)
value = c("Japan/Korea" = colors[1],"Western Asia" = colors[3],"North America" = colors[6],"Northern China" = colors[8], "Southern China" = colors[11], 
          "South-eastern Asia"= colors[4], "Southern Asia"= colors[5], "Europe"= colors[2], "Oceania"= colors[7],
          "North China" = colors[8], "South China" = colors[11], "Russia"= colors[10],  "South America"= colors[12], 
          "Africa"= colors[9], "Americas" = colors1[7], "Asia" = "#D8BFD8", "China" = colors1[4])

#================================================================
#==Genetic data were not provided due to the policy restriction==
#================================================================
h1n1_meta <- read.csv("h1n1_meta.csv") %>% select(-"date") %>% left_join(date_match)
h3n2_meta <- read.csv("h3n2_meta.csv") %>% select(-"date") %>% left_join(date_match)
bv_meta <- read.csv("BV_meta.csv") %>% select(-"date") %>% left_join(date_match)
by_meta <- read.csv("BY_meta.csv") %>% select(-"date") %>% left_join(date_match)

h1n1_num <- h1n1_meta %>% group_by(date) %>% summarise(h1n1_seq = n())
h3n2_num <- h3n2_meta %>% group_by(date) %>% summarise(h3n2_seq = n())
bv_num <- bv_meta %>% group_by(date) %>% summarise(bv_seq = n())
by_num <- by_meta %>% group_by(date) %>% summarise(by_seq = n())

#=====================================
#==Epidemiological surveillance data==
#=====================================
epi_glo_flu_region1 <- read.csv("../data_part/epi_data/epi_glo_flu_region1_no_roll.csv") %>% mutate(date = as.Date(date) + 3); range(epi_glo_flu_region1$date)
epi_glo_flua1 <- read.csv("../data_part/epi_data/epi_glo_flua1_no_roll.csv") %>% mutate(date = as.Date(date) + 3); range(epi_glo_flua1$date)
epi_glo_flub1 <- read.csv("../data_part/epi_data/epi_glo_flub1_no_roll.csv") %>% mutate(date = as.Date(date) + 3); range(epi_glo_flub1$date)

epi_glo_flua1 <- left_join(epi_glo_flua1, h1n1_num) %>% left_join(h3n2_num)
epi_glo_flub1 <- left_join(epi_glo_flub1, bv_num) %>% left_join(by_num)

#==Figure 1==
max(epi_glo_flua1$h1n1_num[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")]/
  epi_glo_flua1$test[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")])
max(epi_glo_flua1$h3n2_num[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")]/
      epi_glo_flua1$test[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")])
max(epi_glo_flub1$BV_num[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")]/
      epi_glo_flub1$test[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")])
max(epi_glo_flub1$BY_num[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")]/
      epi_glo_flub1$test[epi_glo_flua1$date >= as.Date("2020-05-01") & epi_glo_flua1$date <= as.Date("2021-07-31")])

#PNAEL c,e,f,g
ggplot(data = h1n1_meta) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -20,ymax = 1200,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = -20,ymax = 1200,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, fill = region))+
  geom_ribbon(data = epi_glo_flua1, aes(x = date,
                                        ymin = h1n1_num_roll_LL/test_roll*5000,
                                        ymax = h1n1_num_roll_UL/test_roll*5000),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flua1, aes(x = date, y = h1n1_num_roll/test_roll*5000),
            stat = "identity",size = 0.5,color = "darkred")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     limits = c(-20,1200),
                     breaks = seq(0,1200,300),
                     expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/5000,
                                         name="Positivity rate (%)",
                                         breaks = seq(0,0.2,0.05),
                                         labels = seq(0,20,5)
                     ))+
  theme_bw()+
  scale_fill_manual("Continents",values = value)+
  theme(legend.position = c(0.65,0.88))+
  theme(axis.text.y.right = element_text(color = "darkred"),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.3,"cm"),
        legend.background = element_blank(),
        axis.title.y.right = element_text(color = "darkred"),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(nrow = 1))+
  labs(subtitle = "H1N1pdm09") -> p1_2

ggplot(data = h3n2_meta) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -40,ymax = 2000,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = -40,ymax = 2000,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, fill = region))+
  geom_ribbon(data = epi_glo_flua1, aes(x = date, 
                                        ymin = h3n2_num_roll_LL/test_roll*8500,
                                        ymax = h3n2_num_roll_UL/test_roll*8500),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flua1 ,
            aes(x = date, y = h3n2_num_roll/test_roll*8500), 
            stat = "identity",size = 0.5,color = "darkred")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     limits = c(-40,2000),
                     breaks = seq(0,2000,500),
                     expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/8500,
                                         name="Positivity rate (%)",
                                         breaks = seq(0,0.2,0.05),
                                         labels = seq(0,20,5)
                     ))+
  theme_bw()+
  # scale_fill_manual("Genome sources",values = c("darkorange","lightblue"))+
  scale_fill_manual("Continents",values = value)+
  theme(legend.position = 'none')+
  theme(axis.text.y.right = element_text(color = "darkred"),
        axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.title.y.right = element_text(color = "darkred"))+
  labs(subtitle = "H3N2") -> p2_2

ggplot(data = bv_meta) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, fill = region))+
  geom_ribbon(data = epi_glo_flub1, aes(x = date, ymin = BV_num_roll_LL/test_roll*5000,
                                        ymax = BV_num_roll_UL/test_roll*5000),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flub1 ,
            aes(x = date, y = BV_num_roll/test_roll*5000), stat = "identity",size = 0.5,color = "darkred")+
  # geom_line(data = epi_glo_flub1 ,
  #           aes(x = date, y = BV_num_roll/30), stat = "identity",size = 0.2,color = "blue")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     breaks = seq(0,600,150),
                     limits = c(-10,600),
                    
                     expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/5000,
                                         name="Positivity rate (%)",
                                         breaks = seq(0,0.12,0.03),
                                         labels = seq(0,12,3)
                     ))+
  theme_bw()+
  # scale_fill_manual("Genome sources",values = c("darkorange","lightblue"))+
  scale_fill_manual("Continents",values = value)+
  theme(axis.text.y.right = element_text(color = "darkred"),
        axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.title.y.right = element_text(color = "darkred"))+
  theme(legend.position = "none")+
  labs(subtitle = "B/Victoria")-> p3_2

ggplot(data = by_meta) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, fill = region))+
  geom_ribbon(data = epi_glo_flub1[epi_glo_flub1$date <= as.Date("2020-03-31"),],
              aes(x = date, ymin = BY_num_roll_LL/test_roll*3500,
                                        ymax = BY_num_roll_UL/test_roll*3500),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flub1[epi_glo_flub1$date <= as.Date("2020-03-31"),] ,
            aes(x = date, y = BY_num_roll/test_roll*3500), stat = "identity",size = 0.5,color = "darkred")+
  # geom_line(data = epi_glo_flub1 ,
  #           aes(x = date, y = BY_num_roll/40), stat = "identity",size = 0.2,color = "blue")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     breaks = seq(0,600,150),
                     limits = c(-10,600),
                      expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/3500,
                                         name="Positivity rate (%)",
                                         breaks = seq(0,0.15,0.05),
                                         labels = seq(0,15,5)
                     ))+
  theme_bw()+
  # scale_fill_manual("Genome sources",values = c("darkorange","lightblue"))+
  geom_segment(aes(x= as.Date("2020-03-11") , y= 200 , xend= as.Date("2020-03-11") , yend= 0 ), 
               arrow = arrow(length=unit(0.2, 'cm')),lwd= 0.2)+
  annotate("text", x = as.Date("2020-03-11"), y = 300,size =3, label = "WHO declared\na pandemic")+
  geom_segment(aes(x= as.Date("2023-05-05") , y= 200 , xend= as.Date("2023-05-05") , yend= 0 ), 
               arrow = arrow(length=unit(0.2, 'cm')),lwd= 0.2)+
  annotate("text", x = as.Date("2023-05-05"), y = 300,size =3, label = "WHO declared end\nto COVID-19’s emergency")+
  scale_fill_manual("Continents",values = value)+
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(color = "darkred"),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.title.y.right = element_text(color = "darkred"))+
  labs(subtitle = "B/Yamagata")-> p4_2

#==PANEL a, b==
table(epi_glo_flua1$test == epi_glo_flub1$test)
ggplot(data = epi_glo_flua1[epi_glo_flua1$date >= as.Date("2017-01-01"),]) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 400000,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = 0,ymax = 400000,alpha = 0.2,fill = colors[7])+
  annotate("text", x= c(as.Date("2018-08-15")),y = 330000, size = 3,
           label = c("Pre-pandemic\nperiod"))+
  annotate("text", x= c(as.Date("2020-10-01")),y = 330000, size = 3,
           label = c("Acute\nphase"))+
  annotate("text", x= c(as.Date("2022-04-01")),y = 70000, size = 3,
           label = c("Transition\nphase"))+
  annotate("text", x= c(as.Date("2023-11-15")),y = 80000, size = 2.5,
           label = c("Post-\npandemic\nperiod"))+
  geom_line(aes(x = date, y = test_roll))+
  scale_x_date("Date", date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0))+
  scale_y_continuous("No. of specimens (× 1k)", labels = seq(0,400,100),
                     limits = c(0, 400000),expand = c(0,0))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),)+
  labs(subtitle = "Intensity of virological surveillance")-> fig1

meta <- rbind(h1n1_meta[,c(24,26)], h3n2_meta[,c(24,26)], bv_meta[,c(23,25)], by_meta[,c(22,24)])
seq_epi <- meta %>%
  group_by(date) %>%
  summarise(seq_no = n()) %>%
  left_join(epi_glo_flua1) %>%
  left_join(epi_glo_flub1[,c(2:18)]) %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(epi_num = sum(h1n1_num+h3n2_num+BV_num+BY_num, na.rm = T),
            seq_no = sum(seq_no, na.rm = T))
seq_epi1 <- seq_epi %>%
  mutate(epi_num_roll = rollmean(epi_num, k=5, fill=NA, align='center'),
         seq_no_roll = rollmean(seq_no, k=5, fill=NA, align='center'))

ggplot(data = seq_epi1[seq_epi1$date >= as.Date("2017-01-01") & seq_epi1$date <= as.Date("2023-12-31"),]) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 0.3,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = 0,ymax = 0.3,alpha = 0.2,fill = colors[7])+
  # geom_hline(yintercept = 0.05, linetype = 2, color = "red")+
  geom_line(aes(x = date, y = seq_no_roll/epi_num_roll))+
  scale_x_date("Date", date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0) )+
  scale_y_continuous("Prop. of lab-confirmed\ninfluenza cases\nsequenced (%)",
                     expand = c(0,0),limits = c(0,0.3),
                     breaks = seq(0,0.3,0.1),labels = seq(0, 30, 10))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  labs(subtitle = "Intensity of genomic surveillance")-> fig2

#==positivity rates in several regions (panel d, f, h)==
epi_glo_flu_region2 <- epi_glo_flu_region1[epi_glo_flu_region1$region_final %in% c("Africa", "South-eastern Asia", "Southern Asia"),]
epi_glo_flu_region2 <- epi_glo_flu_region2[(epi_glo_flu_region2$date >= as.Date("2020-01-01") & 
                                             epi_glo_flu_region2$date < as.Date("2021-07-01")),]

ggplot(epi_glo_flu_region2) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 0.3,alpha = 0.2,fill = colors[5])+
  geom_ribbon(aes(x = date, ymin = h1n1_num_roll_LL/test_roll,ymax = h1n1_num_roll_UL/test_roll, fill = region_final),
              stat = "identity",color = "transparent", alpha = 0.4) +
  geom_line(aes(x = date, y = h1n1_num_roll/test_roll, color = region_final))+
  scale_x_date("Date", date_labels = "%b %Y", breaks = c(as.Date("2020-04-01"),as.Date("2020-10-01"),as.Date("2021-04-01")))+
  scale_y_continuous("Positivity rate (%)",breaks = seq(0,0.3,0.1),
                     labels = seq(0,30,10), expand = c(0, 0),limits = c(-0.01,0.3))+
  theme_bw()+
  theme(
    # axis.title.y = element_blank(),
        # axis.text.y = element_text(color = "darkred")
        )+
  scale_color_manual("Regions",values = value,
                     labels = c("Africa",  "Southeast Asia", "South Asia"))+
  scale_fill_manual("Regions",values = value,
                     labels = c("Africa",  "Southeast Asia", "South Asia"))+
  theme(axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", color ="transparent"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.3,"cm"),
        # legend.title = element_blank(),
        # axis.title.y.right = element_text(color = "darkred")
        )+
  theme(legend.position = c(0.6,0.65))-> p9

ggplot(epi_glo_flu_region2) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 0.15,alpha = 0.2,fill = colors[5])+
  geom_ribbon(aes(x = date, ymin = h3n2_num_roll_LL/test_roll,ymax = h3n2_num_roll_UL/test_roll, fill = region_final),
              stat = "identity",color = "transparent", alpha = 0.4) +
  geom_line(aes(x = date, y = h3n2_num_roll/test_roll, color = region_final))+
  scale_x_date("Date", date_labels = "%b %Y", breaks = c(as.Date("2020-04-01"),as.Date("2020-10-01"),as.Date("2021-04-01")))+
  scale_y_continuous("Positivity rate (%)",breaks = seq(0,0.15,0.05),
                     labels = seq(0,15,5), expand = c(0, 0),limits = c(-0.01/2,0.3/2))+
  theme_bw()+
  theme(
    # axis.title.y = element_blank(),
        # axis.text.y = element_text(color = "darkred")
        )+
  theme(axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        legend.title = element_blank(),
        # axis.title.y.right = element_text(color = "darkred")
        )+
  scale_color_manual("Regions",values = value)+
  scale_fill_manual("Regions",values = value)+
  theme(legend.position = "none") -> p10

ggplot(epi_glo_flu_region2) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 0.15,alpha = 0.2,fill = colors[5])+
  geom_ribbon(aes(x = date, ymin = BV_num_roll_LL/test_roll,ymax = BV_num_roll_UL/test_roll, fill = region_final),
              stat = "identity",color = "transparent", alpha = 0.4) +
  geom_line(aes(x = date, y = BV_num_roll/test_roll, color = region_final))+
  scale_x_date("Date", date_labels = "%b %Y", breaks = c(as.Date("2020-04-01"),as.Date("2020-10-01"),as.Date("2021-04-01")))+
  scale_y_continuous("Positivity rate (%)",breaks = seq(0,0.15,0.05),
                     labels = seq(0,15,5), expand = c(0, 0),limits = c(-0.01/2,0.3/2))+
  theme_bw()+
  theme(
    # axis.title.y = element_blank(),
        # axis.text.y = element_text(color = "darkred")
        )+
  theme(axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        legend.title = element_blank(),
        axis.title.y.right = element_text(color = "darkred"))+
  scale_color_manual("Regions",values = value)+
  scale_fill_manual("Regions",values = value)+
  theme(legend.position = "none")-> p11

ggplot()+theme(panel.background = element_rect(fill = "transparent", color = "transparent"))+
  labs(y = "")-> f_n

#=========================================================================
#we didn't provide the map data as below due to the data sharing agreement
#=========================================================================
library(sf)
library(rgdal)
library(rnaturalearth)

ggplot() + 
  geom_sf(data = world, color = "grey50", fill = "grey92") +
  geom_sf(data = world1[world1$region_final %in% c("Southern Asia","South-eastern Asia","Africa"),], aes(fill = region_final), color = "grey50", linewidth = 0.1) +
  geom_sf(data = china, color = "grey50", fill = "white") +
  geom_sf(data = china, color = "grey50", fill = "grey92")+
  geom_sf(data = nine, color="grey50",linewidth = 0.01)+
  annotate("rect", xmin = -180, xmax = 180, ymin = -23.27, ymax = 23.27,size = 0.1, alpha = 0.3, fill = "lightblue")+
  geom_hline(yintercept = c(-35, 35), size = 0.1, linetype = 2, color = "red")+
  annotate("text", x = -180, y = 0, label = "Tropics", hjust = 0, size = 2)+
  annotate("text", x = -180, y = -29, label = "Subtropics", hjust = 0, size = 2)+
  annotate("text", x = -180, y = 29, label = "Subtropics", hjust = 0, size = 2)+
  scale_x_continuous(expand = c(0.03,0))+
  scale_y_continuous(expand = c(0.01,0))+
  theme_void()+
  theme(legend.position = "top")+
  guides(fill = F)+
  scale_fill_manual("Geographic regions",values = value) -> map

#==output==
library(patchwork)
library(grid)

pdf("Fig1.pdf",height = 10, width = 10)
(fig1|fig2)/
  ((p1_2|p9)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.4)))/
  ((p2_2|p10)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.402)))/
     ((p3_2|p11)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.4)))/
        ((p4_2|f_n)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.442)))/
           plot_annotation(tag_levels = "a")
viewport(x = 0.71, y = 0.015, width = 0.3, height = 0.18, just = c("left", "bottom")) -> vp1
print(map,vp = vp1)
dev.off()

tiff("Fig1.tif", width = 10, height = 10,
      units = "in", compression = "lzw", res = 400)
(fig1|fig2)/
  ((p1_2|p9)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.4)))/
  ((p2_2|p10)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.402)))/
  ((p3_2|p11)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.4)))/
  ((p4_2|f_n)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.442)))/
  plot_annotation(tag_levels = "a")
viewport(x = 0.71, y = 0.015, width = 0.3, height = 0.18, just = c("left", "bottom")) -> vp1
print(map,vp = vp1)
dev.off()