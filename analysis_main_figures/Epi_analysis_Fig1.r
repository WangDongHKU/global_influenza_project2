#==load packages==
library(stringr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggpubr)
library(rworldmap)
library(zoo)
library(patchwork)
library(ggsci)
library(scales)
library(sf)
library(rgdal)
library(rnaturalearth)
library(grid)

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

#==read data==
h1n1_num0 <- read.csv("../data/genomic_data/h1n1_seq_num.csv") %>% mutate(date = as.Date(date))
h3n2_num0 <- read.csv("../data/genomic_data/h3n2_seq_num.csv") %>% mutate(date = as.Date(date))
bv_num0 <- read.csv("../data/genomic_data/bv_seq_num.csv") %>% mutate(date = as.Date(date))
by_num0 <- read.csv("../data/genomic_data/by_seq_num.csv") %>% mutate(date = as.Date(date))

h1n1_num <- h1n1_num0 %>% group_by(date) %>% summarise(h1n1_seq = sum(h1n1_seq))
h3n2_num <- h3n2_num0 %>% group_by(date) %>% summarise(h3n2_seq = sum(h3n2_seq))
bv_num <- bv_num0 %>% group_by(date) %>% summarise(bv_seq = sum(bv_seq))
by_num <- by_num0 %>% group_by(date) %>% summarise(by_seq = sum(by_seq))

epi_glo_flu_region1 <- read.csv("../data/epi_data/epi_glo_flu_region1_no_roll.csv") %>% mutate(date = as.Date(date) + 3); range(epi_glo_flu_region1$date)
epi_glo_flua1 <- read.csv("../data/epi_data/epi_glo_flua1_no_roll.csv") %>% mutate(date = as.Date(date) + 3); range(epi_glo_flua1$date)
epi_glo_flub1 <- read.csv("../data/epi_data/epi_glo_flub1_no_roll.csv") %>% mutate(date = as.Date(date) + 3); range(epi_glo_flub1$date)

unique(epi_glo_flu_region1$region_final)
epi_glo_flua1 <- left_join(epi_glo_flua1, h1n1_num) %>% left_join(h3n2_num)
epi_glo_flub1 <- left_join(epi_glo_flub1, bv_num) %>% left_join(by_num)

#==Figure 1==
#PNAEL c,e,f,g
ggplot(data = h1n1_num0) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -20,ymax = 1200,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -20,ymax = 1200,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, y = h1n1_seq, fill = region), stat = "identity")+
  geom_ribbon(data = epi_glo_flua1, aes(x = date,
                                        ymin = h1n1_num_roll_LL/test_roll*5000,
                                        ymax = h1n1_num_roll_UL/test_roll*5000),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flua1, aes(x = date, y = h1n1_num_roll/test_roll*5000),
            stat = "identity",size = 0.5,color = "darkred")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%b\n%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     limits = c(-20,1200),
                     breaks = seq(0,1200,300),
                     expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/5000,
                                         name="Weekly moving pos. rate (%)",
                                         breaks = seq(0,0.2,0.05),
                                         labels = seq(0,20,5)
                     ))+
  theme_bw()+
  scale_fill_manual("Continents",values = value)+
  theme(legend.position = c(0.35,0.88))+
  theme(axis.text.y.right = element_text(color = "darkred"),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.3,"cm"),
        legend.background = element_blank(),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        axis.title.y.right = element_text(color = "darkred"),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(nrow = 1))+
  labs(subtitle = "H1N1pdm09") -> p1_2

ggplot(data = h3n2_num0) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -40,ymax = 2000,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -40,ymax = 2000,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, y = h3n2_seq, fill = region), stat = "identity")+
  geom_ribbon(data = epi_glo_flua1, aes(x = date, 
                                        ymin = h3n2_num_roll_LL/test_roll*8500,
                                        ymax = h3n2_num_roll_UL/test_roll*8500),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flua1 ,
            aes(x = date, y = h3n2_num_roll/test_roll*8500), 
            stat = "identity",size = 0.5,color = "darkred")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%b\n%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     limits = c(-40,2000),
                     breaks = seq(0,2000,500),
                     expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/8500,
                                         name="Weekly moving pos. rate (%)",
                                         breaks = seq(0,0.2,0.05),
                                         labels = seq(0,20,5)
                     ))+
  theme_bw()+
  scale_fill_manual("Continents",values = value)+
  theme(legend.position = 'none')+
  theme(axis.text.y.right = element_text(color = "darkred"),
        axis.title.x = element_blank(),
        text = element_text(size = 7),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        axis.title.y.right = element_text(color = "darkred"))+
  labs(subtitle = "H3N2") -> p2_2

ggplot(data = bv_num0) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, y = bv_seq, fill = region), stat = "identity")+
  geom_ribbon(data = epi_glo_flub1, aes(x = date, ymin = BV_num_roll_LL/test_roll*5000,
                                        ymax = BV_num_roll_UL/test_roll*5000),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flub1 ,
            aes(x = date, y = BV_num_roll/test_roll*5000), stat = "identity",size = 0.5,color = "darkred")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%b\n%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     breaks = seq(0,600,150),
                     limits = c(-10,600),
                    
                     expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/5000,
                                         name="Weekly moving pos. rate (%)",
                                         breaks = seq(0,0.12,0.03),
                                         labels = seq(0,12,3)
                     ))+
  theme_bw()+
  scale_fill_manual("Continents",values = value)+
  theme(axis.text.y.right = element_text(color = "darkred"),
        axis.title.x = element_blank(),
        text = element_text(size = 7),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        axis.title.y.right = element_text(color = "darkred"))+
  theme(legend.position = "none")+
  labs(subtitle = "B/Victoria")-> p3_2

ggplot(data = by_num0) + 
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -10,ymax = 600,alpha = 0.2,fill = colors[7])+
  geom_bar(aes(x = date, y = by_seq, fill = region), stat = "identity")+
  geom_ribbon(data = epi_glo_flub1[epi_glo_flub1$date <= as.Date("2020-03-31"),],
              aes(x = date, ymin = BY_num_roll_LL/test_roll*3500,
                                        ymax = BY_num_roll_UL/test_roll*3500),
              stat = "identity",fill = "darkred", alpha = 0.4)+
  geom_line(data = epi_glo_flub1[epi_glo_flub1$date <= as.Date("2020-03-31"),] ,
            aes(x = date, y = BY_num_roll/test_roll*3500), stat = "identity",size = 0.5,color = "darkred")+
  scale_x_date("", date_breaks = "1 year",date_labels = "%b\n%Y",expand = c(0.01, 0),
               limits = c(as.Date("2017-01-01"),as.Date("2024-03-31")))+
  scale_y_continuous("No. of HA sequences", 
                     breaks = seq(0,600,150),
                     limits = c(-10,600),
                      expand = c(0,0),
                     sec.axis = sec_axis(trans=~.* 1/3500,
                                         name="Weekly moving pos. rate (%)",
                                         breaks = seq(0,0.15,0.05),
                                         labels = seq(0,15,5)
                     ))+
  theme_bw()+
  geom_segment(aes(x= as.Date("2020-03-11") , y= 200 , xend= as.Date("2020-03-11") , yend= 0 ), 
               arrow = arrow(length=unit(0.2, 'cm')),lwd= 0.28)+
  annotate("text", x = as.Date("2020-03-11"), y = 300,size =2, label = "WHO declared\na pandemic")+
  geom_segment(aes(x= as.Date("2021-04-01") , y= 400 , xend= as.Date("2021-04-01") , yend= 0 ), 
               arrow = arrow(length=unit(0.2, 'cm')),lwd= 0.28)+
  annotate("text", x = as.Date("2021-04-01"), y = 500,size =2, label = "International air travel\nstart to resume")+
  geom_segment(aes(x= as.Date("2023-05-05") , y= 200 , xend= as.Date("2023-05-05") , yend= 0 ), 
               arrow = arrow(length=unit(0.2, 'cm')),lwd= 0.28)+
  annotate("text", x = as.Date("2023-05-05"), y = 300,size =2, label = "WHO declared end\nto COVID-19’s emergency")+
  scale_fill_manual("Continents",values = value)+
  theme(legend.position = "none")+
  theme(axis.text.y.right = element_text(color = "darkred"),
        panel.grid.major  = element_blank(),
        text = element_text(size = 7),
        panel.grid.minor  = element_blank(),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        axis.title.y.right = element_text(color = "darkred"))+
  labs(subtitle = "B/Yamagata")-> p4_2

(p1_2/p2_2/p3_2/p4_2)

#PANEL a,b
table(epi_glo_flua1$test == epi_glo_flub1$test)
ggplot(data = epi_glo_flua1[epi_glo_flua1$date >= as.Date("2017-01-01"),]) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 600000,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = 0,ymax = 600000,alpha = 0.2,fill = colors[7])+
  annotate("text", x= c(as.Date("2018-08-15")),y = 500000, size = 2,
           label = c("Pre-pandemic\nperiod"))+
  geom_segment(aes(xend= as.Date("2020-04-15") , y= 500000 , x= as.Date("2021-03-01") , yend= 500000), 
               arrow = arrow(length=unit(0.1, 'cm')),lwd= 0.28)+
  geom_segment(aes(xend= as.Date("2023-04-15") , y= 500000 , x= as.Date("2022-06-01") , yend= 500000 ), 
               arrow = arrow(length=unit(0.1, 'cm')),lwd= 0.28)+
  annotate("text", x= c(as.Date("2021-10-01")),y = 500000, size = 2,
           label = c("Pandemic\nperiod"))+
  annotate("text", x= c(as.Date("2020-10-01")),y = 400000, size = 1.8,
           label = c("(Acute phase)"), color = "red")+
  annotate("text", x= c(as.Date("2022-04-15")),y = 400000, size = 1.8,
           label = c("(Transition phase)"), color = "blue")+
  annotate("text", x= c(as.Date("2023-10-31")),y = 500000, size = 2,
           label = c("Post-\npandemic\nperiod"))+
  geom_line(aes(x = date, y = test_roll))+
  scale_x_date("Date", date_breaks = "1 year", date_labels = "%b\n%Y", expand = c(0.01, 0))+
  scale_y_continuous("No. of specimens (× 1k)", labels = seq(0,600,200), breaks = seq(0,600000,200000),
                     limits = c(0, 600000),expand = c(0,0))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),)+
  labs(subtitle = "Intensity of virological surveillance")-> fig1

meta <- rbind(h1n1_num %>% mutate(type = "h1n1") %>% rename(seq = "h1n1_seq"),
              h3n2_num %>% mutate(type = "h3n2") %>% rename(seq = "h3n2_seq"),
              bv_num %>% mutate(type = "bv") %>% rename(seq = "bv_seq"),
              by_num %>% mutate(type = "by") %>% rename(seq = "by_seq"))
seq_epi <- meta %>%
  group_by(date) %>%
  summarise(seq_no = sum(seq)) %>%
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
  geom_line(aes(x = date, y = seq_no_roll/epi_num_roll))+
  scale_x_date("Date", date_breaks = "1 year", date_labels = "%b\n%Y", expand = c(0.01, 0) )+
  scale_y_continuous("Prop. of lab-confirmed\ninfluenza cases\nsequenced (%)",
                     expand = c(0,0),limits = c(0,0.3),
                     breaks = seq(0,0.3,0.1),labels = seq(0, 30, 10))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        plot.background = element_rect(fill = "transparent", color = "transparent"))+
  labs(subtitle = "Intensity of genomic surveillance")-> fig2

#panel d,f,h
epi_glo_flu_region2 <- epi_glo_flu_region1[epi_glo_flu_region1$region_final %in% c("Africa", "South-eastern Asia", "Southern Asia"),]
epi_glo_flu_region2 <- epi_glo_flu_region2[(epi_glo_flu_region2$date >= as.Date("2020-03-01") & 
                                             epi_glo_flu_region2$date < as.Date("2021-05-01")),]

ggplot(epi_glo_flu_region2) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 0.15,alpha = 0.2,fill = colors[5])+
  geom_ribbon(aes(x = date, ymin = h1n1_num_roll_LL/test_roll,ymax = h1n1_num_roll_UL/test_roll, fill = region_final),
              stat = "identity",color = "transparent", alpha = 0.4) +
  geom_line(aes(x = date, y = h1n1_num_roll/test_roll, color = region_final))+
  scale_x_date("Date", date_labels = "%b\n%Y", breaks = c(as.Date("2020-04-01"),as.Date("2020-10-01"),as.Date("2021-04-01")))+
  scale_y_continuous("Weekly moving pos. rate (%)",breaks = seq(0,0.3/2,0.1/2),
                     labels = seq(0,15,5), expand = c(0, 0),limits = c(-0.01/2,0.3/2))+
  theme_bw()+
  scale_color_manual("Regions",values = value,
                     labels = c("Africa",  "Southeast Asia", "South Asia"))+
  scale_fill_manual("Regions",values = value,
                     labels = c("Africa",  "Southeast Asia", "South Asia"))+
  theme(axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        # legend.background = element_blank(),
        text = element_text(size = 7),
        legend.key = element_rect(fill = "transparent", color ="transparent"),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.key.size = unit(0.3,"cm"),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        # legend.title = element_blank(),
        # axis.title.y.right = element_text(color = "darkred")
        )+
  theme(legend.position = c(0.5,0.65))-> p9

ggplot(epi_glo_flu_region2) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 0.15,alpha = 0.2,fill = colors[5])+
  geom_ribbon(aes(x = date, ymin = h3n2_num_roll_LL/test_roll,ymax = h3n2_num_roll_UL/test_roll, fill = region_final),
              stat = "identity",color = "transparent", alpha = 0.4) +
  geom_line(aes(x = date, y = h3n2_num_roll/test_roll, color = region_final))+
  scale_x_date("Date", date_labels = "%b\n%Y", breaks = c(as.Date("2020-04-01"),as.Date("2020-10-01"),as.Date("2021-04-01")))+
  scale_y_continuous("Weekly moving pos. rate (%)",breaks = seq(0,0.15,0.05),
                     labels = seq(0,15,5), expand = c(0, 0),limits = c(-0.01/2,0.3/2))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
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
  scale_x_date("Date", date_labels = "%b\n%Y", breaks = c(as.Date("2020-04-01"),as.Date("2020-10-01"),as.Date("2021-04-01")))+
  scale_y_continuous("Weekly moving pos. rate (%)",breaks = seq(0,0.15,0.05),
                     labels = seq(0,15,5), expand = c(0, 0),limits = c(-0.01/2,0.3/2))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        text = element_text(size = 7),
        legend.title = element_blank(),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        axis.title.y.right = element_text(color = "darkred"))+
  scale_color_manual("Regions",values = value)+
  scale_fill_manual("Regions",values = value)+
  theme(legend.position = "none")-> p11

ggplot()+theme(panel.background = element_rect(fill = "transparent", color = "transparent"))+
  labs(y = "")+
  theme(axis.title.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        text = element_text(size = 7),
        legend.title = element_blank(),
        plot.tag = element_text(size = 10, face = "bold"))-> f_n #blank panel

#panel j
who_country <- read_xlsx("../data/map_data/WHO_country.xlsx")
world_region <- read.csv("../data/map_data/list_location.csv") %>% filter(alpha.3 %in% c(who_country$ISO3,"HKG","TWN","MAC"))
world_region$alpha.2[world_region$name == "Namibia"] <- "NAA"
world_region$region_final <- world_region$region
world_region$region_final[world_region$alpha.3 == "RUS"] <- "Russia"
world_region$region_final[world_region$sub.region == "Northern America"] <- "North America"
world_region$region_final[world_region$intermediate.region == "South America"] <- "South America"
world_region$region_final[world_region$region_final == "Asia"] <- world_region$sub.region[world_region$region_final == "Asia"]   
world_region$region_final[world_region$alpha.3 %in% c("KOR","JPN")] <- "Japan/Korea"
world_region$region_final[world_region$alpha.3 %in% c("HKG","TWN","MAC", "CHN")] <- "China"
world_region <- world_region %>% filter(!region_final %in% c("Americas","Eastern Asia","Central Asia"))

world <- ne_countries(scale = "medium", returnclass = "sf") %>% mutate(adm0_a3 = ifelse(adm0_a3 == "SDS", "SSD" ,adm0_a3)) %>% filter(!sovereignt %in% c("China", "Taiwan")) %>% filter(admin != "Antarctica")
world_region$alpha.3[(world_region$alpha.3 %in% world$adm0_a3) == F]
world <- left_join(world[,c(4,5,10,11,169)], world_region[,c(3,12)], by = c("adm0_a3" = "alpha.3")) 
world1 <- world[,c(5,6)] %>% filter(!is.na(region_final)) %>% filter(!region_final %in% c("Oceania", "Russia")) %>% group_by(region_final) %>% summarise()
world <- st_transform(world, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
world1 <- st_transform(world1, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

# Map files (shp.) in China at country- and provice- resolution can be downloaded from https://github.com/GaryBikini/ChinaAdminDivisonSHP
china <- st_read("ChinaAdminDivisonSHP-master/1. Country/country.shp") 
china <- st_transform(china, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
nine <- st_read("../data/map_data/nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

ggplot() + 
  geom_sf(data = world, color = "grey50", fill = "grey92", linewidth = 0.05) +
  # geom_sf(data = world[world$region_final %in% c("Oceania", "Russia"),], aes(fill = region_final)) +
  geom_sf(data = world1[world1$region_final %in% c("Southern Asia","South-eastern Asia","Africa"),], aes(fill = region_final), color = "grey50", linewidth = 0.05) +
  geom_sf(data = china, color = "grey50", fill = "white", linewidth = 0.05) +
  geom_sf(data = china, color = "grey50", fill = "grey92", linewidth = 0.05)+
  geom_sf(data = nine, color="grey50",linewidth = 0.01)+
  annotate("rect", xmin = -180, xmax = 180, ymin = -23.27, ymax = 23.27,size = 0.1, alpha = 0.3, fill = "lightblue")+
  geom_hline(yintercept = c(-35, 35), size = 0.1, linetype = 2, color = "red")+
  annotate("text", x = -180, y = 0, label = "Tropics", hjust = 0, size = 1.8)+
  annotate("text", x = -180, y = -29, label = "Subtropics", hjust = 0, size = 1.8)+
  annotate("text", x = -180, y = 29, label = "Subtropics", hjust = 0, size = 1.8)+
  scale_x_continuous(expand = c(0.03,0))+
  scale_y_continuous(expand = c(0.01,0))+
  theme_void()+
  theme(legend.position = "top")+
  guides(fill = F)+
  scale_fill_manual("Geographic regions",values = value)-> map

#==output==
pdf("Fig1.pdf",height = 8.5, width = 7.25)
(fig1|fig2)/
  ((p1_2|p9)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.4)))/
  ((p2_2|p10)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.402)))/
     ((p3_2|p11)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.4)))/
        ((p4_2|f_n)+plot_layout(nrow = 1, ncol = 2,widths = c(1,0.442)))/
           plot_annotation(tag_levels = "A")
viewport(x = 0.7, y = 0.015, width = 0.3, height = 0.18, just = c("left", "bottom")) -> vp1
print(map,vp = vp1)
dev.off()