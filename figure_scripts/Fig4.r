library(ggridges)
library(ggplot2)
library(stringr)
library(Biostrings)
library(ape)
library(treeio)
library(adephylo)
library(tidyverse)
library(gtools)
library(dplyr)
library(timeDate)
library(readxl)
library(sf)
library(rgdal)
library(ggtree)
library(ggsci)
library(lubridate)
library(circlize)
library(coda)
library(cowplot)
library(scales)
library(castor)
library(lubridate)
library(patchwork)
library(grid)

#==define color==
colors <- c(pal_npg("nrc", alpha =1)(10)[c(1:7,9:10)],"darkred","#FADDA9","grey80")
colors1 <- c(pal_aaas("default", alpha =0.7)(10))
show_col(colors)
show_col(colors1)
value = c("Japan/Korea" = colors[1],"Western Asia" = colors[3],"WesternAsia" = colors[3],"North America" = colors[6],
          "Northern America" = colors[6],"North Am" = colors[6],"NorthernAmerica" = colors[6],
          "South-eastern Asia"= colors[4], "Southern Asia"= colors[5],"SoutheasternAsia"= colors[4], "SouthernAsia"= colors[5],
          "Europe"= colors[2], "Oceania"= colors[7],"NorthChina" = colors[8], "SouthChina" = colors[11],
          "North China" = colors[8], "South China" = colors[11], "China (N)" = colors[8], "China (S)" = colors[11],
          "Russia"= colors[10],  "Southern America"= colors[12],  "South Am"= colors[12],  "SouthernAmerica"= colors[12],"South America"= colors[12],
          "Africa"= colors[9], "Americas" = colors1[1], "Asia" = colors1[2], "China" = colors1[4])

#==1. effective pop size==
h1n1_even <- read.delim("../genomic_part/post-analyses/pop_size/h1n1_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "H1N1pdm09") %>%
  filter(date >= as.Date("2012-01-01"))
h3n2_even <- read.delim("../genomic_part/post-analyses/pop_size/h3n2_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "H3N2") %>%
  filter(date >= as.Date("2012-01-01"))
bv_even <- read.delim("../genomic_part/post-analyses/pop_size/bv_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "B/Victoria") %>%
  filter(date >= as.Date("2012-01-01"))
by_even <- read.delim("../genomic_part/post-analyses/pop_size/by_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "B/Yamagata") %>%
  filter(date >= as.Date("2012-01-01"))
pop_size <- rbind(h1n1_even, h3n2_even, bv_even, by_even)
factor(pop_size$type, levels = unique(pop_size$type)) -> pop_size$type

ggplot(data = pop_size) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 300,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = 0,ymax = 300,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = type), alpha = 0.2)+
  geom_line(aes(x = date, y = mean, color = type, group = type))+
  geom_point(aes(x = date, y = mean, fill = type), shape = 21)+
  scale_y_log10(expand = c(0,0))+
  scale_x_date(expand = c(0.01,0),date_labels = "%Y",date_breaks = "1 year",
               limits = c(as.Date("2012-03-01"),as.Date("2024-04-01")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.grid.minor.x = element_blank(),
        legend.background = element_blank())+
  labs(x = "Year", y = "Relative genetic diversity", tag = "b")+
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))+
  scale_color_manual("",values = colors1[c(1:3,9)])+
  scale_fill_manual("",values = colors1[c(1:3,9)])-> p2

#==2. MCC tree==
#B/Yamagata
tree_by <- read.beast("../genomic_part/post-analyses/mcc_tree/by_from2011_mcc.tre")
tree_by@phylo$edge.length[tree_by@phylo$edge.length < 0] <- 0.00001
by_even_meta <- read.csv("metadata_by_even1.csv") %>% select(c("seqName", "region_final")) #NOT provided
names(by_even_meta)[1] <- "taxa"

ggtree(tree_by, mrsd = as.Date("2020-03-20"), as.Date=TRUE,color='grey40',size=0.1) %<+% by_even_meta + geom_tippoint(aes(fill = region_final),size=1.5, color='black',shape=21, stroke=0.1)+
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -50,ymax = 1600,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = -50,ymax = 1600,alpha = 0.2,fill = colors[7])+
  theme_tree2()+
  scale_x_date("Date",date_labels = "%Y",date_breaks = "2 year", expand = c(0,0), limits = c(as.Date("2005-01-01"), as.Date("2024-01-01")))+
  scale_fill_manual("Geographic regions", values = value,
                    labels = c("Africa","Europe", "Japan/Korea","North America","Northern China",
                               "Oceania",  "Russia","Southeast Asia","South America","Southern China",
                                "South Asia", "West Asia"))+
  scale_y_continuous(expand = c(0,0), limits = c(-50, 1600))+
  theme(
    axis.title.x = element_blank(),
    panel.border = element_rect(fill = "transparent", color = "transparent"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.line.x = element_line(linewidth = 0.2),
    plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.background =  element_rect(fill = "transparent", color = "transparent"),
    panel.grid.major.x = element_line(color = "grey90"))+
  guides(fill =guide_legend(keywidth =0.15, keyheight =0.15, default.unit ="inch"))+
  labs(tag = "a")-> p1

by_even_meta1 <- read.csv("metadata_by_even1.csv") %>% select(c("seqName", "clade")) #NOT provided
names(by_even_meta1)[1] <- "taxa"
ggtree(tree_by, mrsd = as.Date("2020-03-20"), as.Date=TRUE,color='grey40',size=0.1) %<+% 
  by_even_meta1 + geom_tippoint(aes(fill = clade),size=1, color='black',shape=21, stroke=0.1)+
  theme_tree2() +
  scale_x_date("Date",date_labels = "%Y",date_breaks = "3 year", expand = c(0,0))+
  scale_fill_manual("B/Yamagata clades", values = colors1[c(2,5,4)])+
  theme(legend.position = c(0.2,0.65))+
  scale_y_continuous(expand = c(0.03,0))+
  theme(
    axis.title.x = element_blank(),
    panel.border = element_rect(fill = "transparent", color = "transparent"),
    axis.line.x = element_line(linewidth = 0.1),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
    legend.background = element_rect(fill = "transparent"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) -> p1_1

#==3. Genetic diversity==
diver_h1n1 <- read.delim("../genomic_part/post-analyses/diversity/h1n1_diversity/out.skylines") %>%
  mutate(virus_type = "H1N1pdm09")
diver_h3n2 <- read.delim("../genomic_part/post-analyses/diversity/h3n2_diversity/out.skylines") %>%
  mutate(virus_type = "H3N2")
diver_bv <- read.delim("../genomic_part/post-analyses/diversity/bv_diversity/out.skylines") %>%
  mutate(virus_type = "B/Victoria")
diver_by <- read.delim("../genomic_part/post-analyses/diversity/by_diversity/out.skylines") %>%
  mutate(virus_type = "B/Yamagata")

diver <- rbind(diver_h1n1, diver_h3n2, diver_bv, diver_by) %>% 
  filter(!is.nan(mean)) %>%
  mutate(date = decimal2Date(time))

factor(diver$virus_type, levels = unique(diver$virus_type)) -> diver$virus_type
ggplot(data = diver[diver$date >= as.Date("2011-01-01") & diver$date <= as.Date("2023-12-20"),]) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 12,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-03-31"),
           ymin = 0,ymax = 12,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = virus_type), alpha = 0.2)+
  geom_line(aes(x = date, y = mean, color = virus_type, group = virus_type))+
  geom_point(aes(x = date, y = mean, color = virus_type))+
  scale_x_date(expand = c(0.01,0),date_labels = "%Y",date_breaks = "1 year",
               limits = c(as.Date("2012-03-01"),as.Date("2024-04-01")))+
  theme_bw()+
  theme(legend.position = "none",
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.background = element_blank())+
  labs(x = "Year", y = "Mean pairwise diversity", tag = "c")+
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))+
  scale_y_continuous(limits = c(0,12), expand = c(0,0))+
  scale_color_manual("",values = colors1[c(1:3,9)])+
  scale_fill_manual("",values = colors1[c(1:3,9)]) -> fig_c

#==output
pdf("Fig4.pdf", width = 8, height = 8)
(p1/((p2/fig_c)+plot_layout(guides = "collect")))+plot_layout(heights = c(0.5,1))
viewport(x = 0.04, y = 0.79, width = 0.29, height = 0.19, just = c("left", "bottom")) -> vp3
print(p1_1, vp = vp3)
dev.off()