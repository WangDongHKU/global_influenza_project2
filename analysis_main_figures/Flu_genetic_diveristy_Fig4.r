library(ggplot2)
library(stringr)
library(ape)
library(treeio)
library(adephylo)
library(tidyverse)
library(gtools)
library(dplyr)
library(timeDate)
library(readxl)
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

#==1. effective pop size (panel b)==
h1n1_even <- read.delim("../analysis_others/pop_size/h1n1_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "H1N1pdm09") %>%
  filter(date >= as.Date("2012-01-01"))
h3n2_even <- read.delim("../analysis_others/pop_size/h3n2_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "H3N2") %>%
  filter(date >= as.Date("2012-01-01"))
bv_even <- read.delim("../analysis_others/pop_size/bv_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "B/Victoria") %>%
  filter(date >= as.Date("2012-01-01"))
by_even <- read.delim("../analysis_others/pop_size/by_even_pop_size.txt") %>%
  mutate(date = as.Date(date), type = "B/Yamagata") %>%
  filter(date >= as.Date("2012-01-01"))
pop_size <- rbind(h1n1_even, h3n2_even, bv_even, by_even)
factor(pop_size$type, levels = unique(pop_size$type)) -> pop_size$type

ggplot(data = pop_size) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 300,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = 0,ymax = 300,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = type), alpha = 0.2)+
  geom_line(aes(x = date, y = mean, color = type, group = type))+
  geom_point(aes(x = date, y = mean, fill = type), shape = 21, color = "transparent", size = 1.5)+
  scale_y_log10(expand = c(0,0))+
  scale_x_date(expand = c(0.01,0),date_labels = "%b\n%Y",date_breaks = "1 year",
               limits = c(as.Date("2012-03-01"),as.Date("2024-04-01")))+
  theme_bw()+
  theme(legend.position = c(0.2, 0.92),
        axis.title.x = element_blank(),
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        legend.key.width = unit(0.3,"cm"),
        legend.key.height = unit(0.3,"cm"),
        plot.tag = element_text(size = 10, face = "bold"),
        plot.background =  element_rect(fill = "transparent",color = "transparent"),
        legend.background = element_blank())+
  labs(x = "Year", y = "Relative genetic diversity", tag = "B")+
  guides(fill = guide_legend(ncol = 2),
         color = guide_legend(ncol = 2))+
  scale_color_manual("",values = colors1[c(1:3,9)])+
  scale_fill_manual("",values = colors1[c(1:3,9)])-> p2

#==MCC tree (panel a)==
#B/Yamagata
tree_by <- read.beast("../analysis_others/mcc_tree/by_from2011_mcc.tre")
tree_by@phylo$edge.length[tree_by@phylo$edge.length < 0] <- 0.00001
by_even_meta <- read.csv("../data/genomic_data/metadata_by.csv") %>% select(c("seqName", "region_final"))
names(by_even_meta)[1] <- "taxa"

ggtree(tree_by, mrsd = as.Date("2020-03-20"), as.Date=TRUE,color='grey40',size=0.1) %<+% 
  by_even_meta + geom_tippoint(aes(fill = region_final),size=1.2, color='transparent',shape=21, stroke=0.1)+
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -50,ymax = 1600,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -50,ymax = 1600,alpha = 0.2,fill = colors[7])+
  theme_tree2()+
  scale_x_date("Date",date_labels = "%b\n%Y",date_breaks = "2 year", expand = c(0,0), limits = c(as.Date("2005-01-01"), as.Date("2023-08-01")))+
  scale_fill_manual("Geographic regions", values = value,
                    labels = c("Africa","Europe", "Japan/Korea","North America","Northern China",
                               "Oceania",  "Russia","Southeast Asia","South America","Southern China",
                                "South Asia", "West Asia"))+
  scale_y_continuous(expand = c(0,0), limits = c(-50, 1600))+
  theme(
    axis.title.x = element_blank(),
    panel.border = element_rect(fill = "transparent", color = "transparent",linewidth = 0.3),
    plot.subtitle = element_text(hjust = 0.5),
    axis.line.x = element_line(linewidth = 0.2),
    plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
    text = element_text(size = 7),
    legend.key.spacing.y  = unit(-0.1, 'cm'),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.background =  element_rect(fill = "transparent", color = "transparent"),
    # panel.grid.major.x = element_line(color = "grey90"),
    panel.grid = element_blank()
    )+
  guides(fill =guide_legend(keywidth =0.15, keyheight =0.15, default.unit ="inch"))+
  labs(tag = "A")-> p1

by_even_meta1 <- read.csv("../data/genomic_data/metadata_by.csv") %>% select(c("seqName", "clade"))
names(by_even_meta1)[1] <- "taxa"
ggtree(tree_by, mrsd = as.Date("2020-03-20"), as.Date=TRUE,color='grey40',size=0.1) %<+% 
  by_even_meta1 + geom_tippoint(aes(fill = clade),size=1, color='transparent',shape=21, stroke=0.1)+
  theme_tree2() +
  scale_x_date("Date",date_labels = "%Y",date_breaks = "3 year", expand = c(0.05,0))+
  scale_fill_manual("B/Yamagata clades", values = colors1[c(2,5,4)])+
  scale_y_continuous(expand = c(0.03,0))+
  annotate("text", x = as.Date("2007-03-01"), y = 1300, label = "B/Yamagata clades", hjust = 0, size = 1.5)+
  theme(
    legend.position = c(0.2,0.65),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_rect(fill = "transparent", color = "black",linewidth = 0.3, linetype = 2),
    plot.subtitle = element_text(hjust = 0.5),
    axis.line.x = element_blank(),
    plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
    text = element_text(size = 5),
    legend.key.spacing.y  = unit(-0.4, 'cm'),
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    # legend.key.size = unit(0.4, 'cm'),
    # legend.spacing.y = unit(-0.1, 'cm'),
    axis.ticks = element_blank(),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.background =  element_rect(fill = "transparent", color = "transparent"),
    panel.background = element_rect(fill = "transparent",color = "transparent"),
    plot.background =  element_rect(fill = "transparent",color = "transparent"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())-> p1_1

#==Genetic diversity (panel c)==
diver_h1n1 <- read.delim("../analysis_others/diversity/h1n1_diversity/out.skylines") %>%
  mutate(virus_type = "H1N1pdm09")
diver_h3n2 <- read.delim("../analysis_others/diversity/h3n2_diversity/out.skylines") %>%
  mutate(virus_type = "H3N2")
diver_bv <- read.delim("../analysis_others/diversity/bv_diversity/out.skylines") %>%
  mutate(virus_type = "B/Victoria")
diver_by <- read.delim("../analysis_others/diversity/by_diversity/out.skylines") %>%
  mutate(virus_type = "B/Yamagata")

diver <- rbind(diver_h1n1, diver_h3n2, diver_bv, diver_by) %>% 
  filter(!is.nan(mean)) %>%
  mutate(date = decimal2Date(time))

factor(diver$virus_type, levels = unique(diver$virus_type)) -> diver$virus_type
ggplot(data = diver[diver$date >= as.Date("2011-01-01") & diver$date <= as.Date("2023-12-20"),]) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = 0,ymax = 12,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = 0,ymax = 12,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = virus_type), alpha = 0.2)+
  geom_line(aes(x = date, y = mean, color = virus_type, group = virus_type))+
  geom_point(aes(x = date, y = mean, color = virus_type), size = 1)+
  scale_x_date(expand = c(0.01,0),date_labels = "%b\n%Y",date_breaks = "1 year",
               limits = c(as.Date("2012-03-01"),as.Date("2024-04-01")))+
  theme_bw()+
  theme(legend.position = "none",
        panel.border = element_rect(linewidth = 0.3),
        plot.subtitle = element_text(hjust = 0.5),
        axis.line.x = element_line(linewidth = 0.2),
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        text = element_text(size = 7),
        legend.key.spacing.y  = unit(-0.3, 'cm'),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        legend.background =  element_rect(fill = "transparent", color = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  labs(x = "Date", y = "Mean pairwise diversity", tag = "C")+
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))+
  scale_y_continuous(limits = c(0,12), expand = c(0,0))+
  scale_color_manual("",values = colors1[c(1:3,9)])+
  scale_fill_manual("",values = colors1[c(1:3,9)]) -> fig_c

#==output
pdf("Fig4.pdf", width = 4.75, height = 6)
viewport(x = 0.015, y = 0.65, width = 0.99, height = 0.35, just = c("left", "bottom")) -> vp1
viewport(x = 0, y = 0, width = 1, height = 0.65, just = c("left", "bottom")) -> vp2
viewport(x = 0.045, y = 0.81, width = 0.29, height = 0.17, just = c("left", "bottom")) -> vp3
print(p1, vp = vp1)
print((p2/fig_c), vp = vp2)
print(p1_1, vp = vp3)
dev.off()