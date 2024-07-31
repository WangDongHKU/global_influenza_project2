library(ggplot2)
library(stringr)
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
library(zoo)
library(grid)
library(patchwork)

#==define color==
colors <- c(pal_npg("nrc", alpha =1)(10)[c(1:7,9:10)],"darkred","#FADDA9","grey80")
colors1 <- c(pal_aaas("default", alpha =0.7)(10))
show_col(colors)
show_col(colors1)
value = c("Japan/Korea" = colors[1],"Western Asia" = colors[3],"WesternAsia" = colors[3],
          "Northern America" = colors[6],"North Am" = colors[6],"NorthernAmerica" = colors[6],
          "South-eastern Asia"= colors[4], "Southern Asia"= colors[5],"SoutheasternAsia"= colors[4], "SouthernAsia"= colors[5],
          "Europe"= colors[2], "Oceania"= colors[7],"NorthChina" = colors[8], "SouthChina" = colors[11],
          "North China" = colors[8], "South China" = colors[11], "China (N)" = colors[8], "China (S)" = colors[11],
          "Northern China" = colors[8], "Southern China" = colors[11],
          "Russia"= colors[10],  "Southern America"= colors[12],  "South Am"= colors[12],  "SouthernAmerica"= colors[12],
          "Africa"= colors[9], "Americas" = colors1[1], "Asia" = colors1[2], "China" = colors1[4])

#==define function==
summarise_hpd_lower <- function(x) {
  if(length(x) <= 1) {
    return(x[1]);
  }
  return(HPDinterval(as.mcmc(x),prob = 0.9)[1])
}

summarise_hpd_upper <- function(x) {
  if(length(x) <= 1) {
    return(x[1]);
  }
  return(HPDinterval(as.mcmc(x),prob = 0.9)[2])
}

#Due to the large sizes of Markov jumps files used in the paper, we have reduced the file size by including few tree files as an example
#==h1n1pdm09==
h1n1_jump <- read.delim("../analysis_others/jump_history/h1n1_even_glm_12_airflow_Jumps.txt", sep = ",")
h1n1_jump$date <- decimal2Date(decimal_date(as.Date("2024-03-28")) - h1n1_jump$time)

h1n1_jump$period[h1n1_jump$date <= as.Date("2020-03-31") & h1n1_jump$date >= as.Date("2017-01-01")] <- "Period 1"
h1n1_jump$period[h1n1_jump$date <= as.Date("2021-03-31") & h1n1_jump$date >= as.Date("2020-04-01")] <- "Period 2"
h1n1_jump$period[h1n1_jump$date <= as.Date("2023-04-30") & h1n1_jump$date >= as.Date("2021-04-01")] <- "Period 3"
h1n1_jump$period[h1n1_jump$date >= as.Date("2023-05-01")] <- "Period 4"

h1n1_jump1 <- h1n1_jump %>%
  filter(!is.na(period)) %>%
  group_by(treeId, startLocation, endLocation, period) %>%
  summarise(n = n())

length(unique(h1n1_jump1$treeId))
length(unique(h1n1_jump1$period))

h1n1_jump1_tmp <- rbind(data.frame(treeId = rep(unique(h1n1_jump1$treeId), each = 12*12),
                              startLocation = rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId)) * 12),
                              endLocation = rep(rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId))), each = 12),
                              period = "Period 1", year_length = 3.25) %>% filter(startLocation != endLocation),
                   data.frame(treeId = rep(unique(h1n1_jump1$treeId), each = 12*12),
                              startLocation = rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId)) * 12),
                              endLocation = rep(rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId))), each = 12),
                              period = "Period 2", year_length = 1) %>% filter(startLocation != endLocation),
                   data.frame(treeId = rep(unique(h1n1_jump1$treeId), each = 12*12),
                              startLocation = rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId)) * 12),
                              endLocation = rep(rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId))), each = 12),
                              period = "Period 3", year_length = 2+1/12) %>% filter(startLocation != endLocation),
                   data.frame(treeId = rep(unique(h1n1_jump1$treeId), each = 12*12),
                              startLocation = rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId)) * 12),
                              endLocation = rep(rep(unique(h1n1_jump1$startLocation), length(unique(h1n1_jump1$treeId))), each = 12),
                              period = "Period 4", year_length = 11/12) %>% filter(startLocation != endLocation))

h1n1_jump1_tmp <- left_join(h1n1_jump1_tmp, h1n1_jump1)
nrow(h1n1_jump1_tmp[!is.na(h1n1_jump1_tmp$n),])
h1n1_jump1_tmp$n[is.na(h1n1_jump1_tmp$n)] <- 0

h1n1_jump2 <- h1n1_jump1_tmp %>%
  mutate(annual_n = n/year_length) %>%
  group_by(period, startLocation, endLocation) %>%
  summarise(mean = mean(annual_n),
            ci_low = summarise_hpd_lower(annual_n),
            ci_upp = summarise_hpd_upper(annual_n))

h1n1_jump2$startLocation[h1n1_jump2$startLocation == "SouthChina"] <- "Southern China"
h1n1_jump2$startLocation[h1n1_jump2$startLocation == "SoutheasternAsia"] <- "Southeast Asia"
h1n1_jump2$startLocation[h1n1_jump2$startLocation == "SouthAmerica"] <- "South America"
h1n1_jump2$startLocation[h1n1_jump2$startLocation == "SouthernAsia"] <- "South Asia"
h1n1_jump2$startLocation[h1n1_jump2$startLocation == "WesternAsia"] <- "West Asia"
h1n1_jump2$startLocation[h1n1_jump2$startLocation == "NorthChina"] <- "Northern China"
h1n1_jump2$startLocation[h1n1_jump2$startLocation == "NorthAmerica"] <- "North America"
h1n1_jump2$endLocation[h1n1_jump2$endLocation == "SouthChina"] <- "Southern China"
h1n1_jump2$endLocation[h1n1_jump2$endLocation == "SoutheasternAsia"] <- "Southeast Asia"
h1n1_jump2$endLocation[h1n1_jump2$endLocation == "SouthAmerica"] <- "South America"
h1n1_jump2$endLocation[h1n1_jump2$endLocation == "SouthernAsia"] <- "South Asia"
h1n1_jump2$endLocation[h1n1_jump2$endLocation == "WesternAsia"] <- "West Asia"
h1n1_jump2$endLocation[h1n1_jump2$endLocation == "NorthChina"] <- "Northern China"
h1n1_jump2$endLocation[h1n1_jump2$endLocation == "NorthAmerica"] <- "North America"
levels <- c("Japan/Korea","Northern China","Southern China", "South Asia", "West Asia","Southeast Asia","Oceania",
           "South America", "North America",   "Europe",  "Russia", "Africa")

##plot (panel b)
factor(h1n1_jump2$startLocation, levels = levels) -> h1n1_jump2$startLocation
factor(h1n1_jump2$endLocation, levels = levels) -> h1n1_jump2$endLocation
range(h1n1_jump2$mean)
cut(h1n1_jump2$mean, breaks = c(-0.1, 1, 2, 5, 10, 15, 25),right = T,
    labels = c("[0, 1]","(1, 2]", "(2, 5]","(5, 10]","(10, 15]","(15, 25]")) -> h1n1_jump2$mean_1
table(h1n1_jump2$mean_1)

colorRampPalette(c("#CDDDE5", "#0061A3", "#E64B35FF")) (6)
ggplot()+
  geom_tile(data = h1n1_jump2[h1n1_jump2$period == "Period 1",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("H1N1pdm09\n\nFrom")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  labs(title = "Pre-pandemic period")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.position = "none")+
  labs(tag = "B")-> p1

ggplot()+
  geom_tile(data = h1n1_jump2[h1n1_jump2$period == "Period 2",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+  labs(title = "Acute phase of pandemic")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    title = element_text(size = 9),
    legend.position = "none") -> p2

ggplot()+
  geom_tile(data = h1n1_jump2[h1n1_jump2$period == "Period 3",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0",   "#E64B35"),
                    na.value="white")+ #adjust the number of colors depening on the number of legends
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+  labs(title = "Transition phase of pandemic")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.position = "none") -> p2_1

ggplot()+
  geom_tile(data = h1n1_jump2[h1n1_jump2$period == "Period 4",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  labs(title = "Post-pandemic period")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none")-> p3

(p1|p2|p2_1|p3)

##panel a
date_match <- data.frame(date_week = seq(as.Date("1995-01-05"),as.Date("2024-04-18"),7))
date_match$ISO_YEAR <- isoyear(date_match$date)
date_match$ISO_WEEK <- isoweek(date_match$date)

h1n1_jump6 <- as.data.frame(h1n1_jump) %>%
  mutate(ISO_YEAR = isoyear(date),
         ISO_WEEK = isoweek(date),
         treeId = as.character(treeId)) %>%
  left_join(date_match) %>%
  filter(!is.na(period)) %>%
  group_by(treeId, date_week) %>%
  summarise(n = n())

range(h1n1_jump6$date_week)
((max(h1n1_jump6$date_week) - min(h1n1_jump6$date_week))/7)+1
date <- seq(min(h1n1_jump6$date_week), max(h1n1_jump6$date_week), 7)
tmp_format <- data.frame(treeId = rep(unique(h1n1_jump6$treeId), length(date)),
                         date_week = rep(date, each = length(unique(h1n1_jump6$treeId))))
h1n1_jump7 <- left_join(tmp_format, h1n1_jump6)
nrow(h1n1_jump7[!is.na(h1n1_jump7$n),])
h1n1_jump7$n[is.na(h1n1_jump7$n)] <- 0

h1n1_jump7 <- h1n1_jump7 %>%
  group_by(date_week) %>%
  summarise(mean = mean(n),
            ci_low = summarise_hpd_lower(n),
            ci_upp = summarise_hpd_upper(n)) %>%
  mutate(mean_roll = rollmean(mean, k=5, fill=NA, na.pad=T, align='center'),
         ci_low_roll = rollmean(ci_low, k=5, fill=NA, na.pad=T, align='center'),
         ci_upp_roll = rollmean(ci_upp, k=5, fill=NA, na.pad=T, align='center'))

#plot (panel a)
ggplot(data = h1n1_jump7) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -0.5,ymax = 25,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -0.5,ymax = 25,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date_week, ymin = 0, ymax = mean_roll), fill = colors1[1], alpha = 0.7)+
  # geom_ribbon(aes(x = date_week, ymin = ci_low_roll, ymax = ci_upp_roll), fill = colors1[1], alpha = 0.7)+
  geom_line(aes(x = date_week, y = mean_roll, group = 1))+
  scale_y_continuous("Number of transition events", limits = c(-0.5,25), expand = c(0,0), breaks = seq(0,25,10))+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2024-03-31")),date_breaks = "1 year", date_labels = "%Y", expand = c(0.01,0))+
  theme_bw()+
  annotate("text", x = as.Date("2017-01-01"), y = 20, label = "H1N1pdm09", hjust = 0, size = 2.5)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        panel.grid = element_blank()) -> m1

#==h3n2==
h3n2_jump <- read.delim("../analysis_others/jump_history/h3n2_even_glm_12_airflow_Jumps.txt", sep = ",")
h3n2_jump$date <- decimal2Date(decimal_date(as.Date("2024-03-30")) - h3n2_jump$time)

h3n2_jump$period[h3n2_jump$date <= as.Date("2020-03-31") & h3n2_jump$date >= as.Date("2017-01-01")] <- "Period 1"
h3n2_jump$period[h3n2_jump$date <= as.Date("2021-03-31") & h3n2_jump$date >= as.Date("2020-04-01")] <- "Period 2"
h3n2_jump$period[h3n2_jump$date <= as.Date("2023-04-30") & h3n2_jump$date >= as.Date("2021-04-01")] <- "Period 3"
h3n2_jump$period[h3n2_jump$date >= as.Date("2023-05-01")] <- "Period 4"

h3n2_jump1 <- h3n2_jump %>%
  filter(!is.na(period)) %>%
  group_by(treeId, startLocation, endLocation, period) %>%
  summarise(n = n())

length(unique(h3n2_jump1$treeId))
length(unique(h3n2_jump1$period))

h3n2_jump1_tmp <- rbind(data.frame(treeId = rep(unique(h3n2_jump1$treeId), each = 12*12),
                                   startLocation = rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId)) * 12),
                                   endLocation = rep(rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId))), each = 12),
                                   period = "Period 1", year_length = 3.25) %>% filter(startLocation != endLocation),
                        data.frame(treeId = rep(unique(h3n2_jump1$treeId), each = 12*12),
                                   startLocation = rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId)) * 12),
                                   endLocation = rep(rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId))), each = 12),
                                   period = "Period 2", year_length = 1) %>% filter(startLocation != endLocation),
                        data.frame(treeId = rep(unique(h3n2_jump1$treeId), each = 12*12),
                                   startLocation = rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId)) * 12),
                                   endLocation = rep(rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId))), each = 12),
                                   period = "Period 3", year_length = 2+1/12) %>% filter(startLocation != endLocation),
                        data.frame(treeId = rep(unique(h3n2_jump1$treeId), each = 12*12),
                                   startLocation = rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId)) * 12),
                                   endLocation = rep(rep(unique(h3n2_jump1$startLocation), length(unique(h3n2_jump1$treeId))), each = 12),
                                   period = "Period 4", year_length = 1-1/12) %>% filter(startLocation != endLocation))

h3n2_jump1_tmp <- left_join(h3n2_jump1_tmp, h3n2_jump1)
nrow(h3n2_jump1_tmp[!is.na(h3n2_jump1_tmp$n),])
h3n2_jump1_tmp$n[is.na(h3n2_jump1_tmp$n)] <- 0

h3n2_jump2 <- h3n2_jump1_tmp %>%
  mutate(annual_n = n/year_length) %>%
  group_by(period, startLocation, endLocation) %>%
  summarise(mean = mean(annual_n),
            ci_low = summarise_hpd_lower(annual_n),
            ci_upp = summarise_hpd_upper(annual_n))

h3n2_jump2$startLocation[h3n2_jump2$startLocation == "SouthChina"] <- "Southern China"
h3n2_jump2$startLocation[h3n2_jump2$startLocation == "SoutheasternAsia"] <- "Southeast Asia"
h3n2_jump2$startLocation[h3n2_jump2$startLocation == "SouthAmerica"] <- "South America"
h3n2_jump2$startLocation[h3n2_jump2$startLocation == "SouthernAsia"] <- "South Asia"
h3n2_jump2$startLocation[h3n2_jump2$startLocation == "WesternAsia"] <- "West Asia"
h3n2_jump2$startLocation[h3n2_jump2$startLocation == "NorthChina"] <- "Northern China"
h3n2_jump2$startLocation[h3n2_jump2$startLocation == "NorthAmerica"] <- "North America"
h3n2_jump2$endLocation[h3n2_jump2$endLocation == "SouthChina"] <- "Southern China"
h3n2_jump2$endLocation[h3n2_jump2$endLocation == "SoutheasternAsia"] <- "Southeast Asia"
h3n2_jump2$endLocation[h3n2_jump2$endLocation == "SouthAmerica"] <- "South America"
h3n2_jump2$endLocation[h3n2_jump2$endLocation == "SouthernAsia"] <- "South Asia"
h3n2_jump2$endLocation[h3n2_jump2$endLocation == "WesternAsia"] <- "West Asia"
h3n2_jump2$endLocation[h3n2_jump2$endLocation == "NorthChina"] <- "Northern China"
h3n2_jump2$endLocation[h3n2_jump2$endLocation == "NorthAmerica"] <- "North America"
levels <- c("Japan/Korea","Northern China","Southern China", "South Asia", "West Asia","Southeast Asia","Oceania",
            "South America", "North America",   "Europe",  "Russia", "Africa")

##plot (panel c)
factor(h3n2_jump2$startLocation, levels = levels) -> h3n2_jump2$startLocation
factor(h3n2_jump2$endLocation, levels = levels) -> h3n2_jump2$endLocation
range(h3n2_jump2$mean)
cut(h3n2_jump2$mean, breaks = c(-0.1, 1, 2, 5, 10, 15, 25),right = T,
    labels = c("[0, 1]","(1, 2]", "(2, 5]","(5, 10]","(10, 15]","(15, 25]")) -> h3n2_jump2$mean_1
table(h3n2_jump2$mean_1)

ggplot()+
  geom_tile(data = h3n2_jump2[h3n2_jump2$period == "Period 1",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("H3N2\n\nFrom")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 1 (Feb 2018 to Jan 2020)")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.position = "none")+
  labs(tag = "C") -> p4

ggplot()+
  geom_tile(data = h3n2_jump2[h3n2_jump2$period == "Period 2",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 2 (Feb 2020 to Jul 2021)")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.position = "none") -> p5

ggplot()+
  geom_tile(data = h3n2_jump2[h3n2_jump2$period == "Period 3",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 2 (Feb 2020 to Jul 2021)")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.position = "none") -> p5_1

ggplot()+
  geom_tile(data = h3n2_jump2[h3n2_jump2$period == "Period 4",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 3 (Aug 2021 to Jul 2023)")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.position = "none") -> p6

(p4|p5|p5_1|p6)

##panel a
h3n2_jump6 <- as.data.frame(h3n2_jump) %>%
  mutate(ISO_YEAR = isoyear(date),
         ISO_WEEK = isoweek(date),
         treeId = as.character(treeId)) %>%
  left_join(date_match) %>%
  filter(!is.na(period)) %>%
  group_by(treeId, date_week) %>%
  summarise(n = n())

range(h3n2_jump6$date_week)
length(unique(h3n2_jump6$treeId))

date1 <- seq(min(h3n2_jump6$date_week), max(h3n2_jump6$date_week), 7)
tmp_format1 <- data.frame(treeId = rep(unique(h3n2_jump6$treeId), length(date1)),
                         date_week = rep(date1, each = length(unique(h3n2_jump6$treeId))))

h3n2_jump7 <- left_join(tmp_format1, h3n2_jump6)
nrow(h3n2_jump7[!is.na(h3n2_jump7$n),])
h3n2_jump7$n[is.na(h3n2_jump7$n)] <- 0

h3n2_jump7 <- h3n2_jump7 %>%
  group_by(date_week) %>%
  summarise(mean = mean(n),
            ci_low = summarise_hpd_lower(n),
            ci_upp = summarise_hpd_upper(n)) %>%
  mutate(mean_roll = rollmean(mean, k=5, fill=NA, na.pad=T, align='center'),
         ci_low_roll = rollmean(ci_low, k=5, fill=NA, na.pad=T, align='center'),
         ci_upp_roll = rollmean(ci_upp, k=5, fill=NA, na.pad=T, align='center'))

ggplot(data = h3n2_jump7) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -0.5,ymax = 25,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -0.5,ymax = 25,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date_week, ymin = 0, ymax = mean_roll), fill = colors1[2], alpha = 0.7)+
  # geom_ribbon(aes(x = date_week, ymin = ci_low_roll, ymax = ci_upp_roll), fill = colors1[2], alpha = 0.7)+
  geom_line(aes(x = date_week, y = mean_roll, group = 1))+
  scale_y_continuous("Number of transition events", limits = c(-0.5,25), expand = c(0,0), breaks = seq(0,25,10))+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2024-03-31")),date_breaks = "1 year", date_labels = "%Y", expand = c(0.01,0))+
  theme_bw()+
  annotate("text", x = as.Date("2017-01-01"), y = 20, label = "H3N2", hjust = 0, size = 2.5)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        panel.grid = element_blank())-> m2

#==B/Victoria==
bv_jump <- read.delim("../analysis_others/jump_history/bv_even_glm_12_airflow_Jumps.txt", sep = ",")
bv_jump$date <- decimal2Date(decimal_date(as.Date("2024-03-28")) - bv_jump$time)

bv_jump$period[bv_jump$date <= as.Date("2020-03-31") & bv_jump$date >= as.Date("2017-01-01")] <- "Period 1"
bv_jump$period[bv_jump$date <= as.Date("2021-03-31") & bv_jump$date >= as.Date("2020-04-01")] <- "Period 2"
bv_jump$period[bv_jump$date <= as.Date("2023-04-30") & bv_jump$date >= as.Date("2021-04-01")] <- "Period 3"
bv_jump$period[bv_jump$date >= as.Date("2023-05-01")] <- "Period 4"

bv_jump1 <- bv_jump %>%
  filter(!is.na(period)) %>%
  group_by(treeId, startLocation, endLocation, period) %>%
  summarise(n = n())

length(unique(bv_jump1$treeId))
length(unique(bv_jump1$period))

bv_jump1_tmp <- rbind(data.frame(treeId = rep(unique(bv_jump1$treeId), each = 12*12),
                                 startLocation = rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId)) * 12),
                                 endLocation = rep(rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId))), each = 12),
                                 period = "Period 1", year_length = 3.25) %>% filter(startLocation != endLocation),
                      data.frame(treeId = rep(unique(bv_jump1$treeId), each = 12*12),
                                 startLocation = rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId)) * 12),
                                 endLocation = rep(rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId))), each = 12),
                                 period = "Period 2", year_length = 1) %>% filter(startLocation != endLocation),
                      data.frame(treeId = rep(unique(bv_jump1$treeId), each = 12*12),
                                 startLocation = rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId)) * 12),
                                 endLocation = rep(rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId))), each = 12),
                                 period = "Period 3", year_length = 2+1/12) %>% filter(startLocation != endLocation),
                      data.frame(treeId = rep(unique(bv_jump1$treeId), each = 12*12),
                                 startLocation = rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId)) * 12),
                                 endLocation = rep(rep(unique(bv_jump1$startLocation), length(unique(bv_jump1$treeId))), each = 12),
                                 period = "Period 4", year_length = 1-1/12) %>% filter(startLocation != endLocation))

bv_jump1_tmp <- left_join(bv_jump1_tmp, bv_jump1)
nrow(bv_jump1_tmp[!is.na(bv_jump1_tmp$n),])
bv_jump1_tmp$n[is.na(bv_jump1_tmp$n)] <- 0

bv_jump2 <- bv_jump1_tmp %>%
  mutate(annual_n = n/year_length) %>%
  group_by(period, startLocation, endLocation) %>%
  summarise(mean = mean(annual_n),
            ci_low = summarise_hpd_lower(annual_n),
            ci_upp = summarise_hpd_upper(annual_n))

bv_jump2$startLocation[bv_jump2$startLocation == "SouthChina"] <- "Southern China"
bv_jump2$startLocation[bv_jump2$startLocation == "SoutheasternAsia"] <- "Southeast Asia"
bv_jump2$startLocation[bv_jump2$startLocation == "SouthAmerica"] <- "South America"
bv_jump2$startLocation[bv_jump2$startLocation == "SouthernAsia"] <- "South Asia"
bv_jump2$startLocation[bv_jump2$startLocation == "WesternAsia"] <- "West Asia"
bv_jump2$startLocation[bv_jump2$startLocation == "NorthChina"] <- "Northern China"
bv_jump2$startLocation[bv_jump2$startLocation == "NorthAmerica"] <- "North America"
bv_jump2$endLocation[bv_jump2$endLocation == "SouthChina"] <- "Southern China"
bv_jump2$endLocation[bv_jump2$endLocation == "SoutheasternAsia"] <- "Southeast Asia"
bv_jump2$endLocation[bv_jump2$endLocation == "SouthAmerica"] <- "South America"
bv_jump2$endLocation[bv_jump2$endLocation == "SouthernAsia"] <- "South Asia"
bv_jump2$endLocation[bv_jump2$endLocation == "WesternAsia"] <- "West Asia"
bv_jump2$endLocation[bv_jump2$endLocation == "NorthChina"] <- "Northern China"
bv_jump2$endLocation[bv_jump2$endLocation == "NorthAmerica"] <- "North America"
levels <- c("Japan/Korea","Northern China","Southern China", "South Asia", "West Asia","Southeast Asia","Oceania",
            "South America", "North America",   "Europe",  "Russia", "Africa")

##plot (panel c)
factor(bv_jump2$startLocation, levels = levels) -> bv_jump2$startLocation
factor(bv_jump2$endLocation, levels = levels) -> bv_jump2$endLocation
range(bv_jump2$mean)
cut(bv_jump2$mean, breaks = c(-0.1, 1, 2, 5, 10, 15, 25),right = T,
    labels = c("[0, 1]","(1, 2]", "(2, 5]","(5, 10]","(10, 15]","(15, 25]")) -> bv_jump2$mean_1
table(bv_jump2$mean_1)
# saveRDS(bv_jump2, "output/summary_fig/bv_movement.rds")

ggplot()+
  geom_tile(data = bv_jump2[bv_jump2$period == "Period 1",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("B/Victoria\n\nFrom")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 1 (Feb 2018 to Jan 2020)")+
  theme(
    # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
    legend.key.width = unit(1,"cm"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.position = "none")+
  labs(tag = "D")-> p7

ggplot()+
  geom_tile(data = bv_jump2[bv_jump2$period == "Period 2",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA",           "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 2 (Feb 2020 to Jul 2021)")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0),
        legend.key.width = unit(1,"cm"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        legend.position = "none") -> p8

ggplot()+
  geom_tile(data = bv_jump2[bv_jump2$period == "Period 3",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA","#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 2 (Feb 2020 to Jul 2021)")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0),
        legend.key.width = unit(1,"cm"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        legend.position = "none")-> p8_1

ggplot()+
  geom_tile(data = bv_jump2[bv_jump2$period == "Period 4",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("From")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  # labs(title = "Epoch 3 (Aug 2021 to Jul 2023)")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0),
        legend.key.width = unit(1,"cm"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        legend.position = "none") -> p9

(p7|p8|p8_1|p9)

##panel a
bv_jump6 <- as.data.frame(bv_jump) %>%
  mutate(ISO_YEAR = isoyear(date),
         ISO_WEEK = isoweek(date),
         treeId = as.character(treeId)) %>%
  left_join(date_match) %>%
  filter(!is.na(period)) %>%
  group_by(treeId, date_week) %>%
  summarise(n = n())
range(bv_jump6$date_week)
length(unique(bv_jump6$date_week))
length(unique(bv_jump6$treeId))

date2 <- seq(min(bv_jump6$date_week), max(bv_jump6$date_week), 7)
tmp_format2 <- data.frame(treeId = rep(unique(bv_jump6$treeId), length(date2)),
                         date_week = rep(date2, each = length(unique(bv_jump6$treeId))))

bv_jump7 <- left_join(tmp_format2, bv_jump6)
nrow(bv_jump7[!is.na(bv_jump7$n),])
bv_jump7$n[is.na(bv_jump7$n)] <- 0

bv_jump7 <- bv_jump7 %>%
  group_by(date_week) %>%
  summarise(mean = mean(n),
            ci_low = summarise_hpd_lower(n),
            ci_upp = summarise_hpd_upper(n)) %>%
  mutate(mean_roll = rollmean(mean, k=5, fill=NA, na.pad=T, align='center'),
         ci_low_roll = rollmean(ci_low, k=5, fill=NA, na.pad=T, align='center'),
         ci_upp_roll = rollmean(ci_upp, k=5, fill=NA, na.pad=T, align='center'))

ggplot(data = bv_jump7) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -0.5,ymax = 25,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -0.5,ymax = 25,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date_week, ymin = 0, ymax = mean_roll), fill = colors1[3], alpha = 0.7)+
  # geom_ribbon(aes(x = date_week, ymin = ci_low_roll, ymax = ci_upp_roll), fill = colors1[3], alpha = 0.7)+
  geom_line(aes(x = date_week, y = mean_roll, group = 1))+
  scale_y_continuous("Number of transition events", limits = c(-0.5,25), expand = c(0,0), breaks = seq(0,25,10))+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2024-03-31")), date_breaks = "1 year", date_labels = "%Y",expand = c(0.01,0))+
  theme_bw()+
  annotate("text", x = as.Date("2017-01-01"), y = 20, label = "B/Victoria", hjust = 0, size = 2.5)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        panel.grid = element_blank()) -> m3

#==B/Yamagata==
by_jump <- read.delim("../analysis_others/jump_history/by_even_glm_12_airflow_Jumps.txt", sep = ",")
by_jump$date <- decimal2Date(decimal_date(as.Date("2020-03-24")) - by_jump$time)

by_jump$period[by_jump$date <= as.Date("2020-03-31") & by_jump$date >= as.Date("2017-01-01")] <- "Period 1"

by_jump1 <- by_jump %>%
  filter(!is.na(period)) %>%
  group_by(treeId, startLocation, endLocation, period) %>%
  summarise(n = n())

length(unique(by_jump1$treeId))
length(unique(by_jump1$period))

by_jump1_tmp <- data.frame(treeId = rep(unique(by_jump1$treeId), each = 12*12),
                           startLocation = rep(unique(by_jump1$startLocation), length(unique(by_jump1$treeId)) * 12),
                           endLocation = rep(rep(unique(by_jump1$startLocation), length(unique(by_jump1$treeId))), each = 12),
                           period = "Period 1", year_length = 3.25) %>% filter(startLocation != endLocation)

by_jump1_tmp <- left_join(by_jump1_tmp, by_jump1)
nrow(by_jump1_tmp[!is.na(by_jump1_tmp$n),])
by_jump1_tmp$n[is.na(by_jump1_tmp$n)] <- 0

by_jump2 <- by_jump1_tmp %>%
  mutate(annual_n = n/year_length) %>%
  group_by(period, startLocation, endLocation) %>%
  summarise(mean = mean(annual_n),
            ci_low = summarise_hpd_lower(annual_n),
            ci_upp = summarise_hpd_upper(annual_n))

by_jump2$startLocation[by_jump2$startLocation == "SouthChina"] <- "Southern China"
by_jump2$startLocation[by_jump2$startLocation == "SoutheasternAsia"] <- "Southeast Asia"
by_jump2$startLocation[by_jump2$startLocation == "SouthAmerica"] <- "South America"
by_jump2$startLocation[by_jump2$startLocation == "SouthernAsia"] <- "South Asia"
by_jump2$startLocation[by_jump2$startLocation == "WesternAsia"] <- "West Asia"
by_jump2$startLocation[by_jump2$startLocation == "NorthChina"] <- "Northern China"
by_jump2$startLocation[by_jump2$startLocation == "NorthAmerica"] <- "North America"
by_jump2$endLocation[by_jump2$endLocation == "SouthChina"] <- "Southern China"
by_jump2$endLocation[by_jump2$endLocation == "SoutheasternAsia"] <- "Southeast Asia"
by_jump2$endLocation[by_jump2$endLocation == "SouthAmerica"] <- "South America"
by_jump2$endLocation[by_jump2$endLocation == "SouthernAsia"] <- "South Asia"
by_jump2$endLocation[by_jump2$endLocation == "WesternAsia"] <- "West Asia"
by_jump2$endLocation[by_jump2$endLocation == "NorthChina"] <- "Northern China"
by_jump2$endLocation[by_jump2$endLocation == "NorthAmerica"] <- "North America"
levels <- c("Japan/Korea","Northern China","Southern China", "South Asia", "West Asia","Southeast Asia","Oceania",
            "South America", "North America",   "Europe",  "Russia", "Africa")

##plot (panel d)
factor(by_jump2$startLocation, levels = levels) -> by_jump2$startLocation
factor(by_jump2$endLocation, levels = levels) -> by_jump2$endLocation
range(by_jump2$mean)
cut(by_jump2$mean, breaks = c(-0.1, 1, 2, 5, 10, 15, 25),right = T,
    labels = c("[0, 1]","(1, 2]", "(2, 5]","(5, 10]","(10, 15]","> 15")) -> by_jump2$mean_1
table(by_jump2$mean_1)

ggplot()+
  geom_tile(data = by_jump2[by_jump2$period == "Period 1",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition event",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("B/Yamagata\n\nFrom")+
  theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title.position = "top"))+
  # labs(title = "Epoch 1 (Feb 2018 to Jan 2020)")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0),
        legend.key.width = unit(1,"cm"),
        panel.grid = element_blank(),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        legend.position = "none")+
  labs(tag = "E")-> p10

ggplot()+
  geom_tile(data = by_jump2[by_jump2$period == "Period 1",],
            aes(x = endLocation, y = startLocation, fill = mean_1))+
  # scale_fill_gradientn("Number of transition event",colors = rev(c("#E64B35FF","#0061A3", "#CDDDE5","grey93")),
  #                      na.value="white",limits = c(1, 42))+
  scale_fill_manual("Number of transition events per year",values = c("grey90","#CDDDE5", "#7BABCA", "#2879B0", "#2E5C8C",  "#E64B35"),
                    na.value="white")+
  scale_x_discrete("To")+
  scale_y_discrete("B/Yamagata\n\nFrom")+
  # theme_bw()+
  # guides(fill = "none")+
  guides(fill = guide_legend(
    # frame.colour = "black",
    # ticks.colour = "black",
    title.position = "top"))+
  # labs(title = "Epoch 1 (Feb 2018 to Jan 2020)")+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white",color = "transparent"),
    panel.background = element_rect(fill = "white",color = "transparent"),
    panel.border = element_rect(fill = "white",color = "transparent"),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.key.width = unit(0.3,"cm"),
    legend.key.height = unit(0.3,"cm"),
    plot.title = element_text(hjust = 0.5,size = 8),
    text = element_text(size = 7),
    # panel.border = element_rect(linewidth = 0.3),
    # axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"),
    legend.direction = "horizontal",
    legend.position = c(0.5,0.5))-> p11

##panel a
by_jump6 <- as.data.frame(by_jump) %>%
  mutate(ISO_YEAR = isoyear(date),
         ISO_WEEK = isoweek(date),
         treeId = as.character(treeId)) %>%
  left_join(date_match) %>%
  filter(!is.na(period)) %>%
  group_by(treeId, date_week) %>%
  summarise(n = n())

range(by_jump6$date_week)
length(unique(by_jump6$date_week))
length(unique(by_jump6$treeId))

date3 <- seq(min(by_jump6$date_week), max(by_jump6$date_week), 7)
tmp_format3 <- data.frame(treeId = rep(unique(by_jump6$treeId), length(date3)),
                         date_week = rep(date3, each = length(unique(by_jump6$treeId))))

by_jump7 <- left_join(tmp_format3, by_jump6)
# 169*901
nrow(by_jump7[!is.na(by_jump7$n),])
by_jump7$n[is.na(by_jump7$n)] <- 0

by_jump7 <- by_jump7 %>%
  group_by(date_week) %>%
  summarise(mean = mean(n),
            ci_low = summarise_hpd_lower(n),
            ci_upp = summarise_hpd_upper(n))%>%
  mutate(mean_roll = rollmean(mean, k=5, fill=NA, na.pad=T, align='center'),
         ci_low_roll = rollmean(ci_low, k=5, fill=NA, na.pad=T, align='center'),
         ci_upp_roll = rollmean(ci_upp, k=5, fill=NA, na.pad=T, align='center'))

ggplot(data = by_jump7) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -0.5,ymax = 28.5,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -0.5,ymax = 28.5,alpha = 0.2,fill = colors[7])+
  geom_ribbon(aes(x = date_week, ymin = 0, ymax = mean_roll), fill = colors1[9], alpha = 0.7)+
  # geom_ribbon(aes(x = date_week, ymin = ci_low_roll, ymax = ci_upp_roll), fill = colors1[9], alpha = 0.7)+
  geom_line(aes(x = date_week, y = mean_roll, group = 1))+
  scale_y_continuous("Number of transition events", limits = c(-0.5,28.5), expand = c(0,0), breaks = seq(0,28,10))+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2024-03-31")),date_breaks = "1 year", date_labels = "%b %Y", expand = c(0.01,0))+
  theme_bw()+
  annotate("text", x = as.Date("2017-01-01"), y = 20, label = "B/Yamagata", hjust = 0, size = 2.5)+
  # coord_cartesian(clip = "on")+
  # annotate("segment", x = as.Date("2017-01-01"), xend = as.Date("2017-10-01"),
  #          y = -10, yend = -30)+
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 8),
        text = element_text(size = 7),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        plot.tag = element_text(size = 10, face = "bold"),
        plot.margin =  margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.grid = element_blank())-> m4

#==output==
pdf("Fig3.pdf",width = 7.25,height = 9)
(m1/m2/m3/m4)+plot_layout(axis_titles = "collect") + plot_annotation(subtitle = "A")&theme(plot.subtitle = element_text(size = 10, face = "bold")) -> part1
(p1|p2|p2_1|p3)/
  (p4|p5|p5_1|p6)/
  (p7|p8|p8_1|p9) -> part2
p10 -> part3
viewport(x = 0.008, y = 0.71, width = 0.99, height = 0.297, just = c("left", "bottom")) -> vp1
viewport(x = 0, y = 0.149-0.01, width = 1, height = 0.575, just = c("left", "bottom")) -> vp2
viewport(x = 0.008, y = 0-0.01, width = 0.357, height = 0.234, just = c("left", "bottom")) -> vp3
viewport(x = 0.5, y = 0.035, width = 0.4, height = 0.1, just = c("left", "bottom")) -> vp4
print(part2,vp = vp2)
print(part1,vp = vp1)
print(p10, vp = vp3)
print(p11, vp = vp4)
dev.off()