#==load package==
library(ggplot2)
library(stringr)
library(tidyverse)
library(gtools)
library(dplyr)
library(timeDate)
library(coronavirus)
library(readxl)
library(sf)
library(rgdal)
library(ggsci)
library(reshape2)
library(rworldmap)
library(patchwork)
library(ggsci)
library(grid)
library(scales)
library(rnaturalearth)

#==define color==
colors <- c(pal_npg("nrc", alpha =1)(10)[c(1:7,9:10)],"darkred","#FADDA9","grey80")
colors1 <- c(pal_aaas("default", alpha =0.7)(10))
show_col(colors)
show_col(colors1)
value = c("Japan/Korea" = colors[1],"West Asia" = colors[3],"Western Asia" = colors[3],"Northern America" = colors[6],"North America" = colors[6], "Northern China" = colors[8], "Southern China" = colors[11],
          "Southeast Asia"= colors[4], "South Asia"= colors[5],"South-eastern Asia"= colors[4], "Southern Asia"= colors[5], "Europe"= colors[2], "Oceania"= colors[7],
          "North China" = colors[8], "South China" = colors[11], "Russia"= colors[10],  "Southern America"= colors[12],  "South America"= colors[12], 
          "Africa"= colors[9], "Americas" = colors1[1], "Asia" = colors1[2], "China" = colors1[3])

#==read geo-located data==
world_data<-getMap(resolution='low')@data
lati_long <- read.csv("../data/map_data/lati_long1.csv")
lati_long$region_final[lati_long$region_final == "Northern America"] <- "North America"

#==read air flow data==
air_data <- read.csv("../data/air_traffic_data/air_data_between_region_by_period.csv")

#==read map data==
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
china <- st_read("ChinaAdminDivisonSHP-master/2. Province/province.shp")
china$region_final <- "Southern China"
china$region_final[c(1:8,15,16,26:31)] <- "Northern China"
china <- china[,c(6,5)] %>% group_by(region_final) %>% summarise()
china <- st_transform(china, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
nine <- st_read("../data/map_data/nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

#==air traffic plot (panel a,b,c)==
range(air_data$Num_per_month)
cut(air_data$Num_per_month, breaks = c(0, 100000, 500000, 1000000, 2000000, 5000000, 15000000), right = T,
    labels = c(0.5, 1, 2, 3, 4, 5)) ->  air_data$Num_per_month_type
air_data$Num_per_month_type <- as.numeric(as.character(air_data$Num_per_month_type))

ggplot() + 
  geom_sf(data = world, color = "grey50", fill = "grey92", linewidth = 0.05) +
  geom_sf(data = world[world$region_final %in% c("Oceania", "Russia"),], aes(fill = region_final), color = "black", linewidth = 0.05) +
  geom_sf(data = world1, aes(fill = region_final), color = "black", linewidth = 0.05) +
  geom_sf(data = china, aes(fill = region_final), color = "black", linewidth = 0.05) + 
  geom_sf(data = nine, color="black",linewidth = 0.01)+
  scale_x_continuous(expand = c(0.03,0),limits=c(-170,170))+
  scale_y_continuous(expand = c(0.01,0))+
  geom_curve(data = air_data[air_data$period == "Period 1" & air_data$Num_per_month >= 100000,],
             aes(x = as.double(long.x), 
                 y = as.double(lati.x), 
                 xend = as.double(long.y), 
                 yend = as.double(lati.y),
                 linewidth = Num_per_month_type),
             color = "#0061A3",
             alpha = 0.5,
             curvature = 0.35)+
  # guides(fill = F, linewidth = F)+
  guides(fill = F)+
  # guides(linewidth = guide_legend(nrow = 1, title.position = "top"))+
  theme_void()+
  geom_point(data = lati_long, aes(x = long, y = lati, fill = region_final),shape = 21, size = 2)+
  theme(plot.margin = margin(0,0,0,0, "cm"),
        text = element_text(size = 8),
        plot.subtitle = element_text(hjust = 0.5),
        plot.tag = element_text(size = 10, face = "bold"))+
  theme(legend.direction = 'horizontal')+
  scale_linewidth_continuous("Average monthly air passengers (million)", range = c(0.3,3), 
                             breaks = seq(1,5,1),
                             labels = c("(0.1, 0.5]","(0.5, 1.0]","(1.0, 2.0]","(2.0, 5.0]","> 5.0"), limits = c(1,5))+
  scale_fill_manual(values = value)+
  labs(tag = "A", subtitle = "Pre-pandemic period")-> p1

ggplot() + 
  geom_sf(data = world, color = "grey50", fill = "grey92", linewidth = 0.05) +
  geom_sf(data = world[world$region_final %in% c("Oceania", "Russia"),], aes(fill = region_final), color = "black", linewidth = 0.05) +
  geom_sf(data = world1, aes(fill = region_final), color = "black", linewidth = 0.05) +
  geom_sf(data = china, aes(fill = region_final), color = "black", linewidth = 0.05) + 
  geom_sf(data = nine, color="black",linewidth = 0.01)+
  scale_x_continuous(expand = c(0.03,0),limits=c(-170,170))+
  scale_y_continuous(expand = c(0.01,0))+
  geom_curve(data = air_data[air_data$period == "Period 2" & air_data$Num_per_month >= 100000,],
             aes(x = as.double(long.x), 
                 y = as.double(lati.x), 
                 xend = as.double(long.y), 
                 yend = as.double(lati.y),
                 linewidth = Num_per_month_type),
             color = "#0061A3",
             alpha = 0.5,
             curvature = 0.35)+
  # guides(fill = F, linewidth = F)+
  guides(fill = F)+
  # guides(linewidth = guide_legend(nrow = 1, title.position = "top"))+
  theme_void()+
  geom_point(data = lati_long, aes(x = long, y = lati, fill = region_final),shape = 21, size = 2)+
  theme(plot.margin = margin(0,0,0,0, "cm"),
        text = element_text(size = 8),
        plot.subtitle = element_text(hjust = 0.5),
        plot.tag = element_text(size = 10, face = "bold"))+
  theme(legend.direction = 'horizontal')+
  scale_linewidth_continuous("Average monthly air passengers (million)", range = c(0.3,3), 
                             breaks = seq(1,5,1),
                             labels = c("(0.1, 0.5]","(0.5, 1.0]","(1.0, 2.0]","(2.0, 5.0]","> 5.0"), limits = c(1,5))+
  scale_fill_manual(values = value)+
  labs(tag = "B", subtitle = "Pandemic period (acute phase)") -> p2

ggplot() + 
  geom_sf(data = world, color = "grey50", fill = "grey92", linewidth = 0.05) +
  geom_sf(data = world[world$region_final %in% c("Oceania", "Russia"),], aes(fill = region_final), color = "black", linewidth = 0.05) +
  geom_sf(data = world1, aes(fill = region_final), color = "black", linewidth = 0.05) +
  geom_sf(data = china, aes(fill = region_final), color = "black", linewidth = 0.05) + 
  geom_sf(data = nine, color="black",linewidth = 0.01)+
  scale_x_continuous(expand = c(0.03,0),limits=c(-170,170))+
  scale_y_continuous(expand = c(0.01,0))+
  geom_curve(data = air_data[air_data$period == "Period 4" & air_data$Num_per_month >= 100000,],
             aes(x = as.double(long.x), 
                 y = as.double(lati.x), 
                 xend = as.double(long.y), 
                 yend = as.double(lati.y),
                 linewidth = Num_per_month_type),
             color = "#0061A3",
             alpha = 0.5,
             curvature = 0.35)+
  # guides(fill = F, linewidth = F)+
  guides(fill = F)+
  # guides(linewidth = guide_legend(nrow = 1, title.position = "top"))+
  theme_void()+
  geom_point(data = lati_long, aes(x = long, y = lati, fill = region_final),shape = 21, size = 2)+
  theme(plot.margin = margin(0,0,0,0, "cm"),
        text = element_text(size = 8),
        plot.subtitle = element_text(hjust = 0.5),
        plot.tag = element_text(size = 10, face = "bold"))+
  theme(legend.direction = 'horizontal')+
  scale_linewidth_continuous("Average monthly air passengers (million)", range = c(0.3,3), 
                             breaks = seq(1,5,1),
                             labels = c("(0.1, 0.5]","(0.5, 1.0]","(1.0, 2.0]","(2.0, 5.0]","> 5.0"), limits = c(1,5))+
  scale_fill_manual(values = value)+
  labs(tag = "C", subtitle = "Post-pandemic period")-> p3

#==MDS analysis (panel e,f)==
air_data_mds1 <- read.csv("../data/air_traffic_data/air_data_between_region_by_year.csv")

#================(absolute number-based matrix)=================
mds2 <- as.data.frame(spread(air_data_mds1[,c(1,6,7)], move_dir, Num_per_month))
row.names(mds2) <- mds2[,1]
mds2 <- mds2[,-1]
d <- stats::dist(mds2,method =  "euclidean")
fit <- cmdscale(d, eig = TRUE, k = 2)
# Extract (x, y) coordinates
x <- fit$points[, 1]
y <- fit$points[, 2]
# Create a scatter plot
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Multidimensional Scaling Results", type = "n")
text(x, y, labels = row.names(mds2))

mds_data <- as.data.frame(fit$points)
mds_data$label <- row.names(mds_data)
mds_data$label1 <- str_trim(sapply(str_split(mds_data$label,"\\("), function(x) x[1]))
mds_data$label2 <- str_trim(sapply(str_split(mds_data$label,"\\("), function(x) x[2]))
mds_data$label2 <- str_trim(sapply(str_split(mds_data$label2,"\\)"), function(x) x[1]))

factor(mds_data$label1, levels = unique(mds_data$label1)[c(3,1,2)]) -> mds_data$label1
ggplot(mds_data) +
  geom_point(aes(x = V1, y = V2, fill = label1),shape = 21, size = 3,  color = "grey90") +
  ggrepel::geom_text_repel(aes(x = V1, y = V2, label = label2), 
                           vjust = 0.5, hjust = 0.5, size = 1.5)+
  theme_bw()+
  scale_x_continuous(limits = c(-6000000, 4000000))+
  scale_y_continuous(limits = c(-6000000, 4000000))+
  theme(
    # axis.title = element_blank(),
    legend.position = "none",
    axis.text = element_blank(),
    text = element_text(size = 8),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    plot.tag = element_text(size = 10, face = "bold"))+
  scale_fill_manual(values = c("blue","red","orange"))+
  labs(tag = "E", x = "Dimension 1", y = "Dimension 2", subtitle = "O/D volumes-based similarity")-> fig_mds
 
#================(frequency-based matrix)=================
#frequency
air_data_mds1_tmp <- air_data_mds1 %>% group_by(period) %>% summarise(all_volume = sum(Num))
air_data_mds2 <- left_join(air_data_mds1, air_data_mds1_tmp) %>% mutate(Prop_num = Num/all_volume)
mds3 <- as.data.frame(spread(air_data_mds2[,c(1,7,9)], move_dir, Prop_num))
row.names(mds3) <- mds3[,1]
mds3 <- mds3[,-1]
d2 <- stats::dist(mds3)
fit2 <- cmdscale(d2, eig = TRUE, k = 2)
# Extract (x, y) coordinates
x <- fit2$points[, 1]
y <- fit2$points[, 2]
# Create a scatter plot
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Multidimensional Scaling Results", type = "n")
text(x, y, labels = row.names(mds3))

mds_data2 <- as.data.frame(fit2$points)
mds_data2$label <- row.names(mds_data2)
mds_data2$label1 <- str_trim(sapply(str_split(mds_data2$label,"\\("), function(x) x[1]))
mds_data2$label2 <- str_trim(sapply(str_split(mds_data2$label,"\\("), function(x) x[2]))
mds_data2$label2 <- str_trim(sapply(str_split(mds_data2$label2,"\\)"), function(x) x[1]))

factor(mds_data2$label1, levels = unique(mds_data2$label1)[c(3,1,2)]) -> mds_data2$label1
ggplot(mds_data2) +
  geom_point(aes(x = V1, y = V2, fill = label1),shape = 21,  size = 3,  color = "grey90") +
  ggrepel::geom_text_repel(aes(x = V1, y = V2, label = label2), 
                           vjust = 0.5, hjust = 0.5, size = 1.5)+
  theme_bw()+
  scale_x_continuous(limits = c(-0.25, 0.15), breaks = seq(-0.25, 0.15, 0.08))+
  scale_y_continuous(limits = c(-0.25, 0.15), breaks = seq(-0.25, 0.15, 0.08))+
  theme(
    # axis.title = element_blank(),
    legend.position = c(0.3,0.25),
    legend.key.spacing.y  = unit(-0.3, 'cm'),
    axis.text = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 8),
    panel.border = element_rect(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    panel.background = element_rect(fill = "transparent",color = "transparent"),
    # panel.border = element_rect(fill = "transparent"),
    plot.background =  element_rect(fill = "transparent",color = "transparent"),
    legend.background = element_rect(fill = "transparent",color = "transparent"),
    plot.tag = element_text(size = 10, face = "bold"))+
  scale_fill_manual("Periods", values = c("blue","red","orange"))+
  labs(tag = "F", x = "Dimension 1", y = "Dimension 2", subtitle = "O/D frequency-based similarity")-> fig_mds2

#==air traffic from and to each region (panel d)==
air_data_12region <- read.csv("../data/air_traffic_data/air_data_within_region_by_month.csv") %>%
  mutate(date = as.Date(date))

ggplot(data = air_data_12region) +
  annotate("rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-03-31"),
           ymin = -0.05,ymax = 1.05,alpha = 0.2,fill = colors[5])+
  annotate("rect", xmin = as.Date("2021-04-01"),xmax = as.Date("2023-04-30"),
           ymin = -0.05,ymax = 1.05,alpha = 0.2,fill = colors[7])+
  geom_line(aes(x = date, y = index, color = region_final_dep))+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = c(as.Date("2017-01-01"),as.Date("2023-12-31")), expand = c(0.02,0))+
  labs(x = "Date", y = "Relative air passengers", subtitle = "Volumes from and to that region", tag = "D")+
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(-0.05,1.05), expand = c(0,0))+
  scale_color_manual("Geographic regions",values = value)+
  guides(color = F)+
  theme(legend.position = "none",
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        # axis.text.x = element_text(angle = 45, hjust = 1 ,vjust = 1),
        text = element_text(size = 8),
        plot.tag = element_text(size = 10, face = "bold"),
        panel.grid = element_blank()) -> c0

#==Predictors output from GLM phylo-geography (panel g)==
#define label
text1 <- c("Sample size (D)", "Sample size (O)", "Antigenic distance", "Flu vaccine coverage (D)","Air traffic within region (D)", "Air traffic within region (O)",  "Air traffic between regions", 
          "Influenza activity (D)", "Influenza activity (O)", "Distance between regions", "Pop size (D)", "Pop size (O)")
text <- c("Sample\nsize (D)", "Sample\nsize (O)","Antigenic\ndistance", "Flu vaccine\ncoverage (D)","Air traffic\nwithin region (D)", "Air traffic within\nregion (O)",  "Air traffic\nbetween regions", 
           "Influenza\nactivity (O)", "Distance\nbetween regions", "Population\nsize (D)", "Population\nsize (O)")

#read data
h1n1_even <- data.frame(rbind(t(read.delim("../analysis_others/glm_log_file/h1n1_even_glm_log.tsv"))))
h1n1_even$type <- row.names(h1n1_even)
h1n1_even <- h1n1_even[,c(1,8,12)]
colnames(h1n1_even) <- h1n1_even[1,]
h1n1_even$type <- "H1N1pdm09"
h3n2_even <- data.frame(rbind(t(read.delim("../analysis_others/glm_log_file/h3n2_even_glm_log.tsv"))))
h3n2_even$type <- row.names(h3n2_even)
h3n2_even <- h3n2_even[,c(1,8,12)]
colnames(h3n2_even) <- h3n2_even[1,]
h3n2_even$type <- "H3N2"
bv_even <- data.frame(rbind(t(read.delim("../analysis_others/glm_log_file/bv_even_glm_log.tsv"))))
bv_even$type <- row.names(bv_even)
bv_even <- bv_even[,c(1,8,12)]
colnames(bv_even) <- bv_even[1,]
bv_even$type <- "B/Victoria"
by_even <- data.frame(rbind(t(read.delim("../analysis_others/glm_log_file/by_even_glm_log.tsv"))))
by_even$type <- row.names(by_even)
by_even <- by_even[,c(1,8,12)]
colnames(by_even) <- by_even[1,]
by_even$type <- "B/Yamagata"

even <- rbind(h1n1_even, h3n2_even, bv_even, by_even)
rownames(even) <- NULL
even <- even %>% 
  filter(mean != "mean") %>%
  filter(str_detect(Summary.Statistic,"Times")) %>%
  mutate(mean = as.numeric(mean)) %>%
  mutate(`95% HPD interval` = str_remove_all(`95% HPD interval`,"\\[|\\]")) %>%
  mutate(HPD_low = as.numeric(str_trim(sapply(str_split(`95% HPD interval`,","), function(x) x[1])))) %>%
  mutate(HPD_upp = as.numeric(str_trim(sapply(str_split(`95% HPD interval`,","), function(x) x[2])))) %>%
  mutate(predictor = rep(c(1:7,10,11,8,9),4))

#plot
factor(even$predictor, levels = 1:11) -> even$predictor
factor(even$type, levels = unique(even$type)) -> even$type
ggplot(data = even) +
  # annotate("rect", xmin = 15.5,xmax = 16.5,
  #          ymin = -0.5,ymax = 1.5,fill = "lightblue")+
  geom_hline(yintercept = 0, linetype = 2, size = 0.2)+
  geom_errorbar(aes(x = predictor, ymin = HPD_low, ymax = HPD_upp, group = type), width = 0.2, position = position_dodge(width = 0.7), size = 0.2)+
  geom_point(aes(x = predictor, y = mean, fill = type), shape = 21, size = 2,alpha = 1, position = position_dodge(width = 0.7))+
  scale_x_discrete(labels = rev(text))+
  scale_y_continuous(limits = c(-0.6,1.5),expand = c(0,0))+
  theme_bw()+
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_blank(),
        panel.border = element_rect(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        legend.key.spacing.y  = unit(-0.15, 'cm'),
        panel.background = element_rect(fill = "transparent",color = "transparent"),
        plot.background =  element_rect(fill = "transparent",color = "transparent"),
        text = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        panel.grid = element_blank(),
        plot.tag = element_text(size = 10, face = "bold"),
        legend.background = element_rect(fill = "transparent",color = "transparent"),
        legend.position = c(0.1, 0.7))+
  labs(x = "", y = "Coefficient * Inclusion probability\n(Log effect size)",tag = "G")+
  scale_fill_manual("",values = colors1[c(1:3,9)])-> p4

#==output==
pdf("Fig2.pdf",width = 7.25, height = 6)
((p1 + p2 + p3) + plot_layout(nrow = 1,ncol = 3,heights = c(1,1,1), guides = "collect")&theme(legend.position = "bottom")) -> part1
((c0 + fig_mds + fig_mds2) + plot_layout(nrow = 1,ncol = 3,widths = c(1,0.7,0.7))) -> part2
p4 -> part3
viewport(x = 0.0051, y = 0.71, width = 1, height = 0.29, just = c("left", "bottom")) -> vp1
viewport(x = -0.005, y = 0.345, width = 1.005, height = 0.38, just = c("left", "bottom")) -> vp2
viewport(x = 0.005, y = -0.03, width = 0.985, height = 0.4, just = c("left", "bottom")) -> vp3
print(part2,vp = vp2)
print(part1,vp = vp1)
print(part3,vp = vp3)
dev.off()