##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Maps for FiNe
## Author: Gildas Glemarec (DTU Aqua)
###
### NB.  Just make sure you put all the effort files in the main directory where
###      you saved this script. Shapefiles for ICES and NAFO need to be placed 
###      in a folder called shapefiles where you unzip those:
###        https://gis.ices.dk/shapefiles/ICES_areas.zip
###        https://www.nafo.int/Portals/0/GIS/Divisions.zip
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gc()
par(mfrow = c(1,1))
Sys.setenv(LANG = "en") # force error messages to English
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD LIBRARIES ----
if (!"pacman" %in% rownames(installed.packages())){
  utils::install.packages("pacman")
  ## You should probably accept all the updates from CRAN if asked
}
library(pacman)
p_load(tidyverse,data.table,
       sf,giscoR,rnaturalearth,
       RColorBrewer,wesanderson,viridis,
       patchwork)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD FUNCTIONS ----
`%notin%` <- Negate(`%in%`)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD DATA ----
### Shapefiles
world <- ne_countries(
  scale = "medium",
  returnclass = "sf") %>%  
  dplyr::select(name, name_long, 
                iso_a2, iso_a3, 
                geometry, continent)
ICES.areas <- sf::st_read(
  ## https://gis.ices.dk/shapefiles/ICES_areas.zip
  dsn = "shapefiles/",
  layer = "ICES_Areas_20160601_cut_dense_3857")
NAFO.areas <- sf::st_read(
  ## https://www.nafo.int/Portals/0/GIS/Divisions.zip
  dsn = "shapefiles/",
  layer = "NAFO_Divisions_2021_poly_clipped")
NAFO <- st_transform(NAFO.areas, st_crs(ICES.areas))
NAFO$Area_Full <- paste('21', NAFO$Division, sep = '.')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,
                                           pattern = '-', 
                                           replacement = '')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'A','.A')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'B','.B')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'C','.C')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'D','.D')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'E','.E')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'F','.F')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'G','.G')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'H','.H')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'J','.J')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'K','.K')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'L','.L')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'M','.M')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'N','.N')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'O','.O')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'P','.P')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'R','.R')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'S','.S')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'T','.T')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'V','.V')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'W','.W')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'X','.X')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'Y','.Y')
NAFO$Area_Full <- stringr::str_replace_all(NAFO$Area_Full,'Z','.Z')

# ICES.NAFO <- st_union(x = ICES.areas, y = NAFO)
# mapview::mapview(ICES.NAFO)

### Observers and effort data
byc_dat <- fread("fish_eff_clean_llonly.csv") ## WGBYC data
no_dat <- fread("aggregated_data_fishdays.csv") ## Norwegian data
load('Gr_Longline_data_Aggregated.RData')  ## Greenlandic data
gr_data <- as.data.table(gr_data)
# fo_dat <- fread("something.csv")  ## Faroese data

## Before binding, we need to check the area codes and time window
byc_dat$areaCode <- tolower(byc_dat$areaCode)
byc_dat <- byc_dat[year %in% c(2018:2022)]
no_dat$areaCode <- stringr::str_replace_all(no_dat$areaCode,
                                            pattern = '_', 
                                            replacement = '.')
no_dat$areaCode <- stringr::str_replace(no_dat$areaCode,
                                        pattern = '27.3.A.NK', 
                                        replacement = '27.3.a.20')
no_dat <- no_dat %>% 
  mutate(areaCode = case_when(startsWith(areaCode, '27') ~ tolower(areaCode),
                              .default = areaCode))
no_dat <- no_dat[year %in% c(2018:2024)]
no_dat <- no_dat[!(no_dat$areaCode==""),] ## rm rows with missing info on fishing location
gr_data <- gr_data[year %in% c(2018:2024)]
gr_data[, quarter := case_when(month %in% c(1:3) ~ 1,
                               month %in% c(4:6) ~ 2,
                               month %in% c(7:9) ~ 3,
                               month %in% c(10:12) ~ 4,
                               .default = NA_integer_)]
gr_data <- gr_data[areaCode %notin% c('34.2.0','34.4.2')]
## Now, bind things together
dat <- rbind(byc_dat, gr_data, no_dat, fill = TRUE)

## Keep only demersal fish assemblage?
# dat <- dat[metierL5 == 'DEF']

## Are the vessels inshore or offshore?
dat$vesselLengthRange <- as.factor(dat$vesselLengthRange)
dat[
  , in.off.shore := case_when(vesselLengthRange %in% c("VL0006", "VL0008", "VL0010",
                                                       "VL0015", "VL0608", "VL0612", 
                                                       "VL0810", "VL1012", "VL1215",
                                                       "VL1218", "VL1518",
                                                       ## Some assumptions here
                                                       "F", "NK", "Unknown",
                                                       "Other") ~ "inshore",
                              vesselLengthRange %in% c("VL1824", "VL2440", 
                                                       "VL40XX", 
                                                       ## Assuming it is >18m
                                                       "VL1524","VL15XX") ~ "offshore",
                              .default = NA)]

## We don't have month info for quite some of the data, so use quarters instead
dat[, quarter_split := fifelse(quarter %in% c(1,4),
                               'Q4Q1', ## proxy for non-breeding period
                               'Q2Q3')] ##proxy for breeding period

## Reformat areaCode so that it matches the Area_Full of the sf objects
dat[, Area_Full := fifelse(startsWith(areaCode, '27') | areaCode == '', 
                           areaCode,
                           paste('21', areaCode, sep = '.'))]
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,
                                          pattern = '-', 
                                          replacement = '')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'A','.A')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'B','.B')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'C','.C')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'D','.D')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'E','.E')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'F','.F')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'G','.G')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'H','.H')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'J','.J')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'K','.K')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'L','.L')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'M','.M')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'N','.N')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'O','.O')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'P','.P')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'R','.R')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'S','.S')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'T','.T')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'V','.V')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'W','.W')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'X','.X')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'Y','.Y')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'Z','.Z')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'21.4.Vn','21.4.V')
dat$Area_Full <- stringr::str_replace_all(dat$Area_Full,'21.21.3..M','21.3.M')
## Prepare the data to plot
summary_dat <- dat %>% 
  group_by(quarter,Area_Full,in.off.shore) %>% 
  summarise(meanDaS = round(mean(daysAtSeaF)),
            medianDaS = round(median(daysAtSeaF)),
            sdDaS = round(sd(daysAtSeaF)))
# summary(summary_dat$meanDaS)
summary_dat <- as.data.table(summary_dat)
summary_dat[, quarter_split := fifelse(quarter %in% c(1,4),
                                       'Q4Q1',
                                       'Q2Q3')]

ICES_dat <- merge(ICES.areas,
                  summary_dat,
                  by = 'Area_Full')
NAFO_dat <- merge(NAFO,
                  summary_dat,
                  by = 'Area_Full')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MAPS OF LL FISHING EFFORT #####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MAP EFFORT BREEDING SEASON OFFSHORE FLEET <(top left) ####
effort_breed_off <- ggplot() +
  
  ## Plot Background
  geom_sf(data = ICES.areas,
          aes(group=Area_27),
          fill = "white",
          size = 0) +
  geom_sf(data = NAFO.areas, 
          aes(group=Division),
          fill = "white",
          size = 0) +
  
  ## Plot Effort data ICES
  geom_sf(data = ICES_dat %>% 
            dplyr::filter(in.off.shore == 'offshore') %>% 
            dplyr::filter(quarter_split == 'Q2Q3') ,
          aes(fill = meanDaS) ) +
  ## Plot Effort data NAFO
  geom_sf(data = NAFO_dat %>% 
            dplyr::filter(in.off.shore == 'offshore') %>% 
            dplyr::filter(quarter_split == 'Q2Q3') ,
          aes(fill = meanDaS) ) +
  
  ## Choose a colour scale 
  scale_fill_fermenter(
    limits = c(0,4100),
    breaks = c(0,50,100,250,500,1000,2500,5000),
    # transform = "pseudo_log",
    name = "Mean quarterly effort (DaS)", #"Offshore Quarters 2 and 3",
    palette = "Blues",#"YlOrRd",#"YlGnBu",#
    type = 'seq',
    direction = 1,
    na.value = "white") +
  
  ## Plot world map on top (because it should look better)
  geom_sf(data = world %>%
            dplyr::filter(continent %in% c("Europe","North America")), 
          fill = 'black', colour = 'black') +
  
  ## Limit the map to the area of interest
  coord_sf(
    crs = "EPSG:32633",
    xlim = c(-75, 40),
    ylim = c(33, 70),
    default_crs = sf::st_crs(4326), 
    expand = FALSE
  ) +
  
  ## Title of the sub-plot
  ggtitle("OFFSHORE FLEET Q2-Q3") +
  
  ## Theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(20, 'mm'),
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1.5),
    axis.title = element_blank(),
    axis.text.x =  element_blank()
  )
# effort_breed_off
ggsave(filename = 'Offshore_Q2Q3.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort_breed_off,
       device = "png")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MAP EFFORT BREEDING SEASON INSHORE FLEET (top right) ####
effort_breed_in <- ggplot() +
  
  ## Plot Background
  geom_sf(data = ICES.areas,
          aes(group=Area_27),
          fill = "white",
          size = 0) +
  geom_sf(data = NAFO.areas, 
          aes(group=Division),
          fill = "white",
          size = 0) +
  
  ## Plot background
  geom_sf(data = ICES_dat %>% 
            dplyr::filter(in.off.shore == 'inshore') %>% 
            dplyr::filter(quarter_split == 'Q2Q3'),
          aes(fill = meanDaS) ) +
  geom_sf(data = NAFO_dat %>% 
            dplyr::filter(in.off.shore == 'inshore') %>% 
            dplyr::filter(quarter_split == 'Q2Q3'),
          aes(fill = meanDaS) ) +
  
  ## Choose a colour scale 
  scale_fill_fermenter(
    limits = c(0,4100),
    breaks = c(0,50,100,250,500,1000,2500,5000),
    # transform = "pseudo_log",
    name = "Mean quarterly effort (DaS)", #"Inshore Quarters 2 and 3",
    palette = "Blues",#"YlOrRd",#"YlGnBu",#
    type = 'seq',
    direction = 1,
    na.value = "white") +
  
  ## Plot world on top (because it should look better)
  geom_sf(data = world %>%
            dplyr::filter(continent %in% c("Europe","North America")), 
          fill = 'black', colour = 'black') +
  
  ## Limit the map to the area of interest
  coord_sf(
    crs = "EPSG:32633",
    xlim = c(-75, 40),
    ylim = c(33, 70),
    default_crs = sf::st_crs(4326), 
    expand = FALSE
  ) +
  
  ## Title of the sub-plot
  ggtitle("INSHORE FLEET Q2-Q3") +
  
  ## Theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(20, 'mm'),
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1.5),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
# effort_breed_in
ggsave(filename = 'Inshore_Q2Q3.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort_breed_in,
       device = "png")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MAP EFFORT NON BREEDING SEASON OFFSHORE FLEET (bottom left) ####
effort_nonbreed_off <- ggplot() +
  
  ## Plot Background
  geom_sf(data = ICES.areas,
          aes(group=Area_27),
          fill = "white",
          size = 0) +
  geom_sf(data = NAFO.areas, 
          aes(group=Division),
          fill = "white",
          size = 0) +
  
  ## Plot background
  geom_sf(data = ICES_dat %>% 
            dplyr::filter(in.off.shore == 'offshore') %>% 
            dplyr::filter(quarter_split == 'Q4Q1'),
          aes(fill = meanDaS) ) +
  geom_sf(data = NAFO_dat %>% 
            dplyr::filter(in.off.shore == 'offshore') %>% 
            dplyr::filter(quarter_split == 'Q4Q1'),
          aes(fill = meanDaS) ) +
  
  ## Choose a colour scale 
  scale_fill_fermenter(
    limits = c(0,4100),
    breaks = c(0,50,100,250,500,1000,2500,5000),
    # transform = "pseudo_log",
    name = "Mean quarterly effort (DaS)", #"Offshore Quarters 4 and 1",
    palette = "Blues",#"YlOrRd",#"YlGnBu",#
    type = 'seq',
    direction = 1,
    na.value = "white") +
  
  ## Plot world on top (because it should look better)
  geom_sf(data = world %>%
            dplyr::filter(continent %in% c("Europe","North America")), 
          fill = 'black', colour = 'black') +
  
  ## Limit the map to the area of interest
  coord_sf(
    crs = "EPSG:32633",
    xlim = c(-75, 40),
    ylim = c(33, 70),
    default_crs = sf::st_crs(4326), 
    expand = FALSE
  ) +
  
  ## Title of the sub-plot
  ggtitle("OFFSHORE FLEET Q4-Q1") +
  
  ## Theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(20, 'mm'),
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1.5),
    axis.title = element_blank()
  )
# effort_nonbreed_off
ggsave(filename = 'Offshore_Q4Q1.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort_nonbreed_off,
       device = "png")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MAP EFFORT NON BREEDING SEASON INSHORE FLEET (bottom right) ####
effort_nonbreed_in <- ggplot() +
  
  ## Plot Background
  geom_sf(data = ICES.areas,
          aes(group=Area_27),
          fill = "white",
          size = 0) +
  geom_sf(data = NAFO.areas, 
          aes(group=Division),
          fill = "white",
          size = 0) +
  
  ## Plot background
  geom_sf(data = ICES_dat %>% 
            dplyr::filter(in.off.shore == 'inshore') %>% 
            dplyr::filter(quarter_split == 'Q4Q1'),
          aes(fill = meanDaS) ) +
  geom_sf(data = NAFO_dat %>% 
            dplyr::filter(in.off.shore == 'inshore') %>% 
            dplyr::filter(quarter_split == 'Q4Q1'),
          aes(fill = meanDaS) ) +
  
  ## Choose a colour scale 
  scale_fill_fermenter(
    limits = c(0,4100),
    breaks = c(0,50,100,250,500,1000,2500,5000),
    # transform = "pseudo_log",
    name = "Mean quarterly effort (DaS)", #"Inshore Quarters 4 and 1",
    palette = "Blues",#"YlOrRd",#"YlGnBu",#
    type = 'seq',
    direction = 1,
    na.value = "white") +
  
  ## Plot world on top (because it should look better)
  geom_sf(data = world %>%
            dplyr::filter(continent %in% c("Europe","North America")), 
          fill = 'black', colour = 'black') +
  
  ## Limit the map to the area of interest
  coord_sf(
    crs = "EPSG:32633",
    xlim = c(-75, 40),
    ylim = c(33, 70),
    default_crs = sf::st_crs(4326), 
    expand = FALSE
  ) +
  
  ## Title of the sub-plot
  ggtitle("INSHORE FLEET Q4-Q1") +
  
  ## Theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(20, 'mm'),
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1.5),
    axis.title = element_blank(),
    axis.text.y = element_blank()
  )
# effort_nonbreed_in
ggsave(filename = 'Inshore_Q4Q1.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort_nonbreed_in,
       device = "png")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PLOT ALL 4 MAPS TOGETHER ####
effort_plot <- 
  ## Top row
  effort_breed_in + effort_breed_off +
  ## Bottom row
  effort_nonbreed_in + effort_nonbreed_off
map.full <- effort_plot + 
  plot_layout(ncol = 2, nrow = 2, 
              guides = "collect") & 
  theme(legend.position = "bottom")
ggsave(filename = 'map.LL.effort.png',
       units = 'mm',
       width = 297, height = 297,
       plot = map.full,
       device = "png")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PLOT LINE PLOT ####
dat[, area := case_when(startsWith(Area_Full, '21') ~ substr(Area_Full, 1, nchar(Area_Full)-2),
                        startsWith(Area_Full, '27.10') ~ '27.10',
                        startsWith(Area_Full, '27.11') ~ '27.11',
                        startsWith(Area_Full, '27.12') ~ '27.12',
                        startsWith(Area_Full, '27.13') ~ '27.13',
                        startsWith(Area_Full, '27.14') ~ '27.14',
                        startsWith(Area_Full, '27.1.') ~ '27.1',
                        startsWith(Area_Full, '27.2.') ~ '27.2',
                        startsWith(Area_Full, '27.3.') ~ '27.3',
                        startsWith(Area_Full, '27.4.') ~ '27.4',
                        startsWith(Area_Full, '27.5.') ~ '27.5',
                        startsWith(Area_Full, '27.6.') ~ '27.6',
                        startsWith(Area_Full, '27.7.') ~ '27.7',
                        startsWith(Area_Full, '27.8.') ~ '27.8',
                        startsWith(Area_Full, '27.9.') ~ '27.9',
                        .default = NA_character_)]


summary_dat2 <- dat %>% 
  group_by(quarter,area,in.off.shore) %>% 
  summarise(meanDaS = round(mean(daysAtSeaF)),
            medianDaS = round(median(daysAtSeaF)),
            sdDaS = round(sd(daysAtSeaF)))

line.plot.full2 <- ggplot(summary_dat2 %>% 
                            mutate(meanDaS = ifelse(meanDaS>0,meanDaS,NA_integer_)),
                          aes(x = quarter, 
                              y = meanDaS, 
                              group = area)) +
  geom_line(aes(colour = area), linetype = 1) +
  scale_y_continuous(trans='log10') +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1.5),
    legend.position = "right",
    axis.title = element_blank()
  )

line.plot2 <- line.plot.full2 + facet_grid(vars(in.off.shore), scales = "free")

line.plot2

dat.27.10 <- subset(dat, area == '27.10')

summary_dat3 <- dat.27.10 %>% 
  group_by(quarter,Area_Full,in.off.shore) %>% 
  summarise(meanDaS = round(mean(daysAtSeaF)),
            medianDaS = round(median(daysAtSeaF)),
            sdDaS = round(sd(daysAtSeaF)))
line.plot.full3 <- ggplot(summary_dat3 %>% 
                            mutate(meanDaS = ifelse(meanDaS>0,meanDaS,NA_integer_)),
                          aes(x = quarter, 
                              y = meanDaS, 
                              group = Area_Full)) +
  geom_line(aes(colour = Area_Full), linetype = 1) +
  scale_y_continuous(trans='log10') +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1.5),
    legend.position = "right",
    axis.title = element_blank()
  )

line.plot3 <- line.plot.full3 + facet_grid(vars(in.off.shore), scales = "free")

line.plot3
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat.27.8 <- subset(dat, area == '27.8')

summary_dat4 <- dat.27.8 %>% 
  group_by(quarter,Area_Full,in.off.shore) %>% 
  summarise(meanDaS = round(mean(daysAtSeaF)),
            medianDaS = round(median(daysAtSeaF)),
            sdDaS = round(sd(daysAtSeaF)))
line.plot.full4 <- ggplot(summary_dat4 %>% 
                            mutate(meanDaS = ifelse(meanDaS>0,meanDaS,NA_integer_)),
                          aes(x = quarter, 
                              y = meanDaS, 
                              group = Area_Full)) +
  geom_line(aes(colour = Area_Full), linetype = 1) +
  scale_y_continuous(trans='log10') +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1.5),
    legend.position = "right",
    axis.title = element_blank()
  )

line.plot4 <- line.plot.full4 + facet_grid(vars(in.off.shore), scales = "free")

line.plot4