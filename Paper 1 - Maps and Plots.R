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
ICES.areas <- ICES.areas %>% 
  mutate( subarea = sub("^(27\\.[0-9]+).*", "\\1", Area_Full) ) %>% 
  mutate( division = paste(Major_FA,SubArea,Division, sep = '.') ) %>% 
  mutate( division = if_else( division %in% c('27.1.a', '27.1.b'),
                              '27.1',
                              division) )
ICES.areas.sub <- ICES.areas %>% 
  group_by(division) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()

NAFO.areas <- sf::st_read(
  ## https://www.nafo.int/Portals/0/GIS/Divisions.zip
  dsn = "shapefiles/",
  layer = "NAFO_Divisions_2021_poly_clipped")
NAFO.areas <- st_transform(NAFO.areas, st_crs(ICES.areas))
NAFO.areas$division <- paste('21', NAFO.areas$Division, sep = '.')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,
                                                pattern = '-', 
                                                replacement = '')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'A','.A')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'B','.B')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'C','.C')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'D','.D')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'E','.E')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'F','.F')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'G','.G')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'H','.H')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'J','.J')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'K','.K')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'L','.L')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'M','.M')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'N','.N')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'O','.O')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'P','.P')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'R','.R')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'S','.S')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'T','.T')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'V','.V')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'W','.W')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'X','.X')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'Y','.Y')
NAFO.areas$division <- stringr::str_replace_all(NAFO.areas$division,'Z','.Z')
## For the maps, we need to reduce the NAFO area to the area:
NAFO.areas.sub <- subset(NAFO.areas, 
                         division %in% c('21.1.A', '21.1.B',
                                         '21.1.C', '21.1.D',
                                         '21.1.E', '21.1.F'))
### Observers and effort data #----
#### WGBYC data #----
byc_dat <- fread("BYC_data/FishingEffort_AllYears_Extraction_27092025.csv") ## WGBYC data
byc_dat$areaCode <- tolower(byc_dat$AreaCode)
byc_dat$vesselLengthRange <- byc_dat$VesselLengthRange
byc_dat$country <- byc_dat$Country
byc_dat$year <- byc_dat$Year
byc_dat$quarter <- byc_dat$Quarter
byc_dat$month <- byc_dat$Month
byc_dat$daysAtSeaF <- byc_dat$DaysAtSeaF
byc_dat <- byc_dat[, c('AreaCode','VesselLengthRange','Country',
                       'Year','Quarter','Month',
                       'DaysAtSeaF') := NULL]
byc_dat <- byc_dat[year %in% c(2021:2025)]
byc_dat <- byc_dat[MetierL3 == 'L3LL'] 
byc_dat <- byc_dat[MetierL4 %in% c('LLD','LLS')] ## Keep only relevant Metiers
byc_dat <- byc_dat[MetierL5 %in% c('DEF','DWS')] ## Keep only demersal fish assemblage
byc_dat <- byc_dat[Ecoregion %notin% c(
  'Adriatic Sea',
  'Aegean-Levantine Sea',
  'Black Sea',
  'Ionian Sea and the Central Mediterranean Sea',
  'Western Mediterranean Sea')]

#### Norwegian data #----
no_dat <- fread("NO data/aggregated_data_fishdays.csv") ## Norwegian data
no_dat$country <- 'NO'
no_dat$areaCode <- stringr::str_replace_all(no_dat$areaCode,
                                            pattern = '_', 
                                            replacement = '.')
no_dat$areaCode <- stringr::str_replace(no_dat$areaCode,
                                        pattern = '27.3.A.NK', 
                                        replacement = '27.3.a.20')
no_dat <- no_dat %>% 
  mutate(areaCode = case_when(startsWith(areaCode,
                                         '27') ~ tolower(areaCode),
                              .default = areaCode))
no_dat <- no_dat[year %in% c(2021:2025)]
no_dat <- no_dat[!(no_dat$areaCode == ""),] ## rm rows with missing info on fishing location
no_dat <- no_dat[areaCode %notin% c('0-A','0-B',
                                    '2H','2J','6G',
                                    '21.3.M')]

#### Greenlandic data #----
load("Gr data/fo_long_lines_EastGr.RData") ## East Greenland data (only FO effort)
gr_east_data <- as.data.table(fo_long_lines_EastGr)
gr_east_data$country <- 'FO'
rm(fo_long_lines_EastGr)
load("Gr data/Gr_Longline_data_Aggregated.RData") ## Greenland data (incl. NAFO)
gr_data <- as.data.table(gr_data)
gr_data$country <- 'GL'
gr_data <- rbind(gr_data, gr_east_data[, areaType := NA_character_])
gr_data <- gr_data[year %in% c(2021:2025)]
gr_data[, quarter := case_when(month %in% c(1:3) ~ 1,
                               month %in% c(4:6) ~ 2,
                               month %in% c(7:9) ~ 3,
                               month %in% c(10:12) ~ 4,
                               .default = NA_integer_)]
gr_data <- gr_data[areaCode %notin% c('34.2.0','34.4.2')]

#### Faroese data #----
# fo_dat <- fread("something.csv")
# fo_dat <- fo_dat

## Bind fishing effort datasets together #----
dat <- rbind(byc_dat, gr_data, no_dat, fill = TRUE)
# dat <- rbind(byc_dat, gr_data, no_dat, fo_dat, fill = TRUE)

#### Align the variables across datasets #----
dat$vesselLengthRange <- as.factor(dat$vesselLengthRange)
dat[, length.cat :=
      case_when(vesselLengthRange %in% c("VL0006", "VL0008", "VL0010",
                                         "VL0608", "VL0612", "VL0810", 
                                         "VL1012", 
                                         ## Some assumptions here
                                         "F", "NK", "Unknown", "Other") ~ "<12m",
                vesselLengthRange %in% c("VL1215", "VL1218", "VL1518",
                                         "VL1524", "VL1824") ~ "12m-24m",
                vesselLengthRange %in% c("VL2440", "VL40XX") ~ ">24m",
                .default = NA)]
dat[, quarter_split := fifelse(quarter %in% c(1,4),
                               'Q4Q1', ## proxy for non-breeding period
                               'Q2Q3')] ##proxy for breeding period
dat[, areaCode := case_when(
  areaCode == 'ICES XIVa' ~ '27.4.a',
  areaCode == 'ICES XIVb2' ~ '27.4.b',
  areaCode == '27.1.a' ~ '27.1',
  areaCode == '27.1.b' ~ '27.1',
  areaCode == '27.3.d.28.1' ~ '27.3.d.28',
  areaCode == '27.3.d.28.2' ~ '27.3.d.28',
  startsWith(areaCode, '1')  ~ paste('21', areaCode, sep = '.'),
  .default = areaCode)]
dat <- subset(dat, areaCode %notin% c('0-A','0-B','2H','2J','6G'))
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'A','.A')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'B','.B')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'C','.C')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'D','.D')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'E','.E')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'F','.F')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'G','.G')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'H','.H')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'J','.J')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'K','.K')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'L','.L')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'M','.M')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'N','.N')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'O','.O')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'P','.P')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'R','.R')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'S','.S')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'T','.T')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'V','.V')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'W','.W')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'X','.X')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'Y','.Y')
dat$areaCode <- stringr::str_replace_all(dat$areaCode,'Z','.Z')
# dat[, division := sub("\\.[0-9]+$", "", areaCode)]
dat$division <- dat$areaCode
dat[, division := case_when(
  division == '27.10.a.1' ~ '27.10.a',
  division == '27.10.a.2' ~ '27.10.a',
  division == '27.12.a.1' ~ '27.12.a',
  division == '27.12.a.2' ~ '27.12.a',
  division == '27.12.a.3' ~ '27.12.a',
  division == '27.12.a.4' ~ '27.12.a',
  division == '27.14.b.1' ~ '27.14.b',
  division == '27.14.b.2' ~ '27.14.b',
  division == '27.2.a.1' ~ '27.2.a',
  division == '27.2.a.2' ~ '27.2.a',
  division == '27.2.b.1' ~ '27.2.b',
  division == '27.2.b.2' ~ '27.2.b',
  division == '27.5.a.1' ~ '27.5.a',
  division == '27.5.a.2' ~ '27.5.a',
  division == '27.5.b.1.a' ~ '27.5.b',
  division == '27.5.b.1.b' ~ '27.5.b',
  division == '27.6.b.1' ~ '27.6.b',
  division == '27.6.b.2' ~ '27.6.b',
  division == '27.7.c.1' ~ '27.6.b',
  division == '27.7.c.2' ~ '27.6.b',
  division == '27.7.j.1' ~ '27.7.j',
  division == '27.7.j.2' ~ '27.7.j',
  division == '27.7.k.1' ~ '27.7.k',
  division == '27.7.k.2' ~ '27.7.k',
  division == '27.8.d.1' ~ '27.8.d',
  division == '27.8.d.2' ~ '27.8.d',
  division == '27.9.b.1' ~ '27.9.b',
  division == '27.9.b.2' ~ '27.9.b',
  .default = division)]

#### Reduce effort dataset size
fwrite(subset(dat,
              select = c("areaCode","length.cat","country",
                         "year","quarter","month","daysAtSeaF")),
              'effort.data.csv')

## Prepare the data for plotting #----
summary_dat <- dat %>% 
  group_by(division,length.cat) %>% 
  summarise(meanDaS = round(mean(daysAtSeaF, na.rm = T)),
            medianDaS = round(median(daysAtSeaF, na.rm = T)),
            sdDaS = round(sd(daysAtSeaF, na.rm = T))) 
# summary(summary_dat$meanDaS)
summary_dat <- as.data.table(summary_dat)
# summary_dat[, quarter_split := fifelse(quarter %in% c(1,4),
#                                        'Q4Q1',
#                                        'Q2Q3')]
ICES_dat <- merge(ICES.areas.sub, 
                  summary_dat,
                  by = 'division',
                  all = TRUE) %>%
  mutate(log_meanDaS = log10(meanDaS + 1))
ICES_dat <- ICES_dat %>% 
  group_by(division) %>% 
  summarise(
    geometry = st_union(geometry),  # Merge geometries
    across(everything(), ~ first(.x, na.rm = TRUE))
  ) %>%
  st_as_sf()
NAFO_dat <- merge(NAFO.areas.sub,
                  summary_dat,
                  by = 'division',
                  all = TRUE) %>% 
  mutate(log_meanDaS = log10(meanDaS + 1))
ICES_NAFO_dat <- rbind(ICES_dat %>% 
                         dplyr::select(division,
                                       length.cat,
                                       meanDaS,medianDaS,sdDaS,
                                       log_meanDaS),
                       NAFO_dat %>% 
                         dplyr::select(division,
                                       length.cat,
                                       meanDaS,medianDaS,sdDaS,
                                       log_meanDaS))
# ICES_NAFO_dat$meanDaS <- replace_na(ICES_NAFO_dat$meanDaS, 0)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MAPS OF LL FISHING EFFORT #####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MAP EFFORT per vessel length category across the entire study are
### 3 maps with mean(das) or mean(log(das)) per subarea
#### Effort for smaller vessels (<12m) #----
effort.0.12 <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.areas.sub, 
          aes(group=division),
          fill = "grey"
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  geom_sf(data = NAFO.areas.sub, 
          aes(group=division),
          fill = "grey"
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat %>% 
            dplyr::filter(length.cat == '<12m'),
          aes(fill = meanDaS)
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  ## Choose a colour scale 
  scale_fill_fermenter(
    limits = c(0,1500),
    breaks = c(0, 10, 50, 100, 250, 500, 750, 1000, 1500),
    # transform = "pseudo_log",
    name = "Mean yearly effort (DaS)", 
    palette = "YlGnBu", #"Blues", #"YlOrRd", #
    type = 'seq',
    direction = 1,
    na.value = "grey") +
  ## Plot world map on top (because it should look better)
  geom_sf(data = world %>%
            dplyr::filter(continent %in% c("Europe","North America")), 
          fill = 'black', colour = 'black', size = 0) +
  ## Limit the map to the area of interest
  coord_sf(
    crs = "EPSG:32633",
    xlim = c(-75, 40),
    ylim = c(33, 70),
    default_crs = sf::st_crs(4326), 
    expand = FALSE
  ) +
  ## Title of the sub-plot
  ggtitle("Vessels <12m") +
  ## Theme
  theme_minimal() +
  theme(
    # plot.background = element_rect(fill = "grey",
    #                                linewidth = 0,
    #                                color = NA),
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(20, 'mm'),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text.x =  element_blank()
  )
# effort.0.12
ggsave(filename = 'effort.0.12.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort.0.12,
       device = "png")

#### Effort for smaller vessels (12m-24m) #----
effort.12.24 <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.areas.sub, 
          aes(group=division),
          fill = "grey"
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  geom_sf(data = NAFO.areas.sub, 
          aes(group=division),
          fill = "grey"
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat %>% 
            dplyr::filter(length.cat == '12m-24m'),
          aes(fill = meanDaS)
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  ## Choose a colour scale 
  scale_fill_fermenter(
    limits = c(0,1500),
    breaks = c(0, 10, 50, 100, 250, 500, 750, 1000, 1500),
    # transform = "pseudo_log",
    name = "Mean yearly effort (DaS)", 
    palette = "Blues", #"YlOrRd", #"YlGnBu",
    type = 'seq',
    direction = 1,
    na.value = "grey") +
  ## Plot world map on top (because it should look better)
  geom_sf(data = world %>%
            dplyr::filter(continent %in% c("Europe","North America")), 
          fill = 'black', colour = 'black', size = 0) +
  ## Limit the map to the area of interest
  coord_sf(
    crs = "EPSG:32633",
    xlim = c(-75, 40),
    ylim = c(33, 70),
    default_crs = sf::st_crs(4326), 
    expand = FALSE
  ) +
  ## Title of the sub-plot
  ggtitle("Vessels 12m-24m") +
  ## Theme
  theme_minimal() +
  theme(
    # plot.background = element_rect(fill = "grey",
    #                                linewidth = 0,
    #                                color = NA),
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(20, 'mm'),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text.x =  element_blank()
  )
# effort.12.24
ggsave(filename = 'effort.12.24.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort.12.24,
       device = "png")

#### Effort for larger vessels (>24m) #----
effort.24plus <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.areas.sub, 
          aes(group=division),
          fill = "grey"
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  geom_sf(data = NAFO.areas.sub, 
          aes(group=division),
          fill = "grey"
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat %>% 
            dplyr::filter(length.cat == '>24m'),
          aes(fill = meanDaS)
          # ,
          # colour = NA,  # Remove the border
          # size = 0
  ) +
  ## Choose a colour scale 
  scale_fill_fermenter(
    limits = c(0,1500),
    breaks = c(0, 10, 50, 100, 250, 500, 750, 1000, 1500),
    # transform = "pseudo_log",
    name = "Mean yearly effort (DaS)", 
    palette = "Blues", #"YlOrRd", #"YlGnBu",
    type = 'seq',
    direction = 1,
    na.value = "grey") +
  ## Plot world map on top (because it should look better)
  geom_sf(data = world %>%
            dplyr::filter(continent %in% c("Europe","North America")), 
          fill = 'black', colour = 'black', size = 0) +
  ## Limit the map to the area of interest
  coord_sf(
    crs = "EPSG:32633",
    xlim = c(-75, 40),
    ylim = c(33, 70),
    default_crs = sf::st_crs(4326), 
    expand = FALSE
  ) +
  ## Title of the sub-plot
  ggtitle("Vessels >24m") +
  ## Theme
  theme_minimal() +
  theme(
    # plot.background = element_rect(fill = "grey",
    #                                linewidth = 0,
    #                                color = NA),
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(20, 'mm'),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text.x =  element_blank()
  )
# effort.24plus
ggsave(filename = 'effort.24plus.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort.24plus,
       device = "png")

### PLOT ALL 3 MAPS TOGETHER #----
effort_plot <- 
  effort.0.12 + effort.12.24 + effort.24plus 
map.full <- effort_plot + 
  plot_layout(ncol = 3, nrow = 1, 
              guides = "collect") & 
  theme(legend.position = "bottom")
ggsave(filename = 'map.LL.effort.per.length.png',
       units = 'mm',
       width = 297, height = 297,
       plot = map.full,
       device = "png")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






































































































# ### MAP EFFORT BREEDING SEASON OFFSHORE FLEET (top left) ####
# effort_breed_off <- ggplot() +
#   ## Plot Background
#   geom_sf(data = ICES.areas,
#           aes(group=Area_27),
#           fill = "white",
#           size = 0) +
#   geom_sf(data = NAFO.areas, 
#           aes(group=Division),
#           fill = "white",
#           size = 0) +
#   ## Plot Effort data ICES (max(ICES_dat$meanDaS) = 115)
#   geom_sf(data = ICES_NAFO_dat %>% 
#             dplyr::filter(in.off.shore == 'offshore') %>% 
#             dplyr::filter(quarter_split == 'Q2Q3') ,
#           aes(fill = log_meanDaS) ) +
#   ## Choose a colour scale 
#   scale_fill_fermenter(
#     limits = c(0,3.12),
#     breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 3.12),
#     # transform = "pseudo_log",
#     name = "Mean quarterly effort (DaS)", #"Offshore Quarters 2 and 3",
#     palette = "Blues", #"YlOrRd", #"YlGnBu",
#     type = 'seq',
#     direction = 1,
#     na.value = "white") +
#   ## Plot world map on top (because it should look better)
#   geom_sf(data = world %>%
#             dplyr::filter(continent %in% c("Europe","North America")), 
#           fill = 'black', colour = 'black') +
#   ## Limit the map to the area of interest
#   coord_sf(
#     crs = "EPSG:32633",
#     xlim = c(-75, 40),
#     ylim = c(33, 70),
#     default_crs = sf::st_crs(4326), 
#     expand = FALSE
#   ) +
#   ## Title of the sub-plot
#   ggtitle("OFFSHORE FLEET Q2-Q3") +
#   ## Theme
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0, vjust = 0,
#                               color="black",
#                               size = 16, face = "bold"),
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     legend.justification = "center",
#     legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
#     legend.text = element_text(size = 12, vjust = 0.7),
#     legend.key.width = unit(20, 'mm'),
#     panel.border = element_rect(colour = "black",
#                                 fill = NA, linewidth = 1.5),
#     axis.title = element_blank(),
#     axis.text.x =  element_blank()
#   )
# # effort_breed_off
# ggsave(filename = 'Offshore_Q2Q3.png',
#        units = 'mm',
#        width = 297, height = 297,
#        plot = effort_breed_off,
#        device = "png")
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### MAP EFFORT BREEDING SEASON INSHORE FLEET (top right) ####
# effort_breed_in <- ggplot() +
#   
#   ## Plot Background
#   geom_sf(data = ICES.areas,
#           aes(group=Area_27),
#           fill = "white",
#           size = 0) +
#   geom_sf(data = NAFO.areas, 
#           aes(group=Division),
#           fill = "white",
#           size = 0) +
#   
#   ## Plot background
#   geom_sf(data = ICES_NAFO_dat %>% 
#             dplyr::filter(in.off.shore == 'inshore') %>% 
#             dplyr::filter(quarter_split == 'Q2Q3'),
#           aes(fill = log_meanDaS) ) +
#   
#   ## Choose a colour scale 
#   scale_fill_fermenter(
#     limits = c(0, 3.12),
#     breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 3.12),
#     # transform = "pseudo_log",
#     name = "Mean quarterly effort (DaS)", #"Inshore Quarters 2 and 3",
#     palette = "Blues",#"YlOrRd",#"YlGnBu",#
#     type = 'seq',
#     direction = 1,
#     na.value = "white") +
#   
#   ## Plot world on top (because it should look better)
#   geom_sf(data = world %>%
#             dplyr::filter(continent %in% c("Europe","North America")), 
#           fill = 'black', colour = 'black') +
#   
#   ## Limit the map to the area of interest
#   coord_sf(
#     crs = "EPSG:32633",
#     xlim = c(-75, 40),
#     ylim = c(33, 70),
#     default_crs = sf::st_crs(4326), 
#     expand = FALSE
#   ) +
#   
#   ## Title of the sub-plot
#   ggtitle("INSHORE FLEET Q2-Q3") +
#   
#   ## Theme
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0, vjust = 0,
#                               color="black",
#                               size = 16, face = "bold"),
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     legend.justification = "center",
#     legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
#     legend.text = element_text(size = 12, vjust = 0.7),
#     legend.key.width = unit(20, 'mm'),
#     panel.border = element_rect(colour = "black",
#                                 fill = NA, linewidth = 1.5),
#     axis.title = element_blank(),
#     axis.text = element_blank()
#   )
# # effort_breed_in
# ggsave(filename = 'Inshore_Q2Q3.png',
#        units = 'mm',
#        width = 297, height = 297,
#        plot = effort_breed_in,
#        device = "png")
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### MAP EFFORT NON BREEDING SEASON OFFSHORE FLEET (bottom left) ####
# effort_nonbreed_off <- ggplot() +
#   
#   ## Plot Background
#   geom_sf(data = ICES.areas,
#           aes(group=Area_27),
#           fill = "white",
#           size = 0) +
#   geom_sf(data = NAFO.areas, 
#           aes(group=Division),
#           fill = "white",
#           size = 0) +
#   
#   ## Plot background
#   geom_sf(data = ICES_NAFO_dat  %>% 
#             dplyr::filter(in.off.shore == 'offshore') %>% 
#             dplyr::filter(quarter_split == 'Q4Q1'),
#           aes(fill = log_meanDaS) ) +
#   
#   ## Choose a colour scale 
#   scale_fill_fermenter(
#     limits = c(0, 3.12),
#     breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 3.12),
#     # transform = "pseudo_log",
#     name = "Mean quarterly effort (DaS)", #"Offshore Quarters 4 and 1",
#     palette = "Blues",#"YlOrRd",#"YlGnBu",#
#     type = 'seq',
#     direction = 1,
#     na.value = "white") +
#   
#   ## Plot world on top (because it should look better)
#   geom_sf(data = world %>%
#             dplyr::filter(continent %in% c("Europe","North America")), 
#           fill = 'black', colour = 'black') +
#   
#   ## Limit the map to the area of interest
#   coord_sf(
#     crs = "EPSG:32633",
#     xlim = c(-75, 40),
#     ylim = c(33, 70),
#     default_crs = sf::st_crs(4326), 
#     expand = FALSE
#   ) +
#   
#   ## Title of the sub-plot
#   ggtitle("OFFSHORE FLEET Q4-Q1") +
#   
#   ## Theme
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0, vjust = 0,
#                               color="black",
#                               size = 16, face = "bold"),
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     legend.justification = "center",
#     legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
#     legend.text = element_text(size = 12, vjust = 0.7),
#     legend.key.width = unit(20, 'mm'),
#     panel.border = element_rect(colour = "black",
#                                 fill = NA, linewidth = 1.5),
#     axis.title = element_blank()
#   )
# # effort_nonbreed_off
# ggsave(filename = 'Offshore_Q4Q1.png',
#        units = 'mm',
#        width = 297, height = 297,
#        plot = effort_nonbreed_off,
#        device = "png")
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### MAP EFFORT NON BREEDING SEASON INSHORE FLEET (bottom right) ####
# effort_nonbreed_in <- ggplot() +
#   
#   ## Plot Background
#   geom_sf(data = ICES.areas,
#           aes(group=Area_27),
#           fill = "white",
#           size = 0) +
#   geom_sf(data = NAFO.areas, 
#           aes(group=Division),
#           fill = "white",
#           size = 0) +
#   
#   ## Plot background
#   geom_sf(data = ICES_NAFO_dat %>% 
#             dplyr::filter(in.off.shore == 'inshore') %>% 
#             dplyr::filter(quarter_split == 'Q4Q1'),
#           aes(fill = log_meanDaS) ) +
#   
#   ## Choose a colour scale 
#   scale_fill_fermenter(
#     limits = c(0, 3.12),
#     breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 3.12),
#     # transform = "pseudo_log",
#     name = "Mean quarterly effort (DaS)", #"Inshore Quarters 4 and 1",
#     palette = "Blues",#"YlOrRd",#"YlGnBu",#
#     type = 'seq',
#     direction = 1,
#     na.value = "white") +
#   
#   ## Plot world on top (because it should look better)
#   geom_sf(data = world %>%
#             dplyr::filter(continent %in% c("Europe","North America")), 
#           fill = 'black', colour = 'black') +
#   
#   ## Limit the map to the area of interest
#   coord_sf(
#     crs = "EPSG:32633",
#     xlim = c(-75, 40),
#     ylim = c(33, 70),
#     default_crs = sf::st_crs(4326), 
#     expand = FALSE
#   ) +
#   
#   ## Title of the sub-plot
#   ggtitle("INSHORE FLEET Q4-Q1") +
#   
#   ## Theme
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0, vjust = 0,
#                               color="black",
#                               size = 16, face = "bold"),
#     legend.position = "bottom",
#     legend.direction = "horizontal",
#     legend.justification = "center",
#     legend.title = element_text(size = 20, face = "bold", vjust = 0.95),
#     legend.text = element_text(size = 12, vjust = 0.7),
#     legend.key.width = unit(20, 'mm'),
#     panel.border = element_rect(colour = "black",
#                                 fill = NA, linewidth = 1.5),
#     axis.title = element_blank(),
#     axis.text.y = element_blank()
#   )
# # effort_nonbreed_in
# ggsave(filename = 'Inshore_Q4Q1.png',
#        units = 'mm',
#        width = 297, height = 297,
#        plot = effort_nonbreed_in,
#        device = "png")
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### PLOT ALL 4 MAPS TOGETHER ####
# effort_plot <- 
#   ## Top row
#   effort_breed_in + effort_breed_off +
#   ## Bottom row
#   effort_nonbreed_in + effort_nonbreed_off
# map.full <- effort_plot + 
#   plot_layout(ncol = 2, nrow = 2, 
#               guides = "collect") & 
#   theme(legend.position = "bottom")
# ggsave(filename = 'map.LL.effort.png',
#        units = 'mm',
#        width = 297, height = 297,
#        plot = map.full,
#        device = "png")
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## PLOT LINE PLOT ####
# dat[, area := case_when(startsWith(areaCode, '21') ~ substr(areaCode, 1, nchar(areaCode)-2),
#                         startsWith(areaCode, '27.10') ~ '27.10',
#                         startsWith(areaCode, '27.11') ~ '27.11',
#                         startsWith(areaCode, '27.12') ~ '27.12',
#                         startsWith(areaCode, '27.13') ~ '27.13',
#                         startsWith(areaCode, '27.14') ~ '27.14',
#                         startsWith(areaCode, '27.1.') ~ '27.1',
#                         startsWith(areaCode, '27.2.') ~ '27.2',
#                         startsWith(areaCode, '27.3.') ~ '27.3',
#                         startsWith(areaCode, '27.4.') ~ '27.4',
#                         startsWith(areaCode, '27.5.') ~ '27.5',
#                         startsWith(areaCode, '27.6.') ~ '27.6',
#                         startsWith(areaCode, '27.7.') ~ '27.7',
#                         startsWith(areaCode, '27.8.') ~ '27.8',
#                         startsWith(areaCode, '27.9.') ~ '27.9',
#                         .default = NA_character_)]
# summary_dat2 <- dat %>% 
#   group_by(quarter,area,in.off.shore) %>% 
#   summarise(meanDaS = round(mean(daysAtSeaF)),
#             medianDaS = round(median(daysAtSeaF)),
#             sdDaS = round(sd(daysAtSeaF)),
#             log_meanDaS = log10(meanDaS + 1))
# line.plot.full2 <- ggplot(summary_dat2 %>% 
#                             mutate(meanDaS = ifelse(meanDaS>0,
#                                                     meanDaS,
#                                                     NA_integer_)),
#                           aes(x = quarter, 
#                               y = meanDaS, 
#                               group = area)) +
#   geom_line(aes(colour = area), linetype = 1) +
#   scale_y_continuous(trans='log10') +
#   theme_minimal() +
#   theme(
#     plot.title = element_blank(),
#     panel.border = element_rect(colour = "black",
#                                 fill = NA, linewidth = 1.5),
#     legend.position = "right",
#     axis.title = element_blank()
#   )
# line.plot2 <- line.plot.full2 + facet_grid(vars(in.off.shore), scales = "free")
# line.plot2
# dat.27.10 <- subset(dat, area == '27.10')
# summary_dat3 <- dat.27.10 %>% 
#   group_by(quarter,areaCode,in.off.shore) %>% 
#   summarise(meanDaS = round(mean(daysAtSeaF)),
#             medianDaS = round(median(daysAtSeaF)),
#             sdDaS = round(sd(daysAtSeaF)))
# line.plot.full3 <- ggplot(summary_dat3 %>% 
#                             mutate(meanDaS = ifelse(meanDaS>0,meanDaS,NA_integer_)),
#                           aes(x = quarter, 
#                               y = meanDaS, 
#                               group = areaCode)) +
#   geom_line(aes(colour = areaCode), linetype = 1) +
#   scale_y_continuous(trans='log10') +
#   theme_minimal() +
#   theme(
#     plot.title = element_blank(),
#     panel.border = element_rect(colour = "black",
#                                 fill = NA, linewidth = 1.5),
#     legend.position = "right",
#     axis.title = element_blank()
#   )
# 
# line.plot3 <- line.plot.full3 + facet_grid(vars(in.off.shore), scales = "free")
# 
# line.plot3
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dat.27.8 <- subset(dat, area == '27.8')
# 
# summary_dat4 <- dat.27.8 %>% 
#   group_by(quarter,areaCode,in.off.shore) %>% 
#   summarise(meanDaS = round(mean(daysAtSeaF)),
#             medianDaS = round(median(daysAtSeaF)),
#             sdDaS = round(sd(daysAtSeaF)))
# line.plot.full4 <- ggplot(summary_dat4 %>% 
#                             mutate(meanDaS = ifelse(meanDaS>0,meanDaS,NA_integer_)),
#                           aes(x = quarter, 
#                               y = meanDaS, 
#                               group = areaCode)) +
#   geom_line(aes(colour = areaCode), linetype = 1) +
#   scale_y_continuous(trans='log10') +
#   theme_minimal() +
#   theme(
#     plot.title = element_blank(),
#     panel.border = element_rect(colour = "black",
#                                 fill = NA, linewidth = 1.5),
#     legend.position = "right",
#     axis.title = element_blank()
#   )
# 
# line.plot4 <- line.plot.full4 + facet_grid(vars(in.off.shore), scales = "free")
# 
# line.plot4