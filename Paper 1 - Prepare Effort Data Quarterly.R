##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Prepare Fishing Effort data for WKFINE
## Author: Gildas Glemarec (DTU Aqua)
###
### NB.  Just make sure you put all the effort files in the main directory where
###      you saved this script. Shapefiles for ICES and NAFO need to be placed 
###      in a folder called shapefiles where you unzip those:
###        https://gis.ices.dk/shapefiles/ICES_areas.zip
###        https://www.nafo.int/Portals/0/GIS/Divisions.zip
### EFFORT DATASETS:
### 1. Aggregate all national effort data to the level used in WGBYC
###       * #DaS / month / year grouped by division (ICES/NAFO) & length class
### 2. Merge and summarise the data
### (3. Plot on a map)
### MONITORING DATSET
###
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
p_load(tidyverse,data.table, sf)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD FUNCTIONS ----
`%notin%` <- Negate(`%in%`)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD DATA ----
### Shapefiles ----
#### ICES divisions (& fixes) ----
ICES.divisions <- sf::st_read(
  dsn = "shapefiles/",
  layer = "ICES_Areas_20160601_cut_dense_3857")
ICES.divisions <- st_make_valid(ICES.divisions) ## Fix invalid polygons
ICES.divisions <- ICES.divisions %>% 
  mutate( division = paste(Major_FA,SubArea,Division, sep = '.') ) %>% 
  mutate( division = if_else( division %in% c('27.1.a', '27.1.b'),
                              '27.1',
                              division) )
ICES.divisions <- ICES.divisions %>%
  group_by( division, .add = T ) %>%
  summarise(geometry = st_union(geometry), 
            is_coverage = TRUE) %>%
  ungroup()
ICES.divisions <- st_transform(ICES.divisions, crs = 4326)
# plot(ICES.divisions$geometry)
#### NAFO divisions (& fixes) ----
NAFO.divisions <- sf::st_read(
  dsn = "shapefiles/",
  layer = "NAFO_Divisions_2021_poly_clipped")
NAFO.divisions <- st_transform(NAFO.divisions, st_crs(ICES.divisions))
NAFO.divisions$division <- paste('21', NAFO.divisions$Division, sep = '.')
NAFO.divisions$division <- stringr::str_replace_all(NAFO.divisions$division,
                                                pattern = '-', 
                                                replacement = '')
NAFO.divisions$division <- gsub("([A-Z])$", ".\\1", NAFO.divisions$division)
NAFO.divisions.sub <- subset(NAFO.divisions, 
                         division %in% c('21.1.A', '21.1.B',
                                         '21.1.C', '21.1.D',
                                         '21.1.E', '21.1.F'))

### Fishing Effort data ----
#### WGBYC data ----
byc_dat <- fread("BYC_data/FishingEffort_AllYears_Extraction_27092025.csv") ## WGBYC data
byc_dat$areaCode <- tolower(byc_dat$AreaCode)
setnames(byc_dat, old = 'areaCode', new = 'division')
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
byc_dat <- byc_dat[MetierL5 %in% c('DEF','DWS', 'FIF')] ## Keep only demersal fish assemblage
byc_dat <- byc_dat[Ecoregion %notin% c(
  'Adriatic Sea',
  'Aegean-Levantine Sea',
  'Black Sea',
  'Ionian Sea and the Central Mediterranean Sea',
  'Western Mediterranean Sea')]
#### Norwegian data ----
# no_dat <- fread("NO data/aggregated_data_fishdays.csv") ## Aggregated Norwegian data, but with missing values
no_dat <- fread("NO data/Effort_demersal longline_2011-24.csv")
#### Assuming that one row is one DaS (probably wrong):
no_dat$daysAtSeaF <- 1
no_dat$country <- 'NO'
no_dat$vessel_length <- as.numeric(no_dat$vessel_length)
no_dat[, length.cat := case_when(vessel_length < 12 ~ "<12m",
                                 vessel_length > 24 ~ ">24m",
                                 between(vessel_length, lower = 12, upper = 24,
                                         incbounds = F) ~ "12-24m",
                                 .default = NA_character_)]
no_dat <- st_as_sf(no_dat, 
                   coords = c("lon", "lat"),
                   crs = 4326)
no_dat <- st_transform(no_dat, st_crs(ICES.divisions))
no_dat$id <- seq_len(nrow(no_dat))
no_dat_sf <- st_as_sf(no_dat, coords = c("lon", "lat"),
                      crs = st_crs(ICES.divisions))
# #### Points in ICES division (works but slow)
# no_dat$division <- sapply(1:nrow(no_dat_sf), function(i) {
#   intersected <- st_intersection(no_dat_sf[i, ], ICES.divisions)
#   if (nrow(intersected) > 0) {
#     return(intersected$division[1])
#   } else {
#     return(NA)
#   }
# })
#### Points in ICES division (works much faster but would be even faster still with another crs)
no_dat <- as.data.table(st_join(no_dat_sf,
                                ICES.divisions,
                                join = st_within,
                                left = T))
#### Points in NAFO division
temp <- no_dat[is.na(division)]
temp <- as.data.table(st_join(
  no_dat_sf %>% filter(id %in% temp$id),
  NAFO.divisions,
  join = st_within,
  left = T))
setkey(no_dat, id)
setkey(temp, id)
no_dat[temp, division := fifelse(is.na(division) & !is.na(i.division), 
                                 i.division, division)]
#### Some NAs remain, so find the nearest polygon for each point
# table(no_dat$division, useNA = 'always')
#### Visual check: where are the NAs?
# plot(st_geometry(ICES.divisions), col = "lightblue", main = "ICES Divisions")
# points(st_coordinates(temp), col = "red", pch = 19)
#### Only on the ICES side, so we will put them in the closest ICES division:
temp <- no_dat[is.na(division),]
nearest_idx <- st_nearest_feature(no_dat[is.na(division),] %>% st_as_sf,
                                  ICES.divisions)
temp[, nearest.ICES.division := ICES.divisions$division[nearest_idx]]
setkey(temp, id)
no_dat[temp, division := fifelse(is.na(division), 
                                 i.nearest.ICES.division, 
                                 division)]
rm(no_dat_sf)
rm(temp)
no_dat[, date_fishing_start := dmy(date_fishing_start)]
no_dat[, year := year(date_fishing_start)]

no_dat <- no_dat[year %in% c(2021:2025)]
no_dat <- no_dat[!(no_dat$division == ""),] ## rm rows with missing info on fishing location

#### Greenlandic data ----
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
setnames(gr_data, old = 'areaCode', new = 'division')
gr_data[, division := 
          case_when(division == 'ICES XIVa' ~ '27.14.a',
                    division == 'ICES XIVb2' | division == '27.14.b.2' ~ '27.14.b',
                    .default = division)]
gr_data[, division := fifelse(!startsWith(division, '27.'),
                              paste('21', division, sep = '.'),
                              division)]
gr_data$division <- stringr::str_replace_all(gr_data$division,
                                             pattern = '-', 
                                             replacement = '')
gr_data$division <- gsub("([A-Z])$", ".\\1", gr_data$division)

#### Faroese data #----
#### NB. The data are already summarised to area/year, so we'll add later
fo_dat <- fread("FO data/FO_fishing_days_clean.csv")

## Bind BYC (which incl. UK + IS), NO, and GR fishing effort datasets together #----
dat_effort <- rbind(byc_dat, gr_data, no_dat, fill = TRUE)

#### Align the variables across datasets #----
dat_effort$vesselLengthRange <- as.factor(dat_effort$vesselLengthRange)
dat_effort$quarter <- as.factor(dat_effort$quarter)
dat_effort[, length.cat :=
             case_when(vesselLengthRange %in% c("VL0006", "VL0008", "VL0010",
                                                "VL0608", "VL0612", "VL0810", 
                                                "VL1012", 
                                                ## Some assumptions here
                                                "F", "NK", "Unknown", "Other") ~ "<12m",
                       vesselLengthRange %in% c("VL1215", "VL1218", "VL1518",
                                                "VL1524", "VL1824") ~ "12-24m",
                       vesselLengthRange %in% c("VL2440", "VL40XX") ~ ">24m",
                       .default = length.cat)]
dat_effort[, division := case_when(
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
  division == '27.3.a.20' ~ '27.3.a',
  division == '27.3.a.21' ~ '27.3.a',
  division == '27.3.b.23' ~ '27.3.b',
  division == '27.3.c.22' ~ '27.3.c',
  division == '27.3.d.24' ~ '27.3.d',
  division == '27.3.d.25' ~ '27.3.d',
  division == '27.3.d.26' ~ '27.3.d',
  division == '27.3.d.27' ~ '27.3.d',
  division == '27.3.d.28.1' ~ '27.3.d',
  division == '27.3.d.28.2' ~ '27.3.d',
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

## Summarise & add FO data ----
summary_dat_effort <- dat_effort %>% 
  group_by(division,quarter,year) %>% 
  summarise(
    sumDaS = sum(daysAtSeaF)) %>%
  ungroup()
summary_dat_effort <- summary_dat_effort %>% ## That's where we add FO effort data
  group_by(division,quarter,year) %>% 
  summarise(
    sumDaS = sum(sumDaS)) %>% 
  ungroup() %>% 
  group_by(division,quarter) %>% 
  summarise(
    meanDaS = round(mean(sumDaS, na.rm = T))) %>% 
  mutate(meanDaS = if_else(meanDaS==1, NA_integer_, meanDaS))
# summary(summary_dat_effort$meanDaS)
summary_dat_effort <- as.data.table(summary_dat_effort)
fwrite(summary_dat_effort, 'summary_dat_effort.csv')
