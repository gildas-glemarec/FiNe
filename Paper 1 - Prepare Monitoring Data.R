## Author: Gildas Glemarec (DTU Aqua)
###
### NB.  Just make sure you put all the monitoring effort files in the main directory where
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
p_load(tidyverse,data.table,sf)
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
### Monitoring data (data all FiNE countries) ----
dat_mon <- fread("dat_monitoring_withByc.csv")
dat_mon$set.date <- dmy(dat_mon$set.date)
dat_mon$haul.date <- dmy(dat_mon$haul.date)
dat_mon$num.hooks.shot <- as.numeric(dat_mon$num.hooks.shot)
dat_mon$num.hooks.lost <- as.numeric(dat_mon$num.hooks.lost)
# table(dat_mon$division, useNA = 'always')
dat_mon$id <- seq_len(nrow(dat_mon))
temp <- dat_mon[division == '']
temp_sf <- st_as_sf(temp, coords = c("haul.lon.start", "haul.lat.start"),
                        crs = st_crs(ICES.divisions))
temp <- as.data.table(st_join(
  temp_sf %>% filter(id %in% temp$id),
  ICES.divisions,
  join = st_within,
  left = T))
setkey(dat_mon, id)
setkey(temp, id)
dat_mon[temp, division := fifelse(division == '' & !is.na(i.division.y), 
                                 i.division.y, division)]
# table(dat_mon$division, useNA = 'always')
dat_mon[, division := case_when(
  division == '21.1' ~ '21.1.A',
  division == '27.10.a.2' ~ '27.10.a',
  division == '27.3.d.26' ~ '27.3.d',
  .default = division
)]
fwrite(dat_mon, 'dat_monitoring.csv')