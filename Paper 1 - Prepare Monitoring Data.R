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
dat_mon <- fread("mon_data_merged/dat_monitoring_withByc.csv")
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
dat_mon[, length.cat := fifelse(length.cat == '', 
                                case_when(vessel.length < 12 ~ "<12m",
                                          vessel.length > 24 ~ ">24m",
                                          between(vessel.length, lower = 12, upper = 24,
                                                  incbounds = F) ~ "12m-24m",
                                          .default = NA_character_),
                                length.cat)]
fwrite(dat_mon, 'dat_monitoring.csv')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dat_mon <- fread("WKFINE.mon.data.csv")
# setnames(dat_mon, old = dput(names(dat_mon)), 
#          new = c('country','trip.no','haul.no','vessel.length',
#                  'set.date','haul.date','set.time.start','set.time.end','soak',
#                  'haul.time.start','haul.time.end','haul.lat.start',
#                  'haul.lon.start','haul.lat.end','haul.lon.end',
#                  'perc.haul.obs','view.lines','LL.config','target','bait.type',
#                  'bait.cond','num.hooks.shot','num.hooks.lost','IMR.qual',
#                  'LL.length','mitig.set','mitig.haul','depth','sea.state',
#                  'wind','cloud','LL.material','LL.diam','snood.length',
#                  'hook.size','hook.type','dist.snood.hook','float.type',
#                  'float.interv','tot.catch.weight','tot.catch.num','LL.set.type',
#                  'BSL.length','comments.set','attach.pos.set','LL.haul.type',
#                  'comments.haul','attach.pos.haul',
#                  'fulmar','gannet','skua','gbb.gull','kittiwake','g.gull',
#                  'h.gull','lbb.gull','fulmar2','gannet2','unid.gull',
#                  'smthg.else'))
# dat_mon$haul.lat.start <- as.numeric(dat_mon$haul.lat.start)
# dat_mon$haul.lon.start <- as.numeric(dat_mon$haul.lon.start)
# dat_mon$set.date <- lubridate::dmy(dat_mon$set.date)
# dat_mon$haul.date <- lubridate::dmy(dat_mon$haul.date)
# # dat_mon$set.time.start <- lubridate::hms(dat_mon$set.time.start)
# # dat_mon$set.time.end <- lubridate::hms(dat_mon$set.time.end)
# dat_mon$soak <- as.numeric(dat_mon$soak)
# dat_mon$haul.lon.end <- as.numeric(dat_mon$haul.lon.end)
# dat_mon$depth <- as.numeric(dat_mon$depth)
# dat_mon[, ID := paste0(country, trip.no, haul.no, haul.time.start)]
# setkey(dat_mon, ID)
# dat.points <- st_as_sf(subset(dat_mon, 
#                               select = c("ID",
#                                          "haul.lon.start",
#                                          "haul.lat.start")),
#                        coords = c("haul.lon.start",
#                                   "haul.lat.start"),
#                        crs = st_crs(ICES.divisions)) ## Assuming this is true
# divisions.ICES <- st_intersection(dat.points, ICES.divisions)
# divisions.NAFO <- st_intersection(dat.points, NAFO.divisions)
# divisions.ICES.NAFO <- rbind(divisions.ICES %>% 
#                                dplyr::select(ID, division), 
#                              divisions.NAFO %>% 
#                                dplyr::select(ID, division))
# dat_mon.w.divisions <- left_join(dat_mon, divisions.ICES.NAFO, by = 'ID')
# dat_mon.w.divisions <- dat_mon.w.divisions %>% 
#   ## Making up the NA's in division "by hand"
#   mutate(division = if_else(condition = !is.na(division), 
#                             division,
#                             case_when(startsWith(ID, 'GR') ~ '21.1',
#                                       startsWith(ID, 'UK') ~ '27.7.a',
#                                       startsWith(ID, 'IS') ~ '27.5.a',
#                                       .default = NA_character_))) %>% 
#   ## Update count data for fulmars and gannets (or ignore them? These are birds
#   ## registered as "bycatch" but not dead)
#   mutate(fulmar2 = replace_na(fulmar2, 0) ) %>% 
#   mutate(gannet2 = replace_na(gannet2, 0) ) %>% 
#   mutate(fulmar = fulmar + fulmar2) %>% 
#   mutate(gannet = gannet + gannet2) %>% 
#   dplyr::select(-ID, -geometry, -fulmar2, -gannet2)
# 
# ## Missing info on vessel length so stuck for now.
# fwrite(dat_mon.w.divisions, 'dat_monitoring.csv')
