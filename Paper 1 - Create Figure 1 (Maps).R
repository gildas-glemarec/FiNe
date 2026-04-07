##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Prepare Fishing Effort and Maonitoring Coverage Mpas (FIGURE 1) for WKFINE
## Author: Gildas Glemarec (DTU Aqua)
###
### NB.  Just make sure you put all the effort files in the main directory where
###      you saved this script. Shapefiles for ICES and NAFO need to be placed 
###      in a folder called shapefiles where you unzip those:
###        https://gis.ices.dk/shapefiles/ICES_areas.zip
###        https://www.nafo.int/Portals/0/GIS/Divisions.zip
### 1. Get fishing effort data
### 2. Get Monitoring coverage data
### 3. Plot on a map
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
p_load(tidyverse,data.table,
       sf,giscoR,rnaturalearth,
       RColorBrewer,wesanderson,viridis,
       patchwork, ggnewscale, ggpattern)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD FUNCTIONS ----
`%notin%` <- Negate(`%in%`)
# get_ices_division <- function(lat, lon, divisions) {
#   point <- st_point(c(lon, lat))
#   point <- st_sf(geometry = st_sfc(point), crs = st_crs(divisions))
#   divivion <- 
#   }
## PALETTES ----
pal <- wes_palette("Zissou1", 100, type = "continuous")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOAD DATA ----
### Shapefiles ----
#### World coastlines ----
world <- ne_countries(
  scale = "medium",
  returnclass = "sf") %>%  
  dplyr::select(name, name_long, 
                iso_a2, iso_a3, 
                geometry, continent)
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
summary_dat_effort <- fread('summary_dat_effort.csv')
ICES_dat_effort <- merge(ICES.divisions, 
                         summary_dat_effort,
                         by = 'division',
                         all = TRUE) %>%
  st_as_sf()
NAFO_dat_effort <- merge(NAFO.divisions.sub,
                         summary_dat_effort,
                         by = 'division',
                         all = TRUE) %>% 
  st_as_sf()
ICES_NAFO_dat_effort <- rbind(ICES_dat_effort %>% 
                                dplyr::select(division,
                                              length.cat,
                                              meanDaS),
                              NAFO_dat_effort %>% 
                                dplyr::select(division,
                                              length.cat,
                                              meanDaS))

#### Monitoring coverage data (incomplete w. vessel length cat.)----
dat_mon <- fread('mon_data_merged/dat_monitoring.csv')
dat_mon <- dat_mon[length.cat != '']
dat_mon <- dat_mon[year %in% c(2021:2025)] ## Restrict monitoring period
summary_dat_mon <- dat_mon %>% 
  group_by(division,length.cat,year) %>% 
  summarise(
    sumObsDays = sum(daysAtSeaOb, na.rm = T)) %>%
  ungroup() %>%
  group_by(division,length.cat) %>%
  summarise(meanObsDays = round(mean(sumObsDays, na.rm = T)))
summary_dat_mon <- merge(summary_dat_effort,
                         summary_dat_mon,
                         by = c("division", "length.cat"),
                         all = T) %>% 
  mutate(meanObsDays  = replace_na(meanObsDays, 0)) %>% 
  mutate(ratio.mon = meanObsDays / meanDaS)

ICES_dat_mon <- merge(ICES.divisions, 
                      summary_dat_mon,
                      by = 'division',
                      all = TRUE) %>%
  st_as_sf()
NAFO_dat_mon <- merge(NAFO.divisions.sub,
                      summary_dat_mon,
                      by = 'division',
                      all = TRUE) %>% 
  st_as_sf()
ICES_NAFO_dat_mon <- rbind(ICES_dat_mon %>% 
                             dplyr::select(division,
                                           length.cat,
                                           ratio.mon),
                           NAFO_dat_mon %>% 
                             dplyr::select(division,
                                           length.cat,
                                           ratio.mon))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MAPS OF LL FISHING EFFORT & MONITORING ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MAP EFFORT per vessel length category across the entire study are
### 3 maps with mean(das) or mean(log(das)) per division
#### Effort for smaller vessels (<12m) ----
effort.0.12 <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.divisions, 
          aes(group=division),
          fill = "grey"
  ) +
  geom_sf(data = NAFO.divisions.sub, 
          aes(group=division),
          fill = "grey"
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat_effort %>% 
            dplyr::filter(length.cat == '<12m'),
          aes(fill = meanDaS)
  ) +
  ## Choose a colour scale 
  scale_fill_gradientn(colours = pal, 
                       name = "Mean yearly\neffort (DaS)",
                       limits = c(0, 10000),
                       na.value = "grey",
                       oob = scales::squish,
                       labels = c("0", "2500", "5000", "7500", ">10000")) +
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
    expand = F
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
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "right",     
    legend.key.height = unit(0.1, "npc"), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(10, 'mm'),     
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text =  element_blank()
  )
# effort.0.12
ggsave(filename = 'effort.0.12.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort.0.12,
       device = "png")

#### Effort for smaller vessels (12-24m) #----
effort.12.24 <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.divisions, 
          aes(group=division),
          fill = "grey"
  ) +
  geom_sf(data = NAFO.divisions.sub, 
          aes(group=division),
          fill = "grey"
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat_effort %>% 
            dplyr::filter(length.cat == '12-24m'),
          aes(fill = meanDaS)
  ) +
  ## Choose a colour scale
  scale_fill_gradientn(colours = pal, 
                       name = "Mean yearly\neffort (DaS)",
                       limits = c(0, 10000),
                       na.value = "grey",
                       oob = scales::squish,
                       labels = c("0", "2500", "5000", "7500", ">10000")) +
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
  ggtitle("Vessels 12-24m") +
  ## Theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "right",     
    legend.key.height = unit(0.1, "npc"), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(10, 'mm'),     
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text =  element_blank()
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
  geom_sf(data = ICES.divisions, 
          aes(group=division),
          fill = "grey"
  ) +
  geom_sf(data = NAFO.divisions.sub, 
          aes(group=division),
          fill = "grey"
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat_effort %>% 
            dplyr::filter(length.cat == '>24m'),
          aes(fill = meanDaS)
  ) +
  ## Choose a colour scale 
  scale_fill_gradientn(colours = pal, 
                       name = "Mean yearly\neffort (DaS)",
                       limits = c(0, 10000),
                       na.value = "grey",
                       oob = scales::squish,
                       labels = c("0", "2500", "5000", "7500", ">10000")) +
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
    plot.title = element_text(hjust = 0, vjust = 0,
                              color="black",
                              size = 16, face = "bold"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "right",     
    legend.key.height = unit(0.1, "npc"), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(10, 'mm'), 
    
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text =  element_blank()
  )
# effort.24plus
ggsave(filename = 'effort.24plus.png',
       units = 'mm',
       width = 297, height = 297,
       plot = effort.24plus,
       device = "png")

### PLOT ALL 3 MAPS TOGETHER #----
effort_map <- effort.0.12 + effort.12.24 + effort.24plus  +
  plot_annotation(title = "A", # "Mean Yearly Fishing Effort (DaS)", 
                  theme = theme(
                    plot.title = element_text(hjust = 0, vjust = 0,
                                              color="black",
                                              size = 20, face = "bold")))
map.full.effort <- (effort.0.12 | effort.12.24 | effort.24plus) + 
  plot_layout(axes = "collect", guides = "collect") & 
  theme(legend.position = "right")

ggsave(filename = 'map.LL.effort.per.length.png',
       units = 'mm',
       width = 297, height = 120,
       plot = map.full.effort,
       device = "png")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LL FISHING MONITORING vs EFFORT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Monitoring coverage per vessel length category across the entire study area
#### Monitoring coverage for smaller vessels (<12m) ----
mon.0.12 <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.divisions, 
          aes(group=division),
          fill = "grey"
  ) +
  geom_sf(data = NAFO.divisions.sub, 
          aes(group=division),
          fill = "grey"
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat_mon %>% 
            dplyr::filter(length.cat == '<12m'),
          aes(fill = ratio.mon)
  ) +
  ## Choose a colour scale 
  scale_fill_gradientn(colours = pal, 
                       oob = scales::squish,
                       name = "Mean\nmonitoring\ncoverage",
                       na.value = "grey",
                       guide = guide_colorbar(order = 1),
                       limits = c(0.00005, 0.25),
                       breaks = c(0.00005, 0.05, 0.1, 0.15, 0.2, 0.25),
                       labels = c(">0.00","0.05","0.10","0.15","0.20","0.25")
  ) +
  ## Select separate colour/pattern for zero monitoring divisions
  new_scale_fill() +
  geom_sf_pattern(
    data = ICES_NAFO_dat_mon %>%
      dplyr::filter(length.cat == '<12m') %>%
      dplyr::filter(ratio.mon == 0),
    aes(fill = "No\nmonitoring"),
    pattern = "stripe",
    pattern_colour = 'black',
    pattern_fill = "#2D8093",
    pattern_density = 0.5,
    pattern_spacing = 0.05,
    pattern_angle = 25
    ) +
  scale_fill_manual(
    values = c("No\nmonitoring" = '#2D8093'),
    name = "",
    guide = guide_legend(order = 2)
  ) +
  scale_pattern_manual(values = c("No\nmonitoring" = "stripe")) +
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
    expand = F
  ) +
  ## Title of the sub-plot
  # ggtitle("Vessels <12m") +
  ## Theme
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(-0.2, "cm"),
    legend.direction = "vertical",
    legend.margin = margin(0,0,0,0),
    legend.justification = "right",     legend.key.height = unit(0.1, "npc"), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(10, 'mm'),     
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text =  element_blank()
  )
ggsave(filename = 'mon.0.12.png',
       units = 'mm',
       width = 297, height = 200,
       plot = mon.0.12,
       device = "png")
#### Monitoring coverage for smaller vessels (12-24m) #----
mon.12.24 <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.divisions, 
          aes(group=division),
          fill = "grey"
  ) +
  geom_sf(data = NAFO.divisions.sub, 
          aes(group=division),
          fill = "grey"
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat_mon %>% 
            dplyr::filter(length.cat == '12-24m'),
          aes(fill = ratio.mon)
  ) +
  ## Choose a colour scale 
  scale_fill_gradientn(colours = pal, 
                       oob = scales::squish,
                       name = "Mean\nmonitoring\ncoverage",
                       na.value = "grey",
                       guide = guide_colorbar(order = 1),
                       limits = c(0.00005, 0.25),
                       breaks = c(0.00005, 0.05, 0.1, 0.15, 0.2, 0.25),
                       labels = c(">0.00","0.05","0.10","0.15","0.20","0.25")
  ) +
  ## Select separate colour/pattern for zero monitoring divisions
  new_scale_fill() +
  geom_sf_pattern(
    data = ICES_NAFO_dat_mon %>%
      dplyr::filter(length.cat == '12-24m') %>%
      dplyr::filter(ratio.mon == 0),
    aes(fill = "No\nmonitoring"),
    pattern = "stripe",
    pattern_colour = 'black',
    pattern_fill = "#2D8093",
    pattern_density = 0.5,
    pattern_spacing = 0.05,
    pattern_angle = 25
    ) +
  scale_fill_manual(
    values = c("No\nmonitoring" = '#2D8093'),
    name = "",
    guide = guide_legend(order = 2)
  ) +
  scale_pattern_manual(values = c("No\nmonitoring" = "stripe")) +
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
    expand = F
  ) +
  ## Title of the sub-plot
  # ggtitle("Vessels 12-24m") +
  ## Theme
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(-0.2, "cm"),
    legend.direction = "vertical",
    legend.margin = margin(0,0,0,0),
    legend.justification = "right",     legend.key.height = unit(0.1, "npc"), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(10, 'mm'),     
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text =  element_blank()
  )
ggsave(filename = 'mon.12.24.png',
       units = 'mm',
       width = 297, height = 200,
       plot = mon.12.24,
       device = "png")
#### Monitoring coverage for smaller vessels (>24m) #----
mon.24plus <- ggplot() +
  ## Plot Background
  geom_sf(data = ICES.divisions, 
          aes(group=division),
          fill = "grey"
  ) +
  geom_sf(data = NAFO.divisions.sub, 
          aes(group=division),
          fill = "grey"
  ) +
  ## Plot Effort data ICES
  geom_sf(data = ICES_NAFO_dat_mon %>% 
            dplyr::filter(length.cat == '>24m'),
          aes(fill = ratio.mon)
  ) +
  ## Choose a colour scale 
  scale_fill_gradientn(colours = pal, 
                       oob = scales::squish,
                       name = "Mean\nmonitoring\ncoverage",
                       na.value = "grey",
                       guide = guide_colorbar(order = 1),
                       limits = c(0.00005, 0.25),
                       breaks = c(0.00005, 0.05, 0.1, 0.15, 0.2, 0.25),
                       labels = c(">0.00","0.05","0.10","0.15","0.20","0.25")
  ) +
  ## Select separate colour/pattern for zero monitoring divisions
  new_scale_fill() +
  geom_sf_pattern(
    data = ICES_NAFO_dat_mon %>%
      dplyr::filter(length.cat == '>24m') %>%
      dplyr::filter(ratio.mon == 0),
    aes(fill = "No\nmonitoring"),
    pattern = "stripe",
    pattern_colour = 'black',
    pattern_fill = "#2D8093",
    pattern_density = 0.5,
    pattern_spacing = 0.05,
    pattern_angle = 25
    ) +
  ## Adding separate legend for zero monitoring
  scale_fill_manual(
    values = c("No\nmonitoring" = '#2D8093'),
    name = "",
    guide = guide_legend(order = 2)
  ) +
  scale_pattern_manual(values = c("No\nmonitoring" = "stripe")) +
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
    expand = F
  ) +
  ## Title of the sub-plot
  # ggtitle("Vessels >24m") +
  ## Theme
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(-0.2, "cm"),
    legend.direction = "vertical",
    legend.margin = margin(0,0,0,0),
    legend.justification = "right",     legend.key.height = unit(0.1, "npc"), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, vjust = 0.7),
    legend.key.width = unit(10, 'mm'),
    
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA, 
                                linewidth = 1.5),
    axis.title = element_blank(),
    axis.text =  element_blank()
  )
ggsave(filename = 'mon.24plus.png',
       units = 'mm',
       width = 297, height = 200,
       plot = mon.24plus,
       device = "png")
### PLOT ALL 3 MAPS TOGETHER ----
map.full.mon <- (mon.0.12 | mon.12.24 | mon.24plus) + 
  plot_layout(axes = "collect", guides = "collect") & 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.spacing.y = unit(-0.2, "cm"),
        legend.margin = margin(0,0,0,0),
        legend.key.height = unit(0.07, "npc"))
ggsave(filename = 'map.LL.mon.per.length.png',
       units = 'mm',
       width = 297, height = 120,
       plot = map.full.mon,
       device = "png")

### STACK ALL 6 MAPS TOGETHER NOW----
end.map <- map.full.effort / map.full.mon + plot_annotation(tag_levels = 'A')

ggsave(filename = 'end.map.png',
       units = 'mm',
       width = 297, height = 297,
       plot = end.map,
       device = "png")

# library(cowplot)
# legend.top <- get_legend(map.full.effort)
# legend.bottom <- get_legend(map.full.mon)
# top.no.legend <- get_legend(map.full.effort) + theme(legend.position = 'none')
# bottom.no.legend <- get_legend(map.full.mon) + theme(legend.position = 'none')
# end_map <- plot_grid(
#   legend.top, top.no.legend,  
#   top.no.legend, bottom.no.legend, ncol = 2, rel_widths = c(3, 1),
#   nrow = 2
# )
# # print(end_map)
# ggsave(filename = 'end.map.png',
#        units = 'mm',
#        width = 297, height = 297,
#        plot = end_map,
#        device = "png")
