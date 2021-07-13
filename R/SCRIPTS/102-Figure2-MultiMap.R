###------Figure 2 -----
## @knitr MapFigure

source("./R/SCRIPTS/000-Libraries.R")
## Loading in the Projections from Scott Kulpt and Climate Central
files <- paste0("R/DATA/Dat/", list.files(path = "./R/DATA/Dat/",pattern = 'rcp45\\.csv'))
files <- files[files != "R/DATA/Dat/BlockgroupProjectedEAE.v3.LA.rcp45.csv" ]
files2 <- read_csv("./R/DATA/Dat/BlockgroupProjectedEAE.v3.LA.rcp45.csv") %>%
  pivot_longer(-GEOID10, names_to = "variable", values_to = "count") %>%
  mutate(GEOID = substr(GEOID10,1,5)) %>%
  group_by(GEOID, variable) %>%
  dplyr::summarise(Population = sum(count)) %>%
  separate(variable, c("variable", "year", "prob", "SSP")) %>%
  filter(variable != "LECZ")
temp <- lapply(files, read_csv)

## Converting the Climate Central data into a useable format.
### Also summing to the county level
together <- rbindlist( temp ) %>%
  pivot_longer(-GEOID10, names_to = "variable", values_to = "count") %>%
  mutate(GEOID = substr(GEOID10,1,5)) %>%
  group_by(GEOID, variable) %>%
  dplyr::summarise(Population = sum(count)) %>%
  separate(variable, c("variable", "year", "prob", "SSP")) %>%
  rbind(., files2) %>%
  group_by(GEOID,variable, year, prob, SSP) %>%
  dplyr::summarise(Population = sum(Population)) %>%
  ungroup() %>%
  filter(year %in% c("2100", "2000")) %>%
  mutate(SSP2 = if_else(is.na(SSP), prob, SSP),
         prob2 = if_else(is.na(SSP),"a", prob),
         Type = case_when(
           variable == "EAE" ~ "Exp. Ann. Flood",
           variable == "MHHW" ~ "Inundated",
           variable == "RL100" ~ "100-year FP"
         ),
         year = as.numeric(year)) %>%
  filter(prob2 %in% c("a", "p50")) %>%
  dplyr::select(GEOID, SSP2, Type, year, Population) %>%
  pivot_wider(names_from = Type, values_from = Population)  %>%
  mutate(STATE = substr(GEOID, 1,2))

together2 <- together %>%
  filter(SSP2 == "SSP2") %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(EAE = `Exp. Ann. Flood` / lag(`Exp. Ann. Flood`),
         MHHW = Inundated / lag(Inundated),
         RL100 = `100-year FP` / lag(`100-year FP`)) %>%
  ungroup() %>%
  filter(year == 2100)

together2[sapply(together2, is.nan)] <- NA
together2[sapply(together2, is.infinite)] <- NA
together2[is.na(together2)] <- 0

together2$groups_inundated <- factor(
  cut(together2$MHHW, c(0, getJenksBreaks(together2$MHHW[which(together2$MHHW>1)], 6))),
  labels = c("0 to 1", "1 to 22", "22 to 85", "85 to 266", "266 to 769", "769+"))

together2$groups_annual <- factor(
  cut(together2$EAE, c(0, getJenksBreaks(together2$EAE[which(together2$EAE>1)], 6))),
  labels = c("0 to 1", "1 to 8", "8 to 20", "20 to 37", "37 to 60", "50+"))

together2$groups_100fp <- factor(
  cut(together2$RL100, c(0, getJenksBreaks(together2$RL100[which(together2$RL100>1)], 6))),
  labels = c("0 to 1", "1 to 3", "3 to 5", "5 to 10", "10 to 21", "21+"))

# together2$EAE[which(!is.finite(together2))] <- 1
# together2[!is.finite(together2)] <- 0
# downloading a shapefile for US counties. H0010001 is just a generic variable that is not used.
shape <- get_decennial("county",
                       variables = "H001001",
                       year = 2010,
                       sumfile = "sf1",
                       geometry = TRUE,
                       shift_geo = TRUE)

# downloading a shapefile for US states H0010001 is just a generic variable that is not used.
states <- get_decennial("state",
                        variables = "H001001",
                        year = 2010,
                        sumfile = "sf1",
                        geometry = TRUE,
                        shift_geo = TRUE) %>%
  mutate(STATE = substr(GEOID, 1,2))

z <- left_join(shape, together2, by = "GEOID") %>%
  mutate(STATE = substr(GEOID, 1,2))

westcoast_counties <- z %>% filter(STATE %in% c("06", "41", "53"))
westcoast_states <- states  %>% filter(STATE %in% c("06", "41", "53"))
other_states <- states %>% filter(STATE %in% setdiff(unique(together$STATE), c("06", "41", "53")))
other_counties <- z %>% filter(STATE %in% setdiff(unique(together$STATE), c("06", "41", "53")))


mapmaker <- function(groups, labelss, pal){
  pal2 <- c("#A6611A", brewer_pal("seq", pal)(5))
  # westcoast_counties$rgb <- " "
  westcoast_counties$rgb <- "#999999"
  westcoast_counties$rgb[which(westcoast_counties$groups_inundated == levels(westcoast_counties$groups_inundated)[1])] <- pal2[1]
  westcoast_counties$rgb[which(westcoast_counties$groups_inundated == levels(westcoast_counties$groups_inundated)[2])] <- pal2[2]
  westcoast_counties$rgb[which(westcoast_counties$groups_inundated == levels(westcoast_counties$groups_inundated)[3])] <- pal2[3]
  westcoast_counties$rgb[which(westcoast_counties$groups_inundated == levels(westcoast_counties$groups_inundated)[4])] <- pal2[4]
  westcoast_counties$rgb[which(westcoast_counties$groups_inundated == levels(westcoast_counties$groups_inundated)[5])] <- pal2[5]
  westcoast_counties$rgb[which(westcoast_counties$groups_inundated == levels(westcoast_counties$groups_inundated)[6])] <- pal2[6]
  
  # other_counties$rgb <- " "
  other_counties$rgb <- "#999999"
  other_counties$rgb[which(other_counties$groups_inundated == levels(other_counties$groups_inundated)[1])] <- pal2[1]
  other_counties$rgb[which(other_counties$groups_inundated == levels(other_counties$groups_inundated)[2])] <- pal2[2]
  other_counties$rgb[which(other_counties$groups_inundated == levels(other_counties$groups_inundated)[3])] <- pal2[3]
  other_counties$rgb[which(other_counties$groups_inundated == levels(other_counties$groups_inundated)[4])] <- pal2[4]
  other_counties$rgb[which(other_counties$groups_inundated == levels(other_counties$groups_inundated)[5])] <- pal2[5]
  other_counties$rgb[which(other_counties$groups_inundated == levels(other_counties$groups_inundated)[6])] <- pal2[6]
  
  map_left <- ggplot() +
    # geom_sf(data = westcoast_counties, aes_string(fill = groups), color = NA) + # setting the counties fill to the rgb value
    geom_sf(data = westcoast_counties, fill = westcoast_counties$rgb, color = NA) + # setting the counties fill to the rgb value
    geom_sf(data = westcoast_states, fill = NA, color = "black") + # overlaying a state boundary with white boundary color
    # scale_fill_manual(values = pal2, na.value = "gray50") +
    # scale_fill_brewer(type = "seq", palette = , na.value = "gray50") +
    theme_void() + # only the map
    coord_sf(datum = NA) +
    theme(
      # plot.background = element_rect(fill = NA),
      #     panel.background = element_rect(fill = NA),
          plot.margin=unit(c(5.5, 5.5, 10, 5.5), "points"),
          legend.position = "none")
  
  map_right <-
    ggplot(data=other_counties) +
    geom_sf(aes_string(fill = groups), color = NA) + # setting the counties fill to the rgb value
    # geom_sf(fill = other_counties$rgb, color = NA) + # setting the counties fill to the rgb value
    
    geom_sf(data = other_states, fill = NA, color = "black") + # overlaying a state boundary with white boundary color
    scale_fill_manual(values =pal2, na.value = "#999999") +
    # scale_fill_brewer(type = "seq", palette = pal, na.value = "gray50") +
    theme_void() + # only the map
    theme(
      # plot.background = element_rect(fill = NA),
          legend.position = c(.25,.6)) +
    coord_sf(datum = NA,
             xlim = c(257903, 4295500)) +
    labs(fill = paste0(labelss))
  
  ggdraw() +
    # draw_plot(map_right, 0.2, 0, 1, 1) +
    draw_plot(map_left, -0.03, 0.3, 0.7, 0.65) +
    draw_plot(map_right, 0.2, 0, 1, 1)
}
# mapmaker("groups_inundated", "Inundated", "Reds")

mapsinundated <- mapmaker("groups_inundated", "Inundated", "Reds")
mapsflood <- mapmaker("groups_annual", "EAE", "Blues")
maps100 <- mapmaker("groups_100fp", "100y-FP", "Greens")

b <- plot_grid(mapsinundated, mapsflood, maps100, ncol=1)

together2 <- together %>%
  filter(SSP2 == "SSP2",
         `100-year FP` >0) 

# downloading a shapefile for US counties. H0010001 is just a generic variable that is not used.
shape <- get_decennial("county",
                       variables = "H001001",
                       year = 2010,
                       sumfile = "sf1",
                       geometry = TRUE,
                       shift_geo = TRUE)

# downloading a shapefile for US states H0010001 is just a generic variable that is not used.
states <- get_decennial("state",
                        variables = "H001001",
                        year = 2010,
                        sumfile = "sf1",
                        geometry = TRUE,
                        shift_geo = TRUE) %>%
  mutate(STATE = substr(GEOID, 1,2))

z <- left_join(shape, together2, by = "GEOID") %>%
  mutate(STATE = substr(GEOID, 1,2))
# 
# z$groups_inundated <- factor(
#   cut(z$MHHW, c(0, 1, 5, 25, 100, 150)),
#   labels = c("0 to 1", "1 to 5", "5 to 25", "25 to 100", "100+"))

z$groups_inundated <- factor(
  cut(z$Inundated, c(0, 100, 7500, 25000, 50000, 75000,100000)),
  labels = c("0 to 100", 
             "100 to 7.5K", 
             "7.5K to 25K", 
             "25K to 50K", 
             "50K to 75k", 
             "75K+"))


# getJenksBreaks(z$`100-year FP`[which(z$`100-year FP`>1)], 6)
# cut(z$`100-year FP`, c(0, 50000, 150000, 300000, 500000, 5000000)),
# labels = c("1 to 50K", "50K to 150K", "150K to 300K", "300K to 500K", "500K+"))

z$groups_annual <- factor(
  cut(z$`Exp. Ann. Flood`, c(0, 1000, 25000, 50000, 100000, 250000, 1000000)),
  labels = c("0 to 1K", 
             "1K to 25K", 
             "25K to 50K", 
             "50K to 100K", 
             "100K to 250K", 
             "250K+"))

z$groups_100fp <- factor(
  cut(z$`100-year FP`, c(0, 50000, 150000, 300000, 500000, 5000000)),
  labels = c("1 to 50K", "50K to 150K", "150K to 300K", "300K to 500K", "500K+"))


westcoast_counties <- z %>% filter(STATE %in% c("06", "41", "53"))
westcoast_states <- states  %>% filter(STATE %in% c("06", "41", "53"))
other_states <- states %>% filter(STATE %in% setdiff(unique(together$STATE), c("06", "41", "53")))
other_counties <- z %>% filter(STATE %in% setdiff(unique(together$STATE), c("06", "41", "53")))


mapmaker <- function(groups, labelss, pal){
  map_left <- ggplot() +
    geom_sf(data = westcoast_counties, aes_string(fill = groups), color = NA) + # setting the counties fill to the rgb value
    geom_sf(data = westcoast_states, fill = NA, color = "black") + # overlaying a state boundary with white boundary color
    scale_fill_brewer(type = "seq", palette = pal, na.value = "#999999") +
    theme_void() + # only the map
    coord_sf(datum = NA) +
    theme_nothing() +
    theme(
      # plot.background = element_rect(fill = NA),
          # panel.background = element_rect(fill = NA),
          plot.margin=unit(c(5.5, 5.5, 10, 5.5), "points"),
          legend.position = "none",
          panel.border     = element_blank())
  
  map_right <-
    ggplot(data=other_counties) +
    geom_sf(aes_string(fill = groups), color = NA) + # setting the counties fill to the rgb value
    geom_sf(data = other_states, fill = NA, color = "black") + # overlaying a state boundary with white boundary color
    scale_fill_brewer(type = "seq", palette = pal, na.value = "#999999") +
    theme_void() + # only the map
    theme_nothing()+
    theme(
      # plot.background = element_rect(fill = NA),
          legend.position = c(.25,.6)) +
    coord_sf(datum = NA,
             xlim = c(257903, 4295500)) +
    labs(fill = paste0(labelss))
  
  ggdraw() +
    # draw_plot(map_right, 0.2, 0, 1, 1) +
    draw_plot(map_left, -0.03, 0.3, 0.7, 0.65) +
    draw_plot(map_right, 0.2, 0, 1, 1) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          panel.border     = element_blank())
}
mapmaker("groups_100fp", "100y-FP", "Greens")

mapsinundated <- mapmaker("groups_inundated", "2100 Inundated", "Reds")
mapsflood <- mapmaker("groups_annual", "2100 EAE", "Blues")
maps100 <- mapmaker("groups_100fp", "2100 100y-FP", "Greens")

a <- plot_grid(mapsinundated, mapsflood, maps100, ncol=1) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

combined <- plot_grid(a, b, ncol=2, labels = "auto") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
save_plot("p422.pdf", combined, ncol = 2, base_height = 15, base_width = 7.8)
save_plot("p422.png", combined, ncol = 2, base_height = 15, base_width = 7.8)
