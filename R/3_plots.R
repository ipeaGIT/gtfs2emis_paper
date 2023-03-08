# 0) Load libraries ----

rm(list=ls())
gc(reset = TRUE)

# gtfs2emis fixed URL
# https://zenodo.org/record/7308585/files/ipeaGIT/gtfs2emis-0.0.1.zip?download=1

easypackages::packages('devtools'
                       # data analysis/visualization
                       , 'data.table', 'magrittr', 'furrr'
                       , 'clipr', 'ggplot2', 'sfheaders', 'sf'
                       , 'ggsn','patchwork'
                       # data
                       , 'aopdata', 'geobr'
                       # gtfs packages
                       , 'gtfs2gps', 'gtfstools'
                       # fonts
                       , 'extrafont', 'extrafontdb', 'fontcm', 'extrafont')

library(ggsci) # devtools::install_github("awhstin/awtools")
# import fonts
extrafont::font_import()
extrafont::loadfonts(device = "win")
extrafont::fonts()

dir.create("figures")

## 1) Plot GTFS trips -----
# Needs to Run Code item b) to reproduce this section

# read gtfs
spo_gtfs <- gtfstools::read_gtfs("data/gtfs_spo_sptrans_prep_jul.zip")

# read Tiles & Boundaries
my_tile <- readr::read_rds("data-raw/bra_spo_maptile.rds")
my_bound <- readr::read_rds("data-raw/bra_spo_boundary.rds")
my_bound$city_name <- "São Paulo"

# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# list-files
gps_lines_sf <- gtfstools::convert_shapes_to_sf(gtfs = spo_gtfs)

# aggregate by sum
gps_lines_sf <- sf::st_transform(gps_lines_sf,3857)
my_bound <- sf::st_transform(my_bound,3857)
gps_lines_sf$label <- "Bus routes"

# plot
map_scale <- as.numeric(sf::st_bbox(my_bound)[3]) -  
  as.numeric(sf::st_bbox(my_bound)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# plot spatial
p <- ggplot() + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add gps_lines_sf
  #ggnewscale::new_scale_color() +
  geom_sf(data = gps_lines_sf,aes(color = label)
          ,alpha = 0.10, linewidth = 0.25,fill = NA) +
  scale_color_manual(values = "red",name = NULL)+
  # add boundary
  #ggnewscale::new_scale_color() +
  geom_sf(data = my_bound#,aes(color = city_name)
          ,color = "black"
          ,linetype = "solid",alpha = 0.75, size = 0.25,fill = NA) +
  #scale_color_manual(values = "black",name = NULL
  #                   ,labels = "City boundary")+
  #coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme_minimal() + 
  theme_void() +
  # map itens
  ggsn::north(data = my_bound,
              location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = sf::st_bbox(sf::st_transform(my_bound,3857))[1] %>% as.numeric(),
                 x.max = sf::st_bbox(sf::st_transform(my_bound,3857))[3] %>% as.numeric(),
                 y.min = sf::st_bbox(sf::st_transform(my_bound,3857))[2] %>% as.numeric(),
                 y.max = sf::st_bbox(sf::st_transform(my_bound,3857))[4] %>% as.numeric(),
                 dist = plot_scale,
                 #text = element_text(family = "LM Roman 10"),
                 family = "LM Roman 10",
                 fontface = "plain",
                 dist_unit = "km",st.size = 3,
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84") +
  theme(legend.position = c(0.75,0.2),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.box.margin = margin(0,0,0,0, "pt")) +
  guides(color = guide_legend(override.aes = list(linewidth = 0.75
                                                  ,alpha = 1)))

# save
ggplot2::ggsave(plot = p,
                filename = "figures/basic_sptrans.png",
                scale = 0.6,width = 14,
                bg = "white",
                height = 20,units = "cm",dpi = 300)

## b) Plot EF | MEF by age----------------
rm(list=ls())
gc(reset = TRUE)

# list-files
files_gps <- list.files(path = 'data/emi_age/jul/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emi_age/jul/',full.names = FALSE)

# read files
tmp_my_age <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()

#tmp_my_age[,VTK := units::drop_units(VTK)]
tmp_my_age[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_my_age[,single_VTK := VTK * fleet_composition]

# aggregate by sum
tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(single_VTK)
  ,avg_ef = sum(emi)/sum(single_VTK)
),by = .(pollutant)]

# aggregate by sum
tmp_my_age <- tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(single_VTK)
  ,avg_ef = sum(emi)/sum(single_VTK)
),by = .(veh_type,euro ,pollutant)]

# factors
tmp_my_age[,model_year_f := factor(x = euro ,levels = c("III","V"))]
tmp_my_age[, pollutant_f := dplyr::recode_factor(pollutant
                                                 , `CO2` = "CO[2]"
                                                 , `PM10` = "PM[10]"
                                                 , `CH4` = "CH[4]"
                                                 , `NOx` = "NO[X]")]
## c) emissions_age ----

ggplot(data = tmp_my_age[pollutant != "CH4"]) + 
  geom_bar(aes(y= as.numeric(avg_ef)
               ,x = model_year_f
               ,fill = veh_type)
           ,stat = "identity"
           ,position = "dodge")+
  facet_wrap(~pollutant_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(fill = "Vehicle \ncategory",x = "Euro"
       ,y = "Marginal \nEmission Factor (g/km)")+
  scale_fill_manual(values = viridis::rocket(4))+
  #scale_x_discrete(breaks = c(seq(2008,2019,3),2019),
  #                 labels = c(seq(2008,2019,3),2019))+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = "bottom"
  )

ggsave(filename = "figures/emissions_age.png"
       ,width = 36,height = 17.5,dpi = 300,units = "cm",scale = 0.5)

## d) Temporal emissions ---------

# import fonts
# extrafont::font_import(paths = "C://Users//B138750230//Downloads//Latin-Modern-Roman-fontfacekit//web fonts/")

# create folder
rm(list=ls())
gc(reset = TRUE)

# list-files
files_gps <- list.files(path = 'data/emi_time/jul/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emi_time/jul/',full.names = FALSE)

#  my function to_compartible_units-
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}

tmp_my_time <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist(fill = TRUE)


# total 
tmp_my_time[,sum(emi),by = .(pollutant)]
# aggregate by summing
my_time <- tmp_my_time[,sum(emi),by = .(timestamp_hour,pollutant)]
my_time <- my_time[pollutant == "NOx"]
data.table::setkeyv(my_time,cols = c("pollutant","timestamp_hour"))
my_time[,V2 := to_compartible_units(V1)]


# plot time-

ggplot(data = my_time) + 
  geom_bar(aes(x = timestamp_hour, y = as.numeric(V2), fill = as.numeric(V2)),
           stat = "identity")+
  labs(title = NULL,
       x = "Hour",
       y = expression(NO[X][] (kg)))+
  #y = paste0("PM10 (",getUnit_graphic,")")) + 
  viridis::scale_fill_viridis(option = "D",direction = -1)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "LM Roman 10"))

ggplot2::ggsave(filename = "figures/temporal_NOx.png",
                scale = .55,bg = "white",
                width = 18,height = 10,units = "cm",dpi = 300)
## e1) zoom emissions ----

rm(list=ls())
gc(reset = TRUE)

# read Tiles & Boundaries
my_tile <- readr::read_rds("data-raw/bra_spo_maptile.rds")
my_bound <- readr::read_rds("data-raw/bra_spo_boundary.rds")


# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# list-files
files_gps <- list.files(path = 'data/emi_grid/jul/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emi_grid/jul/',full.names = FALSE)

#  my function to_compartible_units-
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}

# read files
tmp_my_grid <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()

# aggregate by sum
colPol <- c("CH4","CO2","NOx","PM10")

my_grid <- tmp_my_grid[,(colPol) := lapply(.SD,sum),by = id_hex,.SDcols = colPol]
my_grid <- my_grid[,.SD[1],by = id_hex]
my_grid <- my_grid[,(colPol) := lapply(.SD,to_compartible_units),.SDcols = colPol]
my_grid <- sf::st_as_sf(my_grid) %>% sf::st_transform(3857)


# plot
getUnit_map <- c("t","g") # CO2, PM10

map_scale <- as.numeric(sf::st_bbox(my_grid)[3]) -  
  as.numeric(sf::st_bbox(my_grid)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# organize data to plot
bind_grid <- data.table::melt(data = my_grid
                              , variable.name = "pollutant"
                              , id.vars = c("id_hex"
                                            ,"abbrev_muni"
                                            ,"name_muni"
                                            ,"code_muni","geom"))
bind_grid <- sf::st_as_sf(bind_grid)
bind_grid <- sf::st_set_crs(x = bind_grid,value = sf::st_crs(my_grid))


get_bbox <- c(-46.63319,-23.54904) %>% 
  sf::st_point(.,dim = "XY") %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(3857) %>% 
  sf::st_buffer(4000) %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X),max(X),min(X))
          ,Y = c(min(Y) + 1000,min(Y) + 1000,max(Y) - 1000,max(Y) - 1000))] %>% 
  sfheaders::sf_polygon(.,x = "X",y = "Y") %>% 
  sf::st_set_crs(x = .,3857)

intersect_bound <- sf::st_intersection(x = bind_grid
                                       ,y = get_bbox)

get_bbox_scale <- c(-46.63319,-23.54904) %>% 
  sf::st_point(.,dim = "XY") %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(3857) %>% 
  sf::st_buffer(3500) %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X),max(X),min(X))
          ,Y = c(min(Y) + 1000,min(Y) + 1000,max(Y) - 1000,max(Y) - 1000))] %>% 
  sfheaders::sf_polygon(.,x = "X",y = "Y") %>% 
  sf::st_set_crs(x = .,3857)


lim_coord_zoom <- get_bbox %>%
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X))
          ,Y = c(min(Y),max(Y)))]

lim_coord_zoom

lim_fill_nox <- bind_grid[bind_grid$pollutant == "NOx",] %>% 
  data.table::setDT() %>% 
  .[,list(min(value),max(value))] %>% 
  as.numeric() %>% as.vector()

lim_fill_nox

lim_fill_co2 <- bind_grid[bind_grid$pollutant == "CO2",] %>% 
  data.table::setDT() %>% 
  .[,list(min(value),max(value))] %>% 
  as.numeric() %>% as.vector()

# gtfs
spo_gtfs <- gtfstools::read_gtfs("data/gtfs_spo_sptrans_prep_jul.zip")
gps_lines_sf <- gtfstools::convert_shapes_to_sf(gtfs = spo_gtfs)
stops_sf <- gtfstools::convert_stops_to_sf(gtfs = spo_gtfs)


intersect_lines_sf <- sf::st_intersection(x = sf::st_transform(gps_lines_sf,3857)
                                          ,y = get_bbox)
intersect_stops_sf <- sf::st_intersection(x = sf::st_transform(stops_sf,3857)
                                          ,y = get_bbox)
rm(gps_lines_sf)
rm(stops_sf)
rm(spo_gtfs)

##### ggplot PM10 ----
nox_plot <-  ggplot(bind_grid[bind_grid$pollutant == "NOx",]) + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1)+
  #viridis::scale_fill_viridis(option = "D"
  #                            ,direction = -1
  #                            , breaks = c(0,10,100,200)
  #                            ,labels = c(0,10,100,200)
  #                            ,trans = "pseudo_log"
  #) +
  ## add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "solid",alpha = 0.5, size = 0.15,fill = NA) +
  # facet_wrap(~pollutant)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       #, subtitle =  expression(PM[10][] emissions)
       , subtitle =  expression(NO[X][])
       , fill =  expression(NO[X][] (g))
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme_minimal() + 
  theme_void() +
  # map itens
  ggsn::north(data = my_grid,
              location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = sf::st_bbox(sf::st_transform(my_bound,3857))[1] %>% as.numeric()
                 , x.max = sf::st_bbox(sf::st_transform(my_bound,3857))[3] %>% as.numeric()
                 , y.min = sf::st_bbox(sf::st_transform(my_bound,3857))[2] %>% as.numeric()
                 , y.max = sf::st_bbox(sf::st_transform(my_bound,3857))[4] %>% as.numeric()
                 , dist = plot_scale/2
                 , family = "LM Roman 10"
                 , fontface = "plain"
                 , dist_unit = "km"
                 , st.size = 2.5
                 , location = "bottomright"
                 , st.dist = 0.035
                 , st.bottom = FALSE
                 , st.color = "black"
                 , transform = FALSE
                 , model = "WGS84") +
  theme(legend.position = c(0.8,0.325),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.key.size = ggplot2::unit(0.03250,"npc"),
        legend.box.margin = margin(3,3,3,3, "pt")) 

nox_plot
#### ggplot CO2-------

co2_plot <-  ggplot(bind_grid[bind_grid$pollutant == "CO2",]) + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "E"
                              ,direction = -1)+
  #viridis::scale_fill_viridis(option = "E"
  #                            ,direction = -1
  #                            , breaks = 1:5
  #                            ,labels = 1:5
  #                            ,trans = "pseudo_log"
  #) +
  
  ## add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "solid",alpha = 0.5, size = 0.15,fill = NA) +
  # facet_wrap(~pollutant)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(#title = "Total emissions"
    subtitle = expression(CO[2][])
    , fill =  expression(CO[2][] (t))
    , color = NULL
    , x = NULL
    , y = NULL) +
  theme(legend.position = c(0.9,0.1)) + 
  theme_minimal() + 
  theme_void() +
  theme(legend.position = c(0.8,0.3),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.key.size = ggplot2::unit(0.035,"npc"),
        legend.box.margin = margin(3,3,3,3, "pt")) 

co2_plot
##### ggplot zoom nox -----

nox_zoom <- ggplot(intersect_bound[intersect_bound$pollutant == "NOx",]) + 
  # data
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  scale_fill_identity() +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1
                              , limits = lim_fill_nox) +
  #viridis::scale_fill_viridis(option = "D"
  #                            ,direction = -1
  #                            ,labels = scales::number_format()
  #                            ,trans = "pseudo_log"
  #                            , limits = lim_fill_nox
  #) +
  geom_sf(data = intersect_lines_sf,alpha = 0.15,size = 0.25,color = "black")+
  # limits
  coord_sf(xlim = lim_coord_zoom$X, ylim = lim_coord_zoom$Y,expand = FALSE) +
  # labels 
  labs(title = NULL
       , color = NULL
       , x = NULL
       , y = NULL) +
  # theme
  theme(axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL, 
        axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
        axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL,
        axis.text = element_blank(), axis.title = element_blank(),
        legend.position = "none")+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.75))+
  ggsn::north(data = get_bbox_scale,
              location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = lim_coord_zoom$X[1],
                 x.max = lim_coord_zoom$X[2]-350,
                 y.min = lim_coord_zoom$Y[1]+350,
                 y.max = lim_coord_zoom$Y[2],
                 st.dist = 0.075,
                 dist = 1,
                 family = "LM Roman 10",
                 fontface = "plain",
                 dist_unit = "km"
                 ,st.size = 2.5,
                 location = "bottomright",
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84")

nox_zoom

##### ggplot zoom co2 -----

co2_zoom <- ggplot(intersect_bound[intersect_bound$pollutant == "CO2",]) + 
  # data
  geom_raster(data = my_tile[my_tile$x > min(lim_coord_zoom$X) & 
                               my_tile$x < max(lim_coord_zoom$X) & 
                               my_tile$y > min(lim_coord_zoom$Y) & 
                               my_tile$y < max(lim_coord_zoom$Y),]
              , aes(x, y, fill = hex), alpha = 1) +
  scale_fill_identity() +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "E"
                              ,direction = -1
                              , limits = lim_fill_co2) +
  #viridis::scale_fill_viridis(option = "E"
  #                            ,direction = -1
  #                            ,labels = scales::number_format()
  #                            ,trans = "pseudo_log"
  #                            , limits = lim_fill_co2) +
  geom_sf(data = intersect_lines_sf,alpha = 0.15,size = 0.25,color = "black")+
  # limits
  coord_sf(xlim = lim_coord_zoom$X, ylim = lim_coord_zoom$Y,expand = FALSE) +
  # labels 
  labs(title = NULL
       , color = NULL
       , x = NULL
       , y = NULL) +
  # theme
  theme(axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL, 
        axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
        axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL,
        axis.text = element_blank(), axis.title = element_blank(),
        legend.position = "none")+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.75))

#### patchwork ----

nox_plot_squared <- nox_plot  +
  ggnewscale::new_scale_color()+
  geom_sf(data = get_bbox,color = "red",fill = NA)

nox_final <- nox_plot_squared / nox_zoom

co2_plot_squared <- co2_plot  +
  ggnewscale::new_scale_color()+
  geom_sf(data = get_bbox,color = "red",fill = NA)

co2_final <- co2_plot_squared / co2_zoom


final <- co2_final | nox_final

#final

ggplot2::ggsave(plot = final
                , filename = "figures/spatial_co2_nox_linear.png"
                , scale = 0.7
                , width = 17
                , bg = "white"
                , height = 19.5
                , units = "cm"
                , dpi = 300)

## e2) Spatial HOUR emissions -----
#### ggplot v1-----
rm(list=ls())
library(magrittr)
gc(reset = TRUE)

to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}
# read Tiles & Boundaries
my_tile <- readr::read_rds("data-raw/bra_spo_maptile.rds")
my_bound <- readr::read_rds("data-raw/bra_spo_boundary.rds")


# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# list-files
files_gps <- list.files(path = 'data/emi_grid_time/oct/',full.names = TRUE)

# read files
tmp_my_grid <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()


# aggregate by sum
colPol <- c("CH4","CO2","NOx","PM10")
tmp_my_grid[timestamp %in% 0:2   ,horario := "0:00 am - 2:59 am"] #
tmp_my_grid[timestamp %in% 3:5   ,horario := "3:00 am - 5:59 am"]
tmp_my_grid[timestamp %in% 6:8   ,horario := "6:00 am - 8:59 am"] #
tmp_my_grid[timestamp %in% 9:11  ,horario := "9:00 am - 11:59 am"]
tmp_my_grid[timestamp %in% 12:14 ,horario := "12:00 am - 2:59 pm"]
tmp_my_grid[timestamp %in% 15:17 ,horario := "3:00 pm - 5:59 pm"]
tmp_my_grid[timestamp %in% 18:20 ,horario := "6:00 pm - 8:59 pm"]
tmp_my_grid[timestamp %in% 21:23 ,horario := "9:00 pm - 11:59 pm"] #

#tmp_my_grid[timestamp %in% 6:7,horario_status := "Peak hour"]
#tmp_my_grid[timestamp %in% 14:15,horario_status := "Off-Peak hours"]

# remove other hours
tmp_my_grid <- tmp_my_grid[timestamp %in% c(0:2,6:8,21:23),]
my_grid <- tmp_my_grid[,(colPol) := lapply(.SD,sum)
                       ,by = .(id_hex,horario)
                       ,.SDcols = colPol]
my_grid <- my_grid[,.SD[1],by = .(id_hex,horario)]
my_grid <- sf::st_as_sf(my_grid) %>% sf::st_transform(3857)



# plot
getUnit_map <- c("t","g") # CO2, nox

map_scale <- as.numeric(sf::st_bbox(my_grid)[3]) -  
  as.numeric(sf::st_bbox(my_grid)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# organize data to plot
bind_grid <- data.table::melt(data = my_grid
                              , variable.name = "pollutant"
                              , id.vars = c("id_hex","horario"
                                            ,"geom"
                                            ,"abbrev_muni"
                                            ,'name_muni'
                                            ,'code_muni',"timestamp"))

pol_limit <- bind_grid
pol_limit <- data.table::setDT(pol_limit) %>% 
  .[pollutant == "NOx",] %>% 
  .[,list(min(value),max(value))] %>% 
  as.numeric() 
pol_limit 


bind_grid <- sf::st_as_sf(bind_grid)
bind_grid <- sf::st_set_crs(x = bind_grid,value = sf::st_crs(my_grid))

bind_grid$horario %>% unique() %>% sort() %>% paste0(.,collapse = "', '")
bind_grid$horario_f <- factor(
  x = bind_grid$horario
  ,levels = c(
    '0:00 am - 2:59 am'
    , '6:00 am - 8:59 am'
    , '9:00 pm - 11:59 pm')
  ,labels = c(
    '0:00 am - 2:59 am\n'
    , '6:00 am - 8:59 am\n'
    , '9:00 pm - 11:59 pm\n')
)




plot_hour_nox <- ggplot(bind_grid[bind_grid$pollutant == "NOx",]) + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  #add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1
                              ,limits = pol_limit)+
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "solid",alpha = 0.5, size = 0.15,fill = NA) +
  facet_wrap(~horario_f ,nrow = 1)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       #, subtitle =  expression(PM[10][] emissions)
       #, subtitle =  expression(PM[10][])
       , fill =  expression(NO[X][] (g))
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme_minimal() + 
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.box.margin = margin(3,3,3,3, "pt")
        ,plot.margin = unit(c(0,0,0.35,0),"cm")) 

plot_hour_nox

ggplot2::ggsave(plot = plot_hour_nox
                , filename = "figures/spatial_hour_nox_linear_3periods.png"
                , scale = 0.7
                , width = 21
                , bg = "white"
                , height = 14
                , units = "cm"
                , dpi = 300)

## f) EF (@ speed = 19 kph) -----
rm(list=ls())
gc(reset = TRUE)

# CETESB
my_ef_br_df <- ef_brazil_cetesb(pollutant = c("PM10","CO2","NOx")
                                ,veh_type = "BUS_URBAN_D"
                                ,model_year = 2011
                                ,as_list = FALSE)
my_ef_br_df <- as.data.frame(my_ef_br_df)
data.table::setDT(my_ef_br_df)[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- ef_europe_emep(speed =  units::set_units(19,"km/h")
                              ,veh_type = "Ubus Std 15 - 18 t"
                              ,euro = "V"
                              ,pollutant = c("PM10","CO2","NOx")
                              ,fuel = "D"
                              ,tech = "SCR"
                              ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- ef_usa_moves(pollutant = c("PM10","CO2","NOx")
                               ,model_year = 2011
                               ,reference_year = 2019
                               ,speed = units::set_units(19,"km/h")
                               ,fuel = "D"
                               ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- ef_usa_emfac(pollutant =  c("PM10","CO2","NOx")
                               ,reference_year = 2019
                               ,fuel = "D"
                               ,model_year = 2011
                               ,speed = units::set_units(19,"km/h")
                               ,as_list = FALSE)
my_ef_emfac_df[,source := "EMFAC"]

# rbind & process
my_ef_bind <- list(my_ef_br_df,my_ef_eu_df
                   ,my_ef_emfac_df,my_ef_moves_df) %>% 
  data.table::rbindlist(use.names = FALSE)


my_ef_plot <- data.table::melt.data.table(
  data = my_ef_bind
  ,id.vars = "source"
  ,measure.vars = c("PM10_2011","CO2_2011","NOx_2011"))
my_ef_plot[, variable := gsub("_2011","",variable)]
my_ef_plot[, variable_f := dplyr::recode_factor(variable
                                                , `CO2` = "CO[2]"
                                                , `PM10` = "PM[10]"
                                                , `NOx` = "NO[X]")]
my_ef_plot[, value := as.numeric(value)]
my_ef_plot[, label := "data source"]

#labeller_status <- c(CO2 = expression(CO[2][]),
#                     PM10 = expression(PM[10][]))
# plot
ggplot(data = my_ef_plot) + 
  geom_bar(aes(y= value,x = source,fill = source)
           ,stat = "identity",position = "dodge")+
  facet_wrap(~variable_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(fill = NULL,x = NULL,y = "EF (g/km)")+
  viridis::scale_fill_viridis(discrete = TRUE
                              ,option = "H",alpha = 0.75) +
  theme_light()+
  guides(fill = "none")+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black'))

# save
ggsave(filename = "figures/ef_plots.png"
       ,width = 36,height = 20,dpi = 300,units = "cm",scale = 0.5)

## g) EF @ different speeds for NOx------


# CETESB
my_ef_br_df <- ef_brazil_cetesb(pollutant = c("PM10","CO2","NOx")
                                ,veh_type = "BUS_URBAN_D"
                                ,model_year = 2011
                                ,as_list = FALSE)
my_ef_br_df <- ef_scaled_euro(ef_local = my_ef_br_df,
                              speed =  units::set_units(seq(10,100,10),"km/h")
                              ,veh_type = "Ubus Std 15 - 18 t"
                              ,euro = "V"
                              ,pollutant = c("PM10","CO2","NOx")
                              ,fuel = "D"
                              ,tech = "SCR")
my_ef_br_df <- my_ef_br_df$EF
my_ef_br_df[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- ef_europe_emep(speed =  units::set_units(seq(10,100,10),"km/h")
                              ,veh_type = "Ubus Std 15 - 18 t"
                              ,euro = "V"
                              ,pollutant = c("PM10","CO2","NOx")
                              ,fuel = "D"
                              ,tech = "SCR"
                              ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- ef_usa_moves(pollutant = c("PM10","CO2","NOx")
                               ,model_year = 2011
                               ,reference_year = 2019
                               ,speed = units::set_units(seq(10,100,10),"km/h")
                               ,fuel = "D"
                               ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- ef_usa_emfac(pollutant =  c("PM10","CO2","NOx")
                               ,reference_year = 2019
                               ,fuel = "D"
                               ,model_year = 2011
                               ,speed = units::set_units(seq(10,100,10),"km/h")
                               ,as_list = FALSE)
my_ef_emfac_df[,source := "EMFAC"]

# rbind & process
my_ef_bind <- list(my_ef_br_df,my_ef_eu_df
                   ,my_ef_emfac_df,my_ef_moves_df) %>% 
  data.table::rbindlist(use.names = FALSE)

my_ef_plot <- data.table::melt.data.table(
  data = my_ef_bind
  ,id.vars = "source"
  ,measure.vars = c("PM10_Euro_V","CO2_Euro_V","NOx_Euro_V")
)
my_ef_plot[, variable := gsub("_Euro_V","",variable)]
my_ef_plot[, variable_f := dplyr::recode_factor(variable
                                                , `CO2` = "CO[2]"
                                                , `NOx` = "NO[X]"
                                                , `PM10` = "PM[10]")]
my_ef_plot[, value := as.numeric(value)]
my_ef_plot[, label := "data source"]
my_ef_plot[, speed := rep(seq(10,100,10),12)]

# plot
p <- ggplot(data = my_ef_plot) + 
  geom_line(aes(y= value,x = speed,color = source),lwd = 0.8)+
  geom_point(aes(y= value,x = speed,color = source))+
  facet_wrap(~variable_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(color = NULL,x = "Speed (km/h)",y = "EF (g/km)")+
  viridis::scale_color_viridis(discrete = TRUE,option = "H",alpha = 1)+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = c(0.9,0.675))

# save
ggsave(plot = p
       ,filename = "figures/ef_speed.png"
       ,scale = 1.0,width = 18,height = 7
       ,units = "cm",dpi = 300)

## i) Fleet of São Paulo ----
rm(list=ls())
gc(reset=TRUE)
# Read fleet 
fleet_spo <- readr::read_rds("data-raw/bra_spo_fleet.rds")

# adjust technology
fleet_spo[fuel %in% "Elétrico",fuel := "Electric"]
fleet_spo[fuel %in% "D",fuel := "Diesel"]


fleet_spo <- fleet_spo[,lapply(.SD,sum)
                       ,by = .(year,type_name_eu,fuel)
                       ,.SDcols = "N"]
fleet_spo[,type_name_eu_f := factor(x = type_name_eu
                                    ,levels = c("Ubus Midi <=15 t",
                                                "Ubus Std 15 - 18 t",
                                                "Ubus Artic >18 t")
                                    ,labels = c("Midi <=15 t",
                                                "Standard 15 - 18 t",
                                                "Articulated >18 t"))]
N_by_year <- fleet_spo[,sum(N), by = .(year)]
N_by_year

# plot
ggplot(data = fleet_spo[fuel == "Diesel"]) + 
  # data
  geom_bar(aes(y= N,x = as.factor(year),fill = type_name_eu_f)
           ,stat = "identity"
           ,position = "dodge"
           #,position = "stack"
           ,width = 0.75)+
  # scale
  #scale_fill_manual(values = ggsci::pal_nejm()(8)[c(7,2,1)])+
  scale_fill_manual(values = ggsci::pal_jama()(7)[c(2,3,6)])+
  # labs
  labs(x = "Model year"
       ,fill = "Vehicle category"
       ,y = "Number of vehicles")+
  # theme
  theme_light()+
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        #,legend.position = c(0.125,0.80)
        ,legend.position = "bottom"
  )+
  guides(fill = guide_legend(title.position = "top"))


# save
ggsave(filename = "figures/fleet_bra_spo.png",scale = 0.9,
       width = 15,height = 10,units = "cm",dpi = 300)

# End -----