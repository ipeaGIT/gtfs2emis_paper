
# 1) Load -----
rm(list=ls())
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
                       , 'extrafont', 'extrafontdb'
                       , 'showtext'
                       , 'fontcm', 'extrafont')

# import fonts
extrafont::font_import()
extrafont::loadfonts(device = "win")
extrafont::fonts()

# 2) Prepare  OCUP data ----
sp_path <- "L:/Proj_emission_routes/data-raw/passengers/bra_spo/50544_CARREGAMENTOS_LINHAS_OUT2019_POR_PTO_SENTIDO.XLSX"
sp_path <- "data-raw/passenger/50544_CARREGAMENTOS_LINHAS_OUT2019_POR_PTO_SENTIDO.XLSX"
dt_raw <- readxl::read_xlsx(path = sp_path)
data.table::setDT(dt_raw)

my_names <- stringr::str_split(names(dt_raw),",")[[1]] 
setnames(dt_raw,old = names(dt_raw),new = "all")
dt_raw[,id_col := 1:.N]

dt_raw <- dt_raw[,{
  sp <- stringr::str_split(string = all
                           ,pattern = "[,]"
                           ,simplify = TRUE)
  sp <- as.vector(sp)
  list("route_id" =  sp[1]      ,"trip_id" =  sp[2],  
       "chave_stop_prox_stop" =  sp[3],
       "stop_sequence" =  sp[4] , 
       "stop_id_atual" =  sp[5] ,"prox_stop_id"  =  sp[6] , 
       "0h_carreg"     =  sp[7] ,"1h_carreg"     =  sp[8] , 
       "2h_carreg"     =  sp[9] ,"3h_carreg"     =  sp[10],
       "4h_carreg"     =  sp[11],"5h_carreg"     =  sp[12],
       "6h_carreg"     =  sp[13],"7h_carreg"     =  sp[14],
       "8h_carreg"     =  sp[15],"9h_carreg"     =  sp[16],
       "10h_carreg"    =  sp[17],"11h_carreg"    =  sp[18],
       "12h_carreg"    =  sp[19],"13h_carreg"    =  sp[20],
       "14h_carreg"    =  sp[21],"15h_carreg"    =  sp[22],
       "16h_carreg"    =  sp[23],"17h_carreg"    =  sp[24],
       "18h_carreg"    =  sp[25],"19h_carreg"    =  sp[26],
       "20h_carreg"    =  sp[27],"21h_carreg"    =  sp[28],
       "22h_carreg"    =  sp[29],"23h_carreg"    =  sp[30])
},by = .(id_col)] 

# character col
ChrCol <- c('route_id','trip_id','chave_stop_prox_stop')
# integer col
IntCol <- setdiff(names(dt_raw),ChrCol)

# view
dt_raw[,.SD,.SDcols = IntCol] %>% head()
dt_raw[,.SD,.SDcols = ChrCol] %>% head()

# transform data
dt_raw[,(IntCol) := lapply(.SD, as.integer),.SDcols = IntCol]
dt_raw[,(ChrCol) := lapply(.SD, as.character),.SDcols = ChrCol]

# order by route_id
data.table::setkeyv(x = dt_raw,cols = c("route_id","trip_id","stop_sequence"))

# sum daily OCUP
colCarreg <- names(dt_raw)[names(dt_raw) %like% "h_carreg"]
dt_raw[, day_carreg := rowSums(.SD), .SDcols = colCarreg]

## save ----
dir.create("data/pax/")
readr::write_rds(dt_raw,"data/pax/carregamento_prep.rds",compress = "gz")

# dt_day_trip -----

rm(list=ls())
dt <- readr::read_rds("data/pax/carregamento_prep.rds")

dt_day_trip <- copy(dt)
dt_day_trip <- dt_day_trip[,trip_carreg := lapply(.SD,sum,na.rm = TRUE),
                           .SDcols = c("day_carreg")
                           ,by = .(route_id,trip_id)]
dt_day_trip <- dt_day_trip[,.SD[1],by = .(route_id,trip_id)]
dt_day_trip <- dt_day_trip[,.(route_id,trip_id,trip_carreg)]

# view number of route_id's and trip_id's
dt_day_trip[,uniqueN(route_id)]
dt_day_trip[,uniqueN(trip_id)]


## save ----
dir.create("data/pax/")
readr::write_rds(dt_day_trip,"data/pax/dt_day_trip.rds",compress = "gz")

# dt_h_trip ----

rm(list=ls())
dt_h_trip <- readr::read_rds("data/pax/carregamento_prep.rds")

dt_h_trip[,day_carreg := NULL]

id_names <- names(dt_h_trip)[!(names(dt_h_trip) %like% "_carreg")]
meas_names <- setdiff(x = names(dt_h_trip),y = id_names)


dt_h_trip <- data.table::melt(data = dt_h_trip,
                              id.vars = id_names,
                              measure.vars = meas_names,
                              variable.name = "horario",
                              value.name = "passageiros")
dt_h_trip[1:3,]

dt_h_trip[,horario := stringr::str_remove(horario,"h_carreg")]
dt_h_trip[,horario := as.numeric(horario)]

## save ----
dir.create("data/pax/")
readr::write_rds(dt_h_trip,"data/pax/dt_h_trip.rds",compress = "gz")


# 3) Read GTFS -----
rm(list=ls())
#filter dt by route_type == 3

gtfs_path <- "L:/Proj_acess_oport/git_jbazzo/gtfs2emis_paper/data-raw/gtfs_spo_sptrans_2019-06.zip"
gtfs_spo <- gtfstools::read_gtfs(gtfs_path) %>% 
  gtfstools::filter_by_weekday("monday") %>% 
  gtfstools::filter_by_route_type(3) %>% 
  gtfstools::frequencies_to_stop_times()

output_path <- "L:/Proj_acess_oport/git_jbazzo/gtfs2emis_paper/data/pax/prep_spo_gtfs.rds"

readr::write_rds(x = gtfs_spo
                 ,file = output_path,compress = "gz")


# stats
head(gtfs_spo$trips,1)
gtfs_spo$trips[,uniqueN(shape_id)] 
gtfs_spo$trips[,uniqueN(route_id)]
gtfs_spo$trips[,uniqueN(trip_id)] 

# 4) Prep VTK per hour -----
rm(list=ls())
gc(reset = TRUE)

# read emi_time

dir.create("data/oct/vtk_hour/")
emi_path <- list.files("data/oct/emissions////",full.names = TRUE)
emi_files <- list.files("data/oct/emissions////",full.names = F)
emi_files[1]
lapply(seq_along(emi_path),function(i){ # i = 1
  emi_dt <- readr::read_rds(emi_path[i])
  emi_dt <- emi_dt$tp_model
  setDT(emi_dt)
  emi_dt[,hour := data.table::hour(from_timestamp)]
  emi_dt[,from_to := paste0(from_stop_id,"_",to_stop_id)]
  emi_dt <- emi_dt[,list(dist = sum(dist,na.rm = TRUE)
                         ,veh_number = length(from_to)),by = hour]
  readr::write_rds(x = emi_dt
                   ,file = paste0("data/oct/vtk_hour/",emi_files[i]))
  return(NULL)
})

# read
rm(list=ls())
gc(reset = TRUE)
emi_files <- list.files("data/oct/vtk_hour////",full.names = T)
emi_dt <- lapply(emi_files,readr::read_rds) %>% data.table::rbindlist()
emi_dt <- emi_dt[,list(dist = sum(dist,na.rm = TRUE)
                       ,veh_number = sum(veh_number,na.rm=TRUE))
                 ,by = hour]
readr::write_rds(x = emi_dt,"data/oct/pax_vtk_hour.rds")

# 5) Plot Co2/cap -----
rm(list=ls())
gc(reset = TRUE)

dt_h_trip <- readr::read_rds("data/pax/dt_h_trip.rds")
tmp_h_trip <- copy(dt_h_trip) %>% 
  .[,passageiros := as.numeric(passageiros)] %>% 
  .[,sum(passageiros,na.rm = TRUE),by = .(horario)]

tmp_h_trip[]
# read emi_time

emi_files <- list.files("data/oct/emi_time/",full.names = TRUE)
emi_dt <- lapply(emi_files,readr::read_rds) %>% data.table::rbindlist()
emi_dt <- emi_dt[pollutant == "CO2",
                 list("emi" = sum(emi,na.rm = TRUE))
                 ,by = .(timestamp_hour)]
emi_dt[1,]


# # read total dist
# dist_dt <- readr::read_rds("data/oct/oct_vtk_hour.rds")
dist_dt <- readr::read_rds("data/oct/pax_vtk_hour.rds")
dist_dt[1,]

# merge
try(units::install_unit("pax"),silent = TRUE)
tmp_h_trip[emi_dt,on = c("horario" = "timestamp_hour"),emi := i.emi]
tmp_h_trip <- tmp_h_trip[dist_dt,on = c("horario" = "hour")]
tmp_h_trip[,pax := units::set_units(V1,"pax")]
tmp_h_trip[,pax := units::set_units(pax,"kpax")]
tmp_h_trip[,emi_capita := units::set_units(emi/pax,"g/pax")]
tmp_h_trip[,veh_cap := units::set_units(veh_number * 70,"pax")]
tmp_h_trip[,veh_cap := units::set_units(veh_cap,"kpax")]
tmp_h_trip[,occupancy := 100 * pax / veh_cap]
tmp_h_trip[,emi_dist := units::set_units(emi/dist,'g/km')]
tmp_h_trip[,emi_dist_cap := units::set_units(emi_dist/pax,"g/(km*pax)")]
#tmp_h_trip[,dist_trip := dist / total_trip]

# dcast
tmp_h_trip1 <- data.table::melt(data = tmp_h_trip,
                                id.vars = "horario",
                                measure.vars = c("emi"
                                                 ,"pax"
                                                 ,"dist"
                                                 ,"veh_number"
                                                 ,"emi_capita"
                                                 ,"veh_cap"
                                                 ,"emi_dist"
                                                 ,"occupancy"
                                                 ,"emi_dist_cap"),
                                variable.name = "variables")

tmp_h_trip1$variables1 <- factor(x = tmp_h_trip1$variables
                                 ,levels = c("emi"
                                             ,"veh_cap"
                                             ,"pax"
                                             ,"occupancy"
                                             ,"emi_capita"
                                             ,"dist"
                                             ,"veh_number"
                                             ,"emi_dist"
                                             ,"emi_dist_cap"
                                            )
                                 ,labels = c( "CO[2] (g)"
                                              ,"'Total Vehicle Capacity ' (thousands)"
                                              ,"'Total Passengers ' (thousands)"
                                              ,"'Mean Vehicle Occupancy (%)'"
                                              ,"'Emissions per capita ' (g/person)"
                                              ,"'Total distance ' (km)"
                                              ,"'Total stop-segments '"
                                              ,"'Emissions per distance ' (g/km)"
                                              ,"'Emissions per distance per capita ' (g/(km/person))"
                                             ))


# save for additional analysis -----

readr::write_rds(tmp_h_trip1,"data/pax/co2_emissions_variables.rds")
number_plots = 6 
if(number_plots == 6){
  tmp_h_trip1 <- tmp_h_trip1 %>% 
    .[variables %in% c("emi","pax","emi_capita","veh_cap","emi_dist_cap","occupancy"),]
  
}


ggplot(tmp_h_trip1)+
  geom_col(aes(y = as.numeric(value),x = horario))+
  # scale
  scale_x_continuous(breaks = c(seq(0,23,3),23)
                     ,limits = c(0-1,23+1)
                     ,expand = c(0,0))+
  # labs
  facet_wrap(~variables1,ncol = 2,scales = "free_y"
             ,labeller = "label_parsed")+
  labs(y = NULL,x = "Hour")+
  # theme
  theme_light()+
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text.x = element_text(hjust = 0,vjust = 1.0
                                     , margin=margin(l=0))
        ,strip.background = element_blank()
        ,strip.text = element_text(colour = 'black')
        ,legend.position = "bottom"
  )+
  guides(fill = guide_legend(title.position = "top"))


ggplot2::ggsave(filename =  fifelse(number_plots == 6,
                                    "figures/pax/co2_per_capita.png",
                                    "figures/pax/co2_per_capita_all.png")
                , scale =  fifelse(number_plots == 6,0.85,0.85) # 2 x 4 plots  
                #, scale = 0.75  # 2 x 2 plots
                , height = fifelse(number_plots == 6,12.5,18.5) # 2 x 4 plots  
                , width = 20 # 2 x 2 plots
                #, height = 12.5
                , bg = "white" 
                  , units = "cm" , dpi = 300 )


## mean stats -----

unique(tmp_h_trip1$variables)

### mean Ef ----
tmp_h_trip1[,{
  emi <- value[variables == "emi"]
  dist <- value[variables == "dist"]
  emi_dist = sum(emi)/sum(dist)
  list("emi_dist" = emi_dist)
}] 

### ef for hour ----
tmp_h_trip1[variables == "emi_dist",] %>% 
  .[order(horario)]

### ef for HPM ----
tmp_h_trip1[,{
  emi <- value[variables == "emi"]
  dist <- value[variables == "dist"]
  emi_dist = sum(emi)/sum(dist)
  list("emi_dist" = emi_dist)
}] 

# 6) CO2, pax, co2/pax zoom ----
rm(list=ls())
units::install_unit("pax")

# read Tiles & Boundaries
my_tile <- readr::read_rds("data-raw/bra_spo_maptile.rds")
my_bound <- readr::read_rds("data-raw/bra_spo_boundary.rds")
my_bound <- sf::st_transform(my_bound,3857)

# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# read grid
gridded_pax_files <- list.files("data/pax/grid/"
                                ,full.names = TRUE)

gridded_pax <- lapply(gridded_pax_files,
                      readr::read_rds) %>% 
  data.table::rbindlist()

gridded_pax <- gridded_pax[,{ 
  list("emi" = sum(emi,na.rm = TRUE)
       ,"pax" = sum(pax,na.rm = TRUE) %>% as.numeric()
       ,"dist" = sum(dist,na.rm = TRUE)
       ,"geometry" = geom[1])
},by = .(id_hex)]
#units::install_unit("pax")
gridded_pax[,pax := units::set_units(pax,"pax")]
gridded_pax[,emi_pax := emi / (pax)]
gridded_pax[,avg_ef := emi / (dist)]

gridded_pax <- sf::st_as_sf(gridded_pax)

# renda 

f_q <- function(i){
  as.numeric(
    cut(i
        , breaks= quantile(x = i
                           ,probs=seq(0, 1, by=0.20)
                           , na.rm=T),
        include.lowest= TRUE, labels=1:5))
}

# dimensions plot

bind_grid <- sf::st_as_sf(gridded_pax) %>% 
  sf::st_transform(3857)

map_scale <- as.numeric(sf::st_bbox(bind_grid)[3]) -  
  as.numeric(sf::st_bbox(bind_grid)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

## zoom ----


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

lim_fill_co2 <- bind_grid %>% 
  data.table::setDT() %>% 
  .[,list(min(emi),max(emi))] %>% 
  as.numeric() %>% as.vector()

## gtfs ----
spo_gtfs <- gtfstools::read_gtfs("data/jul/gtfs_spo_sptrans_prep_oct.zip")
gps_lines_sf <- gtfstools::convert_shapes_to_sf(gtfs = spo_gtfs)
stops_sf <- gtfstools::convert_stops_to_sf(gtfs = spo_gtfs)

intersect_lines_sf <- sf::st_intersection(
  x = sf::st_transform(gps_lines_sf,3857)
  ,y = get_bbox)
intersect_stops_sf <- sf::st_intersection(
  x = sf::st_transform(stops_sf,3857)
  ,y = get_bbox)

rm(gps_lines_sf)
rm(stops_sf)
rm(spo_gtfs)


pr <- bind_grid %>% 
  setDT() %>% 
  .[,q_emi_pax := f_q(emi_pax)] %>% 
  .[,q_emi := f_q(emi)] %>% 
  .[,q_pax := f_q(pax)] %>% 
  melt(data = .,
       measure.vars = c("q_emi","q_pax","q_emi_pax"),
       id.vars = c("geometry")) %>% 
  sf::st_as_sf()

f_full <- function(dt_full,input,map = TRUE){
  if( input == 'q_emi'    ) mytitle <- expression(CO[2][] (g))
  if( input == 'q_pax'    ) mytitle <- "Number of\npassengers"
  if( input == 'q_emi_pax') mytitle <- expression(CO[2]~per~passenger(g))
  
  
  tmp_plot <- ggplot(dt_full)+
    # add raster
    geom_raster(data = my_tile, aes(x, y, fill = hex)
                , alpha = 1) +
    #coord_cartesian(xlim = xlim_coord, ylim = ylim_coord
    #                 ,expand = FALSE) +
    #coord_equal() +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    # add emissions
    ggnewscale::new_scale_fill()+
    geom_sf(data = dt_full
            ,mapping = aes(fill = as.factor(value))
            , color = NA) +
    scale_fill_viridis_d(
      option = "D"
      ,direction = -1
      #,labels = c("1 (bottom)",2:4,"5 (up)")
    )  +
    # add red box
    geom_sf(data = get_bbox,color = "red",fill = NA)+
    ## add boundary
    ggnewscale::new_scale_color()+
    geom_sf(data = my_bound
            ,color = "black"
              ,linetype = "solid",alpha = 0.5
            , linewidth = 0.15,fill = NA) +
    labs(x = NULL,y = NULL,fill = NULL,title = mytitle) +
    theme(legend.position = "none"
          , axis.ticks.length = unit(0, "pt")
          , axis.ticks.length.x = NULL
          , axis.ticks.length.x.top = NULL
          , axis.ticks.length.x.bottom = NULL 
          , axis.ticks.length.y = NULL
          , axis.ticks.length.y.left = NULL
          , axis.ticks.length.y.right = NULL
          , axis.text = element_blank()
          , axis.title = element_blank()
          , text = element_text(family = "LM Roman 10")
          , legend.box.background = element_rect(fill = "white"
                                                   ,color = "white")
          , legend.key.size = ggplot2::unit(0.03250,"npc")
          , legend.box.margin = margin(3,3,3,3, "pt")) 
  
  if(map == TRUE){
    tmp_plot <- tmp_plot + 
      # map itens
      ggsn::north(data = my_bound,
                  location = "topright",symbol = 12) +
      ggsn::scalebar(x.min = sf::st_bbox(my_bound)[1] %>% as.numeric()
                     , x.max = sf::st_bbox(my_bound)[3] %>% as.numeric()
                     , y.min = sf::st_bbox(my_bound)[2] %>% as.numeric()
                     , y.max = sf::st_bbox(my_bound)[4] %>% as.numeric()
                     , dist = plot_scale#/2
                     , family = "LM Roman 10"
                     , fontface = "plain"
                     , dist_unit = "km"
                     , st.size = 2.5
                     , location = "bottomright"
                     , st.dist = 0.035
                     , st.bottom = FALSE
                     , st.color = "black"
                       , transform = FALSE
                     , model = "WGS84") 
  }
  return(tmp_plot)
}


dt_zoom <- sf::st_intersection(pr,get_bbox)

f_zoom <- function(dt_zoom,map = TRUE,rm_fill = TRUE){
  tmp_plot <- ggplot(dt_zoom) + 
    # data
    #geom_raster(data = my_tile[my_tile$x > min(lim_coord_zoom$X) & 
    #                             my_tile$x < max(lim_coord_zoom$X) & 
    #                             my_tile$y > min(lim_coord_zoom$Y) & 
    #                             my_tile$y < max(lim_coord_zoom$Y),]
    #            , aes(x, y, fill = hex), alpha = 1) +
    #scale_fill_identity() +
    ## add emissions
    #ggnewscale::new_scale_fill() +
    geom_sf(aes(fill = as.factor(value))
            , colour = "transparent")  +
    scale_fill_viridis_d(
      option = "D"
      ,direction = -1
      ,labels = c("1 (bottom)",2:4,"5 (top)")
    ) +
    # add street network
    geom_sf(data = intersect_lines_sf
            ,alpha = 0.15
            ,linewidth = 0.25
            ,color = "black")+
    # limits
    coord_sf(xlim = lim_coord_zoom$X
             , ylim = lim_coord_zoom$Y
             ,expand = FALSE) +
    # add maps
    facet_wrap(~variable
               ,labeller = as_labeller(
                 c(`q_emi` = "CO2 emissions (g)",
                   `q_pax` = "Number of\npassenger (pax)",
                   `q_emi_pax` = "CO2 per pax (g/pax)")
               ))+
    # labels 
    labs(fill = "Quintile"
         ,x = NULL
         ,y = NULL) + 
    # theme and map
    theme(text = element_text(family = "LM Roman 10")
          , axis.title = element_blank()
          , axis.text = element_blank()
          , axis.ticks.length = unit(0, "pt")
          , axis.ticks.length.x = NULL
          , axis.ticks.length.x.top = NULL
          , axis.ticks.length.x.bottom = NULL 
          , axis.ticks.length.y = NULL
          , axis.ticks.length.y.left = NULL
          , axis.ticks.length.y.right = NULL
          , strip.background = element_blank()
          , strip.text.x = element_blank()
          , panel.border = element_rect(color = "black"
                                          , fill = NA,linewidth = 0.75))
  #if(rm_fill){
  #  tmp_plot  <- tmp_plot + 
  #    theme(legend.position = "none")
  #}
  
  if(map){
    tmp_plot <- tmp_plot + 
      ggsn::north(data = get_bbox_scale
                  ,location = "topright"
                  ,symbol = 12) +
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
  }
  return(tmp_plot)
}  

## save-----
z1 <- f_zoom(dt_zoom[dt_zoom$variable %in% c("q_emi"),],map = TRUE)
z2 <- f_zoom(dt_zoom[dt_zoom$variable %in% c("q_pax"),],map = FALSE,rm_fill = FALSE)
z3 <- f_zoom(dt_zoom[dt_zoom$variable %in% c("q_emi_pax"),],map = FALSE)

#(z1+z2+z3) + plot_layout(guides = "collect")
#z1
f1 <- f_full(pr[pr$variable %in% c("q_emi"),]    ,input = "q_emi",map = TRUE)
f2 <- f_full(pr[pr$variable %in% c("q_pax"),]    ,input = "q_pax",map = FALSE)
f3 <- f_full(pr[pr$variable %in% c("q_emi_pax"),],input = "q_emi_pax",map = FALSE)

fm <- (f1 + f2 + f3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "none")

zm <- (z1 + z2 + z3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

#pf <- fm / zm
pf1 <- fm / (zm)   +
  plot_layout(heights = c(2, 1))

#pf

ggplot2::ggsave(plot = pf1
                , filename = "figures/pax/mapa_zoom_per_pax_facet1.png"
                , scale = 0.75
                , width = 17*1.15,
                , bg = "white"
                  , height = 17
                , units = "cm"
                , dpi = 300)

# 7) Extra ------
## 7.1) Plot Co2/ (capita * stree link) -----
rm(list=ls())
gc(reset = TRUE)

# read h_trip
dt_h_trip <- readr::read_rds("data/pax/dt_h_trip.rds")
tmp_h_trip <- copy(dt_h_trip) %>% 
  .[,passageiros := as.numeric(passageiros)] %>% 
  .[,stop_id_atual  := as.integer(stop_id_atual) %>% as.character()] %>% 
  .[,prox_stop_id   := as.integer(prox_stop_id) %>% as.character()] %>% 
  .[,passageiros := as.numeric(passageiros)] %>% 
  .[,sum(passageiros,na.rm = TRUE),by = .(stop_id_atual,prox_stop_id)]

tmp_h_trip[1,]

# read emi by stops interval

main_f <- "L://Proj_acess_oport//git_jbazzo//gtfs2emis_paper//data/emissions//"
main_f <- "data/oct/emissions/"
emi_files <- list.files(main_f,full.names = TRUE)
emi_shapes <- list.files(main_f,full.names = F) %>% gsub(".rds","",.)

## processing data -----
future::plan(strategy = "multisession", workers = 1)
emi_dt <- furrr::future_map(.x = seq_along(emi_files)
                            ,.f = function(i){ # i = 1
                              
                              dt1 <- readr::read_rds(emi_files[i])
                              
                              # emi_to_dt by 
                              dt2 <- gtfs2emis::emis_to_dt(emi_list = dt1) %>% 
                                .[pollutant == "CO2",] %>% 
                                .[,list("emi" = sum(emi)),by = .(segment_id)] %>% 
                                .[,dist := dt1$tp_model$dist] %>% 
                                # add info
                                .[,from_stop_id := dt1$tp_model$from_stop_id] %>% 
                                .[,to_stop_id := dt1$tp_model$to_stop_id] %>% 
                                .[,geometry := dt1$tp_model$geometry] %>% 
                                .[,shape_id := emi_shapes[i]] %>% 
                                # sum by stop
                                .[,list("emi" = sum(emi)
                                        ,"dist" = sum(dist)
                                        ,"geometry" = geometry[1])
                                  ,by = .(from_stop_id,to_stop_id,shape_id)] 
                              
                              return(dt2)
                            }) %>% data.table::rbindlist()

readr::write_rds(x = emi_dt
                 ,file = "data/oct/emi_dt_spatial.rds"
                 ,compress = "gz")



## 8) Plot co2/pax*km ------
rm(list=ls())

units::install_unit("pax")
emi_dt <- readr::read_rds("data/oct/emi_dt_spatial.rds")
emi_dt[1,]

emi_dt <- emi_dt[
  ,list("emi" = sum(emi,na.rm = TRUE)
        ,"dist" = sum(dist,na.rm = TRUE)
        ,"geometry" = geometry[1])
  ,by = .(from_stop_id, to_stop_id)]
emi_dt <- emi_dt[from_stop_id != "-" & to_stop_id != "-",]

head(emi_dt)

# read h_trip
tmp_h_trip <- readr::read_rds("data/pax/carregamento_prep.rds")
tmp_h_trip <- copy(tmp_h_trip) %>% 
  .[,stop_id_atual  := as.integer(stop_id_atual) %>% as.character()] %>% 
  .[,prox_stop_id   := as.integer(prox_stop_id) %>% as.character()] %>% 
  .[,list("pax" = sum(day_carreg,na.rm = TRUE))
    ,by = .(stop_id_atual,prox_stop_id)]

# merge datasets


tmp_h_trip[1,]
emi_dt[1,]

tmp_h_trip[emi_dt,on = c("stop_id_atual" = "from_stop_id"
                         ,"prox_stop_id" = "to_stop_id")
           ,":=" (emi = i.emi
                  ,dist = i.dist
                  ,geometry = i.geometry)]

# sum emissions and passengers by interval

tmp_h_trip <- tmp_h_trip[,list(
  "pax" = sum(pax,na.rm = TRUE)
  ,"emi" = sum(emi,na.rm = TRUE)
  ,"dist" = sum(dist,na.rm = TRUE)
  ,"geometry" = geometry[1]
), by = .(stop_id_atual,prox_stop_id)]

tmp_h_trip[,pax := units::set_units(pax,"pax")]
tmp_h_trip[,emi_capita := emi/pax]

tmp_h_trip[1,]
# ratio of missing emi
# (conditions where emi = 0 and pax > 0)
tmp_miss <- tmp_h_trip[as.numeric(pax) > 0 & as.numeric(emi) == 0,]
nrow(tmp_miss);rm(tmp_miss) # 1557

# remove missing
tmp_h_trip <- tmp_h_trip[as.numeric(pax) > 0 & as.numeric(emi) > 0,]

setnames(tmp_h_trip
         ,old = c("stop_id_atual","prox_stop_id")
         ,new = c("from_stop_id","to_stop_id"))

tmp_h_trip[,stop_sequence := 1:.N]
# to sf 
tmp_h_trip <- sf::st_as_sf(tmp_h_trip) #,4326)
tmp_h_trip <- sf::st_set_crs(tmp_h_trip,4326)
tmp_h_trip$shape_id <- "IDK"

hex_spo <- readr::read_rds("data-raw/bra_spo_grid.rds")



# run multiple times
dir.create("data/pax/grid/")
nrow(tmp_h_trip)
b_seq <- seq(1,nrow(tmp_h_trip),by = 100)
end_seq <- shift(b_seq-1,1,nrow(tmp_h_trip),type = "lead") 
head(b_seq);head(end_seq)
tail(b_seq);tail(end_seq)
inter_list <- lapply(seq_along(b_seq),function(i){
  return(b_seq[i]:end_seq[i])
})

tmp_h_trip$pax <- 
  units::set_units(
    as.numeric(tmp_h_trip$pax),"m^2")


grid_pax <- function(i){ # i = 1
  
  intv = inter_list[[i]]
  emi_dt <- tmp_h_trip[intv,c("pax","emi","dist")]
  emi_dt$geometry <- NULL
  
  emi <- list("tp_model" = tmp_h_trip[intv,]
              ,"emi" = emi_dt)
  
  
  tmp_grid <- gtfs2emis::emis_grid(emi_list = emi
                                   ,grid = hex_spo
                                   ,time_resolution = 'day'
                                   ,quiet = TRUE
                                   ,aggregate = FALSE)
  
  output_path <- sprintf("data/pax/grid/%s.rds",i)
  readr::write_rds(x = tmp_grid
                   ,file = output_path)
  return(NULL)
  
}


future::plan("multisession", workers = 1)
lapply(X = seq_along(inter_list),FUN = grid_pax) 

## 8.1) read grids ----
rm(list=ls())
gridded_pax_files <- list.files("data/pax/grid/"
                                ,full.names = TRUE)

gridded_pax <- lapply(gridded_pax_files,
                      readr::read_rds) %>% 
  data.table::rbindlist()

gridded_pax <- gridded_pax[,{ 
  list("emi" = sum(emi,na.rm = TRUE)
       ,"pax" = sum(pax,na.rm = TRUE) %>% as.numeric()
       ,"dist" = sum(dist,na.rm = TRUE)
       ,"geometry" = geom[1])
},by = .(id_hex)]
gridded_pax[,pax := units::set_units(pax,"pax")]
gridded_pax[,emi_pax := emi / (pax)]
gridded_pax[,avg_ef := emi / (dist)]

#gridded_pax <- gridded_pax[as.numeric(pax) > 4,]
gridded_pax$avg_ef %>% summary()
gridded_pax$emi_pax %>% summary()
gridded_pax[as.numeric(emi_pax) > 300,]



# end -----
