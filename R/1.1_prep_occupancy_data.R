
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
readr::write_rds(dt_raw,"data/pax/bra_spo_occupancy_routes_out2019.rds",compress = "gz")

# dt_day_trip -----

rm(list=ls())
dt <- readr::read_rds("data/pax/bra_spo_occupancy_routes_out2019.rds")

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
dt_h_trip <- readr::read_rds("data/pax/bra_spo_occupancy_routes_out2019.rds")

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

gtfs_path <- "data-raw/gtfs_spo_sptrans_2019-10.zip"
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

dir.create("data/vtk_hour/oct/")
emi_path <- list.files("data/emissions//oct///",full.names = TRUE)
emi_files <- list.files("data/emissions///oct///",full.names = F)
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
                   ,file = paste0("data/vtk_hour/oct/",emi_files[i]))
  return(NULL)
})

# read
rm(list=ls())
gc(reset = TRUE)
emi_files <- list.files("data/vtk_hour//oct//",full.names = T)
emi_dt <- lapply(emi_files,readr::read_rds) %>% data.table::rbindlist()
emi_dt <- emi_dt[,list(dist = sum(dist,na.rm = TRUE)
                       ,veh_number = sum(veh_number,na.rm=TRUE))
                 ,by = hour]
readr::write_rds(x = emi_dt,"data/vtk_hour//oct/pax_vtk_hour.rds")


# 5) Pax spatial distribution -----
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

main_f <- "data/emissions/oct/"
emi_files <- list.files(main_f,full.names = TRUE)
emi_shapes <- list.files(main_f,full.names = FALSE) %>% gsub(".rds","",.)

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
                 ,file = "data/pax/emi_dt_spatial.rds"
                 ,compress = "gz")



# 6) Grid analysis ------
rm(list=ls())

units::install_unit("pax")
emi_dt <- readr::read_rds("data/pax/emi_dt_spatial.rds")
emi_dt[1,]

emi_dt <- emi_dt[
  ,list("emi" = sum(emi,na.rm = TRUE)
        ,"dist" = sum(dist,na.rm = TRUE)
        ,"geometry" = geometry[1])
  ,by = .(from_stop_id, to_stop_id)]
emi_dt <- emi_dt[from_stop_id != "-" & to_stop_id != "-",]

head(emi_dt)

# read h_trip
tmp_h_trip <- readr::read_rds("data/pax/bra_spo_occupancy_routes_out2019.rds")
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

## 6.1) read grids ----
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
