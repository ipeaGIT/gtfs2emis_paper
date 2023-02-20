# 0) Load libraries ----

rm(list=ls())
gc(reset = TRUE)

# gtfs2emis fixed URL
# https://zenodo.org/record/7308585/files/ipeaGIT/gtfs2emis-0.0.1.zip?download=1

easypackages::packages('devtools'
                       # data analysis/visualization
                       , 'data.table','readr', 'magrittr', 'furrr'
                       , 'clipr', 'ggplot2', 'sfheaders', 'sf'
                       # data
                       , 'aopdata', 'geobr'
                       # gtfs packages
                       , 'gtfs2gps', 'gtfstools'
                       # fonts
                       , 'extrafont', 'extrafontdb', 'fontcm', 'extrafont')
library(gtfs2emis)


# 1) Transport model ----
# read gtfs
#spo_gtfs <- gtfstools::read_gtfs("data/gtfs_spo_sptrans_prep.zip")
spo_gtfs <- gtfstools::read_gtfs("data/gtfs_spo_sptrans_prep_oct.zip")

spo_gtfs$trips$trip_id %>% uniqueN()      # 2271
spo_gtfs$trips$shape_id %>% uniqueN()     # 2271
spo_gtfs$trips$trip_id  %>% uniqueN()     # 2271
spo_gtfs$shapes$shape_id  %>% uniqueN()   # 2271
spo_gtfs$stop_times$trip_id %>% uniqueN() # 2271
spo_gtfs$trips$shape_id  %>% uniqueN()    # 2271
spo_gtfs$trips %>% nrow() # 2271

# generate gps

dir.create("data/transport_model/jul/")
dir.create("data/transport_model/oct/")

transport_model(gtfs_data = spo_gtfs
                ,min_speed = 2
                ,max_speed = 80
                ,new_speed = NULL
                ,parallel = TRUE
                ,ncores = 37
                ,spatial_resolution = 100
                ,output_path = "data/transport_model/oct/"
                ,continue = TRUE)


## general statistics ----

#' number of routes
uniqueN(spo_gtfs$routes$route_id)

#' trips per day
files_gps <- list.files("data/transport_model/",full.names = TRUE)
files_gps_names <- list.files("data/transport_model/",full.names = FALSE)

future::plan(strategy =  "multisession",workers = 35)

gen_stats <- furrr::future_map(seq_along(files_gps)
                               ,
                               function(i){ # i = 1
                                 
                                 #message(paste0("Stats of Linestring file '",files_gps_names[i],"'"))
                                 
                                 tmp_gps <- readr::read_rds(files_gps[i])
                                 data.table::setDT(tmp_gps)
                                 
                                 #
                                 return(tmp_gps)
                               }
) %>% data.table::rbindlist(use.names = TRUE)

gen_stats$dist %>% sum()

gen_stats[1]
gen_stats[spo_gtfs$trips, on = "shape_id",route_id := i.route_id]
stats_dt <- data.table::data.table(
  #"shape_id" = gsub(".rds","",files_gps_names[i])
  "number_trips" = data.table::uniqueN(gen_stats$trip_id)
  ,"number_stop_id" = data.table::uniqueN(gen_stats$from_stop_id)
  ,"number_stop_id_per_trip" = data.table::uniqueN(gen_stats$from_stop_id) /
    data.table::uniqueN(gen_stats$trip_id)
  ,"Q25" = Hmisc::wtd.quantile(x = gen_stats$speed,weights = as.numeric(gen_stats$dist),probs = .25,na.rm = TRUE)
  ,"Q50" = Hmisc::wtd.quantile(x = gen_stats$speed,weights = as.numeric(gen_stats$dist),probs = .50,na.rm = TRUE)
  ,"Q75" = Hmisc::wtd.quantile(x = gen_stats$speed,weights = as.numeric(gen_stats$dist),probs = .75,na.rm = TRUE)
  ,"VTK" = units::set_units(sum(gen_stats$dist),"km")
  ,"VTK_per_trip" = units::set_units(sum(gen_stats$dist) /
                                       data.table::uniqueN(gen_stats$trip_id),"km")
  ,"total_time" = units::set_units(sum(gen_stats$dist/gen_stats$speed,na.rm=TRUE),"h")
)

stats_dt[1]
#gen_stats[,dist := as.numeric(dist)]

gen_stats <- gen_stats[,{
  list(
    "number_trips_per_route" = data.table::uniqueN(trip_id) / 
      data.table::uniqueN(route_id),
    "number_routes" = data.table::uniqueN(route_id),
    "number_stop_id" = data.table::uniqueN(from_stop_id),
    "number_stop_id_per_route" = data.table::uniqueN(from_stop_id) / 
      data.table::uniqueN(route_id),
    
    "Q25" = weighted.mean(speed,as.numeric(dist)),
    "Q50" = weighted.mean(speed,as.numeric(dist)),
    "Q75" = weighted.mean(speed,as.numeric(dist)),
    "VTK" = sum(dist),
    
    "VTK_per_route" = sum(dist) /data.table::uniqueN(route_id),
    
    "total_time" = sum(units::set_units(sum(
      dist/speed
      ,na.rm=TRUE),"h"))
  )}]

gen_stats[1]
#readr::write_rds(gen_stats,"article/data/general_stats.rds")
gen_stats[,total_time := as.numeric(total_time) ]
gen_stats[,lapply(.SD,weighted.mean,as.numeric(VTK))
          ,.SDcols = c("Q25","Q50","Q75")]
gen_stats[,lapply(.SD,sum)
          ,.SDcols = c("number_routes","total_time","VTK")]
gen_stats$number_stop_id %>% sum()


sum(gen_stats$number_stop_id)/nrow(gen_stats)




# 2) Emissions -----
# obs: this needs to read fleet first
#
rm(list=ls())
gc(reset=TRUE)
#devtools::load_all()
#devtools::document()
#devtools::load_all()

# Read fleet 
fleet_spo <- readr::read_rds("data-raw/bra_spo_fleet.rds")
# adjust technology
fleet_spo <- fleet_spo[fuel %in% "D",] # remove electric
fleet_spo[euro %in% "V", Technology := "SCR"]
fleet_spo[euro %in% "III",Technology := "-"]
fleet_spo <- fleet_spo[,list(N = sum(N))
                       ,by = .(year,fuel,euro,type_name_eu
                               ,Technology)]
fleet_spo[,fleet_composition := N/sum(N)]

setnames(fleet_spo,"Technology","tech")
setnames(fleet_spo,"year","model_year")
setnames(fleet_spo,"type_name_eu","veh_type")
dir.create("data/emissions/")

future::plan(strategy =  "multisession",workers = 35)


emission_model(tp_model = "data/transport_model"
               ,ef_model = "ef_europe_emep"
               ,pollutant = c("CO2","NOx","PM10","CH4")
               ,fleet_data = fleet_spo
               ,parallel = TRUE
               ,ncores = 35
               ,output_path = "data/emissions"
               ,continue = FALSE
               ,quiet = TRUE)

# 4) Processing -----
## a) Time processing -----
rm(list=ls())
gc(reset=TRUE)
dir.create("data/emi_time/")

# list-files
files_gps <- list.files(path = 'data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emissions/',full.names = FALSE)

oplan <- future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')

time_processing <- furrr::future_map(seq_along(files_gps),function(j){ # j = 1
  
  
  # message(paste0("Emi time of file '",files_gps_names[i],"'"))
  
  #message(i)
  temp_emi <- readr::read_rds(files_gps[j])
  emi_post <- emis_summary(emi_list = temp_emi
                           ,by = "time")
  #emi_post <- units::drop_units(emi_post)
  readr::write_rds(x = emi_post
                   ,file = paste0("data/emi_time/",files_gps_names[j])
                   ,compress = 'gz')
  return(NULL)
},.options = furrr::furrr_options(seed=TRUE
                                  ,packages = requiredPackages))

## b) Spatial post-processing-----
rm(list=ls())
gc(reset = TRUE)

dir.create("data/emi_grid/")

# list-files
files_gps <- list.files(path = 'data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emissions/',full.names = FALSE)

temp_grid <- readr::read_rds("data-raw/bra_spo_grid.rds")
temp_grid <-  sf::st_transform(temp_grid,"+proj=utm +zone=23 +ellps=WGS84 +south +units=m")

spatial_f <- function(i){ # i = 2
  #message(paste0("Emi grid of file '",files_gps_names[i],"'"))
  
  temp_emi <- readr::read_rds(files_gps[i])
  
  #  transform
  temp_emi$tp_model <- temp_emi$tp_model %>% 
    sf::st_transform("+proj=utm +zone=23 +ellps=WGS84 +south +units=m")
  
  # grid
  output_grid <-  emis_grid(emi_list =temp_emi
                            ,grid = temp_grid
                            ,quiet = TRUE
                            ,aggregate = TRUE)
  # write
  readr::write_rds(x = output_grid
                   ,file = paste0("data/emi_grid/",files_gps_names[i])
                   ,compress = 'gz')
  return(NULL) 
}


oplan <- future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')

spatial_process <- furrr::future_map(seq_along(files_gps)
                  ,spatial_f 
                  ,.options = furrr::furrr_options(
                    seed=TRUE
                    ,packages = requiredPackages))

## b) Spatial / TIME post-processing-----
rm(list=ls())
gc(reset = TRUE)

dir.create("data/emi_grid_time/")

# list-files
files_gps <- list.files(path = 'data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emissions/',full.names = FALSE)

temp_grid <- readr::read_rds("data-raw/bra_spo_grid.rds")
temp_grid <-  sf::st_transform(temp_grid,"+proj=utm +zone=23 +ellps=WGS84 +south +units=m")

spatial_f <- function(i){ # i = 2
  #message(paste0("Emi grid of file '",files_gps_names[i],"'"))
  
  temp_emi <- readr::read_rds(files_gps[i])
  
  #  transform
  temp_emi$tp_model <- temp_emi$tp_model %>% 
    sf::st_transform("+proj=utm +zone=23 +ellps=WGS84 +south +units=m")
  
  # grid
  output_grid <-  emis_grid(emi_list =temp_emi
                            ,grid = temp_grid
                            ,quiet = TRUE
                            ,aggregate = TRUE
                            ,time_resolution = "hour")
  # write
  readr::write_rds(x = output_grid
                   ,file = paste0("data/emi_grid_time/",files_gps_names[i])
                   ,compress = 'gz')
  return(NULL) 
}


oplan <- future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')

spatial_processing <- furrr::future_map(seq_along(files_gps)
                                        ,spatial_f 
                                        ,.options = furrr::furrr_options(
                                          seed=TRUE
                                          ,packages = requiredPackages))

## c) Vehicle-age post-processing-----
rm(list=ls())
gc(reset = TRUE)

dir.create("data/emi_age/")

# list-files
files_gps <- list.files(path = 'data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emissions/',full.names = FALSE)

f_veh_agr <- function(i){ # i = 1
  # read
  temp_emi <- readr::read_rds(files_gps[i])
  # process
  temp_emi_dt <- emis_summary(emi_list = temp_emi
                              ,by = "vehicle"
                              ,veh_vars = c("veh_type","euro"
                                            ,"fuel","tech","fleet_composition"
                              ))
  temp_emi_dt$VTK <- sum(units::set_units(temp_emi$tp_model$dist,"km"))
  # write
  readr::write_rds(x = temp_emi_dt
                   ,file = paste0("data/emi_age/",files_gps_names[i])
                   ,compress = 'gz')
  return(NULL)
}

future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')

furrr::future_map(seq_along(files_gps)
                  ,f_veh_agr 
                  ,.options = furrr::furrr_options(
                    seed=TRUE
                    ,packages = requiredPackages))
# End -----