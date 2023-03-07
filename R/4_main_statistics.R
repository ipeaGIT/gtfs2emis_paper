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

# 1) Stats avg EF ----
# list-files
rm(list=ls())
gc(reset = TRUE)
files_gps_t <- list.files(path = 'data/emi_age/jul/',full.names = TRUE)

tmp_stats_ef <- lapply(files_gps_t, readr::read_rds) %>% 
  data.table::rbindlist()

tmp_stats_ef

tmp_stats_ef[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_stats_ef[,VTK := as.numeric(as.character(VTK))]
tmp_stats_ef[,single_VTK := VTK * fleet_composition]

tmp_stats_ef1 <- tmp_stats_ef[,lapply(.SD,sum,na.rm=TRUE)
                              ,.SDcols = c("emi","single_VTK")
                              , by = .(pollutant)]

tmp_stats_ef1[,EF := emi / units::set_units(single_VTK,"km")] 
tmp_stats_ef1[,EF := round(EF,3)] 
tmp_stats_ef1

# 7) text------

## b)  emissions by time-----
rm(list=ls())
gc(reset = TRUE)
files_gps <- list.files(path = 'data/emi_time/jul/',full.names = TRUE)
tmp_my_time <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist(fill = TRUE)

to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}

# total
tmp1 <- copy(tmp_my_time)[,sum(emi),by = .(pollutant)]
tmp1 <- dcast(tmp1, .~ pollutant)
tmp1[,. := NULL]

tmp1[,lapply(.SD,to_compartible_units),.SDcols = names(tmp1)]

# pm10 by time intervals
tmp2 <- copy(tmp_my_time)[,sum(emi),by = .(timestamp_hour,pollutant)]
tmp2 <- tmp2[pollutant == "NOx"]
tmp2[,total_pol := sum(V1)]

# total pm10
tmp2[timestamp_hour %in% c(0,1,2),time_classe := "0:00 - 2:59"]
tmp2[timestamp_hour %in% c(6,7,8),time_classe := "6:00 - 8:59"]
tmp2[timestamp_hour %in% c(21,22,23),time_classe := "21:00 - 23:59"]

tmp2 <- tmp2[!is.na(time_classe)]
tmp2
tmp2[,{
  ratio =  100 * sum(V1) / total_pol[1]
  ratio = round(ratio,1)
  ratio = paste0(ratio,"%")
  
  total = units::set_units(sum(V1),"kg")
  total = round(total,1)
  
  total_day = units::set_units(total_pol[1],"kg")
  total_day = round(total_day,1) 
  
  list("total" = total
       ,"total_day" = total_day
       ,"ratio" = ratio)
},by = time_classe]


## c) emissions by veh -----

rm(list=ls())
gc(reset = TRUE)

# prep function
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(paste(round(i,1),"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(paste(round(i / (10^3),1),"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(paste(round(i / (10^6),1),"t"))}
}

# list-files
files_gps <- list.files(path = 'data/emi_age/jul/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emi_age/jul/',full.names = FALSE)

# read files
tmp_my_age <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()

# emissions by vehicle category
emi_cat <- copy(tmp_my_age)
emi_cat <- emi_cat[,list(total_emi = sum(emi)),by = .(veh_type,pollutant)]
emi_cat[,prop_emi := round(100 * total_emi / sum(total_emi),1)
        , by = pollutant]
emi_cat <- emi_cat[,{
  emi_adj <- to_compartible_units(total_emi)
  text_emi <- paste0(emi_adj," (",prop_emi,"%)")
  list(veh_type,text_emi)
},by = pollutant]

emi_cat[pollutant == "PM10"]
# emissions articulated
emi_artic <- copy(tmp_my_age)[veh_type == "Ubus Artic >18 t"]

emi_artic <- emi_artic[,list(total_emi = sum(emi)),by = .(euro,pollutant)]

emi_artic[,prop_emi := round(100 * total_emi / sum(total_emi),1), by = pollutant]

emi_artic
emi_artic[pollutant == "PM10"]
## d) fleet ----
fleet_spo <- readr::read_rds("data-raw/bra_spo_fleet.rds")
fleet_spo <- fleet_spo[,{
  list("total" = sum(N),"perc" = round(100*sum(N)/sum(fleet_spo$N),1))
},by = .(type_name_br)]
fleet_spo[type_name_br == "BUS_URBAN_D",cap := 75]
fleet_spo[type_name_br == "BUS_ARTIC_D",cap := 170]
fleet_spo[type_name_br == "BUS_MICRO_D",cap := 20]
weighted.mean(x = fleet_spo$cap,w = fleet_spo$total)
## e) euro V scenario -----

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

# emissions by euro
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(paste(round(i,1),"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 6){return(paste(round(i / (10^3),1),"kg"))}
  if(median(pol_digits,na.rm = TRUE) > 6){return(paste(round(i / (10^6),1),"t"))}
}

tmp <- tmp_my_age[,list(sum(as.numeric(emi))
                        ,to_compartible_units(sum(as.numeric(emi))))
                  ,by = .(pollutant,euro)]
tmp[, prop := round(100 * V1/sum(V1),2),by = .(pollutant)]
tmp[, prop := round(100 * V1/sum(V1),2)]


tmp_my_age[,list(sum(as.numeric(emi))
                 ,to_compartible_units(sum(as.numeric(emi))))]

# aggregate by sum
tmp <- tmp_my_age[pollutant %in% c('NOx','PM10'),list(
  emi = sum(emi)
  ,fleet_composition = sum(fleet_composition)
),by = .(pollutant,veh_type,euro,fuel,tech)]

tmp[,fleet_composition := fleet_composition/sum(fleet_composition)
    ,by = .(pollutant)]
tmp <- tmp[,{
  euro3_id <- which(euro == "III")
  euro5_id <- which(euro == "V")
  
  emi_fleetcomp <- emi[euro5_id] / fleet_composition[euro5_id]
  new_emi <- fleet_composition * emi_fleetcomp
  list(euro,fuel,tech,emi,fleet_composition,emi_fleetcomp,new_emi)
},by = .(pollutant,veh_type)]

tmp[,{
  total_new_emi <-  sum(as.numeric(new_emi))
  total_emi <- sum(as.numeric(emi))
  rate <- (total_new_emi - total_emi) / (total_emi)
  rate <- round(100 * rate,2)
  list(total_new_emi,total_emi,rate)
} , by = .(pollutant)]

(tmp$fleet_composition/4) %>% sum()
# aggregate by sum
tmp_my_age <- tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(VTK) * fleet_composition 
  ,avg_ef = sum(emi)/(sum(VTK) * fleet_composition )
),by = .(veh_type,euro ,pollutant)]
tmp_my_age
# total fleet

fleet_spo <- readr::read_rds("data-raw/bra_spo_fleet.rds")
sum(fleet_spo$N)

# eletric
electric <- fleet_spo[fuel == "Elétrico",]$N %>% sum()
electric
# prop eletric
round(100 * electric / sum(fleet_spo$N),1)

# fuel
fuel <- fleet_spo[fuel != "Elétrico",]$N %>% sum()
fuel
round(100 * fuel / sum(fleet_spo$N),1)


# prop by vehicle type 
# total and proportion of fuel
tp <- copy(fleet_spo)[fuel != "Elétrico",]
tp <- tp[,{total = sum(N)},by = .(type_name_eu)]

tp[,prop := round(100 * V1 / sum(V1),1)]
tp[,text := paste0(V1," (",prop,"%)")]
tp[]

# pa - Proportion of articulated buses of Euro III
pa <- copy(fleet_spo)
pa <- pa[fuel != "Elétrico" & type_name_eu == "Ubus Artic >18 t",]
pa <- pa[,list(N = sum(N)),by = .(euro)]
pa[,prop := round(100 * N / sum(N),1)]
pa

## f) vehicle per capita -----
rm(list=ls())
gc(reset = TRUE)

veh_cap <- readr::read_rds("data/pax/co2_emissions_variables.rds")
fleet_spo <- readr::read_rds("data-raw/bra_spo_fleet.rds")

# adjust fleet
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

#### find EF ------
return_fleet <- function(fleet,myload = 0.5){
  library(gtfs2emis)
  mean_ef <- gtfs2emis::ef_europe_emep(pollutant = "CO2"
                                       ,speed = units::set_units(14.3,"km/h")
                                       ,veh_type = fleet$veh_type
                                       ,euro = fleet$euro
                                       ,fuel = fleet$fuel
                                       ,tech = fleet$tech
                                       ,slope = 0.00
                                       ,load = myload
                                       ,as_list = FALSE
                                       ,fcorr = 1)
  mean_emi <- gtfs2emis:::multiply_ef(fleet_composition = fleet$fleet_composition
                                      ,dist = units::set_units(1,"km")
                                      ,ef = mean_ef
                                      ,aggregate = FALSE
                                      ,as_list = FALSE)
  mean_emi <- sum(mean_emi)
  return(mean_emi)
}

# check load
min_load <- return_fleet(fleet = fleet_spo,myload = 0.0)
mean_load <- return_fleet(fleet = fleet_spo,myload = 0.5)
max_load <- return_fleet(fleet = fleet_spo,myload = 1)
# diff mean min
100 * (min_load - mean_load)/ mean_load 
100 * (min_load - max_load)/ max_load 
min_load
mean_load
max_load
# mean_emi
mean_ef <- return_fleet(fleet = fleet_spo)
mean_ef <- units::set_units(mean_ef,"g/km")
# only micro
fleet_micro <- copy(fleet_spo)
fleet_micro <- fleet_micro[veh_type == "Ubus Midi <=15 t",]
fleet_micro[,fleet_composition := N/sum(N)]
ef_micro <- return_fleet(fleet = fleet_micro)
ef_micro <- units::set_units(ef_micro,"g/km")

# only size > std.size
fleet_std <- copy(fleet_spo)
fleet_std <- fleet_std[veh_type != "Ubus Midi <=15 t",]
fleet_std[,fleet_composition := N/sum(N)]
ef_std <- return_fleet(fleet = fleet_std)
ef_std <- units::set_units(ef_std,"g/km")

### estimate reduction ------

veh_red <- copy(veh_cap)
veh_red[horario %in% 0:4,new_time := "0-4h"]
veh_red[horario %in% 5:23,new_time := "5-23h"]

veh_red[,{
  #
  emi_std = sum(dist) * ef_std
  emi_micro = sum(dist) * ef_micro
  emi_normal = sum(dist * emi_dist)
  
  emi_std_capita = emi_std / sum(pax)
  emi_micro_capita = emi_micro / sum(pax)
  emi_normal_capita = emi_normal / sum(pax)
  
  # emi_capita
  emi_std_capita    = units::set_units(emi_std_capita,"g/pax")
  emi_micro_capita  = units::set_units(emi_micro_capita,"g/pax")
  emi_normal_capita = units::set_units(emi_normal_capita,"g/pax")
  # diff_emi_capita
  diff_std_capita  = 100 * (emi_std_capita - emi_normal_capita) / emi_normal_capita
  diff_micro_capita = 100 * (emi_micro_capita - emi_normal_capita) / emi_normal_capita
   # return
  list(emi_std_capita
       ,emi_micro_capita
       ,emi_normal_capita
       ,diff_std_capita  
       ,diff_micro_capita
       )
},by = .(new_time)]


## g) non-exhaust emissions -----

# Data from 
# Xu, Y., Gbologah, F. E., Lee, D.-Y., Liu, H., Rodgers, M. O., 
# & Guensler, R. L. (2015). Assessment of alternative fuel and powertrain
# transit bus options using real-world operations data: Life-cycle fuel and 
# emissions modeling. Applied Energy, 154, 143–159. 
# https://doi.org/10.1016/j.apenergy.2015.04.112


# San Francisco (CA)
pw_co2 <- c(1.9,1.6,1.9,1.7,1.6,0,0)
wp_co2 <- c(0.4,0.4,0.1,0.4,0.4,0.7,2.4)
pw_nox <- c(4.8,4.6,4.9,4.6,1,0,0)
wp_nox <- c(1.1,0.8,1.1,1.0,0.9,0.4,1.5)

prop_co2 <- pw_co2 / (pw_co2 + wp_co2)
#round(100*prop_co2,2)
prop_nox <- pw_nox / (pw_nox + wp_nox)
round(100*prop_nox,2)

# Atlanta (GA)
pw_co2 <- c(1.7,1.4,1.7,1.4,1.4,1.4,0.0)
wp_co2 <- c(0.4,0.4,0.1,0.3,0.3,1.2,1.6)
pw_nox <- c(3.6,3.4,3.6,3.4,0.7,0.0,0.0)
wp_nox <- c(1.0,0.7,1.0,0.8,0.8,1.1,1.0)

prop_co2 <- pw_co2 / (pw_co2 + wp_co2)
#round(100*prop_co2,2)
prop_nox <- pw_nox / (pw_nox + wp_nox)
round(100*prop_nox,2)

# Phoenix (AZ)
pw_co2 <- c(2.0,1.7,1.9,1.8,1.8,0.0,0.0)
wp_co2 <- c(0.4,0.4,0.1,0.4,0.4,1.3,1.2)
wp_nox <- c(1.1,0.8,1.2,1.0,1.0,1.9,1.3)
pw_nox <- c(4.4,4.0,4.4,4.2,0.9,0.0,0.0)

prop_co2 <- pw_co2 / (pw_co2 + wp_co2)
round(100*prop_co2,2)
#prop_nox <- pw_nox / (pw_nox + wp_nox)
round(100*prop_nox,2)


# End -----
