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
files_gps_t <- list.files(path = 'data/emi_age/',full.names = TRUE)

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
files_gps <- list.files(path = 'data/emi_time/',full.names = TRUE)
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
tmp2 <- tmp2[pollutant == "PM10"]
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
files_gps <- list.files(path = 'data/emi_age/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emi_age/',full.names = FALSE)

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

## e) euro V scenario -----

rm(list=ls())
gc(reset = TRUE)

# list-files
files_gps <- list.files(path = 'data/emi_age/',full.names = TRUE)
files_gps_names <- list.files(path = 'data/emi_age/',full.names = FALSE)

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

# End -----