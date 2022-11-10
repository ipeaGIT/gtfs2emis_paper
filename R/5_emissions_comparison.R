
#
# check IEMA------
# http://emissoes.energiaeambiente.org.br/graficos

#'      |    IEMA            |  gtfs2emis  |  diff    | IEMA EF               | 
#' PM10 |   449.04 kg        |   314.2 kg  |  -30%    | 0.1621083 g/km        | 
#' NOx  |   25 t             |   32.20  t  |   29%    |  10.41667  g/km      | 
#' CO2  |   2.356 kt         |   3.052 kt  |   29%    |
#' CH4  |   144 kg           |   70.5  kg  |  -49%    |
#' VKM  |   2.4 x 10^6 km    |    2.77 Mkm |   15%    |                      | 
#' VKM dia
#' 0.0574+0.0229+0.0127+0.0222+0.0661+0.1167+
#' 0.1353+0.1330+0.1331+0.1268+0.1171+0.1099+
#' 0.1064+0.1082+0.1104+0.1143+0.1213+0.1208+
#' 0.1157+0.1230+0.1231+0.1107+0.0997+0.0901

units::set_units(units::set_units(449.04,"kg"),"g")/units::set_units(2.4 * 10^6,"km")
units::set_units(units::set_units(137,"kg"),"g")/units::set_units(2.77 * 10^6,"km")
units::set_units(units::set_units(25,"t"),"g")/units::set_units(2.4 * 10^6,"km")


ef_europe_emep(speed = units::set_units(11,"km/h")
               ,veh_type = rep("Ubus Std 15 - 18 t",3)
               ,euro = c("III","IV","V")
               ,pollutant = "CH4"
               ,fuel = "D"
               #,tech = "-"
               ,slope = 0
               ,load = 0.5
               ,as_list = FALSE) %>% 
  as.numeric() %>% 
  paste0(.,collapse = " - ")

#' GTFS2emis
#' 137 kg/dia  
#' 11.95 t/dia
#' 2.77 x 10^6 km
#' 



rm(list=ls())
gc(reset = TRUE)

# list-files
files_gps <- list.files(path = 'article/data/emi_age_old_veh//',full.names = TRUE)
tmp_my_age <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()
tmp_my_age[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_my_age[,list("emi"= sum(emi)
                 ,"vtk" = sum(fleet_composition  * VTK)
                 ,"fe" = round(sum(emi)/sum(fleet_composition  * VTK),4)
) 
,by = pollutant]

#' emi_age
#' pollutant              emi          vtk               fe
#' 1:       CO2 3.052492e+09 [g] 2776257 [km] 1099.4992 [g/km]
#' 2:       NOx 3.220615e+07 [g] 2776257 [km]   11.6006 [g/km]
#' 3:      PM10 3.142045e+05 [g] 2776257 [km]    0.1132 [g/km]
#' 4:       CH4 7.052168e+04 [g] 2776257 [km]    0.0254 [g/km]
#' 
#' emi_age_old
#' pollutant              emi          vtk               fe
#' 1:       CO2 3.160103e+09 [g] 2776257 [km] 1138.2603 [g/km]
#' 2:       NOx 3.307811e+07 [g] 2776257 [km]   11.9146 [g/km]
#' 3:      PM10 3.618146e+05 [g] 2776257 [km]    0.1303 [g/km]
#' 4:       CH4 9.530191e+04 [g] 2776257 [km]    0.0343 [g/km


