# 0) Load libraries ----

rm(list=ls())
gc(reset = TRUE)

# gtfs2emis fixed URL
# https://zenodo.org/record/7308585/files/ipeaGIT/gtfs2emis-0.1.0.zip?download=1

easypackages::packages('devtools'
                       # data analysis/visualization
                       , 'data.table', 'magrittr', 'furrr'
                       , 'clipr', 'ggplot2', 'sfheaders', 'sf'
                       # data
                       , 'aopdata', 'geobr'
                       # gtfs packages
                       , 'gtfs2gps', 'gtfstools'
                       # fonts
                       , 'extrafont', 'extrafontdb', 'fontcm', 'extrafont')

# install local
download.file(url = "https://zenodo.org/record/7308585/files/ipeaGIT/gtfs2emis-0.1.0.zip?download=1"
              ,destfile = "gtfs2emis-0.1.0.zip",mode = "wb")
unzip(zipfile = "gtfs2emis-0.0.1.zip")

devtools::install("ipeaGIT-gtfs2emis-a89ddd9/")
library(gtfs2emis)


# 1) Download data ----
fleet_url <- "https://github.com/ipeaGIT/gtfs2emis/releases/download/research_paper_data/bra_spo_fleet.rds"
gtfs_jul_url <- "https://github.com/ipeaGIT/gtfs2emis/releases/download/research_paper_data/gtfs_spo_sptrans_2019-06.zip"
gtfs_oct_url <- "https://github.com/ipeaGIT/gtfs2emis/releases/download/research_paper_data/gtfs_spo_sptrans_2019-10.zip"
tile_url <- "https://github.com/ipeaGIT/gtfs2emis/releases/download/research_paper_data/bra_spo_maptile.rds"
pax_url <- "https://github.com/ipeaGIT/gtfs2emis/releases/download/research_paper_data/bra_spo_occupancy_routes_out2019.rds"

dir.create("data-raw")
download.file(url = fleet_url,destfile = "data-raw/bra_spo_fleet.rds",mode = "wb")
download.file(url = gtfs_jul_url,destfile = "data-raw/gtfs_spo_sptrans_2019-06.zip",mode = "wb")
download.file(url = gtfs_oct_url,destfile = "data-raw/gtfs_spo_sptrans_2019-10.zip",mode = "wb")
download.file(url = tile_url,destfile = "data-raw/bra_spo_maptile.rds",mode = "wb")
download.file(url = pax_url,destfile = "data-raw/bra_spo_occupancy_routes_out2019.rds",mode = "wb")

# 2) read GTFS  ----

spo_gtfs <- gtfstools::read_gtfs("data-raw/gtfs_spo_sptrans_2019-06.zip")
spo_gtfs_oct <- gtfstools::read_gtfs("data-raw/gtfs_spo_sptrans_2019-10.zip")
spo_gtfs_oct$`_transparencia_e-SIC_42374_email_05-09-19` <- NULL

# filter by bus route
spo_gtfs <- gtfstools::filter_by_route_type(spo_gtfs,route_type = 3)
spo_gtfs_oct <- gtfstools::filter_by_route_type(spo_gtfs_oct,route_type = 3)

# filter by weekday
spo_gtfs <- gtfstools::filter_by_weekday(spo_gtfs,weekday = "wednesday")
spo_gtfs_oct <- gtfstools::filter_by_weekday(spo_gtfs_oct,weekday = "wednesday")
dir.create("data/")

# save gtfs
gtfstools::write_gtfs(spo_gtfs,"data/gtfs_spo_sptrans_prep_jul.zip")
gtfstools::write_gtfs(spo_gtfs_oct,"data/gtfs_spo_sptrans_prep_oct.zip")

# 3) download SP boundary ----
spo_boundary <- geobr::read_municipality(code_muni = 3550308,simplified = FALSE)
saveRDS(spo_boundary,file = "data-raw/bra_spo_boundary.rds")


# 4) download SP grid ------
spo_grid <- aopdata::read_grid(city = "spo")
saveRDS(object = spo_grid,file = "data-raw/bra_spo_grid.rds")

# End ----