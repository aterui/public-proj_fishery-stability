
# setup -------------------------------------------------------------------

rm(list = ls())

source(here::here("code/library.R"))
source(here::here("code/set_crs.R"))
source(here::here("code/set_functions.R"))

wbt_init()


# channel delineation -----------------------------------------------------

## specify temporary file location to avoid white space in file path
v_name <- tempdir() %>% 
  paste(c("upa.tif",
          "stream.tif",
          "point.shp",
          "point_snap.shp",
          "dir.tif",
          "wsd.tif"),
        sep = "\\")

## write flow accumulation layer to tempdir()
sr_upa <- terra::rast(here::here("data_raw/gis/epsg4326_upa.tif"))
terra::writeRaster(sr_upa,
                   filename = v_name[str_detect(v_name, "upa")],
                   overwrite = TRUE)

## write point data to tempdir()
sf_point <- read_csv("data_raw/gis/site-coordinate_hogosuimen_terui-org-2019.csv") %>% 
  drop_na(longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  dplyr::select(-ID, -source) %>% 
  arrange(river, site) %>% 
  mutate(siteid = row_number()) %>% 
  relocate(siteid)

sf_point %>% 
  st_write(dsn = v_name[str_detect(v_name, "point\\.")],
           append = FALSE)

## extract stream cells
wbt_extract_streams(flow_accum = v_name[str_detect(v_name, "upa")],
                    output = v_name[str_detect(v_name, "stream")],
                    threshold = 1)

## snap points to streams
wbt_jenson_snap_pour_points(pour_pts = v_name[str_detect(v_name, "point\\.")],
                            streams = v_name[str_detect(v_name, "stream")],
                            output = v_name[str_detect(v_name, "point_snap")],
                            snap_dist = 2)


# watershed delineation ---------------------------------------------------

## flow direction, convert Arc to D8
terra::rast(here::here("data_raw/gis/epsg4326_dir.tif")) %>% 
  arc2d8() %>% 
  terra::writeRaster(filename = v_name[str_detect(v_name, "dir")],
                     overwrite = TRUE)

## watershed delineation
wbt_unnest_basins(d8_pntr = v_name[str_detect(v_name, "dir")],
                  pour_pts = v_name[str_detect(v_name, "point_snap")],
                  output = v_name[str_detect(v_name, "wsd")])

## raster to polygon
### multiple polygons appears for several outlets
### pick the largest polygon from each outlet to avoid duplicates

albers_sf_wsd <- list.files(path = tempdir(),
                            pattern = "wsd",
                            full.names = TRUE) %>% 
  lapply(terra::rast) %>% 
  lapply(stars::st_as_stars) %>% 
  lapply(sf::st_as_sf,
         merge = TRUE,
         as_points = FALSE) %>%
  bind_rows() %>% 
  st_transform(crs = wkt_jgd_albers) %>% 
  rowwise() %>% 
  mutate(wsdid = sum(c_across(cols = ends_with("tif")),
                     na.rm = TRUE)) %>% 
  select(wsdid) %>% 
  ungroup() %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) %>% 
  group_by(wsdid) %>% 
  slice(which.max(area)) %>% # remove duplicates by outlet
  ungroup() %>% 
  relocate(wsdid, area) %>% 
  arrange(wsdid)


# export ------------------------------------------------------------------

saveRDS(albers_sf_wsd,
        here::here("data_raw/gis/albers_wsd.rds"))

st_write(albers_sf_wsd,
         here::here("data_raw/gis/albers_wsd.gpkg"),
         append = FALSE)


# check -------------------------------------------------------------------
# 
# sf_point <- st_read(v_name[str_detect(v_name, "point_snap")]) %>%
#   st_set_crs(4326) %>% 
#   st_transform(wkt_jgd_albers)
# 
# library(tmap)
# tmap_mode("view")
#  
# tm_shape(albers_sf_wsd) +
#  tm_polygons(alpha = 0.3) +
#  tm_shape(sf_point) +
#  tm_dots()
#    

# remove ------------------------------------------------------------------

file.remove(list.files(tempdir(), full.names = T))


