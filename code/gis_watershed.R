
# setup -------------------------------------------------------------------

rm(list = ls())

source(here::here("code/library.R"))
source(here::here("code/set_crs.R"))
source(here::here("code/set_functions.R"))

wbt_init()


# snap points -------------------------------------------------------------

### specify temporary file location to avoid white space in file path
v_name <- tempdir() %>% 
  paste(c("upa.tif",
          "stream.tif",
          "channel.shp",
          "point.shp",
          "point_snap.shp",
          "outlet.shp",
          "outlet_snap.shp",
          "dir.tif",
          "wsd.tif",
          "cat.tif"),
        sep = "\\")

### write flow accumulation layer to tempdir()
sr_upa <- terra::rast(here::here("data_raw/gis/epsg4326_upa.tif"))
terra::writeRaster(sr_upa,
                   filename = v_name[str_detect(v_name, "upa")],
                   overwrite = TRUE)

## sampling sites
### write point data to tempdir()
sf_point <- read_csv("data_raw/gis/site-coordinate_hogosuimen_terui-org-2019.csv") %>% 
  drop_na(longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  dplyr::select(-ID, -source) %>% 
  arrange(river, site) %>% 
  mutate(site_id = row_number()) %>% 
  relocate(site_id)

sf_point %>% 
  st_write(dsn = v_name[str_detect(v_name, "point\\.")],
           append = FALSE)

### extract stream cells
wbt_extract_streams(flow_accum = v_name[str_detect(v_name, "upa")],
                    output = v_name[str_detect(v_name, "stream")],
                    threshold = 1)

### snap points to streams
wbt_jenson_snap_pour_points(pour_pts = v_name[str_detect(v_name, "point\\.")],
                            streams = v_name[str_detect(v_name, "stream")],
                            output = v_name[str_detect(v_name, "point_snap")],
                            snap_dist = 2)

## outlet
### write outlet data to tempdir()
sf_outlet <- st_read(here::here("data_raw/gis/epsg4326_outlet.gpkg")) %>% 
  dplyr::select(river = join_ws_na) %>% 
  arrange(river) %>% 
  mutate(river = str_to_lower(river),
         site_id = row_number()) %>% 
  relocate(site_id)

sf_outlet %>% 
  st_write(dsn = v_name[str_detect(v_name, "outlet\\.")],
           append = FALSE)

### snap points to streams
wbt_jenson_snap_pour_points(pour_pts = v_name[str_detect(v_name, "outlet\\.")],
                            streams = v_name[str_detect(v_name, "stream")],
                            output = v_name[str_detect(v_name, "outlet_snap")],
                            snap_dist = 2)


# watershed & channel delineation -----------------------------------------

## flow direction, convert Arc to D8
terra::rast(here::here("data_raw/gis/epsg4326_dir.tif")) %>% 
  arc2d8() %>% 
  terra::writeRaster(filename = v_name[str_detect(v_name, "dir")],
                     overwrite = TRUE)

## stream raster to vector
wbt_raster_streams_to_vector(streams = v_name[str_detect(v_name, "stream")],
                             d8_pntr = v_name[str_detect(v_name, "dir")],
                             output = v_name[str_detect(v_name, "channel")])

## watershed delineation
wbt_unnest_basins(d8_pntr = v_name[str_detect(v_name, "dir")],
                  pour_pts = v_name[str_detect(v_name, "point_snap")],
                  output = v_name[str_detect(v_name, "wsd")])

wbt_unnest_basins(d8_pntr = v_name[str_detect(v_name, "dir")],
                  pour_pts = v_name[str_detect(v_name, "outlet_snap")],
                  output = v_name[str_detect(v_name, "cat")])

## raster to polygon
### multiple polygons appears for several outlets
### pick the largest polygon from each outlet to avoid duplicates

albers_sf_wsd <- foreach(x = c("wsd", "cat")) %do% {
  y0 <- list.files(path = tempdir(),
                   pattern = x,
                   full.names = TRUE) %>% 
    lapply(terra::rast) %>% 
    lapply(stars::st_as_stars) %>% 
    lapply(sf::st_as_sf,
           merge = TRUE,
           as_points = FALSE) %>%
    bind_rows() %>% 
    st_transform(crs = wkt_jgd_albers) %>% 
    rowwise() %>% 
    mutate(site_id = sum(c_across(cols = ends_with("tif")),
                         na.rm = TRUE)) %>% 
    dplyr::select(site_id) %>% 
    ungroup() %>% 
    mutate(area = units::set_units(st_area(.), "km^2")) %>% 
    group_by(site_id) %>% 
    slice(which.max(area)) %>% # remove duplicates by outlet
    ungroup() %>% 
    relocate(site_id, area) %>% 
    arrange(site_id)
  
  if(x == "wsd") {
    
    y0 %>% 
      left_join(as_tibble(sf_point) %>% dplyr::select(-geometry),
                by = "site_id") %>% 
      relocate(site_id, river, site) %>% 
      return()
    
  } else {
    
    y0 %>% 
      left_join(as_tibble(sf_outlet) %>% dplyr::select(-geom),
                by = "site_id") %>% 
      relocate(site_id, river) %>% 
      return()
    
  }
}


# export ------------------------------------------------------------------

## upstream watershed
saveRDS(albers_sf_wsd[[1]],
        here::here("data_raw/gis/albers_wsd.rds"))

st_write(albers_sf_wsd[[1]],
         here::here("data_raw/gis/albers_wsd.gpkg"),
         append = FALSE)

## outlet catchment
saveRDS(albers_sf_wsd[[2]],
        here::here("data_raw/gis/albers_wsd_outlet.rds"))

st_write(albers_sf_wsd[[2]],
         here::here("data_raw/gis/albers_wsd_outlet.gpkg"),
         append = FALSE)

## channel network
st_read(v_name[str_detect(v_name, "channel")]) %>% 
  st_set_crs(4326) %>% 
  saveRDS(here::here("data_raw/gis/epsg4326_channel.rds"))

st_read(v_name[str_detect(v_name, "channel")]) %>% 
  st_set_crs(4326) %>% 
  st_write(here::here("data_raw/gis/epsg4326_channel.gpkg"),
           append = FALSE)

## snap point
st_read(dsn = v_name[str_detect(v_name, "point_snap")]) %>% 
  saveRDS(here::here("data_raw/gis/epsg4326_point_snap.rds"))

st_read(dsn = v_name[str_detect(v_name, "point_snap")]) %>% 
  st_write(here::here("data_raw/gis/epsg4326_point_snap.gpkg"),
           append = FALSE)


# check -------------------------------------------------------------------

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


# remove ------------------------------------------------------------------

file.remove(list.files(tempdir(), full.names = T))


