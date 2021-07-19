
# setup -------------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse,
               foreach,
               sf,
               raster,
               RSAGA,
               maptools)

setwd(here::here("code_empirical"))


# define function ---------------------------------------------------------

st_snap_points = function(x, y, max_dist = 1000) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}


# gis ---------------------------------------------------------------------

df_channel <- st_read("data_gis/albers_channel_hkd.gpkg")

df_site <- read_csv("data_gis/site-coordinate_hogosuimen_terui-org-2019.csv") %>% 
  drop_na(longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(df_channel))

df_site_snap <- st_snap_points(df_site, df_channel)
df_coord <- st_coordinates(df_site_snap) %>% 
  as_tibble() %>% 
  bind_cols(df_site) %>% 
  as_tibble() %>% 
  dplyr::select(-geometry)

df_coord_sf <- st_as_sf(df_coord,
                        coords = c("X", "Y")) %>% 
  st_set_crs(st_crs(df_channel))

st_write(df_coord_sf,
         "data_gis/albers_point_snap_prwsd.gpkg",
         append = FALSE)


# watersehd delineation ---------------------------------------------------

#rsaga.get.libraries()
#rsaga.get.modules("ta_hydrology")
#rsaga.get.usage("ta_hydrology", module = 4)

df_wsd_polygon <- foreach(i = seq_len(nrow(df_coord)),
                          .combine = dplyr::bind_rows) %do% {
  
  rsaga.geoprocessor(lib = "ta_hydrology",
                     module = 4,
                     param = list(TARGET_PT_X = df_coord$X[i], 
                                  TARGET_PT_Y = df_coord$Y[i], 
                                  ELEVATION = "tempdir/albers_filled_dem_hkd.sgrd", 
                                  AREA = "tempdir/raster.sgrd",
                                  METHOD = 0)
  )
  
  wsd <- stars::read_stars("tempdir/raster.sdat")
  st_crs(wsd) <- st_crs(df_channel)
  wsd[wsd == 0] <- NA
  wsd_polygon <- st_as_sf(wsd,
                          as_points = FALSE,
                          merge = TRUE)
  
  wsd_polygon <- wsd_polygon %>% 
    mutate(id = i,
           river = df_site$river[i],
           site = df_site$site[i])
  
  file.remove(c("tempdir/raster.mgrd",
                "tempdir/raster.prj",
                "tempdir/raster.sdat",
                "tempdir/raster.sgrd"))
    
  return(wsd_polygon)
}

st_write(df_wsd_polygon,
         dsn = "data_gis/albers_wsd_prtwsd.gpkg",
         append = FALSE)

# mapping -----------------------------------------------------------------

df_site_snap <- st_read("data_gis/albers_point_snap_prwsd.gpkg")
df_wsd_polygon <- st_read("data_gis/albers_wsd_prtwsd.gpkg")

mapview::mapView(df_site_snap,
                 color = "red") +
  mapview::mapView(df_wsd_polygon)
