
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))

source(here::here("code/library.R"))
source(here::here("code/set_crs.R"))


# read polygons and points ------------------------------------------------

## watershed polygons
### by site
albers_sf_wsd <- readRDS("data_raw/gis/albers_wsd.rds") %>%
  dplyr::select(river,
                site)

### by watershed
albers_sf_wsd_outlet <- readRDS("data_raw/gis/albers_wsd_outlet.rds")

## sampling sites
wgs84_sf_site <- readRDS("data_raw/gis/epsg4326_point_snap.rds")

## 50m buffer around sampling sites
albers_sf_site_bf50 <- wgs84_sf_site %>% 
  st_transform(crs = wkt_jgd_albers) %>% 
  st_buffer(dist = 50)

# climate -----------------------------------------------------------------

wgs84_sf_mask <- st_read("data_raw/gis/albers_hkd_shape.gpkg") %>% 
  st_set_crs(st_crs(albers_sf_wsd)) %>% 
  st_transform(4326)

albers_stack_clim <- list.files(here::here("data_raw/gis"),
                                pattern = "CHELSA",
                                full.names = TRUE) %>% 
  terra::rast() %>% 
  terra::crop(raster::extent(wgs84_sf_mask)) %>% 
  terra::project(y = wkt_jgd_albers,
                 method = "bilinear")

names(albers_stack_clim) <- c("temp", "ppt")

# buffer average
df_clim <- exact_extract(albers_stack_clim,
                         albers_sf_site_bf50,
                         fun = "mean",
                         append_cols = TRUE) %>% 
  rename(ppt = mean.ppt,
         temp = mean.temp) %>% 
  mutate(temp = 0.1 * temp)


# land use ----------------------------------------------------------------

albers_rs_lu <- terra::rast("data_raw/gis/epsg4326_lu_hkd.tif") %>% 
  terra::project(y = st_crs(albers_sf_wsd)$wkt,
                 method = 'near')

## forest: 111-126
albers_rs_forest <- terra::classify(albers_rs_lu,
                                    rcl = rbind(c(110.9, 126.1, 1),
                                                c(2, 500, 0)))

## urban: 50
albers_rs_urban <- terra::classify(albers_rs_lu,
                                   rcl = rbind(c(49.9, 50.1, 1),
                                               c(2, 500, 0)))

## agri: 40
albers_rs_agri <- terra::classify(albers_rs_lu,
                                  rcl = rbind(c(39.9, 40.1, 1),
                                              c(2, 500, 0)))

## multi-layer stake
albers_rs_fua <- terra::rast(list(albers_rs_forest,
                                  albers_rs_urban,
                                  albers_rs_agri))

names(albers_rs_fua) <- c("forest", "urban", "agri")

df_lu <- exact_extract(albers_rs_fua,
                       albers_sf_wsd,
                       "mean",
                       append_cols = TRUE) %>% 
  rename(frac_forest = mean.forest,
         frac_urban = mean.urban,
         frac_agri = mean.agri)


# elevation ---------------------------------------------------------------

## read dem data
dem_path <- here::here() %>% 
  str_remove("/public-proj_fishery-stability") %>% 
  paste0("/priv-proj_hokkaido-gis/data_org_dem")

albers_dem <- list.files(dem_path, full.names = T) %>% 
  lapply(terra::rast) %>% 
  terra::sprc() %>% 
  terra::merge() %>% 
  terra::project(y = wkt_jgd_albers,
                 method = "bilinear")

df_elev <- exact_extract(albers_dem,
              albers_sf_wsd_outlet,
              c("mean", "stdev"),
              append_cols = TRUE) %>% 
  as_tibble()

# merge data --------------------------------------------------------------

albers_sf_wsd <- albers_sf_wsd %>% 
  left_join(df_lu, by = c("river", "site")) %>% 
  left_join(df_clim, by = c("river", "site")) %>% 
  left_join(df_elev, by = "river") %>% 
  mutate(area = st_area(.),
         area = units::set_units(area, km^2))

st_write(albers_sf_wsd,
         dsn = "data_raw/gis/albers_wsd_env.gpkg",
         append = FALSE)

df_env <- albers_sf_wsd %>% 
  as_tibble() %>% 
  dplyr::select(-geometry)

saveRDS(df_env,
        file = "data_fmt/data_env_fmt.rds")
