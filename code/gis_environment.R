
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))

pacman::p_load(raster,
               rgdal,
               tidyverse,
               sf,
               stars,
               exactextractr)  

source("code/gis_crs_fmt.R")


# read polygons and points ------------------------------------------------

## watershed polygons
albers_sf_wsd <- st_read(dsn = "data_raw/gis/epsg4326_upstr_watershed.gpkg") %>%
  dplyr::select(river,
                site) %>% 
  st_transform(wkt_jgd_albers)

## sampling sites
wgs84_sf_site <- st_read(dsn = "data_raw/gis/epsg4326_point_snap_prtwsd_edit.gpkg") %>% 
  dplyr::select(-ID,
                -source)

## 50m buffer around sampling sites
albers_sf_site_bf50 <- wgs84_sf_site %>% 
  st_transform(crs = wkt_jgd_albers) %>% 
  st_buffer(dist = 50)

# climate -----------------------------------------------------------------

wgs84_sf_mask <- st_read("data_raw/gis/albers_hkd_shape.gpkg") %>% 
  st_set_crs(st_crs(albers_sf_wsd)) %>% 
  st_transform(4326)

wgs84_rs_temp <- raster("data_raw/gis/CHELSA_bio10_01.tif") %>% 
  crop(extent(wgs84_sf_mask)) %>% 
  mask(mask = wgs84_sf_mask)

wgs84_rs_ppt <- raster("data_raw/gis/CHELSA_bio10_12.tif") %>% 
  crop(extent(wgs84_sf_mask)) %>% 
  mask(mask = wgs84_sf_mask)

albers_stack_clim <- stack(wgs84_rs_temp,
                           wgs84_rs_ppt) %>% 
  projectRaster(crs = wkt_jgd_albers,
                res = 1000,
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

wgs84_rs_lu <- raster("data_raw/gis/epsg4326_lu_hkd.tif")
albers_rs_lu <- projectRaster(from = wgs84_rs_lu,
                              crs = st_crs(albers_sf_wsd)$wkt,
                              method = 'ngb',
                              res = 100)

albers_rs_forest <- calc(albers_rs_lu,
                         fun = function(x) ifelse(dplyr::between(x, 111, 126), 1, 0))
albers_rs_urban <- calc(albers_rs_lu,
                        fun = function(x) ifelse(x == 50, 1, 0))
albers_rs_agri <- calc(albers_rs_lu,
                       fun = function(x) ifelse(x == 40, 1, 0))
albers_rs_fua <- raster::stack(albers_rs_forest,
                               albers_rs_urban,
                               albers_rs_agri)
names(albers_rs_fua) <- c("forest", "urban", "agri")

df_lu <- exact_extract(albers_rs_fua,
                       albers_sf_wsd,
                       "mean",
                       append_cols = TRUE) %>% 
  rename(frac_forest = mean.forest,
         frac_urban = mean.urban,
         frac_agri = mean.agri)


# merge data --------------------------------------------------------------

albers_sf_wsd <- albers_sf_wsd %>% 
  left_join(df_lu, by = c("river", "site")) %>% 
  left_join(df_clim, by = c("river", "site")) %>% 
  mutate(area = st_area(.),
         area = units::set_units(area, km^2))

st_write(albers_sf_wsd,
         dsn = "data_raw/gis/albers_upstr_watershed_env.gpkg",
         append = FALSE)

df_albers_sf_wsd <- albers_sf_wsd %>% 
  as_tibble() %>% 
  dplyr::select(-geom)

write_csv(df_albers_sf_wsd,
          file = "data_fmt/data_env_fmt.csv")
