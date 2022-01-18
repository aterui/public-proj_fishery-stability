
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
albers_sf_wsd <- st_read(dsn = "data_gis/epsg4326_watershed.gpkg") %>%
  dplyr::select(-epsg4326_watershed) %>% 
  st_transform(wkt_jgd_albers) %>% 
  mutate(id = seq_len(nrow(.)),
         area = st_area(.)) %>% 
  mutate(area = units::set_units(area, km^2))

## sampling sites
wgs84_sf_site <- st_read(dsn = "data_gis/epsg4326_point_snap_prtwsd_edit.gpkg")


# climate -----------------------------------------------------------------

wgs84_sf_mask <- st_read("data_gis/albers_hkd_shape.gpkg") %>% 
  st_set_crs(st_crs(albers_sf_wsd)) %>% 
  st_transform(4326)

wgs84_rs_temp <- raster("data_gis/CHELSA_bio10_01.tif") %>% 
  crop(extent(wgs84_sf_mask)) %>% 
  mask(mask = wgs84_sf_mask)

wgs84_rs_ppt <- raster("data_gis/CHELSA_bio10_12.tif") %>% 
  crop(extent(wgs84_sf_mask)) %>% 
  mask(mask = wgs84_sf_mask)

wgs84_rs_clim <- raster::stack(wgs84_rs_temp,
                               wgs84_rs_ppt)
names(wgs84_rs_clim) <- c("temp", "ppt")

# for point estimate
df_clim <- raster::extract(wgs84_rs_clim, wgs84_sf_site) %>% 
  as_tibble() %>% 
  mutate(id = seq_len(nrow(.)),
         temp = temp * 0.1) %>% 
  relocate(id)

## for polygon average
#albers_clim <- projectRaster(from = wgs84_rs_clim,
#                             crs = st_crs(albers_sf_wsd)$wkt,
#                             method = 'bilinear',
#                             res = 1000)

#df_clim <- exact_extract(albers_clim,
#                         albers_sf_wsd) %>% 
#  bind_rows(.id = "id") %>% 
#  drop_na(ppt) %>% 
#  mutate(id = as.numeric(id)) %>% 
#  dplyr::group_by(id) %>% 
#  dplyr::summarise(temp = sum(temp * 0.1 * coverage_fraction) / sum(coverage_fraction),
#                   ppt = sum(ppt * coverage_fraction) / sum(coverage_fraction))


# land use ----------------------------------------------------------------

wgs84_rs_lu <- raster("data_gis/epsg4326_lu_hkd.tif")
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
                       "mean") %>% 
  rename(frac_forest = mean.forest,
         frac_urban = mean.urban,
         frac_agri = mean.agri) %>% 
  mutate(id = seq_len(nrow(.)))


# merge data --------------------------------------------------------------

albers_sf_wsd <- albers_sf_wsd %>% 
  left_join(df_lu, by = "id") %>% 
  left_join(df_clim, by = "id") %>% 
  relocate(id)

st_write(albers_sf_wsd,
         dsn = "data_gis/albers_watershed_env.gpkg",
         append = FALSE)

df_albers_sf_wsd <- albers_sf_wsd %>% 
  as_tibble() %>% 
  dplyr::select(-geom)

write_csv(df_albers_sf_wsd,
          file = "data_fmt/data_env_fmt.csv")
