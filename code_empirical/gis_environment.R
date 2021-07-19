
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))

pacman::p_load(raster,
               rgdal,
               tidyverse,
               sf,
               stars,
               exactextractr)  

setwd(here::here("code_empirical"))

# extract values ----------------------------------------------------------

## load watershed polygons
albers_wsd_polygon <- st_read(dsn = "data_gis/albers_wsd_prtwsd.gpkg") %>%
  st_make_valid() %>% 
  mutate(id = seq_len(nrow(.))) %>% # id: watershed polygon id (unit of analysis)
  mutate(area = st_area(.)) %>% 
  mutate(area = units::set_units(area, km^2))

## CHELSA data
wgs84_mask <- st_read("data_gis/albers_hkd_shape.gpkg") %>% 
  st_set_crs(st_crs(albers_wsd_polygon)) %>% 
  st_transform(4326)

wgs84_rs_temp <- raster("data_gis/CHELSA_bio10_01.tif") %>% 
  crop(extent(wgs84_mask)) %>% 
  mask(mask = wgs84_mask)

wgs84_rs_prec <- raster("data_gis/CHELSA_bio10_12.tif") %>% 
  crop(extent(wgs84_mask)) %>% 
  mask(mask = wgs84_mask)

wgs84_clim <- raster::stack(wgs84_rs_temp,
                            wgs84_rs_prec)
names(wgs84_clim) <- c("temp", "ppt")

albers_clim <- projectRaster(from = wgs84_clim,
                             crs = st_crs(albers_wsd_polygon)$wkt,
                             method = 'bilinear',
                             res = 1000)

mu_clim <- exact_extract(albers_clim,
                         albers_wsd_polygon) %>% 
  bind_rows(.id = "id") %>% 
  drop_na(temp | ppt) %>% 
  mutate(id = as.numeric(id)) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(mean_temp = sum(temp * 0.1 * coverage_fraction) / sum(coverage_fraction),
                   mean_ppt = sum(ppt * coverage_fraction) / sum(coverage_fraction))

## land use data
wgs84_lu_hkd <- raster("data_gis/epsg4326_lu_hkd.tif")
albers_lu_hkd <- projectRaster(from = wgs84_lu_hkd,
                               crs = st_crs(albers_wsd_polygon)$wkt,
                               method = 'bilinear',
                               res = 1000)

albers_forest <- calc(albers_lu_hkd, fun = function(x) ifelse(dplyr::between(x, 111, 126),
                                                              1,
                                                              0))
albers_urban <- calc(albers_lu_hkd, fun = function(x) ifelse(x == 50,
                                                             1,
                                                             0))
albers_agri <- calc(albers_lu_hkd, fun = function(x) ifelse(x == 40,
                                                            1,
                                                            0))
albers_fua <- raster::stack(albers_forest,
                            albers_urban,
                            albers_agri)
names(albers_fua) <- c("forest", "urban", "agri")

p_lu <- exact_extract(albers_fua,
                      albers_wsd_polygon) %>% 
  bind_rows(.id = "id") %>%
  drop_na(forest | urban | agri) %>% 
  mutate(id = as.numeric(id)) %>% 
  dplyr::group_by(id) %>% 
  summarise(frac_forest = sum(forest * coverage_fraction) / sum(coverage_fraction),
            frac_urban = sum(urban * coverage_fraction) / sum(coverage_fraction),
            frac_agri = sum(agri * coverage_fraction) / sum(coverage_fraction))


# merge data --------------------------------------------------------------

albers_wsd_polygon <- albers_wsd_polygon %>% 
  left_join(p_lu, by = "id") %>% 
  left_join(mu_clim, by = "id")

df_albers_wsd_polygon <- albers_wsd_polygon %>% 
  as_tibble() %>% 
  dplyr::select(-geom,
                -albers_raster_watershed.sdat)

write_csv(df_albers_wsd_polygon,
          file = "data_fmt/data_env_fmt.csv")