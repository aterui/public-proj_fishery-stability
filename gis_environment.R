
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = T))
  pacman::p_load(raster, rgdal, tidyverse, sf, stars, exactextractr)  


# extract values ----------------------------------------------------------
  
  ## load watershed polygons
  utm_wsd_polygon <- st_read(dsn = "data_gis/utm_watershed_hkd_prtwsd.gpkg") %>%
    st_make_valid() %>% 
    dplyr::select(Watershed) %>% 
    mutate(id = seq_len(nrow(.))) %>% # id: watershed polygon id (unit of analysis)
    mutate(area = st_area(.)) %>% 
    mutate(area = units::set_units(area, km^2))
  
  ## worldclim data
  wgs84_clim <- getData("worldclim", var = "bio", res = 0.5, lon = 143, lat = 43,
                        path = here::here("data_org_wc"))
  
  wgs84_clim <- wgs84_clim[[c(1,12)]]
  names(wgs84_clim) <- c("temp", "ppt")
  
  utm_clim <- projectRaster(from = wgs84_clim,
                            crs = 3100,
                            method = 'bilinear',
                            res = 1000)
  
  mu_clim <- exact_extract(utm_clim, utm_wsd_polygon) %>% 
    bind_rows(.id = "id") %>% 
    drop_na(temp | ppt) %>% 
    mutate(id = as.numeric(id)) %>% 
    dplyr::group_by(id) %>% 
    summarise(mean_temp = sum(temp * 0.1 * coverage_fraction) / sum(coverage_fraction),
              mean_ppt = sum(ppt * coverage_fraction) / sum(coverage_fraction))
  
  ## land use data
  wgs84_lu_hkd <- raster("data_gis/epsg4326_lu_hkd.tif")
  utm_lu_hkd <- projectRaster(from = wgs84_lu_hkd,
                              crs = 3100,
                              method = 'bilinear',
                              res = 1000)
  
  utm_forest <- calc(utm_lu_hkd, fun = function(x) ifelse(dplyr::between(x, 111, 126), 1, 0))
  utm_urban <- calc(utm_lu_hkd, fun = function(x) ifelse(x == 50, 1, 0))
  utm_agri <- calc(utm_lu_hkd, fun = function(x) ifelse(x == 40, 1, 0))
  utm_fua <- stack(utm_forest, utm_urban, utm_agri)
  names(utm_fua) <- c("forest", "urban", "agri")
  
  p_lu <- exact_extract(utm_fua, utm_wsd_polygon) %>% 
    bind_rows(.id = "id") %>%
    drop_na(forest | urban | agri) %>% 
    mutate(id = as.numeric(id)) %>% 
    dplyr::group_by(id) %>% 
    summarise(frac_forest = sum(forest * coverage_fraction) / sum(coverage_fraction),
              frac_urban = sum(urban * coverage_fraction) / sum(coverage_fraction),
              frac_agri = sum(agri * coverage_fraction) / sum(coverage_fraction))
  

# merge data --------------------------------------------------------------

  df_utm_wsd_polygon <- utm_wsd_polygon %>% 
    left_join(p_lu, by = "id") %>% 
    left_join(mu_clim, by = "id") %>% 
    as_tibble() %>% 
    select(-geom) %>% 
    rename(river = Watershed)
  
  write_csv(df_utm_wsd_polygon,
            file = "data_fmt/data_env_fmt.csv")