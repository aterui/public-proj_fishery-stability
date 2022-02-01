
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               sf,
               raster,
               ncdf4,
               exactextractr)

source(here::here("code/gis_crs_fmt.R"))

# data --------------------------------------------------------------------

## for crop extent
wgs84_sf_hkd_bf50 <- list.files(path = here::here("data_gis"),
                                full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "albers_hkd_shape")) %>% 
  pull() %>% 
  st_read(crs = wkt_jgd_albers) %>% 
  st_buffer(dist = 50000) %>% 
  st_transform(crs = 4326)

## MODIS chr a data - 2002 to 2019
wgs84_stack_chr_a <- list.files(path = here::here("data_gis/ocean_data"),
                                full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "chlor_a")) %>% 
  pull() %>% 
  raster::stack() %>% 
  crop(extent(wgs84_sf_hkd_bf50))

albers_stack_chr_a <- projectRaster(from = wgs84_stack_chr_a,
                                    crs = wkt_jgd_albers,
                                    res = 4000,
                                    method = "bilinear")

names(albers_stack_chr_a) <- paste0("chr_a_", 2002:2019)

## MODIS sst data - 2002 to 2019
wgs84_stack_sst <- list.files(path = here::here("data_gis/ocean_data"),
                              full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "sst")) %>% 
  pull() %>% 
  raster::stack() %>% 
  crop(extent(wgs84_sf_hkd_bf50))

albers_stack_sst <- projectRaster(from = wgs84_stack_sst,
                                  crs = wkt_jgd_albers,
                                  res = 4000,
                                  method = "bilinear")

names(albers_stack_sst) <- paste0("sst_", 2002:2019)

# extraction --------------------------------------------------------------

## mask layer: river outlet
albers_sf_outlet_bf30 <- list.files(path = here::here("data_gis"),
                                    full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "outlet")) %>% 
  pull() %>% 
  st_read() %>% 
  st_transform(crs = wkt_jgd_albers) %>% 
  mutate(river = str_to_lower(join_ws_na)) %>% 
  dplyr::select(river) %>% 
  st_buffer(dist = 30000)

## extractr - spatial and temporal average
df_chr_a <- exact_extract(albers_stack_chr_a,
                          albers_sf_outlet_bf30,
                          fun = "mean",
                          append_cols = TRUE) %>% 
  as_tibble() %>% 
  pivot_longer(cols = mean.chr_a_2002:mean.chr_a_2019,
               values_to = "value",
               names_to = "group") %>% 
  group_by(river) %>% 
  summarize(chr_a = mean(value))

df_sst <- exact_extract(albers_stack_sst,
                        albers_sf_outlet_bf30,
                        fun = "mean",
                        append_cols = TRUE) %>% 
  as_tibble() %>% 
  pivot_longer(cols = mean.sst_2002:mean.sst_2019,
               values_to = "value",
               names_to = "group") %>% 
  group_by(river) %>% 
  summarize(sst = mean(value))

# export ------------------------------------------------------------------

df_chr_a %>% 
  left_join(df_sst, by = "river") %>% 
  write_csv("data_fmt/data_ocean_fmt.csv")

