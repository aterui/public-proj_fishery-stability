
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_crs.R"))

# data --------------------------------------------------------------------

## for crop extent
wgs84_sf_hkd_bf50 <- list.files(path = here::here("data_raw/gis"),
                                full.names = T,
                                pattern = "albers_hkd_shape") %>% 
  st_read(crs = wkt_jgd_albers) %>% 
  st_buffer(dist = 50000) %>% 
  st_transform(crs = 4326)

## MODIS chr a data - 2002 to 2019
wgs84_stack_chr_a <- list.files(path = here::here("data_raw/ocean"),
                                full.names = T,
                                pattern = "chlor_a") %>% 
  terra::rast() %>% 
  terra::crop(raster::extent(wgs84_sf_hkd_bf50))

albers_stack_chr_a <- terra::project(x = wgs84_stack_chr_a,
                                     y = wkt_jgd_albers,
                                     method = "bilinear")

names(albers_stack_chr_a) <- paste0("chr_a_", 2002:2019)


# extraction --------------------------------------------------------------

## mask layer: river outlet
albers_sf_outlet_bf30 <- list.files(path = here::here("data_raw/gis"),
                                    full.names = T,
                                    pattern = "outlet") %>% 
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
  pivot_longer(cols = starts_with("mean"),
               values_to = "value",
               names_to = "group") %>% 
  group_by(river) %>% 
  summarize(chr_a = round(mean(value), 2),
            unit = "mg_m^-3")
 

# export ------------------------------------------------------------------

saveRDS(object = df_chr_a,
        file = "data_fmt/data_ocean_fmt.rds")

