
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------
## current - stagnant = 1; slow = 2; fast = 3; various = 4
## vertical position - water column = 1; bottom = 2
## trophic guild - herbivore = 1; detritivore = 2; omnivore = 3; invertivore = 4; invertivore-piscivore = 5
## spawning substrate - vegetation = 1; mineral substrate = 2; various = 3

### fish shape, species-level average
df_fs_sp <- read_csv(here::here("data_fmt/data_fish_shapes.csv")) %>% 
  rename_with(.fn = str_to_lower) %>% 
  group_by(tree_name) %>% 
  summarize(across(.fns = function(x) mean(x, na.rm = T),
                   .cols = where(is.numeric))) %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "trait",
               values_to = "value")

### fish shape, genus-level average
df_fs_genus <- read_csv(here::here("data_fmt/data_fish_shapes.csv")) %>% 
  rename_with(.fn = str_to_lower) %>% 
  separate(tree_name,
           into = c("genus", "species"),
           sep = "_",
           remove = FALSE) %>% 
  group_by(genus) %>% 
  summarize(across(.fns = function(x) mean(x, na.rm = T),
                   .cols = where(is.numeric))) %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "trait",
               values_to = "genus_value")

df_trait <- read_csv(here::here("data_raw/data_trait_full.csv"))

skimr::skim(df_trait)


# summarize trait ---------------------------------------------------------

## -  substitute FishShape values with genus mean only if no species-level value
##    available within the genus
## -  Pseudorasbora spp; data from Pseudorasbora parva were used

## long format
df_trait_l <- df_trait %>% 
  separate(latin,
           into = c("genus", "species"),
           sep = "_",
           remove = FALSE) %>% 
  left_join(df_fs_genus,
            by = c("genus", "trait")) %>%
  group_by(genus) %>% 
  mutate(fishshape = ifelse(trait %in% unique(df_fs_sp$trait), 1, 0),
         value = ifelse(str_detect(taxon, "spp|Rhynchocypris") & fishshape == 1,
                        genus_value,
                        value)) %>% 
  ungroup() %>% 
  group_by(taxon, trait) %>% 
  summarize(value = f_num(value),
            source = source,
            comment = comment) %>% 
  ungroup() %>% 
  mutate(value = ifelse(trait == "current" & !(value %in% 1:3), 4, value),
         comment = ifelse(str_detect(taxon, "spp|Rhynchocypris"),
                          "genus mean",
                          comment)) %>% 
  filter(!(trait %in% c("longevity", "maximum_fecundity")))

## wide format
df_trait_w <- df_trait_l %>% 
  pivot_wider(id_cols = taxon,
              names_from = trait,
              values_from = value,
              values_fn = f_num) %>% 
  mutate(current = case_when(current == 1 ~ "stagnant",
                             current == 2 ~ "slow",
                             current == 3 ~ "fast",
                             current == 4 ~ "various"),
         trophic_guild = case_when(trophic_guild == 1 ~ "algaevore",
                                   trophic_guild == 2 ~ "detritivore",
                                   trophic_guild == 3 ~ "omnivore",
                                   trophic_guild == 4 ~ "invertivore",
                                   trophic_guild == 5 ~ "invertivore-piscivore"),
         vertical_position = case_when(vertical_position == 1 ~ "water_column",
                                       vertical_position == 2 ~ "bottom"),
         spawning_substrate = case_when(spawning_substrate == 1 ~ "vegetation",
                                        spawning_substrate == 2 ~ "mineral_substrate",
                                        spawning_substrate == 3 ~ "various")) %>% 
  relocate(taxon,
           current,
           trophic_guild,
           vertical_position,
           spawning_substrate) %>% 
  mutate(across(.cols = where(is.character),
                .fns = as.factor),
         across(.cols = unique(df_fs_sp$trait),
                .fns = function(x) x / standard_length)) %>% 
  dplyr::select(-standard_length, -total_weight)

saveRDS(df_trait_w,
        here::here("data_fmt/data_trait.rds"))


# functional distance -----------------------------------------------------

## ATTENTION: Lethenteron spp lack FishShape data

## reformat data for FD
df_trait_value <- df_trait_w %>% 
  dplyr::select(-taxon) %>% 
  data.frame()

rownames(df_trait_value) <- df_trait_w$taxon

df_trait_type <- sapply(df_trait_value,
                        function(x) ifelse(class(x)=="factor", "N", "Q")) %>% 
  tibble(trait_name = names(.),
         trait_type = .)

## species traits summary:
mFD::sp.tr.summary(tr_cat = df_trait_type,
                   sp_tr = df_trait_value,
                   stop_if_NA = F)

## functional distance
m_fd <- mFD::funct.dist(sp_tr = df_trait_value,
                        tr_cat = df_trait_type,
                        metric = "gower",
                        stop_if_NA = F) %>% 
  data.matrix()

saveRDS(m_fd,
        here::here("data_fmt/data_fd.rds"))
