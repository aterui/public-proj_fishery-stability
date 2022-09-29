
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------
## current - stagnant = 1; slow = 2; fast = 3; various = 4
## vertical position - water column = 1; bottom = 2
## trophic guild - herbivore = 1; detritivore = 2; omnivore = 3; invertivore = 4; invertivore-piscivore = 5
## spawning substrate - vegetation = 1; mineral substrate = 2; various = 3

df_trait <- read_csv(here::here("data_raw/data_trait_full.csv"))
skimr::skim(df_trait)


# summarize trait ---------------------------------------------------------

## -  Pseudorasbora spp; data from Pseudorasbora parva were used

## long format
df_trait_l <- df_trait %>% 
  separate(latin,
           into = c("genus", "species"),
           sep = "_",
           remove = FALSE) %>% 
  group_by(taxon, trait) %>% 
  summarize(value = f_num(value),
            source = source,
            comment = comment) %>% 
  ungroup() %>% 
  mutate(value = ifelse(trait == "current" & !(value %in% 1:3), 4, value),
         comment = ifelse(str_detect(taxon, "spp|Rhynchocypris"),
                          "genus mean",
                          comment)) %>% 
  filter(!(trait %in% c("longevity", "maximum_fecundity", "standard_length")))

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
                .fns = as.factor))

saveRDS(df_trait_w,
        here::here("data_fmt/data_trait.rds"))
