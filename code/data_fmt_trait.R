
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

f <- function(x) {
  y <- round(unique(na.omit(x)))
  
  if (length(y) == 0) {
    y <- NA
  } 
  
  return(list(y))
}

# data --------------------------------------------------------------------
## current - stagnant = 1; slow = 2; fast = 3
## vertical position - water column = 1; bottom = 2
## trophic guild - herbivore = 1; detritivore = 2; omnivore = 3; carnivore = 4
## spawning substrate - vegetation = 1; mineral substrate = 2; various = 3

df_trait <- read_csv(here::here("data_raw/data_trait_full.csv"))

skimr::skim(df_trait)


# summarize trait ---------------------------------------------------------

df_trait %>% 
  group_by(taxon) %>% 
  pivot_wider(id_cols = taxon,
              names_from = trait,
              values_from = value,
              values_fn = f) %>% 
  view()
  
  
df_trait %>% 
  group_by(taxon, trait) %>% 
  summarize(value = f(value)) %>% 
  view()
