
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(foreach,
               tidyverse,
               ggridges)


# data --------------------------------------------------------------------

variable <- c("richness", "cv", "mu", "sigma")
group <- c("all", "masu", "other")

## loop for response variables
df_mcmc <- foreach(i = seq_len(length(variable)),
                     .combine = bind_rows) %do% {
  
  file_name <- list.files(path = here::here("result"), full.names = TRUE) %>%
    as_tibble() %>%
    filter(str_detect(value, "mcmc")) %>%
    filter(str_detect(value, variable[i])) %>%
    pull()
  
  ## loop for fish group
  df_mcmc_beta <- foreach(j = seq_len(length(file_name)),
          .combine = bind_rows) %do% {
    
    load(file = file_name[j])
    mcmc_beta <- MCMCvis::MCMCchains(mcmc_sample) %>%
      as_tibble() %>%
      select(b2 = `b[2]`) %>%
      mutate(response = variable[i],
             group = group[j])
    
  }
  
  return(df_mcmc_beta)  
}

df_mcmc <- df_mcmc %>%
  filter(!(group != "all" & response == "cv")) %>%
  mutate(group = case_when(group == "all" ~ "Whole",
                           group == "masu" ~ "Enhanced",
                           group == "other" ~ "Unenhanced"),
         response = case_when(response == "richness" ~ "Species richness",
                              response == "cv" ~ "CV",
                              response == "mu" ~ "Mean",
                              response == "sigma" ~ "SD")) %>%
  mutate(group = factor(group,
                        levels = c("Whole",
                                   "Enhanced",
                                   "Unenhanced")),
         response = factor(response,
                           levels = c("SD",
                                      "Mean",
                                      "Species richness",
                                      "CV")))

# plot --------------------------------------------------------------------

g_coef <- df_mcmc %>% 
  ggplot(aes(x = b2,
             y = response,
             color = group,
             fill = group)) +
  geom_vline(xintercept = 0,
             color = grey(0.5),
             linetype = "dashed") +
  geom_density_ridges(alpha = 0.5) +
  labs(y = "Response",
       x = "Release effect",
       fill = "Species group",
       color = "Species group") +
  theme_ridges()

ggsave(g_coef,
       filename = here::here("output/figure_coef.pdf"),
       width = 8,
       height = 6)
