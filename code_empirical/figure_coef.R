
# setup -------------------------------------------------------------------

#rm(list = ls())
pacman::p_load(foreach,
               tidyverse,
               see)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

variable <- c("cv", "mu", "sigma")
group <- c("all", "masu", "other")

## loop for response variables
df_mcmc <- foreach(i = seq_len(length(variable)),
                     .combine = bind_rows) %do% {
  
  file_name <- list.files(path = "result") %>%
    as_tibble() %>%
    filter(str_detect(value, variable[i])) %>%
    pull() %>%
    paste0("result/", .)
  
  ## loop for fish group
  df_mcmc_beta <- foreach(j = seq_len(length(group)),
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
  mutate(group = case_when(group == "all" ~ "All",
                           group == "masu" ~ "Enhanced",
                           group == "other" ~ "Unenhanced"),
         response = case_when(response == "cv" ~ "CV",
                              response == "mu" ~ "Mean",
                              response == "sigma" ~ "SD")) %>%
  mutate(group = factor(group, levels = c("All", "Enhanced", "Unenhanced")))

# plot --------------------------------------------------------------------

source("figure_set_theme.R")
theme_set(plt_theme)

g_coef <- df_mcmc %>% 
  ggplot(aes(x = response,
             y = b2,
             fill = group)) +
  geom_hline(aes(yintercept = 0),
             size = 0.25,
             alpha = 0.25,
             linetype = "dotted") +
  geom_violin(alpha = 0.5,
              draw_quantiles = 0.5) +
  labs(x = "Response",
       y = "Effect of enhancement",
       fill = "Species group")

ggsave(g_coef,
       filename = here::here("document_output/figure_coef.pdf"),
       width = 6,
       height = 3)
