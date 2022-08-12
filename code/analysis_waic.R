
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))


# data --------------------------------------------------------------------

list_ic <- list.files(path = here::here("result"),
                      full.names = TRUE,
                      pattern = "post") %>% 
  lapply(FUN = function(x) {
    post <- readRDS(x)
    
    cname <- colnames(post$mcmc[[1]])
    cid <- cname[str_detect(cname, "loglik")]
    m_loglik <- sapply(cid, function(x) unlist(post$mcmc[, x]))
    r_eff <- relative_eff(m_loglik,
                          chain_id = rep(1:length(post$mcmc),
                                         each = post$sample))
    
    x <- waic(m_loglik, r_eff = r_eff)
    
    return(x)
  })

# model1 = order 3, model2 = order 4
# almost no difference

list_compare <- lapply(1:3, function(i) loo_compare(list_ic[[i]],
                                                    list_ic[[i + 3]]))

names(list_compare) <- list.files(here::here("result"),
                                  pattern = "post_geom3") %>% 
  str_extract(., pattern = "(all|masu|other)")

saveRDS(list_compare,
        file = here::here("result/list_waic.rds"))