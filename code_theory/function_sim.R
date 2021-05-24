
dynsim <- function(n_timestep = 1000,
                   n_warmup = 100,
                   n_burnin = 100,
                   n_species = 10,
                   k = 100,
                   r_type = "constant",
                   r = 1.5,
                   r_min = 1.0,
                   r_max = 3.5,
                   sd_env = 0.1,
                   stock = 0,
                   u = 0.6,
                   int_type = "constant",
                   alpha = 0.5,
                   alpha_min = 0,
                   alpha_max = 0.5,
                   model = "ricker",
                   seed = 5
                   ) {

# function ----------------------------------------------------------------
  
  # ricker function
  if (model == "ricker") {
    fun_dyn <- function(r, n, k, m_int) {
      n * exp(r * (1 - (m_int %*% n / k)))
    }   
  }
  
  # beverton-holt function
  if (model == "bh") {
    fun_dyn <- function(r, n, k, m_int) {
      (n * r) / (1 + ((r - 1) / k) * (m_int %*% n))
    }   
  }
  
  
# variables ---------------------------------------------------------------
  
  # basic objects
  n_sim <- n_warmup + n_burnin + n_timestep
  n_discard <- n_warmup + n_burnin
  
  m_dyn <- matrix(NA,
                  nrow = n_timestep * n_species,
                  ncol = 3)
  colnames(m_dyn) <- c("timestep",
                       "species",
                       "density")
  
  st_row <- seq(from = 1,
                to = nrow(m_dyn),
                by = n_species)
  
  v_n <- rpois(n = n_species, seed)
  
  # parameters: species interaction
  if (int_type == "random") {
    m_int <- matrix(runif(n_species * n_species,
                          min = alpha_min,
                          max = alpha_max),
                    nrow = n_species,
                    ncol = n_species)
  } else {
    if (int_type == "constant") {
      m_int <- matrix(alpha,
                      nrow = n_species,
                      ncol = n_species)
    } else {
      message("int_type must be either random or constant")
    }
  }
  
  diag(m_int) <- 1
  
  # parameters: population dynamics
  if (r_type == "random") {
    v_r <- runif(n= n_species,
                 min = r_min,
                 max = r_max)
  } else {
    if (r_type == "constant") {
      v_r <- rep(r, n_species)
    } else {
      message("r_type must be either random or constant")
    }
  }
  
  u_stock <- rbinom(n = n_sim,
                    size = stock,
                    prob = u)
  
  m_eps <- matrix(rnorm(n = n_sim * n_species,
                        mean = 0,
                        sd = sd_env),
                  nrow = n_sim,
                  ncol = n_species)
  
# dynamics ----------------------------------------------------------------
  
  for (i in seq_len(n_sim)) {
    
    if (i > n_warmup) {
      v_n[1] <- v_n[1] + u_stock[i]
    }
    
    v_n_hat <- fun_dyn(r = v_r,
                       n = v_n,
                       k = k,
                       m_int = m_int)
    v_n <- v_n_hat * exp(m_eps[i, ])
    
    if (i > n_discard) {
      row_id <- seq(from = st_row[i - n_discard],
                    to = st_row[i - n_discard] + n_species - 1,
                    by = 1)
      
      m_dyn[row_id, ] <- cbind(rep(i, n_species) - n_discard, # timestep
                               seq_len(n_species), # species ID
                               v_n) # density
    }
  }
  
  df_dyn <- dplyr::as_tibble(m_dyn)
  
  df_community <- df_dyn %>% 
    dplyr::group_by(timestep) %>% 
    dplyr::summarize(summed_density = sum(density)) %>% 
    dplyr::summarize(mean_density = mean(summed_density),
                     sd_density = sd(summed_density))
  
  df_species <- df_dyn %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarize(mean_density = mean(density),
                     sd_density = sd(density)) %>% 
    dplyr::mutate(species = seq_len(n_species),
                  k = k,
                  r = v_r,
                  alpha1 = m_int[,1]) %>% 
    dplyr::relocate(species)
  
  
# return ------------------------------------------------------------------
  
  return(
    list(df_dyn = df_dyn,
         df_community = df_community,
         df_species = df_species,
         interaction_matrix = m_int)
  )
  
} 
  