library(tidyverse)


# safe conversion from matrix to vector -----------------------------------

m2v <- function(x) {
  
  row <- rownames(x)
  col <- colnames(x)
  
  if(is.null(row)) row <- as.character(1:nrow(x))
  if(is.null(col)) col <- as.character(1:ncol(x))
  
  m_name <- outer(row, col,
                  paste, sep="_by_" )
  
  df_x <- dplyr::tibble(value = c(x),
                        id = c(m_name)) %>% 
    tidyr::separate(col = id,
                    into = c("row", "col"),
                    sep = "_by_",
                    convert = TRUE)
  
  return(df_x)
}


# remove brackets ---------------------------------------------------------

fn_brrm <- function(x) {
  y <- lapply(str_extract_all(x, pattern = "\\[.{1,}\\]"),
              FUN = function(z) ifelse(identical(z, character(0)),
                                       NA,
                                       z))
  str_remove_all(y, pattern = "\\[|\\]")
}


# convert Arc pointer to D8 pointer ---------------------------------------

arc2d8 <- function(x) {
  z <- 0:7
  fdir_d8 <- 2^z
  fdir_arc <- fdir_d8[c(8, 1:7)] * 3
  
  # unique pointer values for ArcGIS flow direction
  x <- x * 3
  
  for(i in seq_len(length(fdir_d8))) {
    x[x == fdir_arc[i]] <- fdir_d8[i]
  }
  
  # remove unassigned cells
  us <- c(247, 255) * 3
  x[x %in% us] <- NA
  
  return(x)  
}


# get mean or NA ----------------------------------------------------------

f_num <- function(x) {
  y <- unique(na.omit(x))
  
  if (length(y) == 0) {
    y <- NA
  }
  
  if (length(y) > 1) {
    y <- mean(x, na.rm = T)
  }
  
  return(round(y, 1))
}
