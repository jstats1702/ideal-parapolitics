### BAYESIAN MODEL: IDEAL POINT ESTIMATION ###
# Adapted from Carolina Luque's original code for the 2010–2014 Colombian Senate.

# ---------------------------
# Environment Setup
# ---------------------------
rm(list = ls())

# Load required libraries
suppressMessages({
     library(readxl)
     library(readr)
     library(dplyr)
     library(magrittr)
     library(tidyverse)
     library(rstan)
})

# Configure Stan for efficient sampling
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ---------------------------
# Load and Inspect Data
# ---------------------------
s2006_votes <- read_excel("s2006_votes.xlsx")

# Number of legislators (N) and voting items (M)
N <- max(s2006_votes$id_legis)
M <- max(s2006_votes$id_list)

# Path for storing outputs
path <- "Congreso2006-2010"
file.names <- dir(path, pattern = ".stan", full.names = TRUE)

# ---------------------------
# Compile Stan Model
# ---------------------------
mod_ideal_point <- stan_model("ideal_point_0.stan")
print(mod_ideal_point)

# ---------------------------
# Set Anchor Constraints
# ---------------------------
# Carlos Cárdenas (PU) is anchored at +1
# Guillermo Jaramillo (PDA) is anchored at -1
# Other legislators initialized based on coalition membership

xi <- s2006_votes %>%
     select(legislador, id_legis, grupo) %>%
     distinct() %>%
     arrange(id_legis) %>%
     mutate(
          xi = case_when(
               legislador == "Carlos Cardenas Ortiz:PU" ~ 1,
               legislador == "Guillermo Alfonso Jaramillo Martinez:PDA" ~ -1,
               TRUE ~ NA_real_
          ),
          init = if_else(grupo == "coalicion", 1, -1)
     )

# ---------------------------
# Prepare Data for Stan
# ---------------------------
legislators_data <- within(list(), {
     # Votes and indexing
     y <- as.integer(s2006_votes$voto)
     y_idx_leg <- as.integer(s2006_votes$id_legis)
     y_idx_vote <- as.integer(s2006_votes$id_list)
     Y_obs <- length(y)
     N <- max(y_idx_leg)
     M <- max(y_idx_vote)
     
     # Prior hyperparameters
     mu_loc <- 0
     mu_scale <- 5
     alpha_loc <- 0
     alpha_scale <- 5
     beta_loc <- 0
     beta_scale <- 1
     
     # Anchoring information
     N_xi_obs <- sum(!is.na(xi$xi))
     idx_xi_obs <- which(!is.na(xi$xi))
     xi_obs <- xi$xi[!is.na(xi$xi)]
     N_xi_param <- sum(is.na(xi$xi))
     idx_xi_param <- which(is.na(xi$xi))
})

# ---------------------------
# Initial Values
# ---------------------------
legislators_init <- list(
     list(xi_param = xi$init[is.na(xi$xi)])
)

# ---------------------------
# Run MCMC Sampling
# ---------------------------
# 80,000 iterations, 16,000 warmup, thinning every 5 steps
# Only 1 chain is used, and progress is printed every 8,000 iterations

ini <- Sys.time()

stan.fit <- sampling(
     object = mod_ideal_point,
     data = legislators_data,
     chains = 1,
     iter = 80000,
     warmup = 16000,
     thin = 5,
     init = legislators_init,
     refresh = 8000,
     seed = 12345
)

end <- Sys.time()

# Display total run time
cat("Elapsed time:", end - ini, "\n")

# ---------------------------
# Save Results
# ---------------------------
save(stan.fit, file = "fit0_L1.Rda")