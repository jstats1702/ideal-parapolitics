# ------------------------------------------
# Clear workspace and set working directory
rm(list = ls())

setwd("C:/Users/User/Dropbox/PAPERS/projects/JV/TrabajoDeGradoSenado20062010")

# ------------------------------------------
# Load required libraries and data
library(readxl)

# Read voting matrix
dat <- read_excel("matriz2006.xlsx")

# Convert to numeric matrix (senators x motions)
Y <- as.matrix(dat[, 2:ncol(dat)])
dim(Y)

# ------------------------------------------
# Load SLFM1D package and perform Bayesian estimation

# If needed:
# install.packages("devtools")
# devtools::install_github("Xingchen-Yu/SLFM1D")

library(SLFM1D)

# Set seed for reproducibility
set.seed(2021)

# Run SLFM sampler (uncomment if not precomputed)
# samples <- SLFM(Y, n_pos = 20000, burnin = 10000, core = 2)
# save(samples, file = "samples_circlar_model.RData")

# Load precomputed posterior samples
load("samples_circlar_model.RData")

# ------------------------------------------
# Extract and visualize results

# Transpose to obtain samples: B x I (samples x legislators)
beta_samples <- t(samples$beta_i)
dim(beta_samples)

# Plot trace of marginal log-likelihood
plot(samples$likeli,
     type = "l",
     col = "steelblue",
     lwd = 2,
     xlab = "Iteration",
     ylab = "Log-Likelihood",
     main = "SLFM1D Log-Likelihood Trace")

# ------------------------------------------
# Align samples using identification strategy

identified_samples <- identify_beta_samples(
     beta_samples,
     ref_index = 17,         # e.g., Carlos Cardenas (PU)
     target_angle = pi / 2,
     second_index = 49,      # e.g., Guillermo Jaramillo (PDA)
     enforce_positive = FALSE
)

# ------------------------------------------
# Project aligned samples to tangent space

tangent_samples <- project_to_tangent_space(
     identified_samples,
     ref_angle = pi / 2
)
