# ------------------------------------------------------------------------------

# Read data
listacong <- read_excel("listacong.xlsx")

# Posterior euclidean ideal point samples 
betas <- as.matrix(read.delim2("betas.txt", header=FALSE))

# Extract posterior means of ideal points (Euclidean model)
BetaM <- est[grep("xi", row.names(est)), 1][143:286]  

# Clean and standardize names
listacong$completo <- listacong$completo %>%
     stri_trans_general(id = "Latin-ASCII") %>%
     str_squish()

# Factor and label party affiliations
listacong$partido <- factor(listacong$partido,
                            levels = c("AEC", "AICO", "ASI", "CC", "CD", "CR",
                                       "CV", "MIRA", "PC", "PDA", "PL", "PU"))

# Concatenate full name and party
listacong$completo <- paste(listacong$completo, listacong$partido, sep = ":")

# Recode parapolitics involvement
listacong$parapolitica0 <- ifelse(listacong$parapolitica0 == 1, 
                                  "Involved", 
                                  "Not involved")

# Logistic regression: parapolitics involvement predicted by ideal points (Betas from model 1)

# Remove observations with missing BetaM values
listacong <- listacong %>% drop_na(BetaM)

# Rename variable for clarity
names(listacong)[names(listacong) == 'parapolitica0'] <- 'parapolitica'

# Recode parapolitics involvement as numeric (1 = involved, 0 = not involved)
listacong$parapolitica <- ifelse(listacong$parapolitica == "Involved", 1, 0)

### euclidian testing for parapolitics

# Remove anchors from parapolitics status
parapolitics <- listacong$parapolitica[-c(49, 17)]

# Initialize output matrix: columns for beta0, beta1, AUC
out <- matrix(NA, nrow = nrow(betas), ncol = 3)

for (b in 1:nrow(betas)) {
     
     # Standardize ideal points and shift for positivity (optional)
     bet <- scale(betas[b, ]) + 3
     
     # Fit logistic regression model
     model <- glm(parapolitics ~ bet, family = binomial(link = "logit"))
     
     # Extract predicted probabilities
     fit <- model$fitted.values
     
     # Compute AUC
     roc <- roc.curve(
          scores.class0 = fit[parapolitics == 1],
          scores.class1 = fit[parapolitics == 0],
          curve = FALSE
     )
     
     # Store coefficients and AUC
     out[b, ] <- c(coef(model), roc$auc)
}

# Posterior summary: means and credible intervals
colnames(out) <- c("Intercept", "Slope", "AUC")

posterior_means <- colMeans(out)
credible_interval_95 <- apply(out, 2, quantile, probs = c(0.025, 0.975))
credible_interval_99 <- apply(out, 2, quantile, probs = c(0.005, 0.995))

# Print summary
posterior_means
credible_interval_95
credible_interval_99

### circular testing for parapolitics

# Load circular model samples
load("samples_circlar_model.RData")

# Identify circular samples
identified_samples <- identify_beta_samples(
     beta_samples,
     ref_index = 17,
     target_angle = pi / 2,
     second_index = 49,
     enforce_positive = FALSE
)

# Project to tangent space at reference point
tangent_samples <- project_to_tangent_space(identified_samples, ref_angle = pi / 2)

# Initialize output matrix: columns for intercept, slope, AUC
out <- matrix(NA, nrow = nrow(tangent_samples), ncol = 3)

# Logistic regression loop across posterior draws
for (b in 1:nrow(tangent_samples)) {
     
     # Remove anchor legislators
     parapolitics <- listacong$parapolitica[-c(49, 17)]
     bet <- tangent_samples[b, -c(49, 17)]
     
     # Keep only stable values (avoid outliers near ±π/2)
     valid <- abs(bet) < 5
     bet <- as.numeric(scale(bet[valid])) + 3
     parapolitics <- parapolitics[valid]
     
     # Fit logistic model
     model <- glm(parapolitics ~ bet, family = binomial("logit"))
     fit <- model$fitted.values
     
     # Compute AUC
     roc <- roc.curve(
          scores.class0 = fit[parapolitics == 1],
          scores.class1 = fit[parapolitics == 0],
          curve = FALSE
     )
     
     # Store intercept, slope, and AUC
     out[b, ] <- c(coef(model), roc$auc)
}

# Label output
colnames(out) <- c("Intercept", "Slope", "AUC")

# Posterior summaries
posterior_means <- colMeans(out, na.rm = TRUE)
credible_interval_95 <- apply(out, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
credible_interval_99 <- apply(out, 2, quantile, probs = c(0.005, 0.995), na.rm = TRUE)

# Print results
posterior_means
credible_interval_95
credible_interval_99