# -------------------------------------------------------------------
# INITIAL SETUP
# -------------------------------------------------------------------

rm(list = ls())  # Clear workspace

setwd("C:/Users/User/Dropbox/PAPERS/projects/JV/TrabajoDeGradoSenado20062010")

# Load required libraries
library(readxl)
library(dplyr)

# -------------------------------------------------------------------
# LOAD DATA
# -------------------------------------------------------------------

# Load voting matrix (senators x motions)
Y <- as.matrix(read_excel("matriz2006.xlsx")[, -1])
dim(Y)

# Load vote-level data
s2006_votes <- read_excel("s2006_votes.xlsx")

# Create legislator-level metadata
leg_group <- s2006_votes %>%
     select(legislador, grupo, id_legis, partido, parapolitica0) %>%
     distinct() %>%
     arrange(id_legis)

# -------------------------------------------------------------------
# COLOR ASSIGNMENTS BY GROUP
# -------------------------------------------------------------------

group_colors <- leg_group$grupo
group_colors[group_colors == "coalicion"]     <- "red"
group_colors[group_colors == "independiente"] <- "chartreuse4"
group_colors[group_colors == "minoria"]       <- "blue"
group_colors[group_colors == "oposicion"]     <- "goldenrod2"

# -------------------------------------------------------------------
# FIGURE: CIRCULAR POSTERIOR MEANS BY GROUP
# -------------------------------------------------------------------

pdf("fig_ideal_points_circle.pdf", width = 7, height = 5)

par(mfrow = c(1, 1), mar = c(2.75, 3, 1.5, 0.5), mgp = c(1.7, 0.7, 0))

plot_posterior_means_circle(
     identified_samples,
     tick_length = 1,
     tick_lwd = 0.5,
     tick_col = group_colors,
     show_axes = FALSE,
     show_labels = FALSE
)

legend("topright",
       legend = c("Coalition", "Independents", "Minorities", "Opposition"),
       fill = c("red", "chartreuse4", "blue", "goldenrod2"),
       border = NA, bty = "n")

dev.off()

# -------------------------------------------------------------------
# FIGURES: CIRCULAR POSTERIOR MEANS BY PARTY
# -------------------------------------------------------------------

parties <- unique(leg_group$partido)

for (party in parties) {
     party_indices <- which(leg_group$partido == party)
     
     pdf(paste0("fig_ideal_points_circle_", party, ".pdf"), width = 7, height = 5)
     par(mfrow = c(1, 1), mar = c(2.75, 3, 1.5, 0.5), mgp = c(1.7, 0.7, 0))
     
     plot_posterior_means_circle(
          identified_samples,
          tick_length = 1,
          tick_lwd = 0.5,
          tick_col = group_colors,
          show_axes = FALSE,
          show_labels = FALSE,
          indices = party_indices
     )
     
     legend("topright", legend = party, bty = "n", cex = 3)
     dev.off()
}

# -------------------------------------------------------------------
# FIGURE: COMPARISON BETWEEN EUCLIDEAN AND CIRCULAR ESTIMATES
# -------------------------------------------------------------------

load("dataSen.Rda")
load("fit0_L1.Rda")
est <- summary(stan.fit)$summary

# Extract posterior means from Euclidean model
BetaM <- est[grep("xi", rownames(est)), 1][143:286]

# Compute means from tangent space projections (Circular model)
x <- BetaM
y <- colMeans(tangent_samples)

# Use only stable values for circular projections
valid <- abs(y) < 5
x_std <- scale(x[valid])
y_std <- scale(y[valid])
cc <- group_colors[valid]

pdf("fig_ideal_points_circular_vs_euclidean.pdf", width = 7, height = 5)

par(mfrow = c(1, 1), mar = c(2.75, 3, 1.5, 0.5), mgp = c(1.7, 0.7, 0))

plot(x_std, y_std,
     pch = 16, cex = 1.4,
     col = adjustcolor(cc, alpha.f = 0.5),
     xlab = "Euclidean Model",
     ylab = "Circular Model (Tangent Projection)",
     xlim = range(c(x_std, y_std)),
     ylim = range(c(x_std, y_std)))

abline(0, 1, col = "black", lty = 2)  # Identity line

dev.off()

# Check correlation and number of excluded extreme values
sum(!valid)
cor(as.numeric(x_std), as.numeric(y_std))

# -------------------------------------------------------------------
# FIGURE: CIRCULAR IDEAL POINTS BY PARAPOLITICS INVOLVEMENT
# -------------------------------------------------------------------

# Recode parapolitics variable as color
parapol_colors <- leg_group$parapolitica0
parapol_colors[parapol_colors == 1] <- "black"
parapol_colors[parapol_colors == 0] <- "gray"

pdf("fig_ideal_points_circle_parapolitics.pdf", width = 7, height = 5)

par(mfrow = c(1, 1), mar = c(2.75, 3, 1.5, 0.5), mgp = c(1.7, 0.7, 0))

plot_posterior_means_circle(
     identified_samples,
     tick_length = 1,
     tick_lwd = 0.5,
     tick_col = parapol_colors,
     show_axes = FALSE,
     show_labels = FALSE
)

legend("topright",
       legend = c("Not involved", "Involved"),
       fill = c("gray", "black"),
       border = NA,
       bty = "n")

dev.off()