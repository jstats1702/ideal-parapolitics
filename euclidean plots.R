# -------------------------------------------------------------------
# SETUP
# -------------------------------------------------------------------

# Set working directory and clear environment
setwd("C:/Users/User/Dropbox/PAPERS/projects/JV/TrabajoDeGradoSenado20062010")
rm(list = ls())

# Load required libraries
library(pscl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rstan)
library(readxl)

# -------------------------------------------------------------------
# LOAD DATA AND MODEL OUTPUT
# -------------------------------------------------------------------

load("dataSen.Rda")
load("fit0_L1.Rda")
est <- summary(stan.fit)$summary
s2006_votes <- read_excel("s2006_votes.xlsx")

# -------------------------------------------------------------------
# PREPARE IDEAL POINT ESTIMATES
# -------------------------------------------------------------------

# Extract posterior means and 95% credible intervals
BetaM <- est[grep("xi", row.names(est)), 1][143:286]
BetaQ <- est[grep("xi", row.names(est)), c(4, 8)][143:286, ]
Beta  <- as.data.frame(cbind(BetaM, BetaQ))
colnames(Beta) <- c("Mean", "Lower", "Upper")

# Prepare legislator metadata
leg_group <- s2006_votes %>%
     select(legislador, grupo, id_legis, partido, parapolitica0) %>%
     distinct() %>%
     arrange(id_legis)

# Merge metadata with posterior estimates
Beta <- Beta %>%
     mutate(
          legislator = leg_group$legislador,
          group = leg_group$grupo,
          id_legis = leg_group$id_legis,
          partido = leg_group$partido,
          parapolitica0 = as.character(leg_group$parapolitica0)
     ) %>%
     arrange(Mean)

row.names(Beta) <- NULL

# -------------------------------------------------------------------
# Figure: Ideal Points with Political Group Coloring
# -------------------------------------------------------------------

Y <- seq_along(Beta$Mean)

pdf("fig_Credibilidad1.pdf", width = 5, height = 7, pointsize = 15)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

ggplot(Beta, aes(x = Mean, y = Y)) +
     geom_point(aes(colour = group), shape = 19, size = 1.5) +
     geom_errorbarh(aes(xmin = Lower, xmax = Upper, colour = group), height = 0) +
     geom_text(aes(x = Upper, label = legislator, colour = group),
               size = 1.5, hjust = -0.05) +
     scale_colour_manual(
          values = c("red", "chartreuse4", "blue", "goldenrod2"),
          labels = c("Coalition", "Independents", "Minorities", "Opposition")
     ) +
     scale_x_continuous(limits = c(-2.6, 3.2)) +
     theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(colour = "black", face = "bold", size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(color = "black", size = 9)
     )

dev.off()

# -------------------------------------------------------------------
# Figure: Ideal Points Colored by Parapolitics Involvement
# -------------------------------------------------------------------

pdf("fig_Credibilidad2.pdf", width = 5, height = 7, pointsize = 15)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

ggplot(Beta, aes(x = Mean, y = Y)) +
     geom_point(aes(colour = parapolitica0), shape = 19, size = 1.5) +
     geom_errorbarh(aes(xmin = Lower, xmax = Upper, colour = parapolitica0), height = 0) +
     geom_text(aes(x = Upper, label = legislator, colour = parapolitica0),
               size = 1.5, hjust = -0.05) +
     scale_colour_manual(
          values = c("gray", "black"),
          labels = c("Not Involved", "Involved")
     ) +
     scale_x_continuous(limits = c(-2.6, 3.2)) +
     theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(colour = "black", face = "bold", size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(color = "black", size = 9)
     )

dev.off()

# -------------------------------------------------------------------
# Figure: Ideal Points Faceted by Political Party
# -------------------------------------------------------------------

YP <- round(Beta$Mean, 2)

pdf("fig_Credibilidad_partidos.pdf", width = 5, height = 7, pointsize = 12)

Beta$Party_o <- factor(Beta$partido,
                       levels = c("PU", "PC", "AEC", "CC", "CD", "CV", "CR",
                                  "AICO", "ASI", "MIRA", "PL", "PDA"))

ggplot(Beta, aes(x = Mean, y = YP)) +
     geom_point(aes(colour = Party_o), size = 1.5) +
     geom_errorbarh(aes(xmin = Lower, xmax = Upper, colour = Party_o), height = 0) +
     facet_grid(Party_o ~ ., scales = "free_y") +
     scale_colour_manual(
          name = "Party",
          values = c(
               "red", "red", "red", "red", "red", "red", "red",
               "blue", "blue", "chartreuse4", "goldenrod2", "goldenrod2"
          ),
          labels = c(
               "Social de Unidad Nacional (PU)",
               "Conservador (PC)",
               "Alas Equipo Colombia (AEC)",
               "Convergencia Ciudadana (CC)",
               "Colombia Democrática (CD)",
               "Colombia Viva (CV)",
               "Cambio Radical (CR)",
               "AICO",
               "ASI",
               "MIRA",
               "Liberal (PL)",
               "Polo Democrático (PDA)"
          )
     ) +
     theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(colour = "black", face = "bold", size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0),
          panel.spacing = unit(0.1, "lines"),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(color = "black", size = 9),
          legend.title = element_text(color = "black", size = 10, face = "bold")
     ) +
     xlab(NULL) +
     ylab(NULL)

dev.off()