# ----------------------------------------------------------
# SETUP
# ----------------------------------------------------------

# Set working directory and clear environment
setwd("C:/Users/User/Dropbox/PAPERS/projects/JV/TrabajoDeGradoSenado20062010")
rm(list = ls())

# Load libraries
suppressMessages({
     library(readxl)
     library(dplyr)
     library(tidyr)
     library(stringi)
     library(stringr)
     library(ggplot2)
     library(pscl)
     library(magrittr)
     library(verification)
     library(PRROC)
     library(EnvStats)
     library(rstan)
     library(ggpubr)
     library(caret)
     library(GGally)
     library(corrplot)
     library(bayesplot)
     library(rstanarm)
     library(loo)
     library(projpred)
})

# Set global theme and options
theme_set(bayesplot::theme_default(base_family = "sans"))
options(mc.cores = 1)
SEED <- 14124869

# ----------------------------------------------------------
# LOAD AND PREPARE DATA
# ----------------------------------------------------------

listacong  <- read_excel("listacong.xlsx")
Betamod1   <- read_excel("Betamod1.xlsx")
Betamod2   <- read_excel("Betamod2.xlsx")

listacong$completo <- listacong$completo %>%
     stri_trans_general(id = "Latin-ASCII") %>%
     str_squish()

listacong$partido <- factor(listacong$partido,
                            levels = c("AEC", "AICO", "ASI", "CC", "CD", "CR",
                                       "CV", "MIRA", "PC", "PDA", "PL", "PU"))

listacong$completo <- paste(listacong$completo, listacong$partido, sep = ":")
listacong$parapolitica0 <- ifelse(listacong$parapolitica0 == 1, "Involved", "Not involved")

# ----------------------------------------------------------
# Figure: Attendance by parapolitics involvement
# ----------------------------------------------------------

pdf("fig_AsistenciaPara_english.pdf", width = 7, height = 5)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

boxasist <- listacong[!is.na(listacong$porcasist), ]

boxplot(porcasist ~ parapolitica0, data = boxasist,
        ylab = "Attendance percentage",
        xlab = "Parapolitics involvement",
        col = "white", border = "black", boxwex = 0.4,
        cex.axis = 0.95, outline = FALSE)

stripchart(porcasist ~ parapolitica0, data = boxasist,
           method = "jitter", pch = 21, cex = 1.3,
           bg = adjustcolor("steelblue", 0.5),
           col = adjustcolor("steelblue", 0.5),
           vertical = TRUE, add = TRUE, jitter = 0.15)

dev.off()

# ----------------------------------------------------------
# Figure: Abstention by parapolitics involvement
# ----------------------------------------------------------

pdf("fig_AbstencionPara_english.pdf", width = 7, height = 5)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

boxabst <- listacong[!is.na(listacong$porcabst), ]

boxplot(porcabst ~ parapolitica0, data = boxabst,
        ylab = "Abstention percentage",
        xlab = "Parapolitics involvement",
        col = "white", border = "black", boxwex = 0.4,
        cex.axis = 0.95, outline = FALSE)

stripchart(porcabst ~ parapolitica0, data = boxabst,
           method = "jitter", pch = 21, cex = 1.3,
           bg = adjustcolor("steelblue", 0.5),
           col = adjustcolor("steelblue", 0.5),
           vertical = TRUE, add = TRUE, jitter = 0.15)

dev.off()

# ----------------------------------------------------------
# Figure: Participation by party
# ----------------------------------------------------------

pdf("fig_BoxplotParticipacion_english.pdf", width = 7, height = 5)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

boxpart <- listacong[!is.na(listacong$porcpart), ]

boxplot(porcpart ~ partido, data = boxpart,
        xlab = "Party", ylab = "Participation percentage",
        col = "white", border = "black", boxwex = 0.4,
        cex.axis = 0.95, outline = FALSE, xaxt = "n")

axis(side = 1, at = 1:length(levels(boxpart$partido)),
     labels = levels(boxpart$partido), las = 1, cex.axis = 0.95)

stripchart(porcpart ~ partido, data = boxpart,
           method = "jitter", pch = 21, cex = 1.3,
           bg = adjustcolor("steelblue", 0.5),
           col = adjustcolor("steelblue", 0.5),
           vertical = TRUE, add = TRUE, jitter = 0.15)

dev.off()

# ----------------------------------------------------------
# Figure: Attendance by party
# ----------------------------------------------------------

pdf("fig_BoxplotAsistencia_english.pdf", width = 7, height = 5)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

boxasist <- listacong[!is.na(listacong$porcasist), ]

boxplot(porcasist ~ partido, data = boxasist,
        xlab = "Party", ylab = "Attendance percentage",
        col = "white", border = "black", boxwex = 0.4,
        cex.axis = 0.95, outline = FALSE, xaxt = "n")

axis(side = 1, at = 1:length(levels(boxasist$partido)),
     labels = levels(boxasist$partido), las = 1, cex.axis = 0.95)

stripchart(porcasist ~ partido, data = boxasist,
           method = "jitter", pch = 21, cex = 1.3,
           bg = adjustcolor("steelblue", 0.5),
           col = adjustcolor("steelblue", 0.5),
           vertical = TRUE, add = TRUE, jitter = 0.15)

dev.off()

# ----------------------------------------------------------
# Figure: Abstention by party
# ----------------------------------------------------------

pdf("fig_BoxplotAbstencion_english.pdf", width = 7, height = 5)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

boxabst <- listacong[!is.na(listacong$porcabst), ]

boxplot(porcabst ~ partido, data = boxabst,
        xlab = "Party", ylab = "Abstention percentage",
        col = "white", border = "black", boxwex = 0.4,
        cex.axis = 0.95, outline = FALSE, xaxt = "n")

axis(side = 1, at = 1:length(levels(boxabst$partido)),
     labels = levels(boxabst$partido), las = 1, cex.axis = 0.95)

stripchart(porcabst ~ partido, data = boxabst,
           method = "jitter", pch = 21, cex = 1.3,
           bg = adjustcolor("steelblue", 0.5),
           col = adjustcolor("steelblue", 0.5),
           vertical = TRUE, add = TRUE, jitter = 0.15)

dev.off()
