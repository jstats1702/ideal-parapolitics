# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

# Set working directory and clear environment
setwd("C:/Users/User/Dropbox/PAPERS/projects/JV/TrabajoDeGradoSenado20062010")
rm(list = ls())

# Load libraries
library(readxl)
library(dplyr)
library(stringi)
library(stringr)
library(writexl)
library(tidyverse)

# ------------------------------------------------------------------------------
# LOAD AND CLEAN DATA
# ------------------------------------------------------------------------------

# Load voting data
DataCV <- read_excel("DataCV.xlsx", sheet = "Votaciones")
DataCV$fecha_radicacion <- as.Date(DataCV$fecha_radicacion)
Cong0610 <- DataCV

# Load legislator list and normalize names
listacong <- read_excel("Congresistas1998-2014.xlsx", sheet = "final")
listaconginicial <- listacong[1:102,]
table(listaconginicial$partido)

listacong$completo <- listacong$completo %>%
     stri_trans_general(id = "Latin-ASCII") %>%
     str_squish()
A <- listacong$completo

Cong0610$congresista <- Cong0610$congresista %>%
     stri_trans_general(id = "Latin-ASCII") %>%
     str_squish()
B <- unique(Cong0610$congresista)

# Match and identify valid legislators
C <- intersect(A, B)
setdiff(A, C)

# ------------------------------------------------------------------------------
# SELECT VALID VOTES
# ------------------------------------------------------------------------------

idvot <- unique(Cong0610$id_votacion)
nvot <- length(idvot)
votint <- tamano <- c()

for (i in seq_len(nvot)) {
     B <- Cong0610 %>% filter(id_votacion == idvot[i]) %>% pull(congresista)
     coinc <- length(intersect(A, B))
     if (coinc > 30 && length(B) - coinc == 0) {
          votint <- c(votint, idvot[i])
          tamano <- c(tamano, length(B))
     }
}

votaciones <- Cong0610 %>% filter(id_votacion %in% votint)

vota <- votaciones %>%
     select(congresista, id_congresista, id_votacion, voto) %>%
     filter(!congresista %in% c("Jorge de Jesus Castro Pacheco",
                                "Alvaro Alfonso Garcia Romero",
                                "Jairo Enrique Merlano Fernandez"))

# ------------------------------------------------------------------------------
# CREATE BINARY VOTING MATRIX
# ------------------------------------------------------------------------------

senadores <- vota %>%
     filter(id_votacion == votint[1]) %>%
     pull(congresista) %>%
     sort()

q <- length(senadores)
k <- length(votint)
M <- matrix(NA, q, k)

for (i in seq_len(k)) {
     x <- vota %>% filter(id_votacion == votint[i])
     y <- x$congresista
     for (j in seq_len(q)) {
          ind <- which(y == senadores[j])
          M[j, i] <- if (length(ind) == 0) "No listado" else as.character(x[ind, "voto"])
     }
}

# ------------------------------------------------------------------------------
# PARTICIPATION AND GROUP ASSIGNMENT
# ------------------------------------------------------------------------------

participacion <- rowSums(M == "No") + rowSums(M == "Si") + rowSums(M == "Se abstuvo")
porcpart <- round(100 * participacion / 136, 2)
Porcpart <- data.frame(senadores, porcpart) %>% filter(porcpart >= 70)

# Create long-format vote data
s2006_vot <- map_dfr(seq_len(k), function(i) {
     vota_i <- vota %>% filter(id_votacion == votint[i])
     cbind(vota_i, id_list = i)
})

s2006_votes <- map_dfr(seq_along(senadores), function(i) {
     votai <- s2006_vot %>% filter(congresista == senadores[i])
     cbind(votai, id_legis = i)
})

# Clean vote values
s2006_votes$voto <- recode(s2006_votes$voto,
                           "Si" = 1,
                           "No" = 0,
                           "No aplica" = NA_real_,
                           "No asistio" = NA_real_,
                           "Se abstuvo" = NA_real_)
s2006_votes <- s2006_votes %>% filter(!is.na(voto))

# Assign parties and full names
listacong$partido <- factor(listacong$partido,
                            levels = c("AEC","ASI","AICO","CR","CD","CV",
                                       "PC","CC","PL","MIRA","PU","PDA"))

s2006_votes$partido <- map_chr(seq_len(nrow(s2006_votes)), function(i) {
     b <- which(listacong$completo == s2006_votes$congresista[i])
     as.character(listacong$partido[b])
})

s2006_votes <- s2006_votes %>%
     mutate(legislador = paste(congresista, partido, sep = ":"))

# Define political blocks
pcoalicion     <- c("CR", "CD", "CV", "PC", "CC", "AEC", "PU")
poposicion     <- c("PL", "PDA")
pindependiente <- c("MIRA")
pminoria       <- c("ASI", "AICO")

s2006_votes <- s2006_votes %>%
     mutate(grupo = case_when(
          partido %in% pcoalicion     ~ "coalicion",
          partido %in% poposicion     ~ "oposicion",
          partido %in% pindependiente ~ "independiente",
          partido %in% pminoria       ~ "minoria",
          TRUE                        ~ "ERROR"
     ))

# Final merge with legislator info
listacong$completo <- paste(listacong$completo, listacong$partido, sep = ":")
listacong <- listacong %>% select(legislador = completo, everything())

s2006_votes <- s2006_votes %>%
     select(legislador, id_legis, grupo, id_votacion, voto, id_list, congresista) %>%
     mutate(voto = as.numeric(voto)) %>%
     merge(listacong, by = "legislador", all.x = TRUE)

# ------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

# Binary matrix for votes
cerosyunos <- M %>%
     replace(. == "Si", 1) %>%
     replace(. == "No", 0) %>%
     replace(. %in% c("No aplica", "No asistio", "No listado", "Se abstuvo"), NA) %>%
     matrix(ncol = ncol(M)) %>%
     apply(2, as.numeric)

nacount     <- colSums(is.na(cerosyunos))
votosvalidos <- colSums(M == "No") + colSums(M == "Si") + colSums(M == "Se abstuvo")

asistencia <- rowSums(M == "No") + rowSums(M == "Si") + rowSums(M == "Se abstuvo")
enlista    <- asistencia + rowSums(M == "No asistio")
porcasist  <- round(100 * asistencia / enlista, 2)
porcasist  <- data.frame(senadores, porcasist)

participacion <- asistencia
porcpart <- round(100 * participacion / 136, 2)
porcpart <- data.frame(senadores, porcpart)
quantile(porcpart$porcpart, prob = c(0.25, 0.5, 0.75))

abstencion <- rowSums(M == "Se abstuvo")
porcabst   <- round(100 * abstencion / enlista, 2)
porcabst   <- data.frame(senadores, porcabst)

# ------------------------------------------------------------------------------
# Figure: Histogram of Participation
# ------------------------------------------------------------------------------

pdf("fig_HistogramaParticipacion_english_english.pdf", width = 7, height = 5)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

hist(porcpart$porcpart, breaks = 40, probability = TRUE,
     col = "lightgray", border = "lightgray",
     xlab = "Participation percentage", ylab = "Density", main = NULL)

abline(v = mean(porcpart$porcpart), col = 2, lwd = 2)
text(40, 0.05, paste("Mean =", round(mean(porcpart$porcpart), 3)), col = 2, cex = 1.2)

abline(v = median(porcpart$porcpart), col = 4, lwd = 2)
text(40, 0.035, paste("Median =", round(median(porcpart$porcpart), 3)), col = 4, cex = 1.2)

lines(density(porcpart$porcpart), lwd = 2, col = "chocolate3")
dev.off()

# ------------------------------------------------------------------------------
# Figure: Histogram of Attendance
# ------------------------------------------------------------------------------

pdf("fig_HistogramaAsistencia_english.pdf", width = 7, height = 5)
par(mfrow = c(1,1), mar = c(2.75,3,1.5,0.5), mgp = c(1.7,0.7,0))

hist(porcasist$porcasist, breaks = 40, probability = TRUE,
     col = "lightgray", border = "lightgray",
     xlab = "Attendance Percentage", ylab = "Density", main = NULL)

abline(v = mean(porcasist$porcasist), col = 2, lwd = 2)
text(40, 0.12, paste("Mean =", round(mean(porcasist$porcasist), 3)), col = 2, cex = 1.2)

abline(v = median(porcasist$porcasist), col = 4, lwd = 2)
text(40, 0.08, paste("Median =", round(median(porcasist$porcasist), 3)), col = 4, cex = 1.2)

lines(density(porcasist$porcasist), lwd = 2, col = "chocolate3")
dev.off()

# ------------------------------------------------------------------------------
# Figure: Histogram of Abstention
# ------------------------------------------------------------------------------

pdf("fig_HistogramaAbstencion_english.pdf", width = 7, height = 5)

hist(porcabst$porcabst, breaks = 40, probability = TRUE,
     col = "lightgray", border = "lightgray",
     xlab = "Abstention Percentage", ylab = "Density", main = NULL)

abline(v = mean(porcabst$porcabst), col = 2, lwd = 2)
text(70, 0.03, paste("Mean =", round(mean(porcabst$porcabst), 3)), col = 2, cex = 1.2)

abline(v = median(porcabst$porcabst), col = 4, lwd = 2)
text(70, 0.02, paste("Median =", round(median(porcabst$porcabst), 3)), col = 4, cex = 1.2)

lines(density(porcabst$porcabst), lwd = 2, col = "chocolate3")
dev.off()