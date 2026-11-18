# Séance 5 — « À vous » (diapo 17)
# Fichiers de la séance (Moodle) copiés dans le dossier data/ du projet.

library(tidyverse)
library(readxl)

# 0. Dossier data/ -----------------------------------------------------------
dir.create("data", showWarnings = FALSE)   # puis y copier les fichiers Moodle
list.files("data")

# Fichier de référence (pour les noms de colonnes)
auto <- read.delim("data/auto2004_original.txt")
dim(auto)      # 24 lignes, 7 colonnes
names(auto)

# 1. auto2004_original_ligne3.txt -------------------------------------------
# On regarde d'abord le fichier pour compter les lignes à sauter
readLines("data/auto2004_original_ligne3.txt", n = 5)
# -> 3 lignes de description avant l'en-tête : skip = 3

# Sans préciser l'encodage : les accents sont mal affichés (fichier en latin1)
auto_l3_mauvais <- read.delim("data/auto2004_original_ligne3.txt", skip = 3)
head(auto_l3_mauvais, 3)

# Avec le bon encodage : les accents s'affichent correctement
auto_l3 <- read.delim("data/auto2004_original_ligne3.txt",
                      skip = 3, fileEncoding = "latin1")
head(auto_l3, 3)
str(auto_l3)
all.equal(auto_l3, auto)   # même contenu que l'original ?

# 2. auto2004_sans_nom.txt ---------------------------------------------------
auto_sn <- read.delim("data/auto2004_sans_nom.txt",
                      header = FALSE,
                      col.names = names(auto))
head(auto_sn)
str(auto_sn)

# 3. Puissance moyenne avec / sans na.strings --------------------------------
mauvais <- read.delim("data/auto2004_don_manquan_99999.txt")
bon     <- read.delim("data/auto2004_don_manquan_99999.txt",
                      na.strings = "99999")

summary(mauvais$Puissance)   # le max à 99999 trahit un code de valeur manquante

moy_mauvais <- mean(mauvais$Puissance)
moy_bon     <- mean(bon$Puissance, na.rm = TRUE)
moy_mauvais   # ~ 16848.33
moy_bon       # ~ 218.2
moy_mauvais - moy_bon        # écart absolu
moy_mauvais / moy_bon        # la moyenne est environ 77 fois trop grande
sum(is.na(bon$Puissance))    # nombre de valeurs codées 99999

# 4. Feuille 2024 de pluie_mensuelle.xlsx -----------------------------------
excel_sheets("data/pluie_mensuelle.xlsx")    # "2024" "2025"

pluie24 <- read_excel("data/pluie_mensuelle.xlsx",
                      sheet = "2024",
                      skip = 3)              # sauter le titre
pluie24

# Cumul annuel par station = somme des 12 colonnes de mois
pluie24_cumul <- pluie24 |>
  mutate(cumul = rowSums(across(where(is.numeric)), na.rm = TRUE)) |>
  select(station, cumul) |>
  arrange(desc(cumul))
pluie24_cumul

# Station avec le cumul le plus élevé
pluie24_cumul |> slice_max(cumul, n = 1)

# Variante R de base
pluie24$cumul <- rowSums(pluie24[, -1], na.rm = TRUE)
pluie24$station[which.max(pluie24$cumul)]


# ============================================================================
# Séance 5 — « À vous » (diapo 26) : ranger les données
# ============================================================================

# 1. pluie25 au format long : une ligne par station et par mois ---------------
pluie25 <- read_excel("data/pluie_mensuelle.xlsx",
                      sheet = "2025",
                      skip = 3)

pluie25_long <- pluie25 |>
  pivot_longer(cols = -station,
               names_to = "mois",
               values_to = "pluie_mm") |>
  # garder l'ordre des mois (sinon ordre alphabétique)
  mutate(mois = factor(mois, levels = names(pluie25)[-1]))
pluie25_long
nrow(pluie25_long)   # 4 stations x 12 mois = 48 lignes

# 2. Cumul annuel et mois le plus pluvieux de chaque station ----------------
pluie25_long |>
  summarise(cumul       = sum(pluie_mm, na.rm = TRUE),
            mois_max    = mois[which.max(pluie_mm)],
            pluie_max   = max(pluie_mm, na.rm = TRUE),
            .by = station) |>
  arrange(desc(cumul))

# Variante : mois le plus pluvieux avec slice_max()
pluie25_long |>
  slice_max(pluie_mm, n = 1, by = station)

# 3. Note moyenne par vin et par étape, puis format large -------------------
vins <- read_excel("data/wines.xlsx")
vins_long <- vins |>
  select(Wines, rest1:tasting9) |>
  pivot_longer(-Wines,
               names_to = c("etape", "numero"),
               names_pattern = "([a-z]+)(\\d+)",
               values_to = "note")

vins_moy <- vins_long |>
  summarise(note_moy = mean(note, na.rm = TRUE),
            .by = c(Wines, etape))
vins_moy                      # format long : 21 vins x 4 étapes = 84 lignes

vins_large <- vins_moy |>
  pivot_wider(names_from = etape,
              values_from = note_moy)
vins_large                    # une ligne par vin, colonnes rest, view, shaking, tasting

# 4. Meilleur vin à l'étape tasting ------------------------------------------
vins_large |>
  arrange(desc(tasting)) |>
  select(Wines, tasting)

vins_large |> slice_max(tasting, n = 1)