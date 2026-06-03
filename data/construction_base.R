# ============================================================
# CONSTRUCTION DE LA BASE DE DONNÉES RÉGIONALE DU SÉNÉGAL
# Thème 6 — Indice Composite Robuste — ISEP2 / ENSAE Dakar
# Auteur : Boye DIBA | 2025-2026
# ============================================================
#
# Ce script réalise le pipeline complet de collecte et de
# préparation des données à partir de 5 sources distinctes :
#
#   Source 1 : ANSD / RGPH 2023 — Éducation + Démographie
#   Source 2 : EHCVM 2021       — Économie + Emploi
#   Source 3 : PNUD 2022        — IDH subnational
#   Source 4 : Banque Mondiale  — PIB régional (en USD)
#   Source 5 : OpenAfrica       — Infrastructure + Santé
#
# Étapes du pipeline :
#   [1] Chargement et diagnostic de chaque source
#   [2] Nettoyage (noms de régions, unités, doublons, NA)
#   [3] Fusion (merge) progressive des 5 sources
#   [4] Vérification finale et sauvegarde
#
# Reproductibilité : set.seed(2026)
# Sortie           : donnees_regions.csv (14 régions × 23 indicateurs)
# ============================================================

# ── Packages nécessaires ─────────────────────────────────────
# dplyr : manipulation de tableaux (filter, mutate, select, rename)
# tidyr : restructuration (remplacement de NA, pivots)
# stringr : nettoyage de chaînes de caractères (str_trim, str_remove)
library(dplyr)
library(tidyr)
library(stringr)

# ── Graine de reproductibilité ───────────────────────────────
# set.seed() garantit que toute opération pseudo-aléatoire (imputation,
# jitter de simulation calibrée) donne le même résultat à chaque exécution.
set.seed(2026)

# ── Chemin vers les sources ──────────────────────────────────
# Tous les fichiers sources sont dans le sous-dossier sources/
dir_sources <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
                         "sources")
# Si le script est exécuté hors RStudio (Rscript), utiliser :
# dir_sources <- "data/sources"

cat("==========================================================\n")
cat("  PIPELINE DE CONSTRUCTION DE LA BASE RÉGIONALE\n")
cat("==========================================================\n\n")

# ============================================================
# ÉTAPE 1 — CHARGEMENT ET DIAGNOSTIC DE CHAQUE SOURCE
# ============================================================

cat("--- ÉTAPE 1 : Chargement et diagnostic des 5 sources ---\n\n")

# ── Source 1 : ANSD / RGPH 2023 ──────────────────────────────
# Contient : Éducation (4 indicateurs) + Démographie (5 indicateurs)
# Problèmes connus :
#   - Noms de régions sans accents pour certaines (Kedougou, Sedhiou)
#   - "Saint-Louis" écrit "Saint Louis" (sans tiret)
s1_raw <- read.csv(file.path(dir_sources, "source_ansd_rgph.csv"),
                   stringsAsFactors = FALSE, encoding = "UTF-8")

cat("[Source 1 — ANSD/RGPH]\n")
cat("  Dimensions :", nrow(s1_raw), "lignes ×", ncol(s1_raw), "colonnes\n")
cat("  Colonnes :", paste(names(s1_raw), collapse = ", "), "\n")
cat("  Valeurs manquantes :", sum(is.na(s1_raw)), "\n")
cat("  Noms de régions :", paste(s1_raw$region_ansd, collapse = ", "), "\n\n")

# ── Source 2 : EHCVM 2021 ────────────────────────────────────
# Contient : Économie (5 indicateurs dont PIB absent ici) + Emploi
# Problèmes connus :
#   - revenu_median_mensuel_fcfa = NA pour Kaffrine et Sédhiou
#     (régions créées récemment, sous-couverture de l'enquête)
s2_raw <- read.csv(file.path(dir_sources, "source_ehcvm.csv"),
                   stringsAsFactors = FALSE, na.strings = "NA")

cat("[Source 2 — EHCVM 2021]\n")
cat("  Dimensions :", nrow(s2_raw), "lignes ×", ncol(s2_raw), "colonnes\n")
cat("  Valeurs manquantes par colonne :\n")
print(colSums(is.na(s2_raw)))
cat("  Régions concernées par des NA :",
    paste(s2_raw$region_nom[is.na(s2_raw$revenu_median_mensuel_fcfa)],
          collapse = ", "), "\n\n")

# ── Source 3 : PNUD 2022 ─────────────────────────────────────
# Contient : IDH subnational (1 indicateur)
# Problèmes connus :
#   - Noms de régions avec préfixe "Région de " à supprimer
#   - Nom de colonne "Région" avec accent (problème d'encodage potentiel)
s3_raw <- read.csv(file.path(dir_sources, "source_pnud.csv"),
                   stringsAsFactors = FALSE, encoding = "UTF-8",
                   check.names = FALSE)  # check.names=FALSE : préserve les noms avec accents

cat("[Source 3 — PNUD 2022]\n")
cat("  Dimensions :", nrow(s3_raw), "lignes ×", ncol(s3_raw), "colonnes\n")
cat("  Colonnes brutes :", paste(names(s3_raw), collapse = ", "), "\n")
cat("  Exemple de noms de régions :", s3_raw[1, 1], "...\n\n")

# ── Source 4 : Banque Mondiale 2021 ──────────────────────────
# Contient : PIB régional en millions USD (à convertir en milliards FCFA)
# Problèmes connus :
#   - Ligne dupliquée pour Dakar
#   - PIB en USD → conversion requise (taux : 1 USD ≈ 600 FCFA)
s4_raw <- read.csv(file.path(dir_sources, "source_banque_mondiale.csv"),
                   stringsAsFactors = FALSE)

cat("[Source 4 — Banque Mondiale]\n")
cat("  Dimensions :", nrow(s4_raw), "lignes ×", ncol(s4_raw), "colonnes\n")
cat("  Doublons détectés :", sum(duplicated(s4_raw)), "ligne(s) dupliquée(s)\n")
cat("  Région(s) en doublon :",
    paste(s4_raw$nom_region[duplicated(s4_raw)], collapse = ", "), "\n")
cat("  Unité PIB : millions USD (conversion nécessaire)\n\n")

# ── Source 5 : OpenAfrica ────────────────────────────────────
# Contient : Infrastructure (3 ind.) + Santé (4 ind.)
# Problèmes connus :
#   - Colonne "nb_cs_pour_10000hab" = nom trop long → à renommer "centres_sante"
#   - "Louga " avec espace parasite à droite
s5_raw <- read.csv(file.path(dir_sources, "source_openafrique.csv"),
                   stringsAsFactors = FALSE)

cat("[Source 5 — OpenAfrica]\n")
cat("  Dimensions :", nrow(s5_raw), "lignes ×", ncol(s5_raw), "colonnes\n")
cat("  Noms bruts des colonnes :", paste(names(s5_raw), collapse = ", "), "\n")
cat("  Espaces parasites détectés dans nom_region :",
    sum(s5_raw$nom_region != str_trim(s5_raw$nom_region)), "\n\n")

# ============================================================
# ÉTAPE 2 — NETTOYAGE DE CHAQUE SOURCE
# ============================================================

cat("--- ÉTAPE 2 : Nettoyage ---\n\n")

# ────────────────────────────────────────────────────────────
# Fonction utilitaire : harmonisation des noms de régions
# Objectif : ramener tous les noms de régions à une forme
#   canonique identique dans toutes les sources pour permettre
#   le merge (jointure).
# Traitement :
#   1. Suppression des espaces en début/fin (str_trim)
#   2. Suppression du préfixe "Région de " (PNUD)
#   3. Rétablissement des accents manquants (ANSD)
#   4. Ajout du tiret dans "Saint-Louis"
harmoniser_region <- function(x) {
  x <- str_trim(x)                         # Supprime espaces parasites
  x <- str_remove(x, "^Région de ")        # Supprime "Région de " (PNUD)
  # Rétablissement des accents absents dans la source ANSD
  x <- gsub("^Kedougou$",  "Kédougou",  x)
  x <- gsub("^Sedhiou$",   "Sédhiou",   x)
  x <- gsub("^Saint Louis$","Saint-Louis", x)
  return(x)
}

# ── Nettoyage Source 1 : ANSD ────────────────────────────────
# 1. Harmonisation des noms de régions
# 2. Renommage des colonnes avec nos noms standardisés
# 3. Suppression de la colonne "annee" (non utile pour la base finale)
s1_clean <- s1_raw %>%
  mutate(region = harmoniser_region(region_ansd)) %>%
  rename(
    taux_alpha         = taux_alphabetisation_pct,
    taux_scol_prim     = tx_scol_primaire_pct,
    taux_scol_sec      = tx_scol_secondaire_pct,
    ratio_eleves_maitre = ratio_eleves_maitre,
    surf_agri          = surf_agricole_pct,
    densite_pop        = densite_pop_km2,
    part_urbaine       = part_urbaine_pct,
    tx_fecondite       = isf
  ) %>%
  select(region, taux_alpha, taux_scol_prim, taux_scol_sec,
         ratio_eleves_maitre, surf_agri, densite_pop, part_urbaine, tx_fecondite)

cat("[Source 1 — ANSD nettoyée]\n")
cat("  Noms corrigés :", paste(s1_clean$region, collapse = ", "), "\n")
cat("  NA restants :", sum(is.na(s1_clean)), "\n\n")

# ── Nettoyage Source 2 : EHCVM ───────────────────────────────
# 1. Harmonisation des noms
# 2. Imputation des NA par la médiane des régions voisines
#    (Kaffrine et Sédhiou : régions créées en 2008, moins couvertes)
#    Méthode : médiane des régions à taux de pauvreté similaire
# 3. Renommage des colonnes
median_revenu <- median(s2_raw$revenu_median_mensuel_fcfa, na.rm = TRUE)
cat("[Source 2 — EHCVM] Imputation des NA :\n")
cat("  Médiane du revenu (régions renseignées) :", median_revenu, "FCFA\n")
cat("  Kaffrine et Sédhiou imputées à la médiane\n\n")

s2_clean <- s2_raw %>%
  mutate(region = harmoniser_region(region_nom)) %>%
  mutate(
    # Imputation par la médiane (choix conservateur et transparent)
    revenu_median_mensuel_fcfa = ifelse(
      is.na(revenu_median_mensuel_fcfa),
      median_revenu,
      revenu_median_mensuel_fcfa
    )
  ) %>%
  rename(
    tx_pauvrete   = taux_pauvrete_pct,
    revenu_moy    = revenu_median_mensuel_fcfa,
    tx_chomage    = taux_chomage_pct,
    emploi_inform = emploi_informel_pct,
    tx_emploi_fem = taux_emploi_feminin_pct,
    secu_alim     = score_securite_alimentaire
  ) %>%
  select(region, tx_pauvrete, revenu_moy, tx_chomage,
         emploi_inform, tx_emploi_fem, secu_alim)

cat("[Source 2 — EHCVM nettoyée]\n")
cat("  NA restants :", sum(is.na(s2_clean)), "\n\n")

# ── Nettoyage Source 3 : PNUD ────────────────────────────────
# 1. Renommage robuste de la colonne "Région" (accent)
# 2. Suppression du préfixe "Région de " dans les noms
# 3. Renommage de la colonne IDH
names(s3_raw)[1] <- "region_pnud"   # Renommage défensif (encodage variable)
names(s3_raw)[3] <- "idh_subnational"

s3_clean <- s3_raw %>%
  mutate(region = harmoniser_region(region_pnud)) %>%
  select(region, idh_subnational)

cat("[Source 3 — PNUD nettoyée]\n")
cat("  Noms corrigés :", paste(s3_clean$region, collapse = ", "), "\n\n")

# ── Nettoyage Source 4 : Banque Mondiale ─────────────────────
# 1. Suppression du doublon Dakar (garder la première occurrence)
# 2. Conversion PIB : millions USD → milliards FCFA
#    Taux de change : 1 USD = 600 FCFA (moyenne 2021)
#    millions USD × 600 / 1000 = milliards FCFA
TAUX_USD_FCFA <- 600

n_avant <- nrow(s4_raw)
s4_dedup <- s4_raw[!duplicated(s4_raw$code_region), ]  # Suppression doublons
n_apres  <- nrow(s4_dedup)

cat("[Source 4 — Banque Mondiale]\n")
cat("  Doublons supprimés :", n_avant - n_apres, "\n")
cat("  Taux de conversion appliqué : 1 USD =", TAUX_USD_FCFA, "FCFA\n\n")

s4_clean <- s4_dedup %>%
  mutate(region = harmoniser_region(nom_region)) %>%
  mutate(
    pib_regional = round(pib_millions_usd * TAUX_USD_FCFA / 1000, 1)
    # milliards FCFA = millions USD × 600 / 1000
  ) %>%
  select(region, pib_regional)

# ── Nettoyage Source 5 : OpenAfrica ──────────────────────────
# 1. Suppression de l'espace parasite dans "Louga "
# 2. Renommage des colonnes (noms trop verbeux → noms standardisés)
cat("[Source 5 — OpenAfrica]\n")
cat("  Espace parasite dans 'Louga ' → corrigé en 'Louga'\n\n")

s5_clean <- s5_raw %>%
  mutate(region = harmoniser_region(nom_region)) %>%
  rename(
    acces_electr     = taux_acces_electricite_pct,
    densite_routiere = densite_routes_km_km2,
    acces_internet   = taux_internet_pct,
    acces_eau        = taux_acces_eau_pct,
    acces_assain     = taux_assainissement_pct,
    centres_sante    = nb_cs_pour_10000hab,   # Renommage colonne verbeuse
    tx_mort_inf      = taux_mortalite_inf_pour_1000
  ) %>%
  select(region, acces_electr, densite_routiere, acces_internet,
         acces_eau, acces_assain, centres_sante, tx_mort_inf)

# ============================================================
# ÉTAPE 3 — FUSION (MERGE) PROGRESSIVE DES 5 SOURCES
# ============================================================

cat("--- ÉTAPE 3 : Fusion des sources ---\n\n")

# Merge progressif : on part de la source 1 (ANSD, référence principale)
# et on joint une à une les autres sources par la clé "region"
# all.x = TRUE : LEFT JOIN — on garde toutes les régions de la source principale
# même si une région manque dans une source secondaire

cat("Merge 1/4 : Source ANSD + Source EHCVM\n")
base_v1 <- merge(s1_clean, s2_clean, by = "region", all.x = TRUE)
cat("  Dimensions :", nrow(base_v1), "×", ncol(base_v1), "\n")
cat("  NA détectés :", sum(is.na(base_v1)), "\n\n")

cat("Merge 2/4 : + Source PNUD (IDH)\n")
base_v2 <- merge(base_v1, s3_clean, by = "region", all.x = TRUE)
cat("  Dimensions :", nrow(base_v2), "×", ncol(base_v2), "\n")
cat("  NA détectés :", sum(is.na(base_v2)), "\n\n")

cat("Merge 3/4 : + Source Banque Mondiale (PIB)\n")
base_v3 <- merge(base_v2, s4_clean, by = "region", all.x = TRUE)
cat("  Dimensions :", nrow(base_v3), "×", ncol(base_v3), "\n")
cat("  NA détectés :", sum(is.na(base_v3)), "\n\n")

cat("Merge 4/4 : + Source OpenAfrica (Infrastructure + Santé)\n")
base_v4 <- merge(base_v3, s5_clean, by = "region", all.x = TRUE)
cat("  Dimensions :", nrow(base_v4), "×", ncol(base_v4), "\n")
cat("  NA détectés :", sum(is.na(base_v4)), "\n\n")

# ── Réorganisation des colonnes dans l'ordre thématique ──────
# Ordre : région | Éducation | Santé | Économie | Infrastructure | Démographie
cols_ordre <- c(
  "region",
  # Éducation (4)
  "taux_alpha", "taux_scol_prim", "taux_scol_sec", "ratio_eleves_maitre",
  # Santé (4)
  "tx_mort_inf", "acces_eau", "acces_assain", "centres_sante",
  # Économie (5)
  "tx_pauvrete", "revenu_moy", "tx_chomage", "emploi_inform", "pib_regional",
  # Infrastructure (4)
  "acces_electr", "densite_routiere", "acces_internet", "idh_subnational",
  # Démographie (6)
  "tx_fecondite", "densite_pop", "part_urbaine", "tx_emploi_fem",
  "surf_agri", "secu_alim"
)

base_finale <- base_v4[, cols_ordre]

# ── Tri par ordre alphabétique des régions ───────────────────
base_finale <- base_finale[order(base_finale$region), ]
rownames(base_finale) <- NULL

# ============================================================
# ÉTAPE 4 — VÉRIFICATION FINALE ET SAUVEGARDE
# ============================================================

cat("--- ÉTAPE 4 : Vérification et sauvegarde ---\n\n")

cat("Dimensions de la base finale :\n")
cat("  Régions (lignes) :", nrow(base_finale), "\n")
cat("  Indicateurs (colonnes - 1) :", ncol(base_finale) - 1, "\n\n")

cat("Régions présentes :\n")
print(base_finale$region)
cat("\n")

cat("Valeurs manquantes par colonne :\n")
na_count <- colSums(is.na(base_finale))
print(na_count[na_count > 0])
if (sum(na_count) == 0) cat("  -> Aucune valeur manquante ! Base complète.\n\n")

cat("Statistiques descriptives globales :\n")
print(summary(base_finale[, -1]))

# ── Sauvegarde ───────────────────────────────────────────────
output_path <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
                         "donnees_regions.csv")
write.csv(base_finale, output_path, row.names = FALSE)

cat("\n==========================================================\n")
cat("  BASE SAUVEGARDÉE :", output_path, "\n")
cat("  Format : CSV UTF-8 | Séparateur : virgule\n")
cat("  Régions :", nrow(base_finale), "| Indicateurs :", ncol(base_finale)-1, "\n")
cat("==========================================================\n")
