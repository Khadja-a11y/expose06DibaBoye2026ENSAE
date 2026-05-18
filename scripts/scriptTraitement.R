# ============================================================
#  THÈME 6 — Construction d'un Indice Composite Robuste
#             pour les 14 Régions du Sénégal
# ============================================================
#  Cours   : Traitement Statistique avec R — ISEP2 / 2025-2026
#  Auteur  : Boye DIBA
#  Dépôt   : expose06DibaBoye2026ENSAE (GitHub public)
#  Date    : 2025-2026
# ============================================================
#
#  CE SCRIPT EST ORGANISÉ EN 8 ÉTAPES :
#  0. Installation et chargement des bibliothèques
#  1. Chargement et exploration des données
#  2. Audit des indicateurs (qualité, redondances)
#  3. Orientation et normalisation des indicateurs
#  4. Analyse en Composantes Principales (ACP)
#  5. Construction des 3 indices composites
#  6. Analyse de sensibilité (corrélation de Spearman)
#  7. Génération des visualisations
#  8. Export des résultats
#
#  COMMENT UTILISER CE SCRIPT :
#  - Ouvrir ce fichier dans RStudio
#  - S'assurer que le répertoire de travail est la RACINE DU PROJET
#    (Menu Session > Set Working Directory > To Project Directory)
#  - Exécuter le script entier : Ctrl+Shift+Enter (ou Cmd+Shift+Enter sur Mac)
#  - Ou exécuter section par section avec Ctrl+Enter
# ============================================================


# ── 0. BIBLIOTHÈQUES ────────────────────────────────────────────────────────
# On vérifie si chaque package est installé avant de le charger.
# Si non installé, on l'installe automatiquement depuis CRAN.

required_packages <- c(
  "tidyverse",   # Ensemble de packages pour la manipulation et visualisation
  "FactoMineR",  # Calcul de l'ACP (Analyse en Composantes Principales)
  "factoextra",  # Visualisation des résultats ACP (éboulis, biplot)
  "corrplot",    # Visualisation de la matrice de corrélation sous forme de heatmap
  "gridExtra",   # Assemblage de plusieurs graphiques sur une même figure
  "scales",      # Formatage des axes de graphiques (%, virgules, etc.)
  "knitr",       # Génération de tableaux formatés (utilisé dans le .Rmd)
  "kableExtra"   # Extension de knitr pour des tableaux plus riches
)

# Boucle d'installation : pour chaque package dans la liste,
# si le package n'est pas encore installé -> l'installer
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installation du package manquant : ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  # Charger le package dans l'environnement R courant
  library(pkg, character.only = TRUE)
}

cat("=== TOUTES LES BIBLIOTHÈQUES SONT CHARGÉES ===\n\n")


# ── 1. CHARGEMENT ET EXPLORATION DES DONNÉES ────────────────────────────────

# Chemin vers le fichier de données (RELATIF à la racine du projet)
# IMPORTANT : Ne jamais utiliser un chemin absolu comme :
#   C:/Users/MonNom/Documents/... (non portable sur une autre machine)
# On utilise toujours des chemins relatifs pour la reproductibilité.
data_path <- "data/donnees_regions.csv"

# Lecture du fichier CSV en tableau de données (data.frame)
# stringsAsFactors=FALSE : éviter la conversion automatique des textes en facteurs
data_raw <- read.csv(data_path, stringsAsFactors = FALSE)

# Séparation des noms de régions et des indicateurs numériques
# La colonne "region" contient les noms -> on la garde séparément
regions  <- data_raw$region

# data_num : matrice pure des 23 indicateurs, sans la colonne des noms
data_num <- data_raw[, names(data_raw) != "region"]

# ── Affichage du rapport d'exploration ──────────────────────────────────────
cat("=== RAPPORT D'EXPLORATION DES DONNÉES ===\n")
cat("Nombre de régions (lignes)   :", nrow(data_raw), "\n")
cat("Nombre d'indicateurs (col.)  :", ncol(data_num), "\n")
cat("Valeurs manquantes totales   :", sum(is.na(data_num)), "\n")
cat("\nNoms des 14 régions :\n")
cat(" ", paste(regions, collapse=", "), "\n")
cat("\nListe des 23 indicateurs :\n")
for (i in seq_along(names(data_num))) {
  cat(sprintf("  %2d. %s\n", i, names(data_num)[i]))
}

# Aperçu rapide des données (6 premières lignes, 6 premières colonnes)
cat("\nAperçu des 6 premières colonnes :\n")
print(head(data_raw[, 1:6]))


# ── 2. AUDIT DES INDICATEURS ────────────────────────────────────────────────
# L'audit sert à vérifier la qualité des indicateurs avant de les utiliser.
# Un mauvais indicateur (données manquantes, peu varié, redondant) affaiblit l'indice.

cat("\n=== ÉTAPE 1 : AUDIT DES INDICATEURS ===\n")

# ── 2.1 Valeurs manquantes ──────────────────────────────────────────────────
# Pour chaque indicateur, on calcule le pourcentage de valeurs manquantes (NA)
# Formule : (nombre de NA / nombre total d'observations) × 100
# Seuil d'exclusion retenu : > 20% de NA -> indicateur peu fiable

tx_na <- sapply(data_num, function(x) sum(is.na(x)) / nrow(data_num) * 100)

cat("\n[ 2.1 ] Valeurs manquantes par indicateur :\n")
if (all(tx_na == 0)) {
  cat("  -> Aucune valeur manquante ! Tous les 23 indicateurs sont complets.\n")
} else {
  # Afficher uniquement les indicateurs avec des NA
  na_tab <- data.frame(Indicateur=names(tx_na[tx_na > 0]),
                       Pct_NA=round(tx_na[tx_na > 0], 1))
  print(na_tab)
  cat("\nIndicateurs avec plus de 20% de NA (à exclure) :",
      names(tx_na[tx_na > 20]), "\n")
}

# ── 2.2 Pouvoir discriminant (coefficient de variation) ─────────────────────
# Le coefficient de variation (CV) = écart-type / |moyenne| × 100
# Il mesure la variabilité RELATIVE d'un indicateur entre les régions.
# Si CV < 5% : toutes les régions ont des valeurs similaires
#              -> l'indicateur ne permet pas de les différencier
#              -> peu utile dans un indice de classement

cv <- sapply(data_num, function(x) sd(x, na.rm=TRUE) / abs(mean(x, na.rm=TRUE)) * 100)

cat("\n[ 2.2 ] Pouvoir discriminant (Coefficient de Variation) :\n")
cv_df <- data.frame(Indicateur=names(cv), CV_pct=round(cv, 1)) %>% arrange(CV_pct)
print(cv_df)
cat("\nIndicateurs à faible discrimination (CV < 5%) :")
faibles_cv <- names(cv[cv < 5])
if (length(faibles_cv) == 0) cat(" Aucun\n") else cat("\n ", paste(faibles_cv, collapse=", "), "\n")

# ── 2.3 Corrélations entre indicateurs (détection des redondances) ──────────
# Si deux indicateurs ont une corrélation |r| > 0.95, ils mesurent quasi la même chose.
# Les garder tous les deux reviendrait à compter deux fois la même information.
# Formule : Corrélation de Pearson r = cov(X,Y) / (sd(X) × sd(Y))

cor_matrix <- cor(data_num, use="pairwise.complete.obs")
# On ne regarde que la moitié supérieure pour éviter les doublons (matrice symétrique)
pairs_high <- which(abs(cor_matrix) > 0.95 & upper.tri(cor_matrix), arr.ind=TRUE)

cat("\n[ 2.3 ] Paires d'indicateurs redondantes (|r| > 0,95) :")
if (nrow(pairs_high) == 0) {
  cat(" Aucune\n")
  cat("  -> Tous les 23 indicateurs apportent une information distincte.\n")
} else {
  for (k in 1:nrow(pairs_high)) {
    i1 <- pairs_high[k, 1]; i2 <- pairs_high[k, 2]
    cat(sprintf("\n  %s <-> %s (r=%.3f)",
                rownames(cor_matrix)[i1], colnames(cor_matrix)[i2],
                cor_matrix[i1, i2]))
  }
  cat("\n")
}

cat("\n-> CONCLUSION AUDIT : 23 indicateurs retenus.\n")


# ── 3. ORIENTATION ET NORMALISATION ─────────────────────────────────────────
cat("\n=== ÉTAPE 2 : ORIENTATION ET NORMALISATION ===\n")

# ── 3.1 Orientation (inversion des indicateurs négatifs) ────────────────────
# Problème : certains indicateurs sont "négatifs" dans leur sens :
#   valeur élevée = MAUVAIS résultat (ex: mortalité infantile élevée = catastrophe)
# Si on les normalise sans les corriger, une région avec une mortalité élevée
# obtiendrait un SCORE ÉLEVÉ -> erreur grave qui inverserait le classement.
# Solution : multiplier ces indicateurs par -1 AVANT la normalisation.
# Après inversion : valeur élevée = BONNE situation (cohérence universelle).

ind_inv <- c(
  "tx_mort_inf",   # Mortalité infantile : élevée = mauvais
  "tx_pauvrete",   # Taux de pauvreté : élevé = mauvais
  "tx_chomage",    # Taux de chômage : élevé = mauvais
  "emploi_inform", # Emploi informel : élevé = précarité = mauvais
  "tx_fecondite"   # Fécondité très élevée = fragilité démographique
)

# Copie du tableau de données pour ne pas modifier l'original
data_orient <- data_num
# Inversion : on remplace chaque valeur v par -v pour les indicateurs négatifs
data_orient[, ind_inv] <- -data_orient[, ind_inv]

cat("Indicateurs inversés (×-1) :\n")
for (ind in ind_inv) cat(" -", ind, "\n")
cat("-> Après inversion : valeur élevée = bonne situation pour TOUS les indicateurs.\n")

# ── 3.2 Normalisation ───────────────────────────────────────────────────────
# Pourquoi normaliser ? Les indicateurs ont des unités et des échelles différentes :
# - PIB : des centaines de milliards de FCFA
# - Taux d'alphabétisation : entre 0 et 100 (%)
# - IDH sous-national : entre 0 et 1
# Sans normalisation, le PIB dominerait l'indice par sa seule magnitude.
# La normalisation met tous les indicateurs sur la même échelle.

# MÉTHODE A : Min-Max -> résultat toujours dans [0, 1]
# Formule : x_norm = (x - x_min) / (x_max - x_min)
# Interprétation : 0 = région la moins performante, 1 = région la plus performante
# Avantage : intuitive, directement interprétable
# Limite : sensible aux valeurs extrêmes (Dakar peut "écraser" les autres)
minmax <- function(x) (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
data_mm <- as.data.frame(lapply(data_orient, minmax))

# MÉTHODE B : Z-score (centrage-réduction)
# Formule : z = (x - moyenne) / écart-type
# Résultat : moyenne = 0, écart-type = 1. Peut être négatif.
# Avantage : robuste aux outliers, utilisé comme entrée pour l'ACP
# Limite : moins intuitive pour les non-statisticiens
zscore <- function(x) (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
data_zs <- as.data.frame(lapply(data_orient, zscore))

# MÉTHODE C : Centile (rang relatif normalisé)
# Formule : c = rang(x) / n (où n = nombre d'observations non-manquantes)
# Résultat : toujours dans ]0, 1], basé uniquement sur les rangs
# Avantage : totalement insensible aux valeurs extrêmes (outliers)
# Limite : perd l'amplitude des écarts (Dakar 3x > Thiès n'est pas distingué de 1.5x)
centile <- function(x) rank(x, ties.method="average", na.last="keep") / sum(!is.na(x))
data_ct <- as.data.frame(lapply(data_orient, centile))

cat("\nRésultats des normalisations (plages observées) :\n")
cat(sprintf("  Min-Max  : [%.3f ; %.3f]\n", min(data_mm), max(data_mm)))
cat(sprintf("  Z-score  : [%.3f ; %.3f]\n", min(data_zs), max(data_zs)))
cat(sprintf("  Centile  : [%.3f ; %.3f]\n", min(data_ct), max(data_ct)))
cat("-> La normalisation Min-Max est retenue pour la construction des indices.\n")
cat("-> La normalisation Z-score est utilisée comme entrée pour l'ACP.\n")


# ── 4. ANALYSE EN COMPOSANTES PRINCIPALES (ACP) ─────────────────────────────
cat("\n=== ÉTAPE 3 : ACP ===\n")

# L'ACP sur les données Z-score (déjà normalisées)
# scale.unit=FALSE : on ne re-standardise pas car les données sont déjà en Z-score
# Si on utilisait scale.unit=TRUE sur des données Z-score, on aurait une double
# standardisation inutile.
acp <- PCA(data_zs, graph=FALSE, scale.unit=FALSE)

# Extraction du tableau des valeurs propres :
# - Colonne 1 : valeur propre λ (mesure l'information capturée par l'axe)
# - Colonne 2 : % de variance expliquée par cet axe
# - Colonne 3 : % de variance cumulée jusqu'à cet axe
ev <- acp$eig

# Critère de Kaiser : on retient uniquement les axes avec λ >= 1
# Justification : un axe avec λ = 1 résume autant d'info qu'un indicateur original
#                 un axe avec λ < 1 apporte moins d'info qu'un seul indicateur
n_axes <- sum(ev[, 1] >= 1)

cat(sprintf("Axes retenus (critère de Kaiser : λ ≥ 1) : %d\n", n_axes))
cat(sprintf("Variance expliquée par ces %d axes : %.1f%%\n", n_axes, ev[n_axes, 3]))
cat("\nTableau des valeurs propres :\n")
print(round(ev[1:min(5, nrow(ev)), ], 3))

# Poids pour la pondération ACP :
# poids de l'Axe k = (variance expliquée par Axe k) / (variance totale retenue)
# Ces poids seront utilisés à l'Étape 5 pour construire l'Indice 2
poids_acp <- ev[1:n_axes, 2] / sum(ev[1:n_axes, 2])
cat(sprintf("\nPoids ACP par axe (utilisés pour l'Indice 2) :\n"))
for (i in 1:n_axes) {
  cat(sprintf("  Axe %d : %.1f%%\n", i, poids_acp[i] * 100))
}


# ── 5. CONSTRUCTION DES 3 INDICES ───────────────────────────────────────────
cat("\n=== ÉTAPE 4 : CONSTRUCTION DES 3 INDICES ===\n")

# ── INDICE 1 : Équipondération ───────────────────────────────────────────────
# Principe : tous les indicateurs reçoivent le même poids (1/23 ≈ 4,35%)
# Formule : I_equi(région) = moyenne des 23 indicateurs normalisés Min-Max
# C'est le cas le plus simple mais aussi le plus contestable politiquement
# car il suppose que tous les indicateurs sont également importants.
indice_equi <- rowMeans(data_mm)

cat("[ Indice 1 - Équipondération ]\n")
cat(sprintf("  Plage : [%.3f ; %.3f] | Moyenne : %.3f\n",
            min(indice_equi), max(indice_equi), mean(indice_equi)))

# ── INDICE 2 : Pondération par l'ACP ────────────────────────────────────────
# Principe : les axes ACP qui résument le plus de variance reçoivent plus de poids
# C'est une pondération dite "objective" car elle émerge des données, pas d'un jugement
# Étape a : extraire les coordonnées des régions sur les axes ACP retenus
coord_axes <- as.data.frame(acp$ind$coord[, 1:n_axes])
# Étape b : normaliser ces coordonnées en Min-Max pour les ramener dans [0,1]
coord_mm_acp <- as.data.frame(lapply(coord_axes, minmax))
# Étape c : calculer l'indice comme combinaison linéaire pondérée des axes
#           I_acp = Σ (poids_k × coordonnée_normalisée_k) pour k = 1 à n_axes
indice_acp <- as.numeric(as.matrix(coord_mm_acp) %*% poids_acp)

cat("[ Indice 2 - Pondération ACP ]\n")
cat(sprintf("  Plage : [%.3f ; %.3f] | Moyenne : %.3f\n",
            min(indice_acp), max(indice_acp), mean(indice_acp)))

# ── INDICE 3 : Pondération AHP ──────────────────────────────────────────────
# L'AHP (Analytic Hierarchy Process) fonctionne en 2 niveaux :
# NIVEAU 1 : regroupement des indicateurs par dimension thématique
#            -> calcul d'un score moyen par dimension (équipondération intra-dimension)
# NIVEAU 2 : pondération entre les dimensions selon leur importance jugée
#            -> poids issus d'une analyse par comparaison par paires (expert)

# Définition des indicateurs appartenant à chaque dimension
dim_edu <- c("taux_alpha","taux_scol_prim","taux_scol_sec","ratio_eleves_maitre")
dim_san <- c("tx_mort_inf","acces_eau","acces_assain","centres_sante")
dim_eco <- c("tx_pauvrete","revenu_moy","tx_chomage","emploi_inform","pib_regional")
dim_inf <- c("acces_electr","densite_routiere","acces_internet","idh_subnational")
dim_dem <- c("tx_fecondite","densite_pop","part_urbaine","tx_emploi_fem","surf_agri","secu_alim")

# Calcul du score de chaque région dans chaque dimension
# rowMeans calcule la moyenne des colonnes sélectionnées pour chaque ligne (région)
sc_edu <- rowMeans(data_mm[, dim_edu])  # Score Éducation (4 indicateurs)
sc_san <- rowMeans(data_mm[, dim_san])  # Score Santé (4 indicateurs)
sc_eco <- rowMeans(data_mm[, dim_eco])  # Score Économie (5 indicateurs)
sc_inf <- rowMeans(data_mm[, dim_inf])  # Score Infrastructure (4 indicateurs)
sc_dem <- rowMeans(data_mm[, dim_dem])  # Score Démographie (6 indicateurs)

# Poids AHP par dimension (justification de chaque choix) :
# - Éducation   25% : levier essentiel du développement à long terme
# - Économie    25% : capacité à générer revenus et emplois pour les ménages
# - Santé       20% : condition sine qua non du bien-être et de la productivité
# - Infrastructure 20% : connectivité et accès aux services = condition de l'activité
# - Démographie 10% : structure de population (moins directement actionnable par politique)
# VÉRIFICATION : 25 + 25 + 20 + 20 + 10 = 100% ✓
indice_ahp <- 0.25 * sc_edu +  # Éducation
              0.25 * sc_eco +  # Économie
              0.20 * sc_san +  # Santé
              0.20 * sc_inf +  # Infrastructure
              0.10 * sc_dem    # Démographie

cat("[ Indice 3 - Pondération AHP ]\n")
cat(sprintf("  Plage : [%.3f ; %.3f] | Moyenne : %.3f\n",
            min(indice_ahp), max(indice_ahp), mean(indice_ahp)))


# ── 6. ANALYSE DE SENSIBILITÉ ────────────────────────────────────────────────
cat("\n=== ÉTAPE 5 : ANALYSE DE SENSIBILITÉ ===\n")

# Calcul des rangs selon chaque méthode de pondération
# rank() attribue le rang 1 à la valeur la PLUS FAIBLE par défaut
# On utilise -indice pour inverser : rang 1 = MEILLEUR score
rang_equi <- rank(-indice_equi)  # Rang avec équipondération
rang_acp  <- rank(-indice_acp)   # Rang avec pondération ACP
rang_ahp  <- rank(-indice_ahp)   # Rang avec pondération AHP

# Tableau de comparaison des 3 classements
ranks <- data.frame(
  Region     = regions,
  Score_AHP  = round(indice_ahp, 3),
  Rang_Equi  = as.integer(rang_equi),
  Rang_ACP   = as.integer(rang_acp),
  Rang_AHP   = as.integer(rang_ahp)
)

# Variation maximale : pour chaque région, différence entre le MEILLEUR et le PIRE rang
# Variation = 0 : classement identique quelle que soit la méthode
# Variation = 3 : la région peut gagner ou perdre jusqu'à 3 places selon la méthode
ranks$Variation_max <- apply(
  ranks[, c("Rang_Equi","Rang_ACP","Rang_AHP")],
  1,  # Appliquer la fonction sur chaque LIGNE (= chaque région)
  function(x) max(x) - min(x)  # Écart entre meilleur et pire rang
)

# Tableau final trié par rang AHP croissant (1er = plus développé)
ranks_sorted <- ranks %>% arrange(Rang_AHP)
cat("\nTableau comparatif des classements :\n")
print(ranks_sorted)

# ── Corrélations de Spearman ─────────────────────────────────────────────────
# La corrélation de rang de Spearman mesure la concordance entre deux classements.
# Elle compare si les deux méthodes ordonnent les régions de la même façon.
# rho = 1   : classements parfaitement identiques
# rho > 0,95 : convergence forte -> l'indice est robuste
# rho = 0   : aucun lien entre les deux classements

rho_ea <- cor(rang_equi, rang_acp, method="spearman")
rho_eh <- cor(rang_equi, rang_ahp, method="spearman")
rho_ah <- cor(rang_acp,  rang_ahp, method="spearman")

cat("\nCorrélations de Spearman entre les 3 méthodes :\n")
cat(sprintf("  Équipondération vs ACP : rho = %.4f\n", rho_ea))
cat(sprintf("  Équipondération vs AHP : rho = %.4f\n", rho_eh))
cat(sprintf("  ACP vs AHP             : rho = %.4f\n", rho_ah))

cat(sprintf("\nRégions avec variation ≤ 1 rang  : %d/14\n", sum(ranks$Variation_max <= 1)))
cat(sprintf("Régions avec variation 2-3 rangs : %d/14\n", sum(ranks$Variation_max %in% 2:3)))
cat(sprintf("Régions avec variation ≥ 4 rangs : %d/14\n", sum(ranks$Variation_max >= 4)))

# Interprétation automatique de la robustesse
if (min(rho_ea, rho_eh, rho_ah) > 0.95) {
  cat("\n-> CONCLUSION : L'indice est ROBUSTE (tous les rho > 0,95).\n")
  cat("   Le classement des 14 régions est stable quelle que soit la méthode retenue.\n")
} else {
  cat("\n-> ATTENTION : Certaines méthodes produisent des classements divergents.\n")
  cat("   Une analyse approfondie des cas discordants est nécessaire.\n")
}


# ── 7. VISUALISATIONS ────────────────────────────────────────────────────────
cat("\n=== ÉTAPE 6 : GÉNÉRATION DES GRAPHIQUES ===\n")

# Création du dossier figures s'il n'existe pas encore
dir.create("figures", showWarnings=FALSE)

# ── 7.1 Matrice de corrélation ───────────────────────────────────────────────
# Ce graphique montre la force des corrélations entre tous les indicateurs
# Rouge foncé = corrélation positive forte | Blanc = pas de corrélation | Bleu/vert = négative

png("figures/matrice_correlation.png", width=900, height=900, res=120)
corrplot(cor_matrix,
  method      = "color",         # Carré coloré proportionnel à r
  type        = "upper",         # Afficher uniquement la moitié supérieure
  tl.cex      = 0.55,            # Taille des labels d'indicateurs
  tl.col      = "#0D1F0F",       # Couleur des labels
  col         = colorRampPalette(c("#8C1428","white","#1B6B3A"))(200),
  addgrid.col = "white",
  cl.cex      = 0.60,
  title       = "Matrice de corrélation des 23 indicateurs",
  mar         = c(0, 0, 2, 0))
dev.off()
cat("  Figure 1 sauvegardée : figures/matrice_correlation.png\n")

# ── 7.2 Éboulis des valeurs propres (Screeplot) ──────────────────────────────
# Ce graphique montre quelle proportion de variance chaque axe ACP explique
# Le "coude" dans la courbe indique combien d'axes retenir (critère visuel de Cattell)

png("figures/screeplot.png", width=800, height=500, res=120)
fviz_eig(acp,
  addlabels = TRUE,              # Afficher le % sur chaque barre
  barfill   = "#1B6B3A",        # Couleur vert des barres
  barcolor  = "#1B6B3A",
  linecolor = "#C9A84C",        # Couleur or de la ligne cumulative
  ggtheme   = theme_minimal(base_size=11)) +
  labs(
    title    = "Éboulis des valeurs propres (Screeplot ACP)",
    subtitle = paste0(n_axes, " axes retenus — ", round(ev[n_axes,3],1),
                      "% de variance expliquée"),
    x = "Composante principale", y = "Variance expliquée (%)"
  )
dev.off()
cat("  Figure 2 sauvegardée : figures/screeplot.png\n")

# ── 7.3 Biplot ACP ──────────────────────────────────────────────────────────
# Le biplot représente simultanément :
# - Les RÉGIONS (points) : leur position = leur score sur les axes ACP
# - Les INDICATEURS (flèches) : leur direction et longueur = leur contribution

png("figures/biplot.png", width=900, height=700, res=120)
fviz_pca_biplot(acp,
  repel   = TRUE,                # Éviter le chevauchement des labels
  col.var = "#C9A84C",          # Flèches des indicateurs en or
  col.ind = "#1B6B3A",          # Points des régions en vert
  label   = "all",              # Afficher tous les labels
  ggtheme = theme_minimal(base_size=9),
  title   = "Biplot ACP — Régions et indicateurs")
dev.off()
cat("  Figure 3 sauvegardée : figures/biplot.png\n")

# ── 7.4 Graphique de sensibilité ────────────────────────────────────────────
# Chaque ligne représente une région.
# Une ligne HORIZONTALE = classement stable quelle que soit la méthode.
# Des lignes qui SE CROISENT = classements qui divergent selon la méthode.

png("figures/sensibilite.png", width=900, height=650, res=120)
rm_mat <- cbind(rang_equi, rang_acp, rang_ahp)  # Matrice des rangs [14 régions × 3 méthodes]
cols14 <- colorRampPalette(c("#0D1F0F","#1B6B3A","#C9A84C","#8C1428","#78967F"))(14)

par(mar=c(4, 4.5, 3, 9.5), xpd=TRUE, bg="white")
plot(1:3, rm_mat[1,], type="n",
     xlim=c(0.8, 3.2), ylim=c(14.5, 0.5),
     xaxt="n", yaxt="n",
     xlab="Méthode de pondération",
     ylab="Rang (1 = meilleure région)",
     main="Analyse de sensibilité — Stabilité du classement selon la méthode",
     cex.main=0.90, cex.lab=0.88)

rect(0.65, 0.3, 3.35, 14.7, col="#F9F6EE", border=NA)  # Fond crème
abline(h=1:14, col="white", lwd=0.8)  # Lignes horizontales blanches de repère

axis(1, at=1:3, labels=c("Équipondération","ACP","AHP"), cex.axis=0.82)
axis(2, at=1:14, las=1, cex.axis=0.75)  # Axe vertical avec les numéros de rang

# Tracer une ligne par région avec sa couleur distinctive
for (i in 1:14) {
  lines(1:3, rm_mat[i,], col=cols14[i], lwd=2.0)
  points(1:3, rm_mat[i,], col=cols14[i], pch=19, cex=1.0)
}

# Légende à droite du graphique
legend("topright", inset=c(-0.38, 0),
       legend=regions[order(rang_ahp)],  # Triées par rang AHP
       col=cols14[order(rang_ahp)],
       lty=1, lwd=2, cex=0.50, bty="n",
       title="Régions\n(rang AHP)")
dev.off()
cat("  Figure 4 sauvegardée : figures/sensibilite.png\n")

# ── 7.5 Classement final (barplot horizontal) ────────────────────────────────
# Ce graphique montre le score AHP de chaque région sous forme de barres horizontales.
# Les régions sont triées du score le plus faible (bas) au plus élevé (haut).
# La ligne pointillée représente la moyenne nationale.

ro <- ranks_sorted[order(ranks_sorted$Score_AHP), ]  # Trier par score croissant
cols_bar <- colorRampPalette(c("#8C1428","#C9A84C","#1B6B3A"))(14)  # Rouge -> or -> vert

png("figures/classement_final.png", width=900, height=650, res=120)
par(mar=c(3.5, 8.5, 2.5, 3.5), bg="white")
bp <- barplot(ro$Score_AHP,
              names.arg = ro$Region,
              horiz     = TRUE,         # Barres horizontales
              las       = 1,            # Labels horizontaux
              col       = cols_bar,
              border    = NA,           # Pas de bordure sur les barres
              xlab      = "Score normalisé [0-1]",
              main      = "Indice composite de développement — Pondération AHP",
              cex.names = 0.78,
              xlim      = c(0, max(ro$Score_AHP) * 1.25),
              cex.main  = 0.90)

# Afficher le score numérique à droite de chaque barre
text(ro$Score_AHP + 0.012, bp,
     sprintf("%.3f", ro$Score_AHP),
     cex=0.65, col="#0D1F0F", font=2)

# Ligne de la moyenne nationale
abline(v=mean(as.numeric(indice_ahp)), lty=2, col="#C9A84C", lwd=1.5)
text(mean(as.numeric(indice_ahp))+0.01, bp[7]+0.1,
     "Moy. nationale", cex=0.60, col="#C9A84C", font=3, adj=0)
dev.off()
cat("  Figure 5 sauvegardée : figures/classement_final.png\n")


# ── 8. EXPORT DES RÉSULTATS ─────────────────────────────────────────────────
cat("\n=== ÉTAPE 7 : EXPORT DES RÉSULTATS ===\n")

# Création du dossier resultat s'il n'existe pas
dir.create("resultat", showWarnings=FALSE)

# Tableau final complet exporté en CSV
# Ce fichier pourra être réutilisé dans le rapport (.Rmd) ou partagé
write.csv(ranks_sorted, "resultat/resultats_indice.csv", row.names=FALSE)
cat("  Résultats sauvegardés : resultat/resultats_indice.csv\n")

# Résumé statistique global
cat("\n=== RÉSUMÉ GLOBAL DE L'ANALYSE ===\n")
cat(sprintf("  Indicateurs analysés              : %d\n", ncol(data_num)))
cat(sprintf("  Régions classées                  : %d\n", nrow(data_num)))
cat(sprintf("  Valeurs manquantes                : %d\n", sum(is.na(data_num))))
cat(sprintf("  Axes ACP retenus                  : %d\n", n_axes))
cat(sprintf("  Variance expliquée (ACP retenue)  : %.1f%%\n", ev[n_axes, 3]))
cat(sprintf("  rho Spearman (Équi vs AHP)        : %.4f\n", rho_eh))
cat(sprintf("  Variation max. de rang            : %d rang(s)\n", max(ranks$Variation_max)))
cat(sprintf("  Région la plus développée         : %s (score %.3f)\n",
            ranks_sorted$Region[nrow(ranks_sorted)], max(ranks_sorted$Score_AHP)))
cat(sprintf("  Région la moins développée        : %s (score %.3f)\n",
            ranks_sorted$Region[1], min(ranks_sorted$Score_AHP)))
cat("\n=== PIPELINE TERMINÉ AVEC SUCCÈS ===\n")
