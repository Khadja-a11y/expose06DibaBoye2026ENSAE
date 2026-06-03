# 🗺️ Indice Composite Robuste — 14 Régions du Sénégal

<div align="center">

![ENSAE Dakar](https://img.shields.io/badge/ENSAE-Dakar-8B0000?style=for-the-badge&logo=graduation-cap)
![Cours](https://img.shields.io/badge/Cours-Traitement_Statistique_avec_R-D4AF37?style=for-the-badge)
![Niveau](https://img.shields.io/badge/Niveau-ISEP2-8B0000?style=for-the-badge)
![Année](https://img.shields.io/badge/Année-2025--2026-D4AF37?style=for-the-badge)

**Thème 6 · expose06DibaBoye2026ENSAE**

*Dans quelle mesure le classement des régions dépend-il des choix méthodologiques plutôt que de la réalité ?*

</div>

---

## 📌 Résumé du projet

Ce projet construit un **indice composite de développement local** pour les **14 régions du Sénégal** à partir de **23 indicateurs** issus de **5 sources**. L'objectif central n'est pas seulement de produire un classement — c'est d'évaluer dans quelle mesure ce classement est **robuste** aux choix méthodologiques (normalisation, pondération, réduction dimensionnelle).

> **Résultat principal :** ρ de Spearman > 0,99 entre les trois systèmes de pondération. Variation maximale d'un seul rang. Le gradient **Nord-Ouest / Sud-Est** est une réalité structurelle du Sénégal, pas un artefact statistique.

---

## 🗂️ Structure du dépôt

```
expose06DibaBoye2026ENSAE/
│
├── 📊 beamer.pdf              ← Présentation Beamer (36 slides)
├── 📝 scriptRtraitement.R     ← Script R commenté — pipeline complet
├── 📄 rmdWorkingFile.Rmd      ← Rapport R Markdown source
├── 📑 rmdWorkingFile.pdf      ← Rapport PDF généré
└── 📋 donnees_regions.csv     ← Base de données simulée (set.seed(2026))
```

---

## 🔬 Méthodologie en 5 étapes

### Étape 1 — Audit des 23 indicateurs
Vérification de la qualité des données avant toute analyse.

| Outil | Seuil | Résultat |
|-------|-------|----------|
| Taux de valeurs manquantes | > 20% → exclusion | ✅ 0 manquant |
| Coefficient de variation | < 5% → exclusion | ✅ 0 quasi-constant |
| Corrélation de Pearson | \|r\| > 0,95 → redondant | ✅ 0 paire redondante |

**→ Les 23 indicateurs sont conservés intégralement.**

---

### Étape 2 — Normalisation
Comparaison de trois méthodes pour mettre tous les indicateurs à la même échelle.

```
Min-Max   →  x* = (x - x_min) / (x_max - x_min)   ∈ [0, 1]
Z-score   →  z  = (x - x̄) / s_x
Centile   →  c  = rang(x) / n
```

> ⚠️ **Piège identifié :** 5 indicateurs à sens négatif (mortalité, pauvreté, chômage, emploi informel, fécondité) sont inversés (× −1) avant normalisation pour que **score élevé = bonne situation** soit universel.

**→ Min-Max retenue** pour sa lisibilité directe entre 0 et 1.

---

### Étape 3 — ACP (FactoMineR)
Réduction de dimensionnalité et identification des structures latentes.

| Axe | Valeur propre λ | Variance | Cumulé | Interprétation |
|-----|----------------|----------|--------|----------------|
| Dim. 1 | 20,03 | **93,8%** | 93,8% | Développement humain global |
| Dim. 2 | 1,01 | 4,7% | 98,5% | Infrastructure & connectivité |
| Dim. 3 | 0,19 | 0,9% | 99,4% | Structure démographique |

> **Message clé :** L'Axe 1 capte seul 93,8% de la variance. Le développement sénégalais est essentiellement **unidimensionnel** — les régions s'alignent de Dakar à Kédougou sur un seul gradient global.

---

### Étape 4 — Construction de l'indice (3 pondérations)

| Système | Méthode | Principe | Limite |
|---------|---------|----------|--------|
| **Équipondération** | w_i = 1/23 | Simple, transparent | Choix politique implicite |
| **Pondération ACP** | w_k ∝ λ_k | Objectif, basé sur les données | Sur-pondère l'Axe 1 dominant |
| **Pondération AHP** ✅ | Expert (Saaty 1980) | Éducation/Éco 25% · Santé/Infra 20% · Démo 10% | Subjectif mais justifié |

> ⚠️ **Piège fondamental :** L'équipondération *semble* neutre — elle ne l'est pas. Donner le même poids à la mortalité infantile et à la superficie agricole est un **choix politique implicite**, pas une neutralité.

---

### Étape 5 — Analyse de sensibilité

```r
# Corrélations de Spearman entre les 3 classements
cor(rank_equi, rank_acp,  method = "spearman")  # → 0.996
cor(rank_equi, rank_ahp,  method = "spearman")  # → 0.996
cor(rank_acp,  rank_ahp,  method = "spearman")  # → 1.000
```

**→ L'indice est robuste.** Variation maximale = 1 rang entre deux méthodes.

---

## 📊 Classement final (Pondération AHP)

```
🥇  1. Dakar         0,871  ████████████████████████████████████████
🥈  2. Thiès         0,519  █████████████████████
🥉  3. Saint-Louis   0,462  ██████████████████
    4. Ziguinchor    0,427  █████████████████
    5. Kaolack       0,388  ███████████████
    6. Diourbel      0,297  ████████████
    7. Fatick        0,296  ████████████
    ─────────────── moyenne nationale (0,296) ───────────────
    8. Louga         0,233  █████████
    9. Matam         0,213  ████████
   10. Tambacounda   0,199  ████████
   11. Kaffrine      0,193  ███████
   12. Kolda         0,164  ██████
   13. Sédhiou       0,161  ██████
   14. Kédougou      0,129  █████
```

> **Paradoxe des ressources :** Kédougou est riche en or et en biodiversité, mais affiche l'IDH le plus faible du pays. Les retombées minières ne bénéficient pas aux populations locales.

---

## 📦 Données

### Sources
| Source | Données | Accès |
|--------|---------|-------|
| ANSD / RGPH 2023 | Démographie, éducation | Partiel (PDF) |
| EHCVM 2021 | Pauvreté, emploi, revenus | Accès restreint |
| PNUD — IDH sous-national | IDH régional | Public |
| Banque Mondiale DataBank | PIB, infrastructure | Public |
| OpenAfrica | Données ouvertes sénégalaises | Public |

### Pourquoi des données simulées ?
Les microdonnées régionales exhaustives ne sont pas toutes publiquement accessibles au moment de ce travail. La simulation est :
- **Calibrée** sur les rapports officiels ANSD disponibles
- **Reproductible** via `set.seed(2026)`
- **Cohérente** avec le gradient Nord-Ouest / Sud-Est documenté dans la littérature

> Ce travail teste une **méthode**, pas des statistiques officielles. La robustesse des résultats reste valide.

---

## ⚙️ Reproductibilité

### Prérequis R
```r
# Packages nécessaires
install.packages(c("FactoMineR", "factoextra", "ggplot2", "sf"))
```

> ⚠️ Le script est écrit en **base R** (sans tidyverse) pour compatibilité maximale.  
> Tous les chemins sont **relatifs** — pas de `setwd()` avec chemin absolu.

### Lancer l'analyse
```r
# Cloner le dépôt, puis :
source("scriptRtraitement.R")

# Ou générer le rapport complet :
rmarkdown::render("rmdWorkingFile.Rmd")
```

---

## 🚧 Limites identifiées

| Limite | Impact | Amélioration possible |
|--------|--------|----------------------|
| Données simulées | Résultats non officiels | Accès RGPH 2023 complet + microdonnées EHCVM |
| Poids AHP subjectifs | Sensibilité aux jugements d'expert | Consultation parties prenantes régionales |
| Pas de dimension temporelle | Pas d'évolution dans le temps | Données panel 2015–2023 |
| Données transversales | Snapshot unique | Analyse des tendances |

---

## 📚 Références

- **Nardo et al. (2005)** — *Handbook on Constructing Composite Indicators*. OCDE/JRC.
- **Saaty, T.L. (1980)** — *The Analytic Hierarchy Process*. McGraw-Hill.
- **Sen, A. (1999)** — *Development as Freedom*. Oxford University Press.
- **Ravallion, M. (2012)** — *Troubling tradeoffs in the Human Development Index*. Journal of Development Economics.
- **ANSD (2023)** — *Résultats préliminaires RGPH 2023*. Dakar : ANSD.
- **PNUD (2023)** — *Rapport sur le développement humain 2023*. New York : PNUD.

---

## 👩‍💻 Auteure

<div align="center">

**Boye DIBA**  
Étudiante ISEP2 — ENSAE Dakar  
Traitement Statistique avec R · 2025-2026  

</div>

---

<div align="center">

*expose06DibaBoye2026ENSAE · ENSAE Dakar · 2025-2026*

</div>
