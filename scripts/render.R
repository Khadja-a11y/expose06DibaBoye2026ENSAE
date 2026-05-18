# render.R — Compilation du rapport et du Beamer
required <- c("rmarkdown","knitr","tinytex")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg)
}

rmarkdown::render(
  input = "rapport/rmdWorkingFile.Rmd",
  output_file = "rmdWorkingFile.pdf",
  output_dir = "rapport",
  clean = TRUE
)

rmarkdown::render(
  input = "beamer/beamer.Rmd",
  output_file = "beamer.pdf",
  output_dir = "beamer",
  clean = TRUE
)

cat("\nRapport et Beamer compilés avec succès.\n")