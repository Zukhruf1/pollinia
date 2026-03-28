# Pollinia — Trait-mediated bee-flower interaction ML
# Anchor paper: Pichler et al. (2020) Methods in Ecology, 198 citations
# Data: Web of Life database (web-of-life.es) + bipartite built-in networks

# Use user library (avoids need for admin rights)
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

pkgs <- c(
  "bipartite", "igraph", "vegan",
  "caret", "randomForest", "e1071", "glmnet", "pROC",
  "tidyverse", "janitor", "httr", "jsonlite",
  "ggraph", "ggplot2", "patchwork", "scales", "viridis",
  "shiny", "bslib", "networkD3", "DT", "plotly", "shinycssloaders",
  "rmarkdown", "knitr"
)

for (pkg in pkgs) {
  if (!pkg %in% installed.packages()[, "Package"]) {
    install.packages(pkg, repos = "https://cran.r-project.org", lib = user_lib)
    cat("Installed:", pkg, "\n")
  } else {
    cat("Already installed:", pkg, "\n")
  }
}

# Verify
failed <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(failed) == 0) {
  cat("\nAll", length(pkgs), "packages verified OK.\n")
} else {
  cat("\nFailed to load:", paste(failed, collapse = ", "), "\n")
}
