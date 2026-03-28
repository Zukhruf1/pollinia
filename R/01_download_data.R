options(timeout = 300)
library(bipartite)
library(tidyverse)

dir.create("data/raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Load ALL built-in pollination networks from bipartite package
# These are real peer-reviewed datasets — same ones used in Pichler et al. 2020
all_nets <- data(package = "bipartite")$results[, "Item"]

saved <- 0
all_edges <- list()

for (net in all_nets) {
  tryCatch({
    e <- new.env()
    data(list = net, package = "bipartite", envir = e)
    obj <- get(net, envir = e)
    if (is.matrix(obj) || is.data.frame(obj)) {
      write.csv(obj, paste0("data/raw/", net, ".csv"))
      df <- as.data.frame(obj) %>%
        rownames_to_column("flower_species") %>%
        pivot_longer(-flower_species, names_to = "bee_species", values_to = "visit_count") %>%
        filter(visit_count > 0) %>%
        mutate(network_id = net)
      all_edges[[net]] <- df
      saved <- saved + 1
      cat("Saved:", net, "\n")
    }
  }, error = function(e) NULL)
}

# Combine all networks
edges_all <- bind_rows(all_edges)
write_csv(edges_all, "data/processed/interactions.csv")

# Save primary network matrix (Safariland) for bipartite functions
data("Safariland", package = "bipartite")
saveRDS(Safariland, "data/processed/network_matrix.rds")

cat("\n-- Summary ------------------------------\n")
cat("Networks loaded:         ", saved, "\n")
cat("Total interactions:      ", nrow(edges_all), "\n")
cat("Unique flower species:   ", n_distinct(edges_all$flower_species), "\n")
cat("Unique bee species:      ", n_distinct(edges_all$bee_species), "\n")
cat("-----------------------------------------\n")
cat("Phase 1 complete! Ready for Phase 2\n")
