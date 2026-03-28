# Pollinia — 02_data_prep.R
# Data cleaning and synthetic trait engineering

library(tidyverse)
library(janitor)

setwd("C:/Users/it-ha/pollinia")

# ── 1. Load interactions ───────────────────────────────────────────────────────

edges <- read_csv("data/processed/interactions.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    flower_species = str_trim(flower_species),
    bee_species    = str_trim(bee_species)
  ) %>%
  filter(visit_count > 0) %>%
  distinct()

# ── 2. Unique species ──────────────────────────────────────────────────────────

flowers <- unique(edges$flower_species)
bees    <- unique(edges$bee_species)
n_f     <- length(flowers)
n_b     <- length(bees)

# ── 3. Synthetic traits ────────────────────────────────────────────────────────

set.seed(42)

flower_traits <- tibble(
  flower_species    = flowers,
  tube_depth_mm     = runif(n_f, 2, 25),
  corolla_diam_mm   = runif(n_f, 5, 40),
  floral_color      = sample(c("UV_yellow", "white", "pink_purple", "red", "yellow"), n_f, replace = TRUE),
  nectar_vol_ul     = runif(n_f, 0, 15),
  nectar_sugar_pct  = runif(n_f, 10, 60),
  symmetry          = sample(c("actinomorphic", "zygomorphic"), n_f, replace = TRUE, prob = c(0.6, 0.4))
)

bee_traits <- tibble(
  bee_species       = bees,
  tongue_length_mm  = runif(n_b, 2, 20),
  body_length_mm    = runif(n_b, 5, 22),
  sociality         = sample(c("social", "solitary"), n_b, replace = TRUE, prob = c(0.4, 0.6)),
  lecty             = sample(c("polylectic", "oligolectic"), n_b, replace = TRUE, prob = c(0.7, 0.3)),
  flight_range_m    = runif(n_b, 100, 3000)
)

# ── 4. Join traits onto edges ──────────────────────────────────────────────────

edges_full <- edges %>%
  left_join(flower_traits, by = "flower_species") %>%
  left_join(bee_traits,    by = "bee_species")

# ── 5. Save ────────────────────────────────────────────────────────────────────

write_csv(edges_full,   "data/processed/interactions.csv")
write_csv(flower_traits, "data/processed/flower_traits.csv")
write_csv(bee_traits,    "data/processed/bee_traits.csv")

# ── 6. Summary ─────────────────────────────────────────────────────────────────

cat("\n── Pollinia data prep complete ──────────────────────\n")
cat(sprintf("  Unique flower species : %d\n",  n_f))
cat(sprintf("  Unique bee species    : %d\n",  n_b))
cat(sprintf("  Total interactions    : %d\n",  nrow(edges_full)))
cat(sprintf("  Networks              : %d\n",  n_distinct(edges_full$network_id)))
cat("─────────────────────────────────────────────────────\n")
cat("\nFirst 5 rows of joined dataset:\n")
print(edges_full %>% select(flower_species, bee_species, visit_count, network_id,
                             tube_depth_mm, tongue_length_mm, floral_color, sociality) %>%
        head(5), width = 120)
