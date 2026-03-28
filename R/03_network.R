# Pollinia — 03_network.R
# Bipartite network analysis

library(bipartite)
library(igraph)
library(tidyverse)
library(ggraph)
library(ggplot2)

setwd("C:/Users/it-ha/pollinia")

# ── 2. Load network matrix ─────────────────────────────────────────────────────

web <- readRDS("data/processed/network_matrix.rds")

# ── 3. Network-level metrics ───────────────────────────────────────────────────

net_metrics <- networklevel(
  web,
  index = c("connectance", "links per species", "nestedness", "H2", "modularity")
)

cat("\n── Network-level metrics ─────────────────────────────\n")
net_df <- tibble(metric = names(net_metrics), value = round(as.numeric(net_metrics), 4))
print(net_df, n = Inf)

saveRDS(net_metrics, "data/processed/net_metrics.rds")

# ── 4. Species-level metrics ───────────────────────────────────────────────────

sp_metrics <- specieslevel(
  web,
  index = c("degree", "normalised degree", "betweenness", "closeness", "d")
)

flower_metrics <- sp_metrics$`lower level` %>%
  rownames_to_column("species")
bee_metrics    <- sp_metrics$`higher level` %>%
  rownames_to_column("species")

write_csv(flower_metrics, "data/processed/flower_metrics.csv")
write_csv(bee_metrics,    "data/processed/bee_metrics.csv")

# ── 5. Nestedness (NODF) ───────────────────────────────────────────────────────

nodf_res <- nestednodf(web)
nodf_val <- nodf_res$statistic["NODF"]
cat(sprintf("\n── Nestedness (NODF): %.4f\n", nodf_val))

# ── 6. Co-extinction robustness ────────────────────────────────────────────────

set.seed(42)
ext_random <- second.extinct(web, participant = "higher",
                             method = "random", nrep = 50, details = FALSE)
rob <- robustness(ext_random)
cat(sprintf("── Robustness (R50 random):  %.4f\n", rob))

# ── 7. Top 5 keystone species by betweenness ──────────────────────────────────

top_bees <- bee_metrics %>%
  arrange(desc(betweenness)) %>%
  select(species, degree, betweenness, closeness) %>%
  head(5)

top_flowers <- flower_metrics %>%
  arrange(desc(betweenness)) %>%
  select(species, degree, betweenness, closeness) %>%
  head(5)

cat("\n── Top 5 keystone BEES (by betweenness) ─────────────\n")
print(top_bees)
cat("\n── Top 5 keystone FLOWERS (by betweenness) ──────────\n")
print(top_flowers)

# ── 8a. Plot A — Bipartite network graph ──────────────────────────────────────

g <- graph_from_incidence_matrix(web, weighted = TRUE)

# Node attributes
V(g)$type_label <- ifelse(V(g)$type, "Bee", "Flower")
V(g)$color_fill <- ifelse(V(g)$type, "#EF9F27", "#8BAF7C")

# Degree for node sizing
deg <- degree(g)
V(g)$deg <- deg

set.seed(1)
p_net <- ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(width = weight), color = "#F4C0D1", alpha = 0.6) +
  geom_node_point(aes(size = deg, color = type_label), alpha = 0.9) +
  scale_color_manual(values = c("Bee" = "#EF9F27", "Flower" = "#8BAF7C"),
                     name = "Guild") +
  scale_edge_width(range = c(0.2, 2.5), guide = "none") +
  scale_size(range = c(2, 8), name = "Degree") +
  labs(title = "Pollinia \u2014 Bee-Flower Interaction Network") +
  theme_graph(background = "#FEFBF0") +
  theme(
    plot.title    = element_text(size = 14, face = "bold", hjust = 0.5,
                                 color = "#3D3D3D", margin = margin(b = 10)),
    legend.position = "right"
  )

ggsave("outputs/figures/01_network.png", p_net,
       width = 10, height = 7, dpi = 150, bg = "#FEFBF0")

# ── 8b. Plot B — Degree distribution ─────────────────────────────────────────

deg_df <- bind_rows(
  bee_metrics    %>% select(species, degree) %>% mutate(guild = "Bee"),
  flower_metrics %>% select(species, degree) %>% mutate(guild = "Flower")
)

p_deg <- ggplot(deg_df, aes(x = degree, fill = guild)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.85) +
  facet_wrap(~guild, scales = "free_y") +
  scale_fill_manual(values = c("Bee" = "#EF9F27", "Flower" = "#8BAF7C"),
                    guide = "none") +
  labs(title = "Degree distribution across species",
       x = "Degree", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#FEFBF0", color = NA),
    panel.background = element_rect(fill = "#FEFBF0", color = NA),
    strip.text       = element_text(face = "bold", color = "#3D3D3D"),
    plot.title       = element_text(hjust = 0.5, face = "bold", color = "#3D3D3D")
  )

ggsave("outputs/figures/02_degree_dist.png", p_deg,
       width = 8, height = 4, dpi = 150, bg = "#FEFBF0")

cat("\n── Plots saved ───────────────────────────────────────\n")
cat("  outputs/figures/01_network.png\n")
cat("  outputs/figures/02_degree_dist.png\n")
cat("─────────────────────────────────────────────────────\n")
