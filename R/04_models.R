# Pollinia — 04_models.R
# ML feature engineering and model training

library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(glmnet)
library(pROC)

setwd("C:/Users/it-ha/pollinia")

# ══════════════════════════════════════════════════════════════════════════════
# PART A — Feature matrix
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Load data ───────────────────────────────────────────────────────────────

interactions   <- read_csv("data/processed/interactions.csv",   show_col_types = FALSE)
flower_traits  <- read_csv("data/processed/flower_traits.csv",  show_col_types = FALSE)
bee_traits     <- read_csv("data/processed/bee_traits.csv",     show_col_types = FALSE)
flower_metrics <- read_csv("data/processed/flower_metrics.csv", show_col_types = FALSE) %>%
  rename(flower_species = species,
         flower_degree = degree, flower_betweenness = betweenness,
         flower_closeness = closeness, flower_nd = `normalised.degree`,
         flower_d = d)
bee_metrics    <- read_csv("data/processed/bee_metrics.csv",    show_col_types = FALSE) %>%
  rename(bee_species = species,
         bee_degree = degree, bee_betweenness = betweenness,
         bee_closeness = closeness, bee_nd = `normalised.degree`,
         bee_d = d)

# ── 2. Positive samples ────────────────────────────────────────────────────────

positives <- interactions %>%
  select(flower_species, bee_species) %>%
  distinct() %>%
  mutate(interacts = 1L)

# ── 3. Negative samples (1:2 ratio) ───────────────────────────────────────────

set.seed(42)
all_pairs <- crossing(
  flower_species = unique(positives$flower_species),
  bee_species    = unique(positives$bee_species)
)

negatives <- all_pairs %>%
  anti_join(positives, by = c("flower_species", "bee_species")) %>%
  slice_sample(n = 2 * nrow(positives)) %>%
  mutate(interacts = 0L)

full_df <- bind_rows(positives, negatives)

# ── 4. Join traits and metrics ────────────────────────────────────────────────

full_df <- full_df %>%
  left_join(flower_traits,  by = "flower_species") %>%
  left_join(bee_traits,     by = "bee_species") %>%
  left_join(flower_metrics %>% select(flower_species, flower_degree, flower_betweenness, flower_closeness),
            by = "flower_species") %>%
  left_join(bee_metrics    %>% select(bee_species, bee_degree, bee_betweenness, bee_closeness),
            by = "bee_species") %>%
  replace_na(list(flower_degree = 0, flower_betweenness = 0, flower_closeness = 0,
                  bee_degree    = 0, bee_betweenness    = 0, bee_closeness    = 0))

# ── 5. Trait-matching features ────────────────────────────────────────────────

full_df <- full_df %>%
  mutate(
    tongue_tube_ratio  = tongue_length_mm / tube_depth_mm,
    tongue_tube_match  = abs(tongue_length_mm - tube_depth_mm),
    body_flower_ratio  = body_length_mm / corolla_diam_mm,
    degree_product     = flower_degree * bee_degree,
    centrality_sum     = flower_betweenness + bee_betweenness
  )

# ── 6. One-hot encode categoricals ────────────────────────────────────────────

full_df <- full_df %>%
  mutate(
    color_UV_yellow   = as.integer(floral_color == "UV_yellow"),
    color_white       = as.integer(floral_color == "white"),
    color_pink_purple = as.integer(floral_color == "pink_purple"),
    color_red         = as.integer(floral_color == "red"),
    color_yellow      = as.integer(floral_color == "yellow"),
    sym_actino        = as.integer(symmetry == "actinomorphic"),
    soc_social        = as.integer(sociality == "social"),
    lec_polylectic    = as.integer(lecty == "polylectic")
  )

# ── 7. Drop identifier and raw categorical columns ────────────────────────────

ml_df <- full_df %>%
  select(-flower_species, -bee_species, -floral_color, -symmetry,
         -sociality, -lecty) %>%
  select(-any_of("network_id"))

# ── 8. Outcome factor ─────────────────────────────────────────────────────────

ml_df <- ml_df %>%
  mutate(interacts_f = factor(interacts, levels = c(0, 1), labels = c("no", "yes"))) %>%
  select(-interacts)

# ── 9. Drop NAs and save ──────────────────────────────────────────────────────

ml_df <- drop_na(ml_df)
write_csv(ml_df, "data/processed/ml_features.csv")

cat("\n── Feature matrix ────────────────────────────────────\n")
cat(sprintf("  Rows    : %d\n", nrow(ml_df)))
cat(sprintf("  Columns : %d (incl. outcome)\n", ncol(ml_df)))
cat("  Class balance:\n")
print(table(ml_df$interacts_f))
cat("──────────────────────────────────────────────────────\n")

# ══════════════════════════════════════════════════════════════════════════════
# PART B — Model training
# ══════════════════════════════════════════════════════════════════════════════

# ── 11. Train/test split ───────────────────────────────────────────────────────

set.seed(42)
train_idx <- createDataPartition(ml_df$interacts_f, p = 0.75, list = FALSE)
train_df  <- ml_df[ train_idx, ]
test_df   <- ml_df[-train_idx, ]

X_test <- test_df %>% select(-interacts_f)
y_test <- test_df$interacts_f

# ── 12. trainControl ──────────────────────────────────────────────────────────

ctrl <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# ── 13. Train models ──────────────────────────────────────────────────────────

cat("\nTraining Random Forest...\n")
set.seed(42)
rf_model <- train(interacts_f ~ ., data = train_df,
                  method = "rf", metric = "ROC",
                  tuneLength = 3, trControl = ctrl,
                  importance = TRUE)

cat("Training Logistic Regression (glmnet)...\n")
set.seed(42)
lr_model <- train(interacts_f ~ ., data = train_df,
                  method = "glmnet", metric = "ROC",
                  tuneLength = 5, trControl = ctrl)

cat("Training SVM...\n")
set.seed(42)
svm_model <- train(interacts_f ~ ., data = train_df,
                   method = "svmRadial", metric = "ROC",
                   tuneLength = 3, trControl = ctrl)

# ── 14. Evaluate on test set ──────────────────────────────────────────────────

eval_model <- function(model, X, y, label) {
  preds  <- predict(model, X)
  probs  <- predict(model, X, type = "prob")[, "yes"]
  cm     <- confusionMatrix(preds, y, positive = "yes")
  roc_obj <- roc(as.integer(y == "yes"), probs, quiet = TRUE)
  tibble(
    Model     = label,
    Accuracy  = round(cm$overall["Accuracy"], 4),
    Precision = round(cm$byClass["Precision"], 4),
    Recall    = round(cm$byClass["Recall"], 4),
    F1        = round(cm$byClass["F1"], 4),
    AUC       = round(auc(roc_obj), 4)
  )
}

benchmark <- bind_rows(
  eval_model(rf_model,  X_test, y_test, "Random Forest"),
  eval_model(lr_model,  X_test, y_test, "Logistic Reg"),
  eval_model(svm_model, X_test, y_test, "SVM")
)

cat("\n── Model benchmark ───────────────────────────────────\n")
print(benchmark, width = 120)
write_csv(benchmark, "outputs/models/benchmark_results.csv")

# ── 15. Feature importance ────────────────────────────────────────────────────

fi_raw <- varImp(rf_model)$importance
fi <- data.frame(
  feature    = rownames(fi_raw),
  importance = rowMeans(fi_raw)
) %>%
  arrange(desc(importance)) %>%
  head(15)

cat("\n── Top 15 features (RF importance) ──────────────────\n")
print(fi, n = 15)

# ── 16. Save models ───────────────────────────────────────────────────────────

saveRDS(rf_model,  "outputs/models/rf_model.rds")
saveRDS(lr_model,  "outputs/models/lr_model.rds")
saveRDS(svm_model, "outputs/models/svm_model.rds")

# ── 17a. Plot — Model benchmark ───────────────────────────────────────────────

bench_long <- benchmark %>%
  pivot_longer(-Model, names_to = "Metric", values_to = "Value")

p_bench <- ggplot(bench_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.9) +
  scale_fill_manual(values = c(
    "Random Forest" = "#EF9F27",
    "Logistic Reg"  = "#8BAF7C",
    "SVM"           = "#C8C0E8"
  )) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Model benchmark \u2014 Pollinia",
       x = NULL, y = "Score", fill = "Model") +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#FEFBF0", color = NA),
    panel.background = element_rect(fill = "#FEFBF0", color = NA),
    plot.title       = element_text(hjust = 0.5, face = "bold", color = "#3D3D3D"),
    legend.position  = "right"
  )

ggsave("outputs/figures/03_benchmark.png", p_bench,
       width = 9, height = 5, dpi = 150, bg = "#FEFBF0")

# ── 17b. Plot — Feature importance ────────────────────────────────────────────

p_fi <- ggplot(fi, aes(x = importance, y = reorder(feature, importance))) +
  geom_col(fill = "#8BAF7C", alpha = 0.9) +
  labs(title = "What predicts a bee visiting a flower?",
       x = "Importance", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#FEFBF0", color = NA),
    panel.background = element_rect(fill = "#FEFBF0", color = NA),
    plot.title       = element_text(hjust = 0.5, face = "bold", color = "#3D3D3D")
  )

ggsave("outputs/figures/04_feature_importance.png", p_fi,
       width = 8, height = 6, dpi = 150, bg = "#FEFBF0")

cat("\n── Files saved ───────────────────────────────────────\n")
cat("  outputs/models/rf_model.rds\n")
cat("  outputs/models/lr_model.rds\n")
cat("  outputs/models/svm_model.rds\n")
cat("  outputs/models/benchmark_results.csv\n")
cat("  outputs/figures/03_benchmark.png\n")
cat("  outputs/figures/04_feature_importance.png\n")
cat("─────────────────────────────────────────────────────\n")
