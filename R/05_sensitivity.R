# Pollinia — 05_sensitivity.R
# Training-size sensitivity analysis (OMGenomics benchmarking approach)

library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(patchwork)

setwd("C:/Users/it-ha/pollinia")
set.seed(42)
options(timeout = 300)

# ── 2. Load data ───────────────────────────────────────────────────────────────

ml_data <- read_csv("data/processed/ml_features.csv", show_col_types = FALSE) %>%
  mutate(interacts_f = factor(interacts_f, levels = c("no", "yes")))

# ── 3. Fixed 20% holdout ──────────────────────────────────────────────────────

set.seed(42)
test_idx <- createDataPartition(ml_data$interacts_f, p = 0.20, list = FALSE)
test_df  <- ml_data[ test_idx, ]
pool_df  <- ml_data[-test_idx, ]

X_test <- test_df %>% select(-interacts_f)
y_test <- test_df$interacts_f

cat(sprintf("Holdout test set: %d rows | Training pool: %d rows\n",
            nrow(test_df), nrow(pool_df)))

# ── 4. Sweep config ───────────────────────────────────────────────────────────

fractions <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)
n_reps    <- 5

ctrl <- trainControl(
  method          = "cv",
  number          = 3,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter     = FALSE
)

# ── 5. Sweep ──────────────────────────────────────────────────────────────────

results <- list()

for (frac in fractions) {
  for (rep in seq_len(n_reps)) {
    tryCatch({
      set.seed(rep * 100 + round(frac * 1000))

      sub_df <- pool_df %>% slice_sample(prop = frac)

      model <- train(
        interacts_f ~ .,
        data      = sub_df,
        method    = "rf",
        metric    = "ROC",
        tuneGrid  = data.frame(mtry = 3),
        trControl = ctrl
      )

      probs   <- predict(model, X_test, type = "prob")[, "yes"]
      preds   <- predict(model, X_test)
      roc_obj <- roc(as.integer(y_test == "yes"), probs, quiet = TRUE)
      cm      <- confusionMatrix(preds, y_test, positive = "yes")

      auc_val <- as.numeric(auc(roc_obj))
      f1_val  <- as.numeric(cm$byClass["F1"])
      acc_val <- as.numeric(cm$overall["Accuracy"])

      cat(sprintf("Fraction %.2f  rep %d  —  AUC: %.3f  F1: %.3f\n",
                  frac, rep, auc_val, f1_val))

      results[[length(results) + 1]] <- tibble(
        fraction = frac,
        rep      = rep,
        n_train  = nrow(sub_df),
        AUC      = auc_val,
        F1       = f1_val,
        Accuracy = acc_val
      )
    }, error = function(e) {
      cat(sprintf("Fraction %.2f  rep %d  —  SKIPPED (%s)\n",
                  frac, rep, conditionMessage(e)))
    })
  }
}

sweep_df <- bind_rows(results)
write_csv(sweep_df, "outputs/models/sensitivity_results.csv")

# ── 6. Summarise ──────────────────────────────────────────────────────────────

summary_df <- sweep_df %>%
  group_by(fraction) %>%
  summarise(
    n_train  = mean(n_train),
    auc_mean = mean(AUC,      na.rm = TRUE),
    auc_sd   = sd(AUC,        na.rm = TRUE),
    f1_mean  = mean(F1,       na.rm = TRUE),
    f1_sd    = sd(F1,         na.rm = TRUE),
    .groups  = "drop"
  )

write_csv(summary_df, "outputs/models/sensitivity_summary.csv")

cat("\n── Sensitivity summary ───────────────────────────────\n")
print(summary_df %>% select(fraction, auc_mean, auc_sd, f1_mean, f1_sd),
      digits = 3, n = Inf)

# ── 7. Minimum fraction for AUC >= 0.70 ──────────────────────────────────────

threshold_row <- summary_df %>%
  filter(auc_mean >= 0.70) %>%
  slice_min(fraction, n = 1)

if (nrow(threshold_row) > 0) {
  cat(sprintf("\nMinimum training data for AUC >= 0.70: %.0f%%\n",
              threshold_row$fraction * 100))
} else {
  cat("\nAUC >= 0.70 not reached at any tested fraction.\n")
}

# ── 8. Plots ──────────────────────────────────────────────────────────────────

bg    <- "#FEFBF0"
amber <- "#EF9F27"
sage  <- "#8BAF7C"
red   <- "#E24B4A"

base_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    plot.title       = element_text(hjust = 0.5, face = "bold", color = "#3D3D3D"),
    axis.title       = element_text(color = "#3D3D3D"),
    panel.grid.minor = element_blank()
  )

p_auc <- ggplot(summary_df, aes(x = fraction * 100)) +
  geom_ribbon(aes(ymin = pmax(auc_mean - auc_sd, 0),
                  ymax = pmin(auc_mean + auc_sd, 1)),
              fill = amber, alpha = 0.2) +
  geom_line(aes(y = auc_mean),  color = amber, linewidth = 1.1) +
  geom_point(aes(y = auc_mean), color = amber, size = 3) +
  geom_hline(yintercept = 0.70, linetype = "dashed", color = red, linewidth = 0.8) +
  annotate("text", x = 12, y = 0.715, label = "AUC = 0.70 threshold",
           color = red, size = 3.2, hjust = 0) +
  scale_x_continuous(breaks = fractions * 100) +
  coord_cartesian(ylim = c(0.4, 1.0)) +
  labs(title = "Learning curve \u2014 AUC",
       x = "Training data used (%)", y = "AUC-ROC") +
  base_theme

p_f1 <- ggplot(summary_df, aes(x = fraction * 100)) +
  geom_ribbon(aes(ymin = pmax(f1_mean - f1_sd, 0),
                  ymax = pmin(f1_mean + f1_sd, 1)),
              fill = sage, alpha = 0.2) +
  geom_line(aes(y = f1_mean),  color = sage, linewidth = 1.1) +
  geom_point(aes(y = f1_mean), color = sage, size = 3) +
  scale_x_continuous(breaks = fractions * 100) +
  coord_cartesian(ylim = c(0, 0.8)) +
  labs(title = "Learning curve \u2014 F1",
       x = "Training data used (%)", y = "F1 score") +
  base_theme

p_combined <- p_auc / p_f1 +
  plot_annotation(
    title = "Pollinia \u2014 Sensitivity analysis",
    theme = theme(
      plot.title      = element_text(hjust = 0.5, face = "bold",
                                     size = 14, color = "#3D3D3D"),
      plot.background = element_rect(fill = bg, color = NA)
    )
  )

ggsave("outputs/figures/05_sensitivity.png", p_combined,
       width = 8, height = 8, dpi = 150, bg = bg)

cat("\n── Saved ─────────────────────────────────────────────\n")
cat("  outputs/models/sensitivity_results.csv\n")
cat("  outputs/models/sensitivity_summary.csv\n")
cat("  outputs/figures/05_sensitivity.png\n")
cat("─────────────────────────────────────────────────────\n")
