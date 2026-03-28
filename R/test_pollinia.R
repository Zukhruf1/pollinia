# test_pollinia.R — Pollinia automated test suite

setwd("C:/Users/it-ha/pollinia")
library(bipartite)
library(caret)
library(randomForest)
library(pROC)

`%||%` <- function(a, b) if (!is.null(a)) a else b

cat("\n════════════════════════════════\n")
cat("POLLINIA AUTOMATED TEST SUITE\n")
cat("════════════════════════════════\n")

pass_count <- 0
fail_count <- 0

check <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) {
    cat("  ERROR:", e$message, "\n"); FALSE
  })
  if (isTRUE(result)) {
    cat("  PASS:", label, "\n")
    pass_count <<- pass_count + 1
  } else {
    cat("  FAIL:", label, "\n")
    fail_count <<- fail_count + 1
  }
}

# ── TEST 1: Data integrity ────────────────────────────────────────────────────

cat("\n[TEST 1a] Interactions CSV\n")
edges <- read.csv("data/processed/interactions.csv", stringsAsFactors=FALSE)
cat("  Rows:", nrow(edges), "\n")
cat("  Unique flowers:", length(unique(edges$flower_species)), "\n")
cat("  Unique bees:", length(unique(edges$bee_species)), "\n")
cat("  Networks:", length(unique(edges$network_id)), "\n")
check("rows > 4000",          nrow(edges) > 4000)
check("flowers >= 9",         length(unique(edges$flower_species)) >= 9)
check("bees column exists",   "bee_species" %in% names(edges))
check("visit_count exists",   "visit_count" %in% names(edges))

cat("\n[TEST 1b] ML features CSV\n")
ml <- read.csv("data/processed/ml_features.csv", stringsAsFactors=FALSE)
ml[["interacts_f"]] <- factor(ml[["interacts_f"]], levels=c("no","yes"))
cat("  Rows:", nrow(ml), "\n")
cat("  Columns:", ncol(ml), "\n")
cat("  Class balance:\n")
print(table(ml[["interacts_f"]]))
check("rows > 10000",              nrow(ml) > 10000)
check("tongue_tube_ratio exists",  "tongue_tube_ratio" %in% names(ml))
check("color_UV_yellow exists",    "color_UV_yellow" %in% names(ml))
check("interacts_f factor",        is.factor(ml[["interacts_f"]]))
check("both classes present",      all(c("yes","no") %in% levels(ml[["interacts_f"]])))

cat("\n[TEST 1c] Network matrix\n")
web <- readRDS("data/processed/network_matrix.rds")
cat("  Dimensions:", nrow(web), "flowers x", ncol(web), "bees\n")
cat("  Total interactions:", sum(web > 0), "\n")
check("9 flower rows",    nrow(web) == 9)
check("27 bee columns",   ncol(web) == 27)
check("no negative vals", all(web >= 0))

# ── TEST 2: Model files ───────────────────────────────────────────────────────

cat("\n[TEST 2a] Model files exist\n")
rf_exists  <- file.exists("outputs/models/rf_model.rds")
lr_exists  <- file.exists("outputs/models/lr_model.rds")
svm_exists <- file.exists("outputs/models/svm_model.rds")
bm_exists  <- file.exists("outputs/models/benchmark_results.csv")
cat("  rf_model.rds:          ", rf_exists, "\n")
cat("  lr_model.rds:          ", lr_exists, "\n")
cat("  svm_model.rds:         ", svm_exists, "\n")
cat("  benchmark_results.csv: ", bm_exists, "\n")
check("rf exists",        rf_exists)
check("lr exists",        lr_exists)
check("svm exists",       svm_exists)
check("benchmark exists", bm_exists)

cat("\n[TEST 2b] Model loads\n")
rf  <- readRDS("outputs/models/rf_model.rds")
lr  <- readRDS("outputs/models/lr_model.rds")
svm <- readRDS("outputs/models/svm_model.rds")
cat("  RF class: ", paste(class(rf),  collapse=", "), "\n")
cat("  LR class: ", paste(class(lr),  collapse=", "), "\n")
cat("  SVM class:", paste(class(svm), collapse=", "), "\n")
check("rf is train object",  inherits(rf,  "train"))
check("lr is train object",  inherits(lr,  "train"))
check("svm is train object", inherits(svm, "train"))

cat("\n[TEST 2c] Benchmark CSV\n")
bm <- read.csv("outputs/models/benchmark_results.csv")
cat("  Columns:", paste(names(bm), collapse=", "), "\n")
print(bm)
check("Model column exists", "Model" %in% names(bm))
check("AUC column exists",   "AUC"   %in% names(bm))
check("3 model rows",        nrow(bm) == 3)
check("RF AUC > 0.65",       bm$AUC[bm$Model == "Random Forest"] > 0.65)

# ── TEST 3: Prediction varies with inputs ─────────────────────────────────────

cat("\n[TEST 3] Prediction varies with inputs\n")

expected_cols <- c(
  "tube_depth_mm","corolla_diam_mm","nectar_vol_ul","nectar_sugar_pct",
  "tongue_length_mm","body_length_mm","flight_range_m",
  "flower_degree","flower_betweenness","flower_closeness",
  "bee_degree","bee_betweenness","bee_closeness",
  "tongue_tube_ratio","tongue_tube_match","body_flower_ratio",
  "degree_product","centrality_sum",
  "color_UV_yellow","color_white","color_pink_purple","color_red","color_yellow",
  "sym_actino","soc_social","lec_polylectic"
)

make_obs <- function(tongue, tube, body, corolla, nectar,
                     color="yellow", symmetry="actinomorphic",
                     sociality="solitary", lecty="polylectic") {
  df <- data.frame(
    tube_depth_mm     = as.numeric(tube),
    corolla_diam_mm   = as.numeric(corolla),
    nectar_vol_ul     = as.numeric(nectar),
    nectar_sugar_pct  = 30,
    tongue_length_mm  = as.numeric(tongue),
    body_length_mm    = as.numeric(body),
    flight_range_m    = 500,
    flower_degree     = 5,
    flower_betweenness = 0.1,
    flower_closeness  = 0.3,
    bee_degree        = 5,
    bee_betweenness   = 0.1,
    bee_closeness     = 0.3,
    tongue_tube_ratio = as.numeric(tongue) / max(as.numeric(tube), 0.1),
    tongue_tube_match = abs(as.numeric(tongue) - as.numeric(tube)),
    body_flower_ratio = as.numeric(body) / max(as.numeric(corolla), 0.1),
    degree_product    = 25,
    centrality_sum    = 0.2,
    color_UV_yellow   = as.integer(color == "UV_yellow"),
    color_white       = as.integer(color == "white"),
    color_pink_purple = as.integer(color == "pink_purple"),
    color_red         = as.integer(color == "red"),
    color_yellow      = as.integer(color == "yellow"),
    sym_actino        = as.integer(symmetry == "actinomorphic"),
    soc_social        = as.integer(sociality == "social"),
    lec_polylectic    = as.integer(lecty == "polylectic")
  )
  df[, expected_cols, drop=FALSE]
}

predict_prob <- function(model, obs) {
  round(predict(model, newdata=obs, type="prob")[1,"yes"] * 100, 1)
}

# Column alignment check
cat("\n  Column alignment check:\n")
test_obs <- make_obs(8, 10, 12, 20, 5)
missing_c <- setdiff(expected_cols, names(test_obs))
extra_c   <- setdiff(names(test_obs), expected_cols)
cat("  Missing cols:", if(length(missing_c)==0) "none" else paste(missing_c, collapse=", "), "\n")
cat("  Extra cols:  ", if(length(extra_c)==0)   "none" else paste(extra_c,   collapse=", "), "\n")
cat("  ncol:", ncol(test_obs), "\n")
check("26 columns, none missing", ncol(test_obs)==26 && length(missing_c)==0)

# Test 3a — short tongue / deep tube (mismatch)
obs_3a <- make_obs(tongue=5, tube=20, body=12, corolla=20,
                   nectar=5, color="white")
prob_3a <- predict_prob(rf, obs_3a)
cat("\n  Test 3a — tongue=5, tube=20, color=white\n")
cat("  tongue_tube_ratio:", obs_3a$tongue_tube_ratio, "\n")
cat("  color_white:", obs_3a$color_white, "\n")
cat("  Probability:", prob_3a, "%\n")
check("3a returns numeric", is.numeric(prob_3a))
check("3a prob 0-100",      prob_3a >= 0 && prob_3a <= 100)

# Test 3b — long tongue / shallow tube (match)
obs_3b <- make_obs(tongue=18, tube=5, body=12, corolla=20,
                   nectar=5, color="pink_purple")
prob_3b <- predict_prob(rf, obs_3b)
cat("\n  Test 3b — tongue=18, tube=5, color=pink_purple\n")
cat("  tongue_tube_ratio:", obs_3b$tongue_tube_ratio, "\n")
cat("  color_pink_purple:", obs_3b$color_pink_purple, "\n")
cat("  Probability:", prob_3b, "%\n")
check("3b returns numeric", is.numeric(prob_3b))
check("3b prob 0-100",      prob_3b >= 0 && prob_3b <= 100)

# Test 3c — extreme mismatch (very long tongue, very deep tube)
obs_3c <- make_obs(tongue=2, tube=30, body=6, corolla=50,
                   nectar=1, color="red", symmetry="zygomorphic",
                   sociality="solitary", lecty="oligolectic")
prob_3c <- predict_prob(rf, obs_3c)
cat("\n  Test 3c — extreme mismatch (tongue=2, tube=30)\n")
cat("  tongue_tube_ratio:", obs_3c$tongue_tube_ratio, "\n")
cat("  Probability:", prob_3c, "%\n")
check("3c returns numeric", is.numeric(prob_3c))

# Critical: are any two predictions different?
cat("\n  3a prob:", prob_3a, "% | 3b prob:", prob_3b,
    "% | 3c prob:", prob_3c, "%\n")
check("3a != 3b (model is sensitive to input)",  prob_3a != prob_3b)
check("any variation across 3 tests",
      length(unique(c(prob_3a, prob_3b, prob_3c))) > 1)

# All 3 models produce a prediction
cat("\n  All models predict on same obs:\n")
p_rf  <- predict_prob(rf,  obs_3b)
p_lr  <- predict_prob(lr,  obs_3b)
p_svm <- predict_prob(svm, obs_3b)
cat("  RF:", p_rf, "% | LR:", p_lr, "% | SVM:", p_svm, "%\n")
check("RF predicts",  is.numeric(p_rf)  && !is.na(p_rf))
check("LR predicts",  is.numeric(p_lr)  && !is.na(p_lr))
check("SVM predicts", is.numeric(p_svm) && !is.na(p_svm))

# ── TEST 4: Sensitivity summary ───────────────────────────────────────────────

cat("\n[TEST 4] Sensitivity analysis output\n")
sens <- tryCatch(
  read.csv("outputs/models/sensitivity_summary.csv"),
  error = function(e) NULL
)
if (is.null(sens)) {
  cat("  SKIP: sensitivity_summary.csv not found\n")
} else {
  cat("  Rows:", nrow(sens), "\n")
  print(sens[, c("fraction","auc_mean","f1_mean")])
  check("10 fraction rows",       nrow(sens) == 10)
  check("max AUC > 0.65",         max(sens$auc_mean, na.rm=TRUE) > 0.65)
  check("AUC improves with data", sens$auc_mean[nrow(sens)] >= sens$auc_mean[1])
}

# ── TEST 5: Output figures exist ─────────────────────────────────────────────

cat("\n[TEST 5] Output figures\n")
figs <- c(
  "outputs/figures/01_network.png",
  "outputs/figures/02_degree_dist.png",
  "outputs/figures/05_sensitivity.png"
)
for (f in figs) {
  exists <- file.exists(f)
  cat(" ", basename(f), ":", exists, "\n")
  check(paste(basename(f), "exists"), exists)
}

# ── Summary ───────────────────────────────────────────────────────────────────

cat("\n════════════════════════════════\n")
cat("RESULTS:", pass_count, "passed,", fail_count, "failed\n")
cat("════════════════════════════════\n")
if (fail_count == 0) cat("ALL TESTS PASSED\n") else cat("SOME TESTS FAILED — see above\n")
