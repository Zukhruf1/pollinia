# fix_benchmark.R
setwd("C:/Users/it-ha/pollinia")
library(caret)
library(randomForest)
library(pROC)

ml <- read.csv("data/processed/ml_features.csv", stringsAsFactors = FALSE)
ml[["interacts_f"]] <- factor(ml[["interacts_f"]], levels = c("no", "yes"))

set.seed(42)
idx  <- createDataPartition(ml[["interacts_f"]], p = 0.75, list = FALSE)
test <- ml[-idx, ]

rf  <- readRDS("outputs/models/rf_model.rds")
lr  <- readRDS("outputs/models/lr_model.rds")
svm <- readRDS("outputs/models/svm_model.rds")

pr  <- predict(rf,  test)
pl  <- predict(lr,  test)
ps  <- predict(svm, test)

cmr <- confusionMatrix(pr, test[["interacts_f"]], positive = "yes")
cml <- confusionMatrix(pl, test[["interacts_f"]], positive = "yes")
cms <- confusionMatrix(ps, test[["interacts_f"]], positive = "yes")

auc_rf  <- round(as.numeric(pROC::auc(pROC::roc(test[["interacts_f"]], predict(rf,  test, type="prob")[,"yes"], quiet=TRUE))), 3)
auc_lr  <- round(as.numeric(pROC::auc(pROC::roc(test[["interacts_f"]], predict(lr,  test, type="prob")[,"yes"], quiet=TRUE))), 3)
auc_svm <- round(as.numeric(pROC::auc(pROC::roc(test[["interacts_f"]], predict(svm, test, type="prob")[,"yes"], quiet=TRUE))), 3)

bench <- data.frame(
  Model     = c("Random Forest", "Logistic Reg", "SVM"),
  Accuracy  = c(round(cmr$overall[["Accuracy"]],  3),
                round(cml$overall[["Accuracy"]],  3),
                round(cms$overall[["Accuracy"]],  3)),
  Precision = c(round(cmr$byClass[["Precision"]], 3),
                round(cml$byClass[["Precision"]], 3),
                round(cms$byClass[["Precision"]], 3)),
  Recall    = c(round(cmr$byClass[["Recall"]],    3),
                round(cml$byClass[["Recall"]],    3),
                round(cms$byClass[["Recall"]],    3)),
  F1        = c(round(cmr$byClass[["F1"]],        3),
                round(cml$byClass[["F1"]],        3),
                round(cms$byClass[["F1"]],        3)),
  AUC       = c(auc_rf, auc_lr, auc_svm)
)

write.csv(bench, "outputs/models/benchmark_results.csv", row.names = FALSE)
print(bench)
cat("DONE\n")
