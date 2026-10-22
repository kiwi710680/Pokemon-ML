# 05_ihdp.R — Lab 5：IHDP 半合成資料。共變數是真的、結果是模擬的，所以知道真值。
# 10 份資料（CEVAE 版 ihdp_npci_1–10；Karim 2026 用 50 份）× {GLM, RF} × {full, cross-fit}
# 執行（約 1 分鐘）：在 lab/ 下 Rscript R/05_ihdp.R
source("R/helpers.R"); source("R/engine.R")
R <- 10
res <- list()
for (r in seq_len(R)) {
  d <- load_ihdp(r)
  for (lr in c("glm", "rf")) for (sc in c("full", "crossfit")) {
    nu <- nuisance(d, lr, sc, ntree = 100, seed = r)
    p <- panel(d, nu, sc); p$learner <- lr; p$r <- r; p$truth <- d$truth
    res[[length(res) + 1]] <- p
  }
}
res <- do.call(rbind, res)
res$err <- res$est - res$truth
res$cover <- ifelse(is.na(res$lo), NA, res$lo <= res$truth & res$truth <= res$hi)
grade <- do.call(rbind, lapply(split(res, list(res$learner, res$scheme, res$estimator), drop = TRUE), function(s)
  data.frame(learner = s$learner[1], scheme = s$scheme[1], estimator = s$estimator[1],
             bias = mean(s$err), rmse = sqrt(mean(s$err^2)), coverage = mean(s$cover),
             mean_ci_width = mean(s$hi - s$lo), mcse_bias = sd(s$err) / sqrt(nrow(s)))))
print(transform(grade, bias = round(bias, 3), rmse = round(rmse, 3), mean_ci_width = round(mean_ci_width, 3), mcse_bias = round(mcse_bias, 3)), row.names = FALSE)
dir.create("results", showWarnings = FALSE)
write.csv(res, "results/ihdp_runs.csv", row.names = FALSE)
write.csv(grade, "results/ihdp_grade.csv", row.names = FALSE)
