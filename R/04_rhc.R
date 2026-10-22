# 04_rhc.R — Lab 4：右心導管（RHC）與 30 天死亡。真實觀察資料、二元結果、沒有真值。
# 重點：彈性 learner 用「全樣本」時會自己改考卷 → 殘差變小 → AIPW 修正變小、CI 變窄
# 執行（約 2 分鐘）：在 lab/ 下 Rscript R/04_rhc.R
source("R/helpers.R"); source("R/engine.R")
d <- load_rhc()
cat(sprintf("n = %d, had RHC %d (%.1f%%), 30-day mortality %.1f%%\n", length(d$A), sum(d$A), 100 * mean(d$A), 100 * mean(d$Y)))
cat(sprintf("unadjusted risk difference = %.3f\n", mean(d$Y[d$A == 1]) - mean(d$Y[d$A == 0])))

res <- list(); chk <- list()
for (lr in c("glm", "rf")) for (sc in c("full", "crossfit")) {
  set.seed(1)
  nu <- nuisance(d, lr, sc, ntree = 100, seed = 1)
  res[[paste(lr, sc)]] <- transform(panel(d, nu, sc), learner = lr)
  chk[[paste(lr, sc)]] <- c(learner = lr, scheme = sc, round(residual_check(d, nu), 4),
                            pseudo_outside_01 = round(mean(abs(eif(d, nu, 0)) > 1), 3))
}
res <- do.call(rbind, res)
print(transform(res, est = round(est, 3), se = round(se, 3), lo = round(lo, 3), hi = round(hi, 3)), row.names = FALSE)
print(do.call(rbind, chk))
dir.create("results", showWarnings = FALSE)
write.csv(res, "results/rhc_panel.csv", row.names = FALSE)
write.csv(do.call(rbind, chk), "results/rhc_residual_check.csv", row.names = FALSE)
