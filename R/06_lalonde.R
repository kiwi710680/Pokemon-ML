# 06_lalonde.R — Lab 6：LaLonde（NSW 職訓 vs PSID 一般族群）。overlap 幾乎不存在時會發生什麼事？
# 執行（約 30 秒）：在 lab/ 下 Rscript R/06_lalonde.R
source("R/helpers.R"); source("R/engine.R")
ex <- load_lalonde("experimental")
cat(sprintf("experimental benchmark (NSW randomized, simple difference) = %.0f USD\n", ex$benchmark))
d <- load_lalonde("psid")
cat(sprintf("NSW trainees %d + PSID controls %d; simple difference = %.0f\n", sum(d$A), sum(1 - d$A),
            mean(d$Y[d$A == 1]) - mean(d$Y[d$A == 0])))

res <- list(); ov <- list()
for (lr in c("glm", "rf")) for (sc in c("full", "crossfit")) {
  nu <- nuisance(d, lr, sc, ntree = 100, seed = 1)
  res[[paste(lr, sc)]] <- transform(panel(d, nu, sc), learner = lr)
  ov[[paste(lr, sc)]] <- c(learner = lr, scheme = sc, round(overlap_summary(nu$g_raw, d$A), 3))
}
res <- do.call(rbind, res)
print(transform(res, est = round(est), se = round(se), lo = round(lo), hi = round(hi)), row.names = FALSE)
print(do.call(rbind, ov))

# Trimming：只留下 ĝ 在 [ε, 1 − ε] 的人，再做一次 cross-fit AIPW（= DML）
nu_ref <- nuisance(d, "glm", "crossfit", seed = 1)
trim <- do.call(rbind, lapply(c(0, 0.05, 0.10), function(eps) {
  keep <- nu_ref$g_raw >= eps & nu_ref$g_raw <= 1 - eps
  dk <- list(X = d$X[keep, ], A = d$A[keep], Y = d$Y[keep], binary = FALSE)
  a <- est_aipw(dk, nuisance(dk, "glm", "crossfit", seed = 1))
  data.frame(rule = if (eps == 0) "keep all" else sprintf("keep g-hat in [%.2f, %.2f]", eps, 1 - eps),
             n_kept = sum(keep), treated_kept = sum(dk$A), pct_kept = round(100 * mean(keep)),
             DML = round(a[["est"]]), lo = round(a[["lo"]]), hi = round(a[["hi"]]))
}))
print(trim, row.names = FALSE)
dir.create("results", showWarnings = FALSE)
write.csv(res, "results/lalonde_panel.csv", row.names = FALSE)
write.csv(trim, "results/lalonde_trimming.csv", row.names = FALSE)
