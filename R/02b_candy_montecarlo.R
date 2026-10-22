# 02b_candy_montecarlo.R — 重複模擬：哪個估計器真的會對準真值？CI 真的有 95% 嗎？
# 執行（約 5–10 分鐘）：在 lab/ 下 Rscript R/02b_candy_montecarlo.R [reps]
source("R/helpers.R")
args <- commandArgs(trailingOnly = TRUE)
reps <- if (length(args)) as.integer(args[1]) else 500
n <- 1500
truth <- candy_truth()
set.seed(20261016)

one_rep <- function(r, k) {
  d <- simulate_candy(n)
  e <- suppressWarnings(all_estimates(d, k = k))
  data.frame(rep = r, k = k, estimator = rownames(e), e, row.names = NULL)
}
raw <- do.call(rbind, lapply(c(10, 1), function(k)
  do.call(rbind, lapply(seq_len(reps), one_rep, k = k))))
raw$cover <- with(raw, ifelse(is.na(lo), NA, lo < truth & hi > truth))
dir.create("results", showWarnings = FALSE)
write.csv(raw, "results/candy_mc_raw.csv", row.names = FALSE)

summ <- do.call(rbind, lapply(split(raw, list(raw$k, raw$estimator), drop = TRUE), function(s)
  data.frame(k = s$k[1], estimator = s$estimator[1], mean = mean(s$est),
             bias = mean(s$est) - truth, sd = sd(s$est),
             mean_se = mean(s$se), coverage = mean(s$cover),
             mcse_bias = sd(s$est) / sqrt(nrow(s)))))
summ$truth <- truth
summ <- summ[order(summ$k, match(summ$estimator, rownames(all_estimates(simulate_candy(200, 1))))), ]
write.csv(summ, "results/candy_mc_summary.csv", row.names = FALSE)
print(transform(summ, mean = round(mean, 2), bias = round(bias, 2), sd = round(sd, 2),
                mean_se = round(mean_se, 2), coverage = round(coverage, 3), mcse_bias = round(mcse_bias, 2)))
