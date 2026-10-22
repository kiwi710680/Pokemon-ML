# 03_nhefs_three_routes.R — 復刻 Karim (2026) "Three Routes to One Answer" 的 NHEFS 主線
# 問題：1971–1982 間戒菸 (qsmk = 1) 對體重變化 (wt82_71, kg) 的 ATE
# 與原 repo 的差異：原文用 5 個 learner 的 SuperLearner；這裡預設用 base R 寫的
# sl_lite（mean / 主效應 GLM / Hernán–Robins GLM / 樣條 GLM 的凸組合），瀏覽器也跑得動。
# 估計器（AIPW / TMLE / EIF）與糖果例子共用 helpers.R，同一份程式碼。
# 執行：在 lab/ 下 Rscript R/03_nhefs_three_routes.R
source("R/helpers.R")
source("R/nhefs_helpers.R")   # 讀資料、定義 learner / sl_lite / nuisance / panel

out <- list()
for (lr in c("hr", "sl_lite")) {
  set.seed(1)
  nf <- nhefs_nuisance(dat, dat, lr)
  nc <- nhefs_crossfit(dat, lr, K = 5, seed = 1)
  p  <- rbind(nhefs_panel(dat, nf, "full sample"), nhefs_panel(dat, nc, "cross-fit K=5"))
  p$learner <- lr
  out[[lr]] <- p
  cat(sprintf("\n=== learner: %s ===   g range %.3f-%.3f\n", lr, min(nf$g), max(nf$g)))
  print(transform(p, est = round(est, 2), se = round(se, 2), lo = round(lo, 2), hi = round(hi, 2)), row.names = FALSE)
}
res <- do.call(rbind, out)

# 重複切分：同一份資料、不同的切法，cross-fit AIPW 會晃多少？
reps <- 20
rep_est <- sapply(seq_len(reps), function(s) {
  nc <- nhefs_crossfit(dat, "sl_lite", K = 5, seed = 100 + s)
  c(AIPW = est_aipw(dat, nc)[["est"]], TMLE = est_tmle(dat, nc)[["est"]])
})
cat(sprintf("\n%d random splits (sl_lite): DML/AIPW median %.2f (range %.2f-%.2f); CV-TMLE median %.2f\n",
            reps, median(rep_est["AIPW", ]), min(rep_est["AIPW", ]), max(rep_est["AIPW", ]),
            median(rep_est["TMLE", ])))

set.seed(1)
w_full <- attr(sl_lite(dat, dat[1:2, ], "Q"), "weights")
cat("\nsl_lite weights for Q:\n"); print(round(w_full, 3))

dir.create("results", showWarnings = FALSE)
write.csv(res, "results/nhefs_panel.csv", row.names = FALSE)
write.csv(data.frame(seed = 100 + seq_len(reps), t(rep_est)), "results/nhefs_repeated_splits.csv", row.names = FALSE)

# overlap 圖
if (file.exists("R/plot_theme.R") && .Platform$OS.type == "windows") {
  source("R/plot_theme.R")
  set.seed(1); g_hat <- nhefs_nuisance(dat, dat, "sl_lite")$g
  open_png("figures/n1_overlap.png", 1600, 900)
  br <- seq(0, 0.85, by = 0.025)
  h1 <- hist(g_hat[dat$A == 1], breaks = br, plot = FALSE); h0 <- hist(g_hat[dat$A == 0], breaks = br, plot = FALSE)
  plot(NA, xlim = c(0, 0.85), ylim = c(-max(h0$counts), max(h1$counts)), axes = FALSE,
       xlab = "estimated propensity  g-hat(X) = P(quit smoking | X)", ylab = "count")
  axis(1); yt <- pretty(c(-max(h0$counts), max(h1$counts))); axis(2, at = yt, labels = abs(yt))
  rect(br[-length(br)], 0, br[-1], h1$counts, col = adjustcolor(pal$red, 0.8), border = pal$bg)
  rect(br[-length(br)], 0, br[-1], -h0$counts, col = adjustcolor(pal$teal, 0.8), border = pal$bg)
  abline(h = 0, col = pal$ink)
  text(0.83, max(h1$counts) * 0.8, "quit (403)", adj = 1, col = pal$red, cex = 1.2)
  text(0.83, -max(h0$counts) * 0.8, "did not quit (1163)", adj = 1, col = pal$teal, cex = 1.2)
  dev.off()
}
