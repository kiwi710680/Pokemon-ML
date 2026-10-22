# 02c_candy_mc_figures.R — 把 02b 的 500 次模擬畫成圖（需先跑 02b）
source("R/plot_theme.R")
raw <- read.csv("results/candy_mc_raw.csv")
summ <- read.csv("results/candy_mc_summary.csv")
truth <- summ$truth[1]

lab <- c(linear = "one straight line", plugin_knn = "plug-in (kNN)", ipw = "IPW",
         aipw_full = "AIPW", tmle_full = "TMLE", aipw_cf_DML = "AIPW + cross-fit (DML)",
         tmle_cf = "TMLE + cross-fit")
ord <- names(lab)
col <- c(linear = pal$muted, plugin_knn = pal$red, ipw = pal$gold, aipw_full = pal$teal,
         tmle_full = pal$teal, aipw_cf_DML = pal$plum, tmle_cf = pal$plum)

open_png("figures/c4_mc_k10.png", 1700, 1000)
par(mar = c(5, 15, 1.2, 1.2))
s <- raw[raw$k == 10 & raw$estimator %in% ord, ]
plot(NA, xlim = c(50, 95), ylim = c(0.5, length(ord) + 0.5), axes = FALSE,
     xlab = "ATE estimates from 500 simulated datasets", ylab = "")
abline(v = truth, lwd = 3, col = pal$ink, lty = 2)
for (i in seq_along(ord)) {
  e <- s$est[s$estimator == ord[i]]
  y <- length(ord) + 1 - i
  points(e, y + runif(length(e), -0.28, 0.28), pch = 16, cex = 0.55, col = adjustcolor(col[ord[i]], 0.35))
  points(mean(e), y, pch = 23, bg = "white", col = pal$ink, cex = 1.8, lwd = 2)
}
axis(1); axis(2, at = length(ord):1, labels = lab[ord], tick = FALSE, cex.axis = 1.1, col.axis = pal$ink)
text(truth, length(ord) + 0.45, sprintf("truth %.1f", truth), pos = 4, cex = 1.05, col = pal$ink)
dev.off()

open_png("figures/c5_mc_k1_se.png", 1500, 900)
par(mar = c(5, 6, 1.2, 1.2))
k1 <- summ[summ$k == 1 & summ$estimator %in% c("aipw_full", "aipw_cf_DML"), ]
m <- rbind(SD = k1$sd, SE = k1$mean_se)
bp <- barplot(m, beside = TRUE, names.arg = c("AIPW (grades its own exam)", "AIPW + cross-fit"),
              col = c(pal$ink, pal$gold), border = NA, ylim = c(0, 3.4), axes = FALSE,
              ylab = "CP (closer bars = better)")
axis(2)
text(bp, m + 0.12, sprintf("%.2f", m), cex = 1.1, col = pal$ink)
text(colMeans(bp), 2.75, sprintf("95%% CI coverage %.0f%%", 100 * k1$coverage), cex = 1.15, col = pal$red)
legend("top", text.col = pal$ink, c("true SD of the estimates", "average formula SE"),
       fill = c(pal$ink, pal$gold), border = NA, bty = "n", horiz = TRUE)
dev.off()
cat("done\n")
