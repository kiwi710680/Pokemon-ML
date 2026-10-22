# 02_candy_plugin.R — 神奇糖果：從 regression 走到 plug-in，再走到 AIPW / TMLE
# 情境（虛構、模擬資料）：訓練師在進化前餵寶可夢「神奇糖果」(A=1)。
# 問題：糖果讓進化後 CP 平均多多少？  ATE = E[Y(1) - Y(0)]
# 執行：在 lab/ 下 Rscript R/02_candy_plugin.R
source("R/helpers.R")
source("R/plot_theme.R")

truth <- candy_truth()
d <- simulate_candy(1500, seed = 2026)
cat(sprintf("true ATE = %.2f   (n = %d, ate candy %d)\n", truth, nrow(d), sum(d$A)))

est <- suppressWarnings(all_estimates(d, k = 10))
print(round(est, 2))
dir.create("results", showWarnings = FALSE)
write.csv(data.frame(estimator = rownames(est), est, truth = truth), "results/candy_demo.csv", row.names = FALSE)

# 診斷：overlap
nf <- nuisance_full(d, k = 10)
cat(sprintf("range of estimated g: %.3f - %.3f\n", min(nf$g), max(nf$g)))

# 為什麼 cross-fitting？k = 1 時，自己是自己的鄰居 → 殘差 = 0
res_tab <- do.call(rbind, lapply(c(1, 10), function(k) {
  f <- nuisance_full(d, k = k); cf <- nuisance_crossfit(d, K = 5, seed = 1, k = k)
  QA_f <- ifelse(d$A == 1, f$Q1, f$Q0); QA_c <- ifelse(d$A == 1, cf$Q1, cf$Q0)
  a_f <- est_aipw(d, f); a_c <- est_aipw(d, cf)
  data.frame(k = k, resid_rms_insample = sqrt(mean((d$Y - QA_f)^2)),
             resid_rms_outfold = sqrt(mean((d$Y - QA_c)^2)),
             plugin_full = est_plugin(f), aipw_full = a_f["est"], se_full = a_f["se"],
             aipw_crossfit = a_c["est"], se_crossfit = a_c["se"])
}))
print(round(res_tab, 2))
write.csv(res_tab, "results/candy_crossfit_demo.csv", row.names = FALSE)

# ---------------- figures ----------------
dir.create("figures", showWarnings = FALSE)
col_A <- ifelse(d$A == 1, pal$red, pal$teal)

# c1：誰吃到糖？（confounding 一眼看出）
open_png("figures/c1_who_gets_candy.png", 1600, 1000)
pg <- d[d$species == "Pidgey", ]
plot(pg$cp, pg$Y, pch = 19, cex = 1.1, col = adjustcolor(ifelse(pg$A == 1, pal$red, pal$teal), 0.75),
     xlab = "Original CP (Pidgey)", ylab = "CP after evolution", axes = FALSE)
axis(1); axis(2); grid_lines()
legend("topleft", text.col = pal$ink, c("Candy (A = 1)", "no Candy (A = 0)"), pch = 19, col = c(pal$red, pal$teal),
       bty = "n", cex = 1.05)
dev.off()

# c2：kNN 的 Q(1, x) 在「很少吃糖的地方」被高估
open_png("figures/c2_knn_bias.png", 1600, 1000)
xs <- data.frame(species = factor("Pidgey", levels = species_levels), cp = 10:460, A = 1, Y = 0)
q_knn <- knn_Q(d, xs, a = 1, k = 10)
q_true <- 1.85 * xs$cp * (1 - xs$cp / 2600) + 20 + 0.30 * xs$cp
plot(pg$cp[pg$A == 1], pg$Y[pg$A == 1], pch = 19, cex = 1.1, col = adjustcolor(pal$red, 0.55),
     xlab = "Original CP (Pidgey)", ylab = "CP after evolution if eat Candy", axes = FALSE,
     xlim = c(0, 470), ylim = c(0, 1050))
axis(1); axis(2); grid_lines()
lines(xs$cp, q_true, lwd = 3, col = pal$ink, lty = 2)
lines(xs$cp, q_knn, lwd = 3, col = pal$red, type = "s")
rug(pg$cp[pg$A == 1], col = pal$red, lwd = 1.2)
legend("topleft", text.col = pal$ink, c("true Q(1, x)", "kNN estimate Q-hat(1, x)  (k = 10)", "Pidgey that ate Candy"),
       lty = c(2, 1, NA), lwd = c(3, 3, NA), pch = c(NA, NA, 19), col = c(pal$ink, pal$red, pal$red),
       bty = "n", cex = 1.05)
dev.off()

# c3：overlap
open_png("figures/c3_overlap.png", 1600, 900)
br <- seq(0, 1, by = 0.04)
h1 <- hist(nf$g[d$A == 1], breaks = br, plot = FALSE); h0 <- hist(nf$g[d$A == 0], breaks = br, plot = FALSE)
plot(NA, xlim = c(0, 1), ylim = c(-max(h0$counts), max(h1$counts)), axes = FALSE,
     xlab = "estimated propensity score  g-hat(X) = P(Candy | species, CP)", ylab = "count")
axis(1); axis(2, at = pretty(c(-max(h0$counts), max(h1$counts))), labels = abs(pretty(c(-max(h0$counts), max(h1$counts)))))
rect(br[-length(br)], 0, br[-1], h1$counts, col = adjustcolor(pal$red, 0.8), border = pal$bg)
rect(br[-length(br)], 0, br[-1], -h0$counts, col = adjustcolor(pal$teal, 0.8), border = pal$bg)
abline(h = 0, col = pal$ink)
text(0.98, max(h1$counts) * 0.85, "Candy", adj = 1, col = pal$red, cex = 1.2)
text(0.98, -max(h0$counts) * 0.85, "no Candy", adj = 1, col = pal$teal, cex = 1.2)
dev.off()
cat("figures written\n")
