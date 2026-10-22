# 01_pokemon_regression.R — 李宏毅老師寶可夢 CP 預測的重現（本機 R）
# 資料：lab/data/classroom_train10.csv、classroom_test10.csv、classroom_train60.csv
# 執行：在 lab/ 資料夾下 Rscript R/01_pokemon_regression.R
# 課堂的 "Average Error" 經數值核對是 sqrt(SSE)/N（不是 RMSE），這裡兩者都列。

source("R/plot_theme.R")
tr10 <- read.csv("data/classroom_train10.csv")
te10 <- read.csv("data/classroom_test10.csv")
tr60 <- read.csv("data/classroom_train60.csv")

lee_err <- function(y, yhat) sqrt(sum((y - yhat)^2)) / length(y)
rmse    <- function(y, yhat) sqrt(mean((y - yhat)^2))

# Step 1-3：y = b + w x，用最小平方法找 b, w
fit1 <- lm(cp_new ~ cp, data = tr10)
cat(sprintf("b = %.2f, w = %.3f\n", coef(fit1)[1], coef(fit1)[2]))

# 多項式 1~5 次：模型（函式集合）越來越大
poly_tab <- do.call(rbind, lapply(1:5, function(p) {
  f <- lm(cp_new ~ poly(cp, p, raw = TRUE), data = tr10)
  data.frame(model = paste0("poly", p), params = p + 1,
             train_lee = lee_err(tr10$cp_new, fitted(f)),
             test_lee  = lee_err(te10$cp_new, predict(f, te10)),
             train_rmse = rmse(tr10$cp_new, fitted(f)),
             test_rmse  = rmse(te10$cp_new, predict(f, te10)))
}))

# 重新設計模型：每個物種自己的一條線（60 筆訓練）
fs <- lm(cp_new ~ species + species:cp, data = tr60)
te10$species <- factor(te10$species, levels = levels(factor(tr60$species)))
sp <- data.frame(model = "species_linear", params = length(coef(fs)),
                 train_lee = lee_err(tr60$cp_new, fitted(fs)),
                 test_lee  = lee_err(te10$cp_new, predict(fs, te10)),
                 train_rmse = rmse(tr60$cp_new, fitted(fs)),
                 test_rmse  = rmse(te10$cp_new, predict(fs, te10)))

# 交給資料決定形狀：同物種裡找 CP 最近的 k 隻取平均（kNN），k 用 leave-one-out 選
knn_pred <- function(train, newdata, k) {
  sapply(seq_len(nrow(newdata)), function(i) {
    tr <- train[train$species == newdata$species[i], ]
    d  <- abs(tr$cp - newdata$cp[i])
    mean(tr$cp_new[order(d)[seq_len(min(k, nrow(tr)))]])
  })
}
loo <- sapply(1:10, function(k) {
  mean(sapply(seq_len(nrow(tr60)), function(i)
    (tr60$cp_new[i] - knn_pred(tr60[-i, ], tr60[i, ], k))^2))
})
k_best <- which.min(loo)
kn <- data.frame(model = paste0("knn_k", k_best), params = NA,
                 train_lee = lee_err(tr60$cp_new, knn_pred(tr60, tr60, k_best)),
                 test_lee  = lee_err(te10$cp_new, knn_pred(tr60, te10, k_best)),
                 train_rmse = rmse(tr60$cp_new, knn_pred(tr60, tr60, k_best)),
                 test_rmse  = rmse(te10$cp_new, knn_pred(tr60, te10, k_best)))

tab <- rbind(poly_tab, sp, kn)
print(transform(tab, train_lee = round(train_lee, 1), test_lee = round(test_lee, 1),
                train_rmse = round(train_rmse, 1), test_rmse = round(test_rmse, 1)))
cat("k chosen by LOO-CV =", k_best, "\n")
dir.create("results", showWarnings = FALSE)
write.csv(tab, "results/pokemon_metrics.csv", row.names = FALSE)

# ---------------- figures ----------------
dir.create("figures", showWarnings = FALSE)
xs <- seq(0, 650, length.out = 300)

open_png("figures/p1_linear.png", 1600, 1050)
plot(tr10$cp, tr10$cp_new, xlim = c(0, 650), ylim = c(0, 1700), pch = 19, col = pal$red,
     cex = 1.6, xlab = "Original CP  (x)", ylab = "CP after evolution  (y)", axes = FALSE)
axis(1); axis(2); grid_lines()
lines(xs, predict(fit1, data.frame(cp = xs)), col = pal$ink, lwd = 3)
points(te10$cp, te10$cp_new, pch = 21, col = pal$teal, bg = "white", cex = 1.6, lwd = 2)
legend("topleft", text.col = pal$ink, c("training data (10)", "testing data (10)", sprintf("y = %.1f + %.2f x", coef(fit1)[1], coef(fit1)[2])),
       pch = c(19, 21, NA), lty = c(NA, NA, 1), lwd = c(NA, 2, 3), col = c(pal$red, pal$teal, pal$ink),
       pt.bg = "white", bty = "n", cex = 1.05)
dev.off()

open_png("figures/p2_poly5.png", 1600, 1050)
plot(tr10$cp, tr10$cp_new, xlim = c(0, 650), ylim = c(-200, 1900), pch = 19, col = pal$red,
     cex = 1.6, xlab = "Original CP  (x)", ylab = "CP after evolution  (y)", axes = FALSE)
axis(1); axis(2); grid_lines()
cols <- c(pal$ink, pal$teal, pal$gold, pal$plum, pal$red)
for (p in c(1, 3, 5)) {
  f <- lm(cp_new ~ poly(cp, p, raw = TRUE), data = tr10)
  lines(xs, predict(f, data.frame(cp = xs)), col = cols[p], lwd = 3, lty = ifelse(p == 5, 1, 1))
}
points(te10$cp, te10$cp_new, pch = 21, col = pal$teal, bg = "white", cex = 1.6, lwd = 2)
legend("topleft", text.col = pal$ink, c("degree 1", "degree 3", "degree 5"), col = cols[c(1, 3, 5)], lwd = 3, bty = "n", cex = 1.05)
dev.off()

open_png("figures/p3_species.png", 1600, 1050)
tr60$species <- factor(tr60$species)
plot(tr60$cp, tr60$cp_new, xlim = c(0, 650), ylim = c(0, 1700), pch = 19,
     col = adjustcolor(species_col[as.character(tr60$species)], 0.85), cex = 1.4,
     xlab = "Original CP  (x)", ylab = "CP after evolution  (y)", axes = FALSE)
axis(1); axis(2); grid_lines()
for (s in levels(tr60$species)) {
  r <- range(tr60$cp[tr60$species == s])
  xx <- seq(r[1], r[2], length.out = 50)
  lines(xx, predict(fs, data.frame(cp = xx, species = s)), col = species_col[s], lwd = 3)
}
legend("topleft", text.col = pal$ink, names(species_col), col = species_col, pch = 19, lwd = 3, bty = "n", cex = 1.05)
dev.off()
cat("figures written\n")
