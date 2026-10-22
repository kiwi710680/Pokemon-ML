# pokemon_helpers.R — Lab 1 用的小工具（李宏毅老師寶可夢例子；資料依課堂投影片重建）
# 課堂的 Average Error 經數值核對是 sqrt(SSE)/N（不是 RMSE）
SPECIES <- c("Pidgey", "Weedle", "Caterpie", "Eevee")
lee_err <- function(y, yhat) sqrt(sum((y - yhat)^2)) / length(y)

read_pokemon <- function(file) {
  d <- read.csv(file)
  d$species <- factor(d$species, levels = SPECIES)
  d
}

# 設計矩陣：每個物種自己的截距與斜率（4 + 4）
X_species <- function(d) {
  S <- sapply(SPECIES, function(s) as.numeric(d$species == s))
  cbind(S, S * d$cp)
}
# 18 個參數（投影片 27）：物種 × (1, cp, cp²) + hp, hp², height, height², weight, weight²
X_extended <- function(d) {
  S <- sapply(SPECIES, function(s) as.numeric(d$species == s))
  cbind(S, S * d$cp, S * d$cp^2, d$hp, d$hp^2, d$height, d$height^2, d$weight, d$weight^2)
}

# Ridge：L = Σ(y − ŷ)² + λ Σ wᵢ²，4 個物種截距不懲罰（投影片 29–30）
# 用「擴增資料」的最小平方法求解，數值比直接解 (XᵀX + λI) 穩定
ridge_fit <- function(X, y, lambda, n_free = 4) {
  p <- ncol(X)
  pen <- diag(c(rep(0, n_free), rep(sqrt(lambda), p - n_free)), p)
  Xa <- rbind(X, pen[(n_free + 1):p, , drop = FALSE]); ya <- c(y, rep(0, p - n_free))
  qr.coef(qr(Xa), ya)
}

# kNN：同物種裡找 CP 最近的 k 隻，取平均
knn_cp <- function(train, newdata, k) {
  sapply(seq_len(nrow(newdata)), function(i) {
    tr <- train[train$species == newdata$species[i], ]
    mean(tr$cp_new[order(abs(tr$cp - newdata$cp[i]))[seq_len(min(k, nrow(tr)))]])
  })
}

# 候選模型：每個都是「拿訓練資料 → 對新資料預測」的函式
pokemon_candidates <- list(
  "linear (cp only)"   = function(tr, nd) predict(lm(cp_new ~ cp, tr), nd),
  "species-linear"     = function(tr, nd) X_species(nd) %*% qr.coef(qr(X_species(tr)), tr$cp_new),
  "18-param OLS"       = function(tr, nd) X_extended(nd) %*% ridge_fit(X_extended(tr), tr$cp_new, 0),
  "18-param ridge"     = function(tr, nd) X_extended(nd) %*% ridge_fit(X_extended(tr), tr$cp_new, 1e4),
  "kNN (k = 3)"        = function(tr, nd) knn_cp(tr, nd, 3)
)

# 依物種分層切 V 折，避免某一折完全沒有伊布
strat_folds <- function(species, V = 5, seed = 1) {
  set.seed(seed)
  f <- integer(length(species))
  for (s in unique(species)) { id <- which(species == s); f[id] <- sample(rep(seq_len(V), length.out = length(id))) }
  f
}

# 非負、加總為 1 的權重，讓組合後的 CV 平方誤差最小（SuperLearner 的凸組合）
convex_weights <- function(Z, y) {
  obj <- function(b) { w <- exp(c(0, b)); w <- w / sum(w); mean((y - Z %*% w)^2) }
  b <- optim(rep(0, ncol(Z) - 1), obj, method = "BFGS")$par
  w <- exp(c(0, b)); setNames(w / sum(w), colnames(Z))
}
