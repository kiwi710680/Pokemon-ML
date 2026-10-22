# helpers.R — 本課所有估計器的「透明版」實作（只用 base R + splines）
# 同一份程式同時給本機 R 與瀏覽器 webR 使用。
# 符號（與投影片一致）：O = (X, A, Y)；X = 共變數（這裡是物種與進化前 CP）, A = 治療(0/1), Y = 結果
#       Q(a, X) = Q_a(X) = E[Y | A = a, X]   outcome regression
#       g(X)    = P(A = 1 | X)                propensity score
#       H(A, X) = A / g(X) − (1 − A) / (1 − g(X))   clever covariate
#       D(O; g, Q, ψ) = H (Y − Q_A) + Q_1 − Q_0 − ψ    ATE 的 EIF

expit <- function(x) 1 / (1 + exp(-x))
bound <- function(p, lo = 0.025, hi = 0.975) pmin(pmax(p, lo), hi)

# ---------------------------------------------------------------
# 1. 神奇糖果：模擬資料（真值已知，才能檢查 bias 與 coverage）
# ---------------------------------------------------------------
species_levels <- c("Pidgey", "Weedle", "Caterpie", "Eevee")

simulate_candy <- function(n = 800, seed = NULL, strength = 1) {
  if (!is.null(seed)) set.seed(seed)
  species <- factor(sample(species_levels, n, replace = TRUE,
                           prob = c(.40, .20, .15, .25)), levels = species_levels)
  top <- c(Pidgey = 450, Weedle = 260, Caterpie = 260, Eevee = 650)[as.character(species)]
  cp  <- round(10 + top * rbeta(n, 2, 2.5))
  # 進化倍率（依物種）＋ 高 CP 時的飽和，讓關係不是一條直線
  mult <- c(Pidgey = 1.85, Weedle = 1.10, Caterpie = 1.08, Eevee = 2.60)[as.character(species)]
  f0   <- mult * cp * (1 - cp / 2600)
  # 訓練師比較愛餵強的、和伊布（Eevee）：這就是 confounding
  # strength 越大，偏心越嚴重，g 越靠近 0 或 1（overlap 越差）；預設 1
  g0 <- expit(strength * (-1.8 + 3.2 * (cp / top)) + 0.5 * (species == "Eevee"))
  A  <- rbinom(n, 1, g0)
  tau <- 20 + 0.30 * cp          # 糖果效果：越強的寶可夢加越多
  Y1 <- f0 + tau + rnorm(n, 0, 30)
  Y0 <- f0 + rnorm(n, 0, 30)
  Y  <- ifelse(A == 1, Y1, Y0)
  data.frame(species, cp, A, Y, g0, Q1 = f0 + tau, Q0 = f0, Y1, Y0)
}

# 母體真值（大樣本近似）：ATE = E[tau(X)]
candy_truth <- function(n = 2e6, seed = 1) {
  set.seed(seed)
  species <- sample(species_levels, n, replace = TRUE, prob = c(.40, .20, .15, .25))
  top <- c(Pidgey = 450, Weedle = 260, Caterpie = 260, Eevee = 650)[species]
  cp  <- round(10 + top * rbeta(n, 2, 2.5))
  mean(20 + 0.30 * cp)
}

# ---------------------------------------------------------------
# 2. 學習器（nuisance learners）
# ---------------------------------------------------------------
# kNN regression：同物種、同治療組裡，找 CP 最接近的 k 隻，取平均
knn_Q <- function(train, newdata, a, k = 10) {
  out <- rep(NA_real_, nrow(newdata))
  for (s in levels(newdata$species)) {
    tr <- train[train$species == s & train$A == a, ]
    id <- which(newdata$species == s)
    if (length(id) == 0) next
    if (nrow(tr) == 0) { out[id] <- mean(train$Y[train$A == a]); next }
    kk <- min(k, nrow(tr))
    D  <- abs(outer(newdata$cp[id], tr$cp, "-"))
    out[id] <- apply(D, 1, function(d) mean(tr$Y[order(d)[seq_len(kk)]]))
  }
  out
}

# 線性模型（刻意寫錯的「一條直線」）
lin_Q <- function(train, newdata, a) {
  fit <- lm(Y ~ A + cp, data = train)
  predict(fit, transform(newdata, A = a))
}

# propensity：logistic regression，每個物種有自己的截距與斜率
logit_g <- function(train, newdata) {
  fit <- glm(A ~ species + species:cp, family = binomial, data = train)
  bound(predict(fit, newdata, type = "response"))
}

# ---------------------------------------------------------------
# 3. 估計器：同一組 nuisance 預測，接成不同的答案
#    nu 是 data.frame(Q1, Q0, g)，每一列對應一個人
# ---------------------------------------------------------------
est_plugin <- function(nu) mean(nu$Q1 - nu$Q0)

est_ipw_ht <- function(dat, nu) {   # Horvitz–Thompson IPW：權重直接乘、除以 n
  mean(dat$A * dat$Y / nu$g - (1 - dat$A) * dat$Y / (1 - nu$g))
}

est_ipw <- function(dat, nu) {      # Hájek（stabilized）IPW：除以各組權重總和（投影片 60）
  w1 <- dat$A / nu$g; w0 <- (1 - dat$A) / (1 - nu$g)
  sum(w1 * dat$Y) / sum(w1) - sum(w0 * dat$Y) / sum(w0)
}

# EIF：D = H (Y - Q_A) + Q1 - Q0 - psi
eif <- function(dat, nu, psi) {
  H  <- dat$A / nu$g - (1 - dat$A) / (1 - nu$g)
  QA <- ifelse(dat$A == 1, nu$Q1, nu$Q0)
  H * (dat$Y - QA) + nu$Q1 - nu$Q0 - psi
}

wald <- function(psi, D) {
  se <- sqrt(mean(D^2) / length(D))
  c(est = psi, se = se, lo = psi - 1.96 * se, hi = psi + 1.96 * se)
}

est_aipw <- function(dat, nu) {     # one-step = plug-in + 平均修正項
  psi <- mean(eif(dat, nu, 0))      # psi = P_n[ Q1 - Q0 + H (Y - Q_A) ]
  wald(psi, eif(dat, nu, psi))
}

est_tmle <- function(dat, nu, ybounds = range(dat$Y)) {
  lo <- ybounds[1]; hi <- ybounds[2]
  sc <- function(y) pmin(pmax((y - lo) / (hi - lo), 1e-6), 1 - 1e-6)
  Ys <- (dat$Y - lo) / (hi - lo)
  Q1s <- sc(nu$Q1); Q0s <- sc(nu$Q0); QAs <- ifelse(dat$A == 1, Q1s, Q0s)
  H  <- dat$A / nu$g - (1 - dat$A) / (1 - nu$g)
  fl <- suppressWarnings(glm(Ys ~ -1 + H, offset = qlogis(QAs), family = quasibinomial()))
  eps <- unname(coef(fl))
  Q1t <- expit(qlogis(Q1s) + eps / nu$g)
  Q0t <- expit(qlogis(Q0s) - eps / (1 - nu$g))
  nu_t <- data.frame(Q1 = lo + (hi - lo) * Q1t, Q0 = lo + (hi - lo) * Q0t, g = nu$g)
  psi <- mean(nu_t$Q1 - nu_t$Q0)
  c(wald(psi, eif(dat, nu_t, psi)), eps = eps)
}

# ---------------------------------------------------------------
# 4. 產生 nuisance 預測：full sample（自己出題自己改）或 cross-fitting
# ---------------------------------------------------------------
fit_nuisance <- function(train, newdata, Qlearner = "knn", k = 10) {
  if (Qlearner == "knn") {
    Q1 <- knn_Q(train, newdata, 1, k); Q0 <- knn_Q(train, newdata, 0, k)
  } else {
    Q1 <- lin_Q(train, newdata, 1); Q0 <- lin_Q(train, newdata, 0)
  }
  data.frame(Q1 = Q1, Q0 = Q0, g = logit_g(train, newdata))
}

nuisance_full <- function(dat, ...) fit_nuisance(dat, dat, ...)

nuisance_crossfit <- function(dat, K = 5, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  fold <- sample(rep(seq_len(K), length.out = nrow(dat)))
  nu <- data.frame(Q1 = NA_real_, Q0 = NA_real_, g = NA_real_)[rep(1, nrow(dat)), ]
  for (k in seq_len(K)) {
    te <- fold == k
    nu[te, ] <- fit_nuisance(dat[!te, ], dat[te, ], ...)
  }
  rownames(nu) <- NULL
  nu
}

# 一次跑完所有估計器（給模擬與 webR 用）
all_estimates <- function(dat, k = 10, K = 5) {
  nf <- nuisance_full(dat, k = k)
  nc <- nuisance_crossfit(dat, K = K, k = k)
  rbind(
    naive        = c(est = mean(dat$Y[dat$A == 1]) - mean(dat$Y[dat$A == 0]), se = NA, lo = NA, hi = NA),
    linear       = c(est = unname(coef(lm(Y ~ A + cp, data = dat))["A"]), se = NA, lo = NA, hi = NA),
    plugin_knn   = c(est = est_plugin(nf), se = NA, lo = NA, hi = NA),
    ipw          = c(est = est_ipw(dat, nf), se = NA, lo = NA, hi = NA),
    aipw_full    = est_aipw(dat, nf),
    tmle_full    = est_tmle(dat, nf)[1:4],
    aipw_cf_DML  = est_aipw(dat, nc),
    tmle_cf      = est_tmle(dat, nc)[1:4]
  )
}

# ---------------------------------------------------------------
# 5. 只有模擬才做得到的事：知道真的 Q、g，把誤差一塊一塊算出來
#    「母體平均 P」用一大份同分布的資料（pop）近似
# ---------------------------------------------------------------
candy_population <- function(n = 20000, seed = 2027, strength = 1) simulate_candy(n, seed, strength)

# 回傳兩組拆解（投影片 61、85、88）：
#   plug-in：ψ̂_plug − ψ = 抽樣誤差 + first-order bias + empirical-process
#   AIPW   ：ψ̂_AIPW − ψ = (ℙn − P)D + R2 + empirical-process
decompose_candy <- function(dat, pop, k = 10, scheme = c("full", "crossfit"), K = 5, seed = 1) {
  scheme <- match.arg(scheme)
  psi <- candy_truth(); n <- nrow(dat)
  set.seed(seed)
  fold <- if (scheme == "full") rep(1L, n) else sample(rep(seq_len(K), length.out = n))
  nu <- data.frame(Q1 = numeric(n), Q0 = numeric(n), g = numeric(n))
  first_order <- 0; R2 <- 0; gerr <- 0; qerr <- 0
  for (f in unique(fold)) {
    te <- fold == f
    tr <- if (scheme == "full") rep(TRUE, n) else !te
    nu[te, ] <- fit_nuisance(dat[tr, ], dat[te, ], k = k)
    np <- fit_nuisance(dat[tr, ], pop, k = k)          # 同一個模型，拿去預測「母體」
    w <- mean(te)
    first_order <- first_order + w * mean((np$Q1 - pop$Q1) - (np$Q0 - pop$Q0))
    R2 <- R2 + w * mean((np$g - pop$g0) / np$g * (np$Q1 - pop$Q1) +
                        (np$g - pop$g0) / (1 - np$g) * (np$Q0 - pop$Q0))
    if (f == fold[1]) units <- data.frame(species = pop$species, cp = pop$cp,
                                          g_err = np$g - pop$g0, Q1_err = np$Q1 - pop$Q1, Q0_err = np$Q0 - pop$Q0,
                                          R2_unit = (np$g - pop$g0) / np$g * (np$Q1 - pop$Q1) +
                                                    (np$g - pop$g0) / (1 - np$g) * (np$Q0 - pop$Q0),
                                          first_order_unit = (np$Q1 - pop$Q1) - (np$Q0 - pop$Q0))
    gerr <- gerr + w * mean((np$g - pop$g0)^2)
    qerr <- qerr + w * mean(((np$Q1 - pop$Q1)^2 + (np$Q0 - pop$Q0)^2) / 2)
  }
  plug <- mean(nu$Q1 - nu$Q0)
  aipw <- est_aipw(dat, nu)[["est"]]
  true_nu <- data.frame(Q1 = dat$Q1, Q0 = dat$Q0, g = dat$g0)
  samp_plug <- mean(dat$Q1 - dat$Q0) - psi
  samp_aipw <- mean(eif(dat, true_nu, psi))           # (ℙn − P)D，因為 P D = 0
  list(
    plugin = c(total = plug - psi, sampling = samp_plug, first_order = first_order,
               emp_process = plug - psi - samp_plug - first_order),
    aipw = c(total = aipw - psi, sampling = samp_aipw, R2 = R2,
             emp_process = aipw - psi - samp_aipw - R2),
    L2 = c(g = sqrt(gerr), Q = sqrt(qerr), product = sqrt(gerr) * sqrt(qerr)),
    units = units     # 母體裡每一隻的誤差（cross-fit 時是第一折的模型），畫圖用
  )
}

# Neyman orthogonality（投影片 87）：讓 g、Q 從真值沿一個方向偏 t，看兩種 moment 怎麼動
#   plug-in moment：E[Q1t − Q0t] − ψ
#   EIF moment    ：E[D(O; ψ, g_t, Q_t)]（用條件期望算，沒有抽樣雜訊）
orthogonality_curves <- function(pop, ts = seq(-1, 1, by = 0.05), dq1 = 40, dq0 = -20, dg = 0.6) {
  g <- pop$g0; Q1 <- pop$Q1; Q0 <- pop$Q0
  psi <- mean(Q1 - Q0)
  gbar <- g + dg * g * (1 - g)                      # 往外推一點、保證 t∈[−1, 1] 時仍在 (0, 1)
  eif_moment <- function(t, move_g, move_Q) {
    gt <- if (move_g) g + t * (gbar - g) else g
    q1 <- if (move_Q) Q1 + t * dq1 else Q1
    q0 <- if (move_Q) Q0 + t * dq0 else Q0
    mean(q1 - q0 + g / gt * (Q1 - q1) - (1 - g) / (1 - gt) * (Q0 - q0)) - psi
  }
  data.frame(t = ts,
             plugin_Q = ts * (dq1 - dq0),
             eif_Q_only = sapply(ts, eif_moment, move_g = FALSE, move_Q = TRUE),
             eif_g_only = sapply(ts, eif_moment, move_g = TRUE, move_Q = FALSE),
             eif_both = sapply(ts, eif_moment, move_g = TRUE, move_Q = TRUE))
}

# 權重的健康檢查：最大權重、有效樣本數（ESS）
weight_summary <- function(dat, nu) {
  w <- ifelse(dat$A == 1, 1 / nu$g, 1 / (1 - nu$g))
  c(max_weight = max(w),
    ESS_treated = sum(w[dat$A == 1])^2 / sum(w[dat$A == 1]^2), n_treated = sum(dat$A),
    ESS_control = sum(w[dat$A == 0])^2 / sum(w[dat$A == 0]^2), n_control = sum(1 - dat$A))
}

# 理論上最小的標準誤：sqrt(V_eff / n)，V_eff = E[σ²/g + σ²/(1−g)] + Var(τ)，σ = 30
candy_se_bound <- function(pop, n) {
  sqrt((mean(30^2 / pop$g0 + 30^2 / (1 - pop$g0)) + var(pop$Q1 - pop$Q0)) / n)
}
