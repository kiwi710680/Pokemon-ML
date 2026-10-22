# nhefs_helpers.R — NHEFS 資料整理、候選 learner、sl_lite、nuisance 與估計面板
# 需先 source("R/helpers.R")（AIPW / TMLE / EIF 與糖果例子共用）

nhefs <- read.csv("data/nhefs.csv")
dat <- subset(nhefs, !is.na(wt82_71))
dat <- data.frame(Y = dat$wt82_71, A = dat$qsmk,
                  sex = factor(dat$sex), race = factor(dat$race), age = dat$age,
                  education = factor(dat$education), smokeintensity = dat$smokeintensity,
                  smokeyrs = dat$smokeyrs, exercise = factor(dat$exercise),
                  active = factor(dat$active), wt71 = dat$wt71)
cat(sprintf("complete cases n = %d; quit smoking %d (%.1f%%)\n", nrow(dat), sum(dat$A), 100 * mean(dat$A)))

# ---------------- 候選 learner（都是「公式」，換掉公式就換掉模型）----------------
rhs <- list(
  mean   = "1",
  main   = "as.numeric(sex) + as.numeric(race) + age + as.numeric(education) + smokeintensity + smokeyrs + as.numeric(exercise) + as.numeric(active) + wt71",
  hr     = "sex + race + age + I(age^2) + education + smokeintensity + I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + exercise + active + wt71 + I(wt71^2)",
  spline = "sex + race + education + exercise + active + splines::ns(age, 3) + splines::ns(smokeintensity, 3) + splines::ns(smokeyrs, 3) + splines::ns(wt71, 3)"
)
# Q 的公式另外放入 A 以及 A × smokeintensity（Hernán & Robins 的標準化模型）
q_formula <- function(r) as.formula(if (r == "1") "Y ~ A" else paste("Y ~ A + A:smokeintensity +", r))
g_formula <- function(r) as.formula(paste("A ~", r))

fit_pred <- function(train, newdata, learner, what) {
  if (what == "g") {
    f <- glm(g_formula(rhs[[learner]]), family = binomial, data = train)
    predict(f, newdata, type = "response")
  } else {
    f <- lm(q_formula(rhs[[learner]]), data = train)
    predict(f, newdata)
  }
}

# sl_lite：內層 V 折 CV 的預測 → 找非負、加總為 1 的權重，使平方誤差最小
convex_weights <- function(Z, y) {
  obj <- function(b) { w <- exp(c(0, b)); w <- w / sum(w); mean((y - Z %*% w)^2) }
  b <- optim(rep(0, ncol(Z) - 1), obj, method = "BFGS")$par
  w <- exp(c(0, b)); setNames(w / sum(w), colnames(Z))
}

sl_lite <- function(train, newdata, what, learners = names(rhs), V = 5) {
  y <- if (what == "g") train$A else train$Y
  fold <- sample(rep(seq_len(V), length.out = nrow(train)))
  Z <- matrix(NA_real_, nrow(train), length(learners), dimnames = list(NULL, learners))
  for (v in seq_len(V)) for (l in learners)
    Z[fold == v, l] <- fit_pred(train[fold != v, ], train[fold == v, ], l, what)
  w <- convex_weights(Z, y)
  P <- sapply(learners, function(l) fit_pred(train, newdata, l, what))
  structure(as.vector(P %*% w), weights = w)
}

# nuisance：同一套 learner 產生 Q1, Q0, g（g_raw 是截斷前的 propensity）
nhefs_nuisance <- function(train, newdata, learner = "sl_lite", bounds = c(0.025, 0.975)) {
  pr <- function(nd, what) if (learner == "sl_lite") sl_lite(train, nd, what) else fit_pred(train, nd, learner, what)
  g_raw <- pr(newdata, "g")
  data.frame(Q1 = pr(transform(newdata, A = 1), "Q"),
             Q0 = pr(transform(newdata, A = 0), "Q"),
             g  = bound(g_raw, bounds[1], bounds[2]), g_raw = g_raw)
}
nhefs_crossfit <- function(dat, learner = "sl_lite", K = 5, seed = 1, bounds = c(0.025, 0.975)) {
  set.seed(seed)
  fold <- sample(rep(seq_len(K), length.out = nrow(dat)))
  nu <- data.frame(Q1 = rep(NA_real_, nrow(dat)), Q0 = NA_real_, g = NA_real_, g_raw = NA_real_)
  for (k in seq_len(K)) nu[fold == k, ] <- nhefs_nuisance(dat[fold != k, ], dat[fold == k, ], learner, bounds)
  nu
}

# 四個估計器（g-computation、IPTW、AIPW、TMLE），同一組 nuisance
nhefs_panel <- function(dat, nu, label) {
  rbind(
    data.frame(estimator = "g-computation", scheme = label, est = est_plugin(nu), se = NA, lo = NA, hi = NA),
    data.frame(estimator = "IPTW (Hajek)",  scheme = label, est = est_ipw(dat, nu), se = NA, lo = NA, hi = NA),
    data.frame(estimator = "AIPW",          scheme = label, t(est_aipw(dat, nu))),
    data.frame(estimator = "TMLE",          scheme = label, t(est_tmle(dat, nu)[1:4]))
  )
}

# 給 balance 診斷用的數值版共變數（類別變數展開成 0/1）
nhefs_X <- function(dat) {
  X <- as.data.frame(model.matrix(~ sex + race + age + education + smokeintensity + smokeyrs + exercise + active + wt71 - 1, data = dat))
  X[, setdiff(names(X), "sex0"), drop = FALSE]
}
