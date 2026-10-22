# engine.R — 真實資料（Lab 3–6）共用的 nuisance 引擎、資料讀取與診斷
# 需先 source("R/helpers.R")：估計器 est_plugin / est_ipw / est_aipw / est_tmle / eif 都在那裡。
# 符號與投影片一致：O = (X, A, Y)，g(X) = P(A = 1 | X)，Q(a, X) = E[Y | A = a, X]

# ---------------------------------------------------------------
# 1. 資料讀取：都回傳 list(name, X, A, Y, binary)
# ---------------------------------------------------------------
load_rhc <- function(path = "data/rhc.csv") {
  d <- read.csv(path, stringsAsFactors = TRUE)
  d[[1]] <- NULL                                    # 第一欄是列號
  A <- as.integer(d$swang1 == "RHC")
  Y <- as.integer(d$dth30 == "Yes")
  # 和 Karim (2026) 相同：拿掉治療、結果、日期與 ID、三個缺失太多的欄位
  drop <- c("swang1", "death", "dth30", "sadmdte", "dschdte", "dthdte", "lstctdte",
            "t3d30", "ptid", "cat2", "adld3p", "urin1")
  covs <- droplevels(d[, setdiff(names(d), drop), drop = FALSE])
  X <- as.data.frame(model.matrix(~ . - 1, data = covs))
  colnames(X) <- make.names(colnames(X), unique = TRUE)
  list(name = "RHC", X = X, A = A, Y = Y, binary = TRUE, raw = covs)
}

load_ihdp <- function(r = 1, dir = "data/ihdp") {
  d <- read.csv(file.path(dir, sprintf("ihdp_npci_%d.csv", r)), header = FALSE)
  names(d) <- c("treatment", "y_factual", "y_cfactual", "mu0", "mu1", paste0("x", 1:25))
  list(name = sprintf("IHDP #%d", r), X = d[, paste0("x", 1:25)], A = d$treatment,
       Y = d$y_factual, binary = FALSE,
       truth = mean(d$mu1 - d$mu0),                 # 這份資料的真值（模擬出來的，所以知道）
       mu0 = d$mu0, mu1 = d$mu1)
}

read_nsw <- function(file) {
  d <- read.table(file, header = FALSE)
  names(d) <- c("treat", "age", "educ", "black", "hisp", "married", "nodegree", "re74", "re75", "re78")
  d
}
load_lalonde <- function(setting = c("psid", "experimental"), dir = "data/lalonde") {
  setting <- match.arg(setting)
  tr <- read_nsw(file.path(dir, "nswre74_treated.txt"))
  ct <- read_nsw(file.path(dir, "nswre74_control.txt"))
  benchmark <- mean(tr$re78) - mean(ct$re78)        # 隨機分派：直接相減就是 NSW 族群的效果（約 1794）
  d <- if (setting == "experimental") rbind(tr, ct) else rbind(tr, read_nsw(file.path(dir, "psid_controls.txt")))
  covs <- c("age", "educ", "black", "hisp", "married", "nodegree", "re74", "re75")
  list(name = paste0("LaLonde (", setting, ")"), X = d[, covs], A = d$treat, Y = d$re78,
       binary = FALSE, benchmark = benchmark)
}

# ---------------------------------------------------------------
# 2. 學習器：glm（主效應）或 rf（random forest）
#    回傳在 newX 上的預測；Q 的部分回傳 Q1 = Q̂(1, X)、Q0 = Q̂(0, X)
# ---------------------------------------------------------------
learn_g <- function(X, A, newX, learner = "glm", ntree = 100) {
  if (learner == "glm") {
    fit <- suppressWarnings(glm(A ~ ., data = data.frame(A = A, X), family = binomial))
    return(unname(suppressWarnings(predict(fit, newX, type = "response"))))
  }
  fit <- randomForest::randomForest(x = X, y = factor(A, levels = 0:1), ntree = ntree)
  unname(predict(fit, newX, type = "prob")[, "1"])
}

learn_Q <- function(X, A, Y, newX, binary, learner = "glm", ntree = 100) {
  tr <- data.frame(A = A, X)
  n1 <- data.frame(A = 1, newX); n0 <- data.frame(A = 0, newX)
  if (learner == "glm") {
    fit <- if (binary) suppressWarnings(glm(Y ~ ., data = data.frame(Y = Y, tr), family = binomial))
           else lm(Y ~ ., data = data.frame(Y = Y, tr))
    pr <- function(nd) unname(suppressWarnings(predict(fit, nd, type = "response")))
    return(list(Q1 = pr(n1), Q0 = pr(n0)))
  }
  if (binary) {
    fit <- randomForest::randomForest(x = tr, y = factor(Y, levels = 0:1), ntree = ntree)
    pr <- function(nd) unname(predict(fit, nd, type = "prob")[, "1"])
  } else {
    fit <- randomForest::randomForest(x = tr, y = Y, ntree = ntree)
    pr <- function(nd) unname(predict(fit, nd))
  }
  list(Q1 = pr(n1), Q0 = pr(n0))
}

# full：用全部資料訓練、再預測同一批人（自己出題自己改）
# crossfit：每個人的預測都來自沒看過他的 K−1 折
nuisance <- function(d, learner = "glm", scheme = c("full", "crossfit"), K = 5, seed = 1,
                     bounds = c(0.025, 0.975), ntree = 100) {
  scheme <- match.arg(scheme)
  n <- length(d$A)
  set.seed(seed)
  fold <- if (scheme == "full") rep(1L, n) else sample(rep(seq_len(K), length.out = n))
  out <- data.frame(Q1 = numeric(n), Q0 = numeric(n), g_raw = numeric(n))
  for (k in unique(fold)) {
    te <- fold == k
    tr <- if (scheme == "full") rep(TRUE, n) else !te
    q <- learn_Q(d$X[tr, , drop = FALSE], d$A[tr], d$Y[tr], d$X[te, , drop = FALSE], d$binary, learner, ntree)
    out$Q1[te] <- q$Q1; out$Q0[te] <- q$Q0
    out$g_raw[te] <- learn_g(d$X[tr, , drop = FALSE], d$A[tr], d$X[te, , drop = FALSE], learner, ntree)
  }
  if (d$binary) { out$Q1 <- pmin(pmax(out$Q1, 0), 1); out$Q0 <- pmin(pmax(out$Q0, 0), 1) }
  out$g <- pmin(pmax(out$g_raw, bounds[1]), bounds[2])
  out$fold <- fold
  out
}

# 一組 nuisance → 四個估計器（同一組食材，四種料理方式）
panel <- function(d, nu, label = "") {
  dd <- list(A = d$A, Y = d$Y)
  yb <- if (d$binary) c(0, 1) else range(d$Y)
  a <- est_aipw(dd, nu); t <- est_tmle(dd, nu, ybounds = yb)
  data.frame(scheme = label,
             estimator = c("g-computation", "IPTW (Hajek)", "AIPW", "TMLE"),
             est = c(est_plugin(nu), est_ipw(dd, nu), a[["est"]], t[["est"]]),
             se = c(NA, NA, a[["se"]], t[["se"]]),
             lo = c(NA, NA, a[["lo"]], t[["lo"]]),
             hi = c(NA, NA, a[["hi"]], t[["hi"]]))
}

# 殘差與修正項：看模型是不是在「自己改考卷」
residual_check <- function(d, nu) {
  QA <- ifelse(d$A == 1, nu$Q1, nu$Q0)
  H  <- d$A / nu$g - (1 - d$A) / (1 - nu$g)
  c(resid_rms = sqrt(mean((d$Y - QA)^2)), correction = mean(H * (d$Y - QA)))
}

# ---------------------------------------------------------------
# 3. 診斷：overlap 與 balance
# ---------------------------------------------------------------
# 標準化平均差（SMD）：|SMD| < 0.1 常被當作「兩組差不多」
smd <- function(x, A, w = rep(1, length(A))) {
  m1 <- weighted.mean(x[A == 1], w[A == 1]); m0 <- weighted.mean(x[A == 0], w[A == 0])
  s <- sqrt((var(x[A == 1]) + var(x[A == 0])) / 2)
  if (s == 0) return(0)
  (m1 - m0) / s
}
balance_table <- function(X, A, g) {
  w <- ifelse(A == 1, 1 / g, 1 / (1 - g))
  data.frame(variable = names(X),
             before = sapply(X, smd, A = A),
             after_IPW = sapply(X, smd, A = A, w = w), row.names = NULL)
}

overlap_summary <- function(g_raw, A, bounds = c(0.025, 0.975)) {
  gb <- pmin(pmax(g_raw, bounds[1]), bounds[2])
  w <- ifelse(A == 1, 1 / gb, 1 / (1 - gb))
  c(min_g = min(g_raw), max_g = max(g_raw),
    pct_clipped = 100 * mean(g_raw < bounds[1] | g_raw > bounds[2]),
    max_weight = max(w),
    ESS_treated = sum(w[A == 1])^2 / sum(w[A == 1]^2),
    ESS_control = sum(w[A == 0])^2 / sum(w[A == 0]^2))
}

# ---------------------------------------------------------------
# 4. 畫圖小工具（本機 R 與 webR 共用；只用 base graphics）
# ---------------------------------------------------------------
COL <- list(red = "#D9480F", teal = "#0B7285", ink = "#1C1B19", muted = "#6B665E",
            gold = "#C98A0C", plum = "#7B3F8C", rule = "#DDD6CA")

lab_par <- function(...) par(mar = c(4.5, 5, 1.5, 1), las = 1, bty = "n", col.axis = COL$muted, ...)

# 兩組 propensity 的鏡像直方圖：上面治療組、下面對照組
overlap_plot <- function(g_raw, A, xlab = "estimated propensity score  ĝ(X)", labels = c("A = 1", "A = 0"), breaks = 40) {
  br <- seq(0, 1, length.out = breaks + 1)
  h1 <- hist(pmin(pmax(g_raw[A == 1], 0), 1), breaks = br, plot = FALSE)$counts
  h0 <- hist(pmin(pmax(g_raw[A == 0], 0), 1), breaks = br, plot = FALSE)$counts
  lab_par()
  plot(NA, xlim = c(0, 1), ylim = c(-max(h0), max(h1)), axes = FALSE, xlab = xlab, ylab = "count")
  axis(1); yt <- pretty(c(-max(h0), max(h1))); axis(2, at = yt, labels = abs(yt))
  rect(br[-length(br)], 0, br[-1], h1, col = adjustcolor(COL$red, .8), border = "white")
  rect(br[-length(br)], 0, br[-1], -h0, col = adjustcolor(COL$teal, .8), border = "white")
  abline(h = 0)
  text(0.98, max(h1) * .85, labels[1], adj = 1, col = COL$red, font = 2)
  text(0.98, -max(h0) * .85, labels[2], adj = 1, col = COL$teal, font = 2)
}

# forest plot：每一列一個估計值（有 CI 就畫線段），可加參考線
forest_plot <- function(lab, est, lo = NA, hi = NA, ref = NULL, ref_lab = "", col = COL$ink, xlab = "estimate", xlim = NULL) {
  n <- length(est); y <- n:1
  rng <- range(c(est, lo, hi, ref), na.rm = TRUE)
  if (is.null(xlim)) xlim <- rng + c(-1, 1) * diff(rng) * 0.08
  op <- par(mar = c(4.5, min(22, max(8, 0.62 * max(nchar(lab)) + 1)), 1.5, 1), las = 1, bty = "n", col.axis = COL$muted); on.exit(par(op))
  plot(NA, xlim = xlim, ylim = c(0.5, n + 0.5), axes = FALSE, xlab = xlab, ylab = "")
  abline(h = y, col = "#F0EBE3")
  if (!is.null(ref)) { abline(v = ref, lty = 2, col = COL$muted); pos <- (ref[1] - xlim[1]) / diff(xlim); mtext(ref_lab, 3, at = ref[1], adj = if (pos > 0.7) 1 else if (pos < 0.3) 0 else 0.5, cex = .8, col = COL$muted) }
  ok <- !is.na(lo)
  segments(lo[ok], y[ok], hi[ok], y[ok], lwd = 3, col = rep_len(col, n)[ok])
  points(est, y, pch = 19, cex = 1.5, col = rep_len(col, n))
  axis(1); axis(2, at = y, labels = lab, tick = FALSE, col.axis = COL$ink)
}
