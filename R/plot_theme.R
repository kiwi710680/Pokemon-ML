# plot_theme.R — 與投影片一致的配色（只在本機 R 產圖時使用）
pal <- list(bg = "#FBF8F3", ink = "#1C1B19", muted = "#6B665E", rule = "#DDD6CA",
            red = "#D9480F", teal = "#0B7285", gold = "#E8A317", plum = "#7B3F8C")
species_col <- c(Pidgey = "#B5651D", Weedle = "#C9A227", Caterpie = "#4C9A2A", Eevee = "#8E5B9E")

if (.Platform$OS.type == "windows") {
  windowsFonts(TC = windowsFont("Microsoft JhengHei"))
  FONT <- "TC"
} else FONT <- "sans"

open_png <- function(file, w = 1600, h = 1000) {
  png(file, width = w, height = h, res = 200, bg = pal$bg, family = FONT)
  par(mar = c(5, 6, 1.2, 1.2), mgp = c(3.4, 0.7, 0), col.axis = pal$muted, col = pal$ink,
      col.lab = pal$ink, fg = pal$rule, las = 1, cex.lab = 1.15, tcl = -0.25, bty = "n")
}

grid_lines <- function() {
  abline(h = axTicks(2), col = pal$rule, lwd = 0.8)
}
