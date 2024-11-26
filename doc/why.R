## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = F----------------------------------------------------------
suppressPackageStartupMessages(library(lasR))

## ----fig.width=5, fig.height=3.5, warning=F, echo=-1, echo=FALSE, fig.align = 'center'----
col1 = rgb(0, 0.5, 1, alpha = 0.5)
col2 = rgb(1, 0, 0, alpha = 0.5)

par(mar = c(4,4,1,1))

m_lasr = c(0, 82.03, 170.51, 354.01, 531.90, 711.60, 895.17, 1074.86, 1214.60, 1259.20, 1297.35, 1297.35, 435.67,550.40, 692.43, 831.39, 971.39, 1113.44, 1282.57, 1285.92, 1345.21, 
1345.21, 498.85, 559.43, 697.85, 837.33, 977.29, 1127.08, 1258.82, 1240.92, 1240.92, 1240.92, 480.92, 606.07, 749.79, 887.94, 1026.13, 1169.48, 1358.71, 1378.30, 1423.42, 1423.42, 
554.91, 560.07, 0)
t_lasr = 1:length(m_lasr)*2/60

m_lidr = c(0, 104.86, 456.99, 802.07, 1116.71, 1765.46, 2116.21, 2502.28, 2276.53, 2666.14, 2986.86, 2300.90, 2686.19, 2989.73, 2004.21, 2579.39, 3195.28, 2827.48, 3512.35, 4633.82, 
5221.53, 5639.37, 3357.23, 4434.14, 5110.58, 4302.41, 3457.53, 4052.46, 4702.15, 4430.27, 4585.12, 4937.42, 5243.30,4713.65, 5063.31, 5376.67, 2866.46, 3207.25, 3459.13, 3188.25, 
3523.44, 3733.16, 0)

t_lidr = 1:length(m_lidr)*8/60

x = t_lidr
y = m_lidr/1000

par(mar = c(4,4,1,1))

plot(x, y, type = "n", ylim = c(0, max(y)), ylab = "Memory (GB)", xlab = "Time (min)")

polygon(c(x, rev(x)), c(rep(0, length(x)), rev(y)), col = col1, border = NA)
lines(x, y, col = "blue", lwd = 2)

x = t_lasr
y = m_lasr/1000

polygon(c(x, rev(x)), c(rep(0, length(x)), rev(y)), col = col2 , border = NA)
lines(x, y, col = "red", lwd = 2)

legend("topleft", legend = c("lidR", "lasR"), fill = c( col = col1, col2), border = NA)

