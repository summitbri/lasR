## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
suppressPackageStartupMessages(library(lasR))
col = grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))(25)

## ----fig.show='hold', fig.width=4---------------------------------------------
f <- system.file("extdata", "Megaplot.las", package="lasR")
aba  = rasterize(20, "zmean")      # ABA
baba = rasterize(c(5,20), "zmean") # BABA
pipeline = aba + baba
ans = exec(pipeline, on = f)

terra::plot(ans[[1]], col = col, main = "ABA")
terra::plot(ans[[2]], col = col, main = "BABA")

## ----fig.show='hold', fig.width=4---------------------------------------------
f <- system.file("extdata", "Topography.las", package="lasR")
c1 <- rasterize(1, "count")
c2  <- rasterize(c(1,4), "count")
pipeline = c1 + c2
res <- exec(pipeline, on = f)
terra::plot(res[[1]]/4, col = gray.colors(15,0,1), main = "Regular")   # divide by 4 to get the density
terra::plot(res[[2]]/25, col = gray.colors(15,0,1), main = "Moving windows")  # divide by 25 to get the density

