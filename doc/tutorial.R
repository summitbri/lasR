## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
suppressPackageStartupMessages(library(lasR))
col = grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))(15)
print.list <- function(my_list, indent = 0) {
  if (is.null(names(my_list))) {
    for (element in my_list) {
      if (is.list(element)) {
        cat(rep(" ", indent), "- List:\n")
        print.list(element, indent + 1)
      } else {
        cat(rep(" ", indent), "-", element, "\n")
      }
    }
  } else {
    for (i in seq_along(my_list)) {
      name = names(my_list)[i]
      cat(rep("  ", indent), "-", name, ": ")
      if (is.list(my_list[[i]])) {
        cat("\n")
        print.list(my_list[[i]], indent + 1)
      } else {
        cat(my_list[[i]], "\n")
      }
    }
  }
}

## ----echo = F-----------------------------------------------------------------
f = paste0(system.file(package="lasR"), "/extdata/bcts")

## ----reader-------------------------------------------------------------------
pipeline = reader_las()
exec(pipeline, on = f)

## ----echo = FALSE-------------------------------------------------------------
f <- system.file("extdata", "Topography.las", package="lasR")

## ----triangulate1-------------------------------------------------------------
pipeline = reader_las() + triangulate(filter = "Classification == 2")
ans = exec(pipeline, on = f)
ans

## ----triangulate2, fig.width=5, fig.height=5----------------------------------
pipeline = reader_las() + triangulate(filter = keep_ground(), ofile = tempgpkg())
ans = exec(pipeline, on = f)
ans

par(mar = c(2, 2, 1, 1))
plot(ans, axes = T, lwd = 0.5)

## ----rasterize----------------------------------------------------------------
# omitting reader_las() for the example
del = triangulate(filter = keep_ground())
dtm = rasterize(1, del)
pipeline = del + dtm
ans = exec(pipeline, on = f)
ans

## ----plotdtm, fig.height=3.5--------------------------------------------------
terra::plot(ans, col = gray.colors(25,0,1), mar = c(1, 1, 1, 3))

## ----echo = FALSE-------------------------------------------------------------
f <- system.file("extdata", "Megaplot.las", package="lasR")

## ----rasterize2, fig.show="hold", fig.height=6,  out.width="50%"--------------
del <- triangulate(filter = keep_first())
chm1 <- rasterize(2, "max")
chm2 <- rasterize(0.5, del)
pipeline <- del + chm1 + chm2
ans <- exec(pipeline, on = f)

terra::plot(ans[[1]], mar = c(1, 1, 1, 3), col = col)
terra::plot(ans[[2]], mar = c(1, 1, 1, 3), col = col)

## ----echo = FALSE-------------------------------------------------------------
f <- system.file("extdata", "Topography.las", package="lasR")

## ----twt, warning=FALSE-------------------------------------------------------
del = triangulate(filter = keep_ground())
norm = transform_with(del, "-")
pipeline = del + norm
ans = exec(pipeline, on = f)
ans

## ----twt2, warning=FALSE, fig.height=4, fig.width=8---------------------------
del = triangulate(filter = keep_ground())
norm = transform_with(del, "-")
chm1 = rasterize(2, "max")
chm2 = rasterize(2, "max")
pipeline = chm1 + del + norm + chm2
ans = exec(pipeline, on = f)

col = grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))(15)
terra::plot(c(ans[[1]], ans[[2]]), col = col)

## ----echo = FALSE-------------------------------------------------------------
f = paste0(system.file(package="lasR"), "/extdata/bcts")
f = list.files(f, pattern = "(?i)\\.la(s|z)$", full.names = TRUE)
f = f[1:2]

## ----writelas, warning=FALSE--------------------------------------------------
write1 = write_las(paste0(tempdir(), "/*_ground.laz"), filter = keep_ground())
write2 = write_las(paste0(tempdir(), "/*_normalized.laz"), )
del = triangulate(filter = keep_ground())
norm = transform_with(del, "-")
pipeline =  write1 + del + norm + write2
ans = exec(pipeline, on = f)
ans

## ----echo = FALSE-------------------------------------------------------------
f <- system.file("extdata", "Example.las", package="lasR")
f = c(f,f)

## ----writelas2----------------------------------------------------------------
ofile = paste0(tempdir(), "/dataset_merged.laz")
merge = reader_las() + write_las(ofile)
ans = exec(merge, on = f)
ans

## ----echo = FALSE-------------------------------------------------------------
f <- system.file("extdata", "MixedConifer.las", package="lasR")

## ----its----------------------------------------------------------------------
del = triangulate(filter = keep_first())
chm = rasterize(0.5, del)
chm2 = pit_fill(chm)
seed = local_maximum_raster(chm2, 3)
tree = region_growing(chm2, seed)
pipeline = del + chm + chm2 +  seed + tree
ans = exec(pipeline, on = f)

## ----fig.show="hold", fig.width=4---------------------------------------------
col = grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))(25)
col2 = grDevices::colorRampPalette(c("purple", "blue", "cyan2", "yellow", "red", "green"))(50)
terra::plot(ans$rasterize, col = col, mar = c(1, 1, 1, 3))
terra::plot(ans$pit_fill, col = col, mar = c(1, 1, 1, 3))
terra::plot(ans$region_growing, col = col2[sample.int(50, 277, TRUE)], mar = c(1, 1, 1, 3))
plot(ans$local_maximum$geom, add = T, pch = 19, cex = 0.5)

## ----echo = FALSE-------------------------------------------------------------
f <- system.file("extdata", "Topography.las", package="lasR")

## ----hulls, fig.width=5, fig.height=5-----------------------------------------
del = triangulate(15, filter = keep_ground(), ofile = tempgpkg())
ans = exec(del, on = f)

par(mar = c(2, 2, 1, 1))
plot(ans, axes = T, lwd = 0.5)

## ----hulls2, fig.width=5, fig.height=5----------------------------------------
del = triangulate(15, filter = keep_ground())
bound = hulls(del)
ans = exec(del+bound, on = f)

par(mar = c(2, 2, 1, 1))
plot(ans, axes = T, lwd = 0.5, col = "gray")

## ----readers, eval = FALSE----------------------------------------------------
# my_metric_fun = function(data) { mean(data$Z) }
# tri <- triangulate(filter = keep_ground())
# trans <- transform_with(tri)
# norm <- tri + trans
# metric <- callback(my_metric_fun, expose = "z", drop_buffer = TRUE)
# pipeline = norm + metric

## ----eval = FALSE-------------------------------------------------------------
# pipeline = reader_las_circles(xcenter, ycenter, 11.28) + pipeline

## ----echo=FALSE---------------------------------------------------------------
f = paste0(system.file(package="lasR"), "/extdata/bcts")
f = list.files(f, pattern = "(?i)\\.la(s|z)$", full.names = TRUE)

## ----summary------------------------------------------------------------------
read = reader_las()
summary = summarise()
pipeline = read + summary
ans = exec(pipeline, on = f)
head(ans)

## ----echo = FALSE-------------------------------------------------------------
f <- c(system.file("extdata", "Topography.las", package="lasR"))

## ----sampling-----------------------------------------------------------------
pipeline = summarise() + sampling_voxel(4) + summarise()
ans = exec(pipeline, on = f)
print(head(ans[[1]]))
print(head(ans[[2]]))

## ----inventory, eval=FALSE----------------------------------------------------
# ofiles_plot <- paste0(tempdir(), "/plot_*.las")
# ofiles_plot_norm <- paste0(tempdir(), "/plot_*_norm.las")
# 
# library(sf)
# inventory <- st_read("shapefile.shp")
# coordinates <- st_coordinates(inventory)
# xcenter <- coordinates[,1]
# ycenter <- coordinates[,2]
# 
# read <- reader_las(xc = xcenter, yc = ycenter, r = 11.28)
# tri <- triangulate(filter = keep_ground())
# trans <- transform_with(tri)
# norm <- tri + trans
# metrics <- summarise(metrics = c("z_mean", "z_p95", "i_median", "count"))
# write1 <- write_las(ofiles_plot)
# write2 <- write_las(ofiles_plot_norm)
# 
# pipeline = read + write1 + norm + write2

## ----echo = FALSE-------------------------------------------------------------
f = paste0(system.file(package="lasR"), "/extdata/bcts/")
f = list.files(f, pattern = "(?i)\\.la(s|z)$", full.names = TRUE)
f = f[1:2]

## ----nowildcard, fig.height=4, fig.width=8, fig.show="hold"-------------------
ofile = paste0(tempdir(), "/chm.tif")   # no wildcard

x = c(885100, 885100)
y = c(629200, 629600)

pipeline = reader_las(xc = x, yc = y, r = 20) + rasterize(2, "max", ofile = ofile)
r0 = exec(pipeline, on = f)

terra::plot(r0, col = col) # covers the entire collection of files

## ----wildcard, fig.height=6, fig.show="hold", out.width="50%"-----------------
ofile = paste0(tempdir(), "/chm_*.tif") # wildcard

x = c(885100, 885100)
y = c(629200, 629600)

pipeline = reader_las(xc = x, yc = y, r = 20) + rasterize(2, "max", ofile = ofile)
ans = exec(pipeline, on = f)

r1 = terra::rast(ans[1])
r2 = terra::rast(ans[2])
terra::plot(r1, col = col)
terra::plot(r2, col = col)

