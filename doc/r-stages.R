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

## ----echo = FALSE-------------------------------------------------------------
f <- system.file("extdata", "Topography.las", package="lasR")

## ----rasterize3, fig.height=3.5-----------------------------------------------
pipeline = rasterize(10, median(Intensity))
ans = exec(pipeline, on = f)

terra::plot(ans, mar = c(1, 1, 1, 3), col = heat.colors(15))

## ----echo = FALSE-------------------------------------------------------------
f <- c(system.file("extdata", "Topography.las", package="lasR"), system.file("extdata", "Megaplot.las", package="lasR"))

## ----callback-----------------------------------------------------------------
meanz = function(data){ return(mean(data$Z)) }
call = callback(meanz, expose = "xyz")
ans = exec(call, on = f)
print(ans)

## ----callback2----------------------------------------------------------------
edit_points = function(data)
{
  data$Classification[5:7] = c(2L,2L,2L)
  data$Withheld = FALSE
  data$Withheld[12] = TRUE
  return(data)
}

call = callback(edit_points, expose = "xyzc")
ans = exec(call, on = f)
ans

## ----callback3----------------------------------------------------------------
read_las = function(f, select = "xyzi", filter = "")
{
  load = function(data) { return(data) }
  read = reader_las(filter = filter)
  call = callback(load, expose = select, no_las_update = TRUE)
  return (exec(read+call, on = f))
}

f <- system.file("extdata", "Topography.las", package="lasR")
las = read_las(f)
head(las)

## ----echo = FALSE-------------------------------------------------------------
f = paste0(system.file(package="lasR"), "/extdata/bcts")
f = list.files(f, pattern = "(?i)\\.la(s|z)$", full.names = TRUE)
f = f[1:2]

## ----buffer-------------------------------------------------------------------
count = function(data) { length(data$X) }
del = triangulate(filter = keep_ground())
npts = callback(count, expose = "x")
sum = summarise()
ans = exec(del + npts + sum, on = f)
print(ans$callback)
ans$callback[[1]]+ ans$callback[[2]]
ans$summary$npoints

## ----buffer2------------------------------------------------------------------
ans = exec(npts + sum, on = f)
ans$callback[[1]]+ ans$callback[[2]]
ans$summary$npoints

## ----buffer3------------------------------------------------------------------
count_buffer_aware = function(data) {
  bbox = attr(data, "bbox")
  npoints = sum(!data$Buffer)
  return(list(bbox = bbox, npoints = npoints))
}

del = triangulate(filter = keep_ground())
npts = callback(count_buffer_aware, expose = "b") # b for buffer
sum = summarise()
ans = exec(del + npts + sum, on = f)
print(ans$callback)
ans$callback[[1]]$npoints+ ans$callback[[2]]$npoints
ans$summary$npoints

