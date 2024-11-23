library(terra)
library(lidR)
library(sf)

set_lidr_threads(8)


# Function to create a variable radius window function for LMF
make_win_fun <- function(a, b, c, diam_min, diam_max) {
  win_fun <- function(x) {
    win <- a + b*x + c*x^2
    win[win < diam_min] = diam_min
    win[win > diam_max] = diam_max
    return(win)
  }
  return(win_fun)
}


chm = "/ofo-share/ofo-itd-crossmapping_data/drone/chms-cropped/chm-mesh/0005.tif"
chm = rast(chm)

winfun = make_win_fun(a = 1,
                       b = 0.1,
                       c = 0,
                       diam_min = 1,
                       diam_max = 30)

ttops = lidR::locate_trees(chm, algorithm = lmf(ws = winfun, shape = "circular", hmin = 5))
nrow(ttops)
plot(chm)
plot(ttops, add = TRUE, col = "red")
