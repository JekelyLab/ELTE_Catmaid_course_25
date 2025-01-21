# packages, functions and CATMAID connectivity info used for the figures of the Platynereis 3d connectome paper

rm(list = ls(all.names = TRUE)) # will clear all objects includes hidden objects.

# free up memory and report the memory usage.
gc()

# load packages of general use across the project --------------
# load packages
{
  library(catmaid)
  library(tidyverse)
  library(cowplot)
  library(png)
  library(patchwork)
  library(RColorBrewer)
  options(nat.plotengine = "rgl")
  require("graphics")
  library(cowplot)
  library(png)
  library(igraph)
  library(networkD3)
  library(visNetwork)
  library(webshot2)
  library(tidygraph)
  library(rgl)
  library(plotly)
  library(htmltools)
}


# define colour-blind-friendly colour palettes --------------
# From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7", "#000000"
)
blues <- brewer.pal(9, "Blues")
bluepurple <- brewer.pal(9, "BuPu")
oranges <- brewer.pal(9, "YlOrRd")

# show as coloured pie charts
dev.new()
pie(rep(1, 8), col = Okabe_Ito, Okabe_Ito, main = "Okabe Ito")


# catmaid connection --------------

conn_http1 <- catmaid_login(
  server = "https://catmaid.jekelylab.ex.ac.uk/",
  authname = "AnonymousUser",
  config = httr::config(ssl_verifypeer = 0, http_version = 1)
)

# read volumes ---------------
# These volumes are 3D structures in the animal's body and provide background
# for reconstructed neurons or help create images
{
  outline <- catmaid_get_volume(
    1,
    rval = c("mesh3d", "catmaidmesh", "raw"),
    invertFaces = T, conn = NULL, pid = 11
  )
  yolk <- catmaid_get_volume(
    4,
    rval = c("mesh3d", "catmaidmesh", "raw"),
    invertFaces = T, conn = NULL, pid = 11
  )
  acicula <- nlapply(
    read.neurons.catmaid(
      "^acicula$",
      pid = 11
    ),
    function(x) smooth_neuron(x, sigma = 6000)
  )
  # these four dots are the most extreme points of the volume, adding them to the 3d view solves the problem with automatic zooming and movement of the field shown
  bounding_dots <- nlapply(
    read.neurons.catmaid(
      "^bounding_dots$",
      pid = 11
    ),
    function(x) smooth_neuron(x, sigma = 6000)
  )
  scalebar_50um_anterior <- read.neurons.catmaid("scalebar_50um_anterior", pid = 11)
  scalebar_50um_ventral <- read.neurons.catmaid("scalebar_50um_ventral", pid = 11)
}

# functions of general use ---------------

plot_background_anterior <- function(x) {
  nopen3d() # opens a pannable 3d window
  plot3d(bounding_dots,
    WithConnectors = F, WithNodes = F, soma = F, lwd = 1,
    add = T, alpha = 1,
    col = "white"
  )
  plot3d(yolk,
    WithConnectors = F, WithNodes = F, soma = F, lwd = 2,
    add = T, alpha = 0.1,
    col = "#E2E2E2"
  )
  # we define a z clipping plane for the frontal view
  par3d(zoom = 0.52)
  nview3d("frontal", extramat = rotationMatrix(0.2, 1, 0.1, 0.5))
  # z-axis clip
  clipplanes3d(0, 0, -1, 65000)
  # y-axis clip
  clipplanes3d(1, 0, 0.16, 7000)
  # x-axis clip
  clipplanes3d(0, -1, 0.16, 130000)
  par3d(windowRect = c(0, 0, 800, 800)) # resize for frontal view
}


# plotting function for ventral view with yolk and acicula
plot_background_ventral <- function(x) {
  nopen3d() # opens a pannable 3d window
  par3d(windowRect = c(20, 30, 600, 800)) # to define the size of the rgl window
  nview3d("ventral", extramat = (rotationMatrix(0.35, 1, 0, 0) %*% rotationMatrix(0.05, 0, 0, 1)))
  par3d(zoom = 0.53)
  plot3d(bounding_dots,
    WithConnectors = F, WithNodes = F, soma = F, lwd = 1,
    add = T, alpha = 1,
    col = "white"
  )
  plot3d(yolk,
    WithConnectors = F, WithNodes = F, soma = F, lwd = 2,
    add = T, alpha = 0.05,
    col = "#E2E2E2"
  )
  plot3d(acicula,
    WithConnectors = F, WithNodes = F, soma = T, lwd = 2,
    add = T, alpha = 1,
    col = "grey70"
  )
  par3d(zoom = 0.48)
}


# functions to retrieve skids (skeleton IDs) based on two or three annotations
{
  skids_by_2annotations <- function(annotation1, annotation2) {
    skids1 <- catmaid_skids(annotation1, pid = 11)
    skids2 <- catmaid_skids(annotation2, pid = 11)
    intersect <- intersect(skids1, skids2)
    return(intersect)
  }

  skids_by_3annotations <- function(annotation1, annotation2, annotation3) {
    skids1 <- catmaid_skids(annotation1, pid = 11)
    skids2 <- catmaid_skids(annotation2, pid = 11)
    skids3 <- catmaid_skids(annotation3, pid = 11)
    intersect1_2 <- intersect(skids1, skids2)
    intersect1_2_3 <- intersect(intersect1_2, skids3)
    return(intersect1_2_3)
  }
}
