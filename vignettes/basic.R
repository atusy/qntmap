## ----setup, include = FALSE----------------------------------------------
wd <- tempdir()
knitr::opts_chunk$set(
  root.dir = wd,
  fig.width = 7,
  fig.height = 7,
  collapse = TRUE,
  comment = "#>"
)
library(qntmap)
library(dplyr)
library(pipeR)
library(ggplot2)
library(stringr)
library(purrr)

formals(read_xmap)$renew <-
  formals(read_qnt)$renew <- 
  TRUE

## ----load-qntmap, eval = FALSE-------------------------------------------
#  library(qntmap)

## ----setwd, eval = FALSE-------------------------------------------------
#  wd <- tempdir() # Arbitrary
#  setwd(wd)

## ----copy-files----------------------------------------------------------
file.copy(
  from = system.file('extdata', 'minimal', package = 'qntmap'),
  to = wd,
  recursive = TRUE
)

## ----file-list-----------------------------------------------------------
dir(file.path(wd, 'minimal'), recursive = TRUE, all.files = TRUE)

## ----specify-directories, include = FALSE, eval = TRUE-------------------
dir_map <- file.path(wd, 'minimal/.map/1')
dir_qnt <- file.path(wd, 'minimal/.qnt')

## ---- message = FALSE----------------------------------------------------
xmap <- read_xmap(dir_map)
qnt <- read_qnt(dir_qnt, renew = TRUE)

## ---- message = FALSE, echo = FALSE, warning = FALSE---------------------
epma <- tidy_epma(qnt, xmap) %>>%
  filter(elm == elm[[1]]) %>>%
  select(x_px, y_px, phase)
plot(xmap, 'Si', interactive = FALSE) +
  scale_fill_gradient(low = 'black', high = 'white') +
  geom_point(
    aes(y_px, x_px, colour = phase), data = epma, inherit.aes = FALSE
  ) +
  guides(
    fill = guide_colourbar(barheight = grid::unit(1, "npc") - unit(10, "line"))
  )

## ---- echo = FALSE-------------------------------------------------------
plot(xmap, 'Mg', interactive = FALSE)

## ------------------------------------------------------------------------
centers <- find_centers(xmap, qnt)
centers

## ---- eval = TRUE, fig.keep = "last"-------------------------------------
cluster <- cluster_xmap(xmap, centers)
plot(cluster, interactive = FALSE)

## ------------------------------------------------------------------------
summary(cluster)

## ------------------------------------------------------------------------
qmap <- quantify(xmap = xmap, qnt = qnt, cluster = cluster)

## ------------------------------------------------------------------------
plot(qmap, 'Si', interactive = FALSE)

## ------------------------------------------------------------------------
summary(qmap)

## ---- include = FALSE----------------------------------------------------
V <- unclass(table(cluster$cluster))
V <- V / sum(V)
E <- mean(qmap)
E <- setNames(round(E[['Whole area']], 2), E[['Element']])

## ------------------------------------------------------------------------
mean(qmap)

## ----img-path, include = FALSE-------------------------------------------
img <- dir(
  file.path(dir_map, "clustering"), 
  pattern = "_map.png",
  full.names = TRUE
)[[1]]
i <- segment(img)

## ------------------------------------------------------------------------
mean(qmap, index = i)

## ----include = FALSE-----------------------------------------------------
unlink('centers0.csv')

