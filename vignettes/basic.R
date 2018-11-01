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

## ---- eval = FALSE-------------------------------------------------------
#  library(qntmap)

## ----eval = FALSE--------------------------------------------------------
#  wd <- tempdir() # Arbitrary
#  setwd(wd)

## ------------------------------------------------------------------------
file.copy(
  from = system.file('extdata', 'minimal', package = 'qntmap'),
  to = wd,
  recursive = TRUE
)

## ------------------------------------------------------------------------
dir(file.path(wd, 'minimal'), recursive = TRUE, all.files = TRUE)

## ------------------------------------------------------------------------
dir_map <- file.path(wd, 'minimal/.map/1')
dir_qnt <- file.path(wd, 'minimal/.qnt')

## ------------------------------------------------------------------------
xmap <- read_xmap(dir_map)
qnt <- read_qnt(dir_qnt)

## ---- message = FALSE, echo = FALSE--------------------------------------
epma <- tidy_epma(qnt, xmap) %>>%
  filter(elm == elm[[1]]) %>>%
  select(x_px, y_px, phase)
plot(xmap, 'Si', interactive = FALSE) +
  scale_fill_gradient(low = 'black', high = 'white') +
  geom_point(
    aes(y_px, x_px, colour = phase),
    data = epma, inherit.aes = FALSE
  )

## ------------------------------------------------------------------------
plot(xmap, 'Mg', interactive = TRUE)

## ------------------------------------------------------------------------
centers <- find_centers(xmap, qnt)
centers

## ----eval = FALSE--------------------------------------------------------
#  cluster <- cluster_xmap(xmap, centers)

## ---- include = FALSE----------------------------------------------------
cluster <- cluster_xmap(xmap, centers)

## ---- echo = FALSE, fig.width = 3, fig.height = 3, fig.show = 'hold'-----
cls_imgs <- dir(
    file.path(dir_map, 'clustering'), 
    full.names = TRUE
  ) %>>%
  str_subset('\\.png$')

cls_imgs %>>%
  map(png::readPNG) %>>%
  map(function(.x) {
    expand.grid(
      y = seq(nrow(.x), 1),
      x = seq(ncol(.x))
    ) %>>%
      mutate(
        fill = rgb(c(.x[, , 1]), c(.x[, , 2]), c(.x[, , 3]))
      )
  }) %>>%
  map(ggplot, aes(x, y, fill = fill)) %>>%
  map(`+`, list(
    geom_raster(),
    coord_fixed(),
    scale_fill_identity(),
    theme_void()
  )) %>>%
  walk(print)

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

## ------------------------------------------------------------------------
i <- segment(cls_imgs[grepl('_map.png$', cls_imgs)][1])

## ------------------------------------------------------------------------
mean(qmap, index = i)

## ----include = FALSE-----------------------------------------------------
unlink('centers0.csv')

