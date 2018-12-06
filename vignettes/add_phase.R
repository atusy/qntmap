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

## ---- include = FALSE----------------------------------------------------
dir_map <- file.path(wd, 'minimal/.map/1')
dir_qnt <- file.path(wd, 'minimal/.qnt')

## ---- message = FALSE----------------------------------------------------
xmap <- read_xmap(dir_map)
qnt <- read_qnt(dir_qnt)

## ------------------------------------------------------------------------
table(qnt$cnd$phase)

## ------------------------------------------------------------------------
phase_list <- read.csv('phase_list0.csv')
phase_list$use[phase_list$phase == 'Qtz'] <- FALSE
write.csv(phase_list, file.path(dir_qnt, 'phase_list_no_qtz.csv'))
qnt <- read_qnt(dir_qnt, phase_list = file.path(dir_qnt, 'phase_list_no_qtz.csv'), renew = TRUE)

## ------------------------------------------------------------------------
table(qnt$cnd$phase)

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
centers <- find_centers(xmap, qnt)
centers

## ------------------------------------------------------------------------
plot(xmap, 'Si', interactive = FALSE)

## ------------------------------------------------------------------------
centers <- add_centers(centers = centers, xmap = xmap, x = 18, y = 28, p = 'Qtz')

## ------------------------------------------------------------------------
print(centers)

## ----fig.show = 'hide'---------------------------------------------------
cluster <- cluster_xmap(xmap, centers)
qmap <- quantify(xmap, qnt, cluster)

## ------------------------------------------------------------------------
summary(cluster)
summary(qmap)

## ----include = FALSE-----------------------------------------------------
unlink(c('centers0.csv', 'center_add.csv'))

