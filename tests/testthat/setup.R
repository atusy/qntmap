if (interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("./minimal/.map/1")
qnt <- read_qnt("./minimal/.qnt", saving = FALSE)

centers <- find_centers(xmap, qnt, saveas = FALSE)
cluster <- cluster_xmap(xmap, centers, saving = FALSE)

epma <- tidy_epma(qnt, xmap, cluster)
epma2 <- tidy_epma_for_quantify(
  qnt, xmap, cluster,
  maps_x = attr(xmap, "pixel")[1],
  maps_y = attr(xmap, "pixel")[2],
  elements = qnt$elm$elem
)

AG <- find_AG(epma2)
B <- find_B(epma2)
params <- tidy_params(AG, B, qnt)
