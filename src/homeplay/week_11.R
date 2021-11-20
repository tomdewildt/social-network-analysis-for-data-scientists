# Homeplay for week 11

## Bipartite ERGM
mdata <- matrix(c(1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1), 10, 4)

vertex.names <- c(LETTERS[1:10], 1:4)
Age <- c(6, 7, 7, 7, 9, 9, 10, 9, 12, 12, NA, NA, NA, NA)
Gender <- as.character(c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, NA, NA, NA, NA))

### 1. Import it as a bipartite network in the network package
kidsnet <- network::network(mdata, directed = FALSE, bipartite = TRUE)
network::set.vertex.attribute(kidsnet, "vertex.names", vertex.names)
network::set.vertex.attribute(kidsnet, "Age", Age)
network::set.vertex.attribute(kidsnet, "Gender", Gender)
network::set.vertex.attribute(kidsnet, "bipratite", value = rep(10, 10), v = 1:10)

kidsnet

### 2. Get your hand dirty trying to improve the kids going to playdate model.
###      * Try other star effects
###      * Try degree effects
###      * Try at least one extra dyadic independent term
###      * Which one is your best model?
###      * Can you find any other interesting results?
m6 <- ergm::ergm(kidsnet ~ edges + b1cov("Age") + b1nodematch("Gender") + b1degree(2) + b1star(2),
                 constraints = ~ bd(minout = 0, maxout=14),
                 control = ergm::control.ergm(MCMC.burnin = 5000,
                                              MCMC.samplesize = 10000,
                                              seed = 1234,
                                              MCMLE.maxit = 20))

ergm::mcmc.diagnostics(m6)

ergm::gof(m6)

## Weighted ERGM
data("lending_2005", package = "GERGM")
data("covariate_data_2005", package = "GERGM")
data("net_exports_2005", package = "GERGM")

### 1. Use these two covariates to improve the model. Finally, follow the 7 step
###    flow approach to organize your assignment.
###      * G8 - whether the country belongs to the G8 international organization
###      * net_exports_2005 the net exports between each pair of countries in
###        2005
f6 <- lending_2005 ~ edges +
  mutual(alpha = 0.8) +
  sender("log_GDP") +
  receiver("log_GDP") +
  nodemix("G8", base = "No") +
  netcov(net_exports_2005)

m6 <- GERGM::gergm(f6,
                   covariate_data = covariate_data_2005,
                   number_of_networks_to_simulate = 100000,
                   thin = 1 / 100,
                   proposal_variance = 0.05,
                   MCMC_burnin = 50000,
                   seed = 456,
                   convergence_tolerance = 0.8,
                   target_accept_rate = 0.25)
