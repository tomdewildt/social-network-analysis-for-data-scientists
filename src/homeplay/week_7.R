# Homeplay for week 7

## Louisiana
data(louis, package = "SNA4DSData")

### 1. Create a Weight matrix based on the adjacency matrix. Make sure to row
###    normalize the matrix. (NOTE: you can use the SNA4DS vignette to find the
###    appropriate functions, in this case the function to create the adjacendy
###    matrix).
adj <- network::as.matrix.network.adjacency(louis)
w_adj <- adj / rowSums(adj)

### 2. Run an autocorrelation model where democratic preference is regressed on
###    "perc_black", "perc_cath", "perc_urban", and "black_political_equality."
###    (do not forget the intercept)
attrs <- SNA4DS::extract_all_vertex_attributes(louis)

mod <- sna::lnam(y = attrs[, "democratic"],
                 x = as.matrix(attrs[, c("Intercept",
                                         "perc_black",
                                         "perc_cath",
                                         "perc_urban",
                                         "black_political_equality")]),
                 W1 = w_adj)
summary(mod)

### 3. Interpret the findings

#### Only the percentage catholic (perc_cath), the percentage urban
#### (perc_urban) and the measure of black political equality in the county
#### (black_political_equality) are statistically significant.

### 4. Now run the same analysis, using structural equivalence for the weight
###    matrix.
distances <- sna::sedist(louis)
w_equiv <- 1 - (distances / max(distances))
diag(w_equiv) <- 0

w_equiv <- w_equiv / rowSums(w_equiv)

mod <- sna::lnam(y = attrs[, "democratic"],
                 x = as.matrix(attrs[, c("Intercept",
                                         "perc_black",
                                         "perc_cath",
                                         "perc_urban",
                                         "black_political_equality")]),
                 W1 = w_equiv)
summary(mod)

## FIFA
data(fifa2006, package = "SNA4DSData")
data(fifa2015, package = "SNA4DSData")

### 1. Calculate the transitivity for both of the networks.
sna::gtrans(fifa2006, mode = "graph", measure = "weak")
sna::gtrans(fifa2015, mode = "graph", measure = "weak")

### 2. Run a test to check if the transitivities of the two fifa networks are
###    statistically significantly high or low. Control for network size and
###    density.
(cug_edges_trans_fifa2006 <- sna::cug.test(fifa2006,
                                           mode = "graph",
                                           FUN = sna::gtrans,
                                           FUN.args = list(
                                             mode = "graph",
                                             measure = "weak"
                                           ),
                                           cmode = "edges",
                                           reps = 200))
(cug_edges_trans_fifa2015 <- sna::cug.test(fifa2015,
                                           mode = "graph",
                                           FUN = sna::gtrans,
                                           FUN.args = list(
                                             mode = "graph",
                                             measure = "weak"
                                           ),
                                           cmode = "edges",
                                           reps = 200))

par(mfrow = c(1, 2))
plot(cug_edges_trans_fifa2006)
plot(cug_edges_trans_fifa2015)
par(mfrow = c(1, 1))

#### By analyzing the plots from the cug tests we can see that the transitivity
#### for both networks are statistically significantly high.

### 3. You could argue that the previous test is insufficient, since committee
###    membership is very skewed among the fifa members most people have few
###    memberships and some have very many.
###
###    Try: hist(sna::degree(fifa2006)) or hist(sna::degree(fifa2015)).
###
###    Therefore, now run a test to check if the transitivities of the two fifa
###    networks are statistically significantly high or low, controling for
###    network size and degree distribution.
hist(sna::degree(fifa2006))
hist(sna::degree(fifa2015))

(cug_dyad_census_trans_fifa2006 <- sna::cug.test(fifa2006,
                                                 mode = "graph",
                                                 FUN = sna::gtrans,
                                                 FUN.args = list(
                                                   mode = "graph",
                                                   measure = "weak"
                                                 ),
                                                 cmode = "dyad.census",
                                                 reps = 200))
(cug_dyad_census_trans_fifa2015 <- sna::cug.test(fifa2015,
                                                 mode = "graph",
                                                 FUN = sna::gtrans,
                                                 FUN.args = list(
                                                   mode = "graph",
                                                   measure = "weak"
                                                 ),
                                                 cmode = "dyad.census",
                                                 reps = 200))

par(mfrow = c(1, 2))
plot(cug_dyad_census_trans_fifa2006)
plot(cug_dyad_census_trans_fifa2015)
par(mfrow = c(1, 1))

### 4. Interpret your findings.

#### By analyzing the plots from the cug tests we can see that the transitivity
#### for both networks is still statistically significantly high.

### 5. Let’s also check if the distribution of advantageous positions in the
###    administrative network is exceptionally high. In this case, calculate
###    some centralization score (e.g., betweenness centralization) and compare
###    that score with networks of similar size+density or of similar degree
###    distribution. You can do this for both networks, or just take one,
###    depending on how much time you are willing to wait for a result.
(cug_dyad_census_betw_fifa2006 <- sna::cug.test(fifa2006,
                                                mode = "graph",
                                                FUN = sna::centralization,
                                                FUN.args = list(
                                                  FUN = sna::betweenness
                                                ),
                                                cmode = "dyad.census",
                                                reps = 200))

plot(cug_dyad_census_betw_fifa2006)

### 6. Interpret your findings.

#### The betweenness centralization score is statistically significantly high,
#### this means that there are very few central members that are highly
#### conntected in the network.
