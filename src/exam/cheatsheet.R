################################################################################
################################### 0. Setup ###################################
################################################################################

# Set random seed
set.seed(42)

# Set network
network <- intergraph::asNetwork(SNA4DSData::Sampson$Sampson_like2)

################################################################################
############################## 1. Create Network ###############################
################################################################################

### 1.1. Directed Network
survey <- data.frame(respondent = NA, alter1 = NA, alter2 = NA)

edges <- data.frame(rbind(cbind(survey$respondent, survey$alter1),
                          cbind(survey$respondent, survey$alter2)))
vertices <- unique(c(survey$respondent, survey$alter1, survey$alter2))
network <- igraph::graph_from_data_frame(edges, vertices, directed = TRUE)

### 1.2 Convert igraph::igraph to sna::network
intergraph::asNetwork(network)

### 1.3 Convert sna::network to igraph::igraph
intergraph::asIgraph(network)

################################################################################
############################### 2. Descriptives ################################
################################################################################

### 2.1 Print Description
igraph::print.igraph(network)

### 2.2 Print Diameter
igraph::diameter(network)

### 2.3 Print Dyad Census (???)
igraph::dyad.census(network)

### 2.4 Print Density
igraph::edge_density(network)

### 2.5 Print Reciprocity
igraph::reciprocity(network)

### 2.6 Print Transitivity
igraph::transitivity(network)
sna::gtrans(network, mode = "digraph") # digraph = directed, graph = undirected

### 2.7 Print Betweenness Centrality
igraph::centralization.betweenness(network)

### 2.8 Print Closeness Centrality
igraph::centralization.closeness(network)

### 2.9 Print Mean Distance (average path length)
igraph::mean_distance(network)

### 2.10 Plot Eccentricity Distribution
hist(igraph::eccentricity(network), main = "Eccentricity Distribution of Network")

### 2.11 Plot Betweenness Distribution
hist(igraph::betweenness(network), main = "Betweenness Distribution of Network")

### 2.12 Plot Closeness Distribution
hist(igraph::closeness(network), main = "Closeness Distribution of Network")

### 2.13 Plot Degree Distribution
hist(igraph::degree(network), main = "Degree Distribution of Network")

### 2.14 Plot Centrality Chart
SNA4DS::centralityChart(network)

### 2.15 Descriptives Function
descriptives <- function(net) {
  cat("Descriptives for Network:\n")
  cat("----------------------------------------\n")
  print(igraph::print.igraph(net))
  cat("-------------------- Diameter --------------------\n")
  print(igraph::diameter(net))
  cat("-------------------- Dyad Census --------------------\n")
  print(igraph::dyad.census(net))
  cat("-------------------- Density --------------------\n")
  print(igraph::edge_density(net))
  cat("-------------------- Reciprocity --------------------\n")
  print(igraph::reciprocity(net))
  cat("-------------------- Transitivity --------------------\n")
  print(igraph::transitivity(net))
  cat("-------------------- Betweenness Centralization --------------------\n")
  print(igraph::centralization.betweenness(net))
  cat("-------------------- Closeness Centralization --------------------\n")
  print(igraph::centralization.closeness(net))
  cat("-------------------- Average Path Length --------------------\n")
  print(igraph::mean_distance(net))
}
descriptives(network)

################################################################################
########################## 3. Network Autocorrelation ##########################
################################################################################

### 3.1 Extract Attributes
attributes <- SNA4DS::extract_all_vertex_attributes(network)

### 3.2 Create Adjacency Matrix
adjacency_matrix <- network::as.matrix.network.adjacency(network)

### 3.2 Create Weight Matrix
diag(adjacency_matrix) <- 0
weight_matrix <- adjacency_matrix / rowSums(adjacency_matrix)

### 3.3 Create Equivalence Matrix
equivalence <- 1 - (sna::sedist(network) / max(sna::sedist(network)))
diag(equivalence) <- 0
equivalence_matrix <- equivalence / rowSums(equivalence)

### 3.4 Create Network Autocorrelation Model (Communication)
attributes$intercept <- 1

model <- sna::lnam(y = attributes[, "y"],
                   x = as.matrix(attributes[, c("intercept", "attribute1", "attribute2")]),
                   W1 = weight_matrix)

### 3.5 Create Network Autocorrelation Model (Comparison)
attributes$intercept <- 1

model <- sna::lnam(y = attributes[, "y"],
                   x = as.matrix(attributes[, c("intercept", "attribute1", "attribute2")]),
                   W1 = equivalence_matrix)

### 3.6 Calculate Structural Equivalence (distances)
sedist <- sna::sedist(network) # higher the number -> less similar

equivalence <- 1 - (sedist / max(sedist))
diag(equivalence) <- 0
round(equivalence, digits = 2) # higher the number -> more similar

################################################################################
################################# 4. CUG & QAP #################################
################################################################################

### 4.1 CUG Test (test measure value)
cug <- sna::cug.test(network,
                     mode = "graph",
                     FUN = sna::gcor,
                     FUN.args = list(),
                     cmode = "edges", # no. vertices, no. vertices + no. edges, no. vertices + dyad.census
                     reps = 200) # significance: Pr(X>=Obs)

### 4.2 QAP Test (test two networks)
qap <- sna::qaptest(list(network1, network2), 
                    FUN = sna::gcor,
                    reps = 1000,
                    g1 = 1,
                    g2 = 2) # significance: p(f(perm) >= f(d))
sna::plot.qaptest(qap)

### 4.3 QAP Linear Model (test whether explanatory network(s) -> influence valued dependent network)
model <- sna::netlm(dependent_network,
                    list(network1, network2, network3),
                    nullhyp = "qapspp",
                    intercept = TRUE,
                    reps = 1001)
model$names <- c("Intercept", "Network 1", "Network 2", "Network 3")

summary(model) # significance: Pr(>=b)

### 4.4 QAP Logistic Model (test whether explanatory network(s) -> influence binary dependent network)
model <- sna::netlogit(dependent_network,
                       list(network1, network2, network3),
                       nullhyp = "qapspp",
                       intercept = TRUE,
                       reps = 1001)
model$names <- c("Intercept", "Network 1", "Network 2", "Network 3")

summary(model) # significance: Pr(>=b)

################################################################################
################################### 5. Plots ###################################
################################################################################

### 5.0 Helper Functions
map_range <- function(x, start, end) (x - min(x)) / max(x - min(x)) * (end - start) + start

### 5.1 Plot Network
plot(network, 
     edge.arrow.size = .2,                    # edge and arrow size
     edge.color = "red",                      # edge color
     vertex.color = "blue",                   # vertex filling color
     vertex.frame.color = "green",            # vertex perimeter color
     vertex.label = igraph::V(network)$label, # vertex labels
     vertex.label.cex = 0.6,                  # vertex label size
     vertex.label.color = "black")            # vertex label color

################################################################################
################################### 6. ERGM ####################################
################################################################################

### 6.1 Structural Terms (Dyadic Independent)
ergm::summary_formula(network ~ edges)
ergm::summary_formula(network ~ density)
ergm::summary_formula(network ~ sender)
ergm::summary_formula(network ~ receiver)

### 6.2 Structural Terms (Dyadic Dependent)
ergm::summary_formula(network ~ mutual)
ergm::summary_formula(network ~ asymmetric)
ergm::summary_formula(network ~ triangles)
ergm::summary_formula(network ~ esp(0:100)) # gwesp(decay = 0.25, fixed = FALSE)
ergm::summary_formula(network ~ desp(0:100, type = "RTP")) # dgwesp(decay=0.25, fixed=FALSE, type= "RTP")
ergm::summary_formula(network ~ triadcensus)
ergm::summary_formula(network ~ balance)
ergm::summary_formula(network ~ transitive)
ergm::summary_formula(network ~ intransitive)
ergm::summary_formula(network ~ degree(0:100)) # gwdegree(decay = 0.25, fixed = FALSE)
ergm::summary_formula(network ~ idegree(0:100)) # gwidegree(decay = 0.25, fixed = FALSE)
ergm::summary_formula(network ~ odegree(0:100)) # gwodegree(decay = 0.25, fixed = FALSE)
ergm::summary_formula(network ~ kstar(0:100))
ergm::summary_formula(network ~ istar(0:100))
ergm::summary_formula(network ~ ostar(0:100))
ergm::summary_formula(network ~ cycle(1))

### 6.3 Covariate Terms
ergm::summary_formula(network ~ nodecov("attribute"))
ergm::summary_formula(network ~ nodeicov("attribute"))
ergm::summary_formula(network ~ nodeocov("attribute"))
ergm::summary_formula(network ~ nodefactor("attribute"))
ergm::summary_formula(network ~ nodeifactor("attribute"))
ergm::summary_formula(network ~ nodeofactor("attribute"))
ergm::summary_formula(network ~ absdiff("attribute"))
ergm::summary_formula(network ~ nodematch("attribute"))
ergm::summary_formula(network ~ edgecov(NA))
ergm::summary_formula(network ~ nodemix("attribute"))

### 6.4 Search Terms
search.ergmTerms(keyword, net, categories, name)

### 6.5 Specification
model <- ergm::ergm(formula,
                    check.degeneracy = TRUE,
                    control = ergm::control.ergm(MCMC.burnin = 5000,
                                                 MCMC.samplesize = 10000,
                                                 seed = 42,
                                                 MCMLE.maxit = 60,
                                                 parallel = 4),
                    verbose = TRUE)

summary(model)

### 6.6 Goodness of Fit
(fit <- ergm::gof(model))

### 6.7 MCMC Diagnostics
ergm::mcmc.diagnostics(model)

### 6.8 Odds
SNA4DS::Ef_int(model, type = "odds")

### 6.9 Probabilities
SNA4DS::Ef_int(model, type = "prob")

################################################################################
################################### 7. GERGM ###################################
################################################################################

### 7.1 Plot Network
GERGM::plot_network(network,
                    seed = 42,
                    white_background = TRUE,
                    show_legend = FALSE)

### 7.2 Specification
model <- GERGM::gergm(network ~ edges(method = "endogenous"), # endogenous or regression
                      covariate_data = covariate_data, # vertex data
                      number_of_networks_to_simulate = 40000,
                      thin = 1/100,
                      proposal_variance = 0.05,
                      MCMC_burnin = 10000,
                      seed = 42,
                      convergence_tolerance = 0.5)
summary(model)

### 7.3 Goodness of Fit
GERGM::GOF(model)

### 7.4 MCMC Diagnostics
GERGM::Trace_Plot(model)

### 7.5 Results
GERGM::Estimate_Plot(model)

################################################################################
################################### 8. TERGM ###################################
################################################################################

### 8.1 Print Active Vertex Attributes (start = 5, end = 8)
networkDynamic::list.vertex.attributes.active(network, onset = 5, terminus = 8)

### 8.2 Print Active Vertex "atttribute" (at = 1)
networkDynamic::get.vertex.attribute.active(network, "atttribute", at = 1)

### 8.3 Print Active Edge Attributes (start = 0, end = 49)
networkDynamic::list.edge.attributes.active(network, onset = 0, terminus = 49)

### 8.4 Print Active Edge "atttribute" (at = 1)
networkDynamic::get.edge.attribute.active(network, "atttribute", at = 1)

### 8.5 Extract Network (start = 0, end = 1)
single_network <- networkDynamic::network.extract(network, onset = 0, terminus = 1)

### 8.6 Collapse Into Static Network (start = 0, end = 1)
static_network <- networkDynamic::network.collapse(network, onset = 0, terminus = 1)

### 8.7 Measure Over Time (function = sna::components, step = 15, length = 15)
sna::tSnaStats(network, snafun = "components", time.interval = 15, aggregate.dur = 15)

### 8.8 Total Activity In Interval
tsna::edgeDuration(network, mode = "counts")

### 8.9 Total Time Each Vertex Has Ties
tsna::tiedDuration(network, mode = "counts")

### 8.10 Count Unique Edges In Interval (number = invertals start end)
SNA4DS::count_edges_in_interval(network, number = 50)

### 8.11 Count Unique Edges In Interval (number = invertals start end)
SNA4DS::count_unique_edges_in_interval(network, number = 50)

### 8.12 Calculate Set Temporally Reachable Vertices From Source (start = 0, end = 0.9, v = vertex index)
forward_path <- tsna::tPath(network, v = 12, direction = "fwd", start = 0, end = .9)

tsna::plotPaths(
  network,
  paths = forward_path,
  displaylabels = FALSE,
  vertex.col = "white",
  edge.label.cex = 1.5
)

### 8.13 Plot Network Time Slices (start = 0, end = 45)
SNA4DS::plot_network_slices(network,
                            number = 9,
                            start = 0,
                            end = 45,
                            digits = 3)
