# Homeplay for week 8

## Network Visualizations
data(UKfaculty, package = "igraphdata")

### 1. explore the network structure (N nodes and N edges, directed or
###    undirected), node attribute(s) (how many? what does it express?) and
###    edge attribute(s) (how many? what does it express?).
UKfaculty

igraph::edge.attributes(UKfaculty)
igraph::vertex.attributes(UKfaculty)

#### The edge attributes contain the amount of times (weight) two nodes have
#### contact.
#### The node attributes contain the school affiliation (group) of the node.

### 2. After you check the help file, so you know what this network is about.
?igraphdata::UKfaculty

### 3. Make a visualization using one edge, and one node attributes that are
###    able to tell what is happening in this network.
plot(UKfaculty,
     edge.arrow.size = 0.1,
     edge.color = sapply(igraph::E(UKfaculty)$weight, function(x) {
       if (x <= 4) return("cadetblue")
       if (x > 4 && x <= 8) return("dodgerblue")
       if (x > 8 && x <= 12) return("blue")
       return("darkblue")
     }),
     vertex.size = 6,
     vertex.label = NA,
     vertex.color = igraph::V(UKfaculty)$Group,
     layout = igraph::layout_with_fr,
     main = "UK Faculty")

### 4. When you are done with an informative plot, please let’s make it funny!
###    Plot the same graph, but with a lattice layout (just for fun) nodes with
###    a rectangular shape black nodes white nodes labels a surprise element of
###    your choice!
plot(UKfaculty,
     edge.arrow.size = 0.5,
     edge.color = "red",
     vertex.size = 12,
     vertex.shape = "square",
     vertex.label.color = "white",
     vertex.color = "black",
     layout = igraph::layout_on_grid,
     main = "UK Faculty")
