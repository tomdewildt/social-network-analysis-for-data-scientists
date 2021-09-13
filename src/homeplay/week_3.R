# Homeplay for week 3

## Blogosphere

### 1. First, make sure to update the SNA4DSData package, to which this dataset
###    has been added.

### 2. Then, load the blogosphere network.
data(blogosphere, package = "SNA4DSData")

### 3. Clean it up a bit: there are some blogs that refer to themselves. That is
###    not what we want, so remove these loops. How do I do that, you ask? Well,
###    search through the documentation for igraph . Alternatively, see if you
###    can find the solution in the vignette inside the SNA4DS package. From
###    here on, continue with this cleaned graph.
blogosphere <- igraph::simplify(blogosphere, remove.loops = TRUE)

### 4. Calculate summary statistics for this network. At least determine:
###    mean distance, diameter, dyad census, reciprocity, transitivity, density.
###    Interpret these numbers, do you understand what they mean? Are they what
###    you would expect?
igraph::mean_distance(blogosphere)
igraph::diameter(blogosphere)
igraph::dyad_census(blogosphere)
igraph::reciprocity(blogosphere)
igraph::transitivity(blogosphere)
igraph::edge_density(blogosphere)

## Communities

### 1. Use the walktrap algorithm to determine communities in the blogosphere
###    network. How many communities do you get? Does the partitioning make
###    sense? What do you notice about the result?
walktrap <- igraph::walktrap.community(blogosphere)

plot(walktrap, blogosphere, vertex.label = NA)
  
### 2. One thing that might affect the result is that a bunch of the blogs do
###    not point to another blog and no other blog points to them. In SNA we
###    call these nodes isolates.

### 3. What do you think would happen to the results if you would remove those
###    isolates first? First, think about this. Then actually remove the
###    isolates from the graph and run the walktrap again. Check if you were
###    correct.
isolates <- which(igraph::degree(blogosphere) == 0)
blogosphere <- igraph::delete.vertices(blogosphere, isolates)

walktrap <- igraph::walktrap.community(blogosphere)

plot(walktrap, blogosphere, vertex.label = NA)

## Bonus

### 1. Add the group membership (which you get with igraph::membership) to the
###    network as a vertex attribute. Then extract all of the vertex attributes
###    using SNA4DS::extract_all_vertex_attributes and explore whether it is
###    indeed the case that like-minded blogs tend cluster together inside the
###    communities you found.
blogosphere <- igraph::set_vertex_attr(
  blogosphere,
  "membership",
  value = igraph::membership(walktrap),
)

attributes <- SNA4DS::extract_all_vertex_attributes(blogosphere)

table(attributes$membership, attributes$party)
