# Homeplay for week 2

## Matrices in R

### 1. Create a square matrix m of dimension 3 x 3 filled with zeros.
m <- matrix(0, nrow = 3, ncol = 3)

### 2. Set the first row name to A, the second to B, and the third to C.
rownames(m) <- c("A", "B", "C")

### 3. Do the same with the columns of m.
colnames(m) <- c("A", "B", "C")

### 4. Replace the 0s with 1 for row 1 col 3, row 2 col 1, row 3 col 1, row 1
###    col 2.
m[1, 3] <- 1
m[2, 1] <- 1
m[3, 1] <- 1
m[1, 2] <- 1

### 5. Inspect m, what can you say about the matrix you just created? Can you
###    check this using an appropriate R function?
m

### 6. Add two columns to the right of m and two rows to the bottom, each
###    containing zeroes? Call them D and E.
m <- cbind(m, D = 0, E = 0)
m <- rbind(m, D = 0, E = 0)

### 7. Does this new matrix have different features than the one before?

#### Yes, it has two extra columns and two extra rows.

### 8. Create a 3x4 matrix that contains the number 1 through 12 as follows:
###      o  p  q  r
###    X 1  2  3  4
###    Y 5  6  7  8
###    Z 9 10 11 12
m <- matrix(c(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12), nrow = 3, ncol = 4)
rownames(m) <- c("X", "Y", "Z")
colnames(m) <- c("o", "p", "q", "r")

## Packages for network analysis

### 1. Load the elegans network from the SNA4DSData package as follows:
###    data(elegans, package = "SNA4DSData").
data(elegans, package = "SNA4DSData")

### 2. Check out the package documentation for the igraph package using
###    help(package = "igraph"). Scroll a bit through the functions to see what
###    the package has to offer.
help(package = "igraph")

### 3. Can you find the function to determine the dyad census of a network? I
###    think so.

#### igraph::dyad.census(graph)

### 4. Calculate the dyad census of the elegans network.
igraph::dyad.census(elegans)

### 5. Hunt through the package documentation to find the function that can
###    calculate the “reciprocity” of a network.

#### igraph::reciprocity(graph)

### 6. Look at the documentation of the reciprocity function and use it to
###    calculate the reciprocity of the elegans network. Use the default
###    settings of the function.
igraph::reciprocity(elegans)

### 7. Now, let’s check out the sna package. There is some overlap between the
###    two package in terms of what they can do. We will explore that a little
###    bit in this homeplay. Scroll a but through the package documentation to
###    get an idea of what this package does. How does it compare to the igraph
###    package in terms of what the functions do? Is it essentially the same
###    thing, or do the two packages have some different focuses? How?
help(package = "sna")

### 8. Let’s use the sna package to do the things we just did with the igraph
###    package. First, you need to convert the data object to class network.
###    Tip: use the intergraph function for this.
elegans <- intergraph::asNetwork(elegans)

### 9. Now, find the function inside the sna package to calculate the dyad
###    census on this “new” network and apply it.
sna::dyad.census(elegans)

### 10. As you already expected, we now ask you to determine the reciprocity of
###     the network using sna Can you find the function to do this? Set the
###     argument measure equal to “edgewise”.
sna::grecip(elegans, measure = "edgewise")

### 11. How do the results compare?

#### The results of the reciprocity function of igraph and the grecip function
#### of sna are the same.

### 12. “Bonus question”: can you figure out how the results from the
###     “reciprocity” analysis and the dyad census are related?
