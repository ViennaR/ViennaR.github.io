
###########################################################
#
# map.R
#
# Several Graph Plotting Basics from the `igraph'-package
#
###########################################################

library(igraph)
library(png)

# Adding vertices:

par(mfrow=c(1,1))
G<-make_graph(c(1,2, 2,3, 3,1, 1,3, 3,4))
plot(G, main="G<-make_graph(c(1,2, 2,3, 3,1, 1,3, 3,4))")
readline();

G <- make_empty_graph()

G <- add_vertices(G, 1, color="red")
plot(G, main="G <- make_empty_graph(); add_vertices(G, 1, color='red')")
readline();
G <- add_vertices(G, 3, color="green")
plot(G, main="G <- add_vertices(G, 3, color='green')")
readline();
for(i in 1:10){ G<-add_vertices(G, 1, color=i);plot(G, main="for(i in 1:10){ G<-add_vertices(G, 1, color=i)}")}
readline();
for(i in 1:10){ G<-add_vertices(G, 1, color=i);plot(G, main="for(i in 1:10){ G<-add_vertices(G, 1, color=i)}")}
readline();
for(i in 1:10){ G<-add_vertices(G, 1, color=i);plot(G, main="for(i in 1:10){ G<-add_vertices(G, 1, color=i)}")}
readline();
for(i in 1:10){ G<-add_vertices(G, 1, color=i);plot(G, main="for(i in 1:10){ G<-add_vertices(G, 1, color=i)}")}
readline();
for(i in 1:10){ G<-add_vertices(G, 1, color=i);plot(G, main="for(i in 1:10){ G<-add_vertices(G, 1, color=i)}")}
readline();

# Adding edges:

G<-add_edges(G, c(1,54))
plot(G, main="1 and 54 are connected - Vertex-Radius too large")
readline();
for(i in 1:20){ G<-add_edges(G, c(i,i+1)); plot(G, main="for(i in 1:20){ G<-add_edges(G, c(i,i+1))")}
readline();
for(i in 21:53){ G<-add_edges(G, c(i,i+1)); plot(G, vertex.size=5, vertex.label=NA, main="Edges added & plot(G, vertex.size=5, vertex.label=NA)")}
readline();



# Layout
lay0 <- matrix(1:108, ncol=2)
plot(G, layout=cbind((1:54)/54, cos(2*pi*(1:54)/54)), vertex.size=5, vertex.label=NA, main="lay<-matrix(1:108, ncol=2; plot(G, layout=cbind((1:54)/54, cos(2*pi*(1:54)/54)")
readline();

# Makeing new graph from old one: make_graph( list_of_edges, .... )
# No!: G0 <- make_graph(E(G), directed=FALSE)

EEG<-1:108; EEG[2*INDEX-1]<-INDEX; EEG[2*INDEX]<-INDEX+1; EEG[107]=54; EEG[108]=1;
#oder: for(i in 1:54){ EEG[2*i-1]<-i; EEG[2*i]<-i+1;}; EEG[107]=54; EEG[108]=1; 
G0 <- make_graph(EEG, directed=FALSE)
plot(G0, layout=lay0, vertex.size=5, vertex.label=NA, main="Add reverse edges =>  G0 undirected: plot(G0, layout=lay0, vertex.size=5, vertex.label=NA")
readline();

# Inheriting attributes:
V(G0)$color=V(G)$color
plot(G0, layout=lay0, vertex.size=8, vertex.label.dist=1.5, main="V(G0)$color=V(G)$color; plot(G0, layout=lay0, vertex.size=8, vertex.label.dist=1.5)")
readline();

# Edge-Attributes
set.edge.attribute(G0, name = "strangeness", value = sin(1:54))
E(G0)$strangeness <- sin(pi*(1:54)/54+1)
G2<-G0
E(G2)$weights <- E(G0)$strangeness
E(G2)$color <- round(5*(E(G2)$weights+1)+1)
plot(G2, vertex.size=3, vertex.label=NA, main="E(G2)$color <- round(5*(sin(pi*(1:54)/54+1)+1)+1)")
readline()

# Adjacency Matrices: A
A <-as.matrix(get.adjacency(G0))
B <- A %*% A
G1 <- graph.adjacency(B, mode=c("undirected"))
plot(G1, vertex.size=3, vertex.label=NA, main="A <-as.matrix(get.adjacency(G0)); G1 <- graph.adjacency(A %*% A, mode=c('undirected'))\n Note: G0 is bipartite; Therefore the graph with adj. matrix A^2 has two components!")
readline() # Note: G0 is bipartite

G1simple <- simplify(G1)
plot(G1simple, main="G1simple <- simplify(G1); plot(G1simple)")
readline() # Note: G0 is bipartite


# Recall: A bipartite graph has symmetric spectrum. We will see later.

D<-diag(degree(G0, mode=c("out")))
Dinv <- solve(D)
P <- Dinv %*% A   # The simple random walk transition probability matrix.
C5 <- P %*% P %*% P %*% P %*% P
C10 <-  P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P
C30 <-  P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% C10 %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P
w5 <- C5[5,]
w10 <- C10[5,]
w30 <- C30[5,]
wc5 <- ifelse(w5>0.1, "red", ifelse(w5>0.02, "orange", ifelse(w5>0, "pink", "blue")))
wc10 <- ifelse(w10>0.1, "red", ifelse(w10>0.02, "orange", ifelse(w10>0, "pink", "blue")))
wc30 <- ifelse(w30>0.1, "red", ifelse(w30>0.02, "orange", ifelse(w30>0, "pink", "blue")))

G5<-G0
V(G5)$color <- wc5
plot(G5, vertex.size=3, vertex.label=NA, main="D<-diag(degree(G0, mode=c('out'))); P<- solve(D) %*% A # Simple Random. Walk\n C5<-P%^%5; C10<-P%^%10; C30<-P%^%30 -- (library('expm')!); Heat-Color Coding:\n C5, C10, C10 are the Trans. Probab. Matrices for the SRW after 5,10,30 Steps! \n V(G5)$color<-ifelse(w5>0.1, 'red', ifelse(w5>0.02, 'orange', ifelse(w5>0, 'pink', 'blue')))")
readline()

G10<-G0
V(G10)$color <- wc10
plot(G10, vertex.size=3, vertex.label=NA, main="V(G10)$color <- ifelse(w10>0.1, 'red', ifelse(w10>0.02, 'orange', ifelse(w10>0, 'pink', 'blue')))\n plot(G10, vertex.size=3, vertex.label=NA)")
readline()

G30<-G0
V(G30)$color <- wc30
plot(G30, vertex.size=3, vertex.label=NA, main=" V(G30)$color<-ifelse(w30>0.1, 'red', ifelse(w30>0.02, 'orange', ifelse(w30>0, 'pink', 'blue'))) \n plot(G30, vertex.size=3, vertex.label=NA)")
readline()

G30<-G0
V(G30)$color <- wc30
plot(G30, vertex.size=3, vertex.label.dist=0.7, main="V(G30)$color<-ifelse(w30>0.1, 'red', ifelse(w30>0.02, 'orange', ifelse(w30>0, 'pink', 'blue')))\n plot(G30, vertex.size=3, vertex.label.dist=0.7)")
readline()

plot(eigen(P)[[1]], main="Spectrum of P on circle C_54 is lambda_j = cos(2 j pi/54), 0<=j<=53\n Bipartiteness of C_54 <==> Spectrum symmetric about 0.")
print(eigen(P)[[1]][1])
print(eigen(P)[[1]][length(V(G0))])
readline()

# Could do:
#A[54,54]<-1
#A[1,1]<-1
#A[54,1]<-0
#A[1,54]<-0
#
# Better:

G0<-make_ring(54)
G0<-delete.edges(G0,"1|54")
G0<-add.edges(G0,c(1,1))
G0<-add.edges(G0,c(54,54))
A <-as.matrix(get.adjacency(G0))

P <- Dinv %*% A   # The simple random walk transition probability matrix.
C5 <- P %*% P %*% P %*% P %*% P
C10 <-  P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P
C30 <-  P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% C10 %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P %*% P
w5 <- C5[5,]
w10 <- C10[5,]
w30 <- C30[5,]
wc5 <- ifelse(w5>0.1, "red", ifelse(w5>0.02, "orange", ifelse(w5>0, "pink", "blue")))
wc10 <- ifelse(w10>0.1, "red", ifelse(w10>0.02, "orange", ifelse(w10>0, "pink", "blue")))
wc30 <- ifelse(w30>0.1, "red", ifelse(w30>0.02, "orange", ifelse(w30>0, "pink", "blue")))
G5<-G0
V(G5)$color <- wc5
plot(G5, vertex.size=3, vertex.label=NA, main="G0<-delete.edges(G0,'1|54') -- Same Heat Color Coding as above\n Since Graph has loops, not bipartite => Heat distributes on odd & even vertices")
readline()

G10<-G0
V(G10)$color <- wc10
plot(G10, vertex.size=3, vertex.label=NA, main="...after 10 steps...")
readline()

G30<-G0
V(G30)$color <- wc30
plot(G30, vertex.size=3, vertex.label=NA, main="...after 30 steps. Heat is reflected ...")
readline()

G30<-G0
V(G30)$color <- wc30
plot(G30, vertex.size=3, vertex.label.dist=0.7, main="... at vertex 1, due to loop. (Reflecting Boundary Condition) ")
readline()

print(P)
readline()

plot(eigen(P)[[1]], main="Largest Eigenvalue = 1  --  Smallest Eigenvalue = -0.9983082\n
-1 not in Spectrum <=> Graph not Bipartite")
print(eigen(P)[[1]][1])
print(eigen(P)[[1]][length(V(G0))])
readline()

sg <- 1-max(abs(eigen(P)[[1]][length(V(G0))]),eigen(P)[[1]][2])
n_relax <- round(1/sg)
library(expm)
P_relax <- P %^% (2*n_relax)   # Attention: Don't forget parentheses!
plot(P_relax[5,1:54], main="Spectral Gap: sg <- 1-max(abs(eigen(P)[[1]][length(V(G0))]),eigen(P)[[1]][2])\n n_relax = round(1/sg); P %^% (2*n_relax) -- 5th row:")
readline()

# Special Graphs:

# Trees:

T<-graph.tree(n=100)
plot(T, main="T<-graph.tree(n=100); plot(T)")
readline()
T<-as.undirected(graph.tree(n=100))
plot(T, vertex.size=3, vertex.label=NA, main="plot(T, vertex.size=3, vertex.label=NA)")
readline()

# Erdoes Renyi Graphs (G_np - Model);  Old Version: G<-Erdoes_Renyi_Game(100, p=0.1)
G_ER <- sample_gnp(100, p=0.012)
plot(G_ER, vertex.size=3, vertex.label=NA, main="G_ER <- sample_gnp(100, p=0.012)")

readline()  # Note the largest component! (Mention theory on this: Order ~ n^(2/3).)

# Plotting only largest component.  Also: Report C.C.'s sizes:
G_list<-decompose(G_ER)   # Decompose into a list of Graphs: The C.C.'s C(v)
Sizes<-sapply(G_list, vcount)    # Reports the cardinalities of each C(v)
Index_largest<-which.max(Sizes)
G0<-G_list[[Index_largest]]
plot(G0, vertex.size=3, vertex.label=NA, main="G_list<-decompose(G_ER); Sizes<-sapply(G_list, vcount) ; Index_largest<-which.max(Sizes)\n G0<-G_list[[Index_largest]]; plot(G0, vertex.size=3, vertex.label=NA)")
readline()   # Plot only largest component.

# Random Walks
walk<-random_walk(G0, 1, 100)     # Perform random walk of 100 steps on G0
for(v in V(G0)) {V(G0)$label[v]<-length(walk[walk==v])}
H0<-induced_subgraph(G0, walk)    # Call induced subgraph by walk: H0
plot(G0 %du% H0, vertex.size=10, main="Discrete Union: plot(G0 %du% H0, vertex.size=10)")  # Look at both of these graphs
is_subgraph_isomorphic_to(H0, G0) # H0 is a subgraph of G0
readline()


# Lattices:
n<-15                             # ...
g <- make_lattice(c(n,n))         # ...
Eh<-sample(E(g),100)
h1<-subgraph.edges(g, Eh, delete.vertices=FALSE)
lay <- layout_on_grid(g,c(n,n))   # ...
ima <- readPNG("WU.png")          # ...
plot(h1, vertex.size=2, vertex.label=NA, layout=lay)
lim <- par()
xmin<-lim$usr[1];xmax<-lim$usr[2];ymin<-lim$usr[3];ymax<-lim$usr[4]
rasterImage(ima, xmin,ymin,xmax,ymax)
par(new=TRUE)

plot(h1, vertex.size=2, vertex.label=NA, layout=lay, main="Bond-Percolation Subgraph of a nxn Raster with 100 edges:\n Eh<-sample(E(g),100), h1<-subgraph.edges(g, Eh, delete.vertices=FALSE)\n (Thanks to `bill_080' from StackOverflow for `Plotting on a PNG-picture')")


