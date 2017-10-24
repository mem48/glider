# Simple File to get Gavin Started

# Step 1: Get R set up correctly

library(igraph) # Load in the igraph library if it is not available use install.packages("igraph")

# Step 2: Read in the data

graph = readRDS("twitterNetwork.Rds") #Read the file called twitterNetwork and make it a variaible called graph

# Step 3: Let’s find out some basics about this graph

gorder(graph) # How many verticies does it have?
ecount(graph) # How many edges does it have?

# Step 4: Make a smaller graph by removing some of the low degree vertices

graph.trim = delete.vertices(graph, which(degree(graph)<=10)) 
# Notice that we have created a new graph called graph.trim
gorder(graph.trim) # Reduced the oerde by about a factor of 10

# Step 5: Calculate some statistics for each vertex, this will take a minute or two

V(graph.trim)$degree.total <- degree(graph.trim, mode = "total") # Total Degree
V(graph.trim)$degree.in <- degree(graph.trim, mode = "in") # Degree In
V(graph.trim)$degree.out <- degree(graph.trim, mode = "out") # Degree Out
V(graph.trim)$strength.total <- strength(graph.trim, mode = "total") # Total Strength
V(graph.trim)$strength.in <- strength(graph.trim, mode = "in") # Strength in
V(graph.trim)$strength.out <- strength(graph.trim, mode = "out") # strength out
V(graph.trim)$between <- betweenness(graph.trim) # betweeness centrality
V(graph.trim)$closeness <- closeness(graph.trim) # closeness centrality
V(graph.trim)$eigenvector <- eigen_centrality(graph.trim, directed = TRUE, scale = FALSE, weights = E(graph.trim)$weight)$vector #eigenvector centrality
V(graph.trim)$PageRank <- page_rank(graph.trim, directed = TRUE, damping = 0.85, weights = E(graph.trim)$weight)$vector #page rank centrality

# Step 6: Let’s export those results as a table so we can see them

vert = as_data_frame(graph.trim, what="vertices")
# Now click on the table icon next to vertices in the environment tab
# on the right side of the screen to view the table
# Notice that you can sort using the arrows on each column
# This is where the excel spreadsheets come from


# We can also save the results and view them in excel
write.csv(vert,"verticies.csv", row.names = FALSE)
# A CSV file will be saved in the same folder as the Rproj file
# CSV files can be opened in Excel

# Step 7: Let’s draw a graph to look at

# Step 7a: First, we need a much smaller graph with only a few hundred vertices


graph.toDraw = delete.vertices(graph.trim, which(V(graph.trim)$strength.in <= 800))
# Notice the different syntax for accessing the variables that we created
# whereas before we had to calculate the degree within the delete.verticies function
# We have already calculated the strenght.in and can access it via V(graph.trim)$strength.in
gorder(graph.toDraw) # Now only have a few hundred vertices


# Step 7b: We can plot graphs witin R
plot(graph.toDraw)
# But the defaults are often not very clear and it is slow to draw large complex graphs
# SO let make a pretty graph

#Step 7c: Let colour the vertices based on one of the variables we have calculated
# to do this we define a colour scale and the add a new colour variable to the graph

c_scale = colorRamp(c('blue','cyan','yellow','red')) #Define a colour scale from blue to red
# Now for each vertex assing a colour 
V(graph.toDraw)$color = apply(c_scale(V(graph.toDraw)$between / max(V(graph.toDraw)$between, na.rm = T)), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )
# Not that we have normaised the betweeness centralisty score on a 0-1 scale by dividing by the max(imum) value

#Step 7d: Draw the graph and export it as a SVG File
#SVG file can be opened in web browsers and graphic programmes
# I found Microsoft Edge on windows 10 to be very quick at opening SVG files

#Create High Resolution Plots
svg(filename="gavindemo.svg", # Set the name of the file
    width=25, # Set width and height of the graph
    height=25, 
    pointsize=10) # Set the font size of the text labels
par(mar = c(1,1,1,1)) # Defines the bounds for the graph, don't change
plot(graph.toDraw, 
     edge.width = E(graph.toDraw)$weight/ 20, # Set the widths of the edges
     vertex.size = V(graph.toDraw)$strength.total / 1000 , # Set the size of the vertex
     edge.arrow.size = 0.2, # set the size of the arrows on the edges
     edge.curved=0.2, # Set how curved the edges are
     vertex.color = V(graph.toDraw)$color, #Use the colour schemes defined above
     vertex.frame.color = V(graph.toDraw)$color,
     vertex.label.family= "Helvetica", #Label font
     vertex.label.color = "black", # Label font colour
     layout = layout_nicely, #Which layout algorithm  to use see  http://igraph.org/r/doc/layout_.html 
     rescale = TRUE, #Scale graph to fit
     axes = FALSE); dev.off() # Don't show XY axis
