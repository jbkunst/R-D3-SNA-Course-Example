# Introduction
A simple example using R and D3.js for show the examples of [SNA Course](https://class.coursera.org/sna-002/lecture/index) in Coursera. 
Live example is [here](https://rawgit.com/jbkunst/R-D3-SNA-Course-Example/master/index.html).

# Details
First, a R script for generate the networks and atributes values of each node, then export the 
data to json file.

The index.html take the json file and show it with d3.js and jquery for make it interactive.

# The Networks
Showing 4 toy networks:

- Butterfly network
- Circular network
- Star network
- Linear network
	
The values for a node are calculated according its own network. The nodes and links are not considered in the same big network.

