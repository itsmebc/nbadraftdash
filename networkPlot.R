library(tidyverse)
library(igraph)

network = tibble(all_seasons %>%
  select(college, team_abbreviation))

networkMat = as.matrix(get.adjacency(graph.data.frame(network)))

all_seasons$season = all_seasons$season %>%
  str_sub(., 1, 4) %>%
  as.numeric()

network = all_seasons %>% 
  select(player_name, team_abbreviation, season) %>%
  group_by(player_name) %>%
  arrange(season, .by_group = TRUE)
  

#if player_name and team_abbreviation is the same in network[i,][3] as it is in network[i+1,][3], discard 
#network[i]

for (i in 1:nrow(networkChain)) {
  if (networkChain[i,][1] == networkChain[i+1,][1] & networkChain[i,][2] == networkChain[i+1,][2]) {
    networkChain$status[i] = "remove"
  }
  else {
    networkChain$status[i] = "keep"
  }
}

networkChain = networkChain %>%
  filter(status == "keep") %>%
  select(player_name, team_abbreviation, season)

networkMat = networkChain  
networkMat$to = ""


for (i in 1:nrow(networkMat)) {
  print(networkMat[i,]$player_name)
}


for (i in 1:(nrow(networkMat)-1)) {
  if (networkMat[i,]$player_name == networkMat[i+1,]$player_name) { 
    networkMat[i,]$to = networkMat[i+1,]$team_abbreviation
  } else {
    networkMat[i,]$to = ""
  }
}

networkMat = networkMat %>%
  filter(to != "")

links = data.frame(
  source =c(networkMat[2]),
  target =c(networkMat[4])
)

linkplot = graph_from_data_frame(d=links, directed = F)

set.seed(3)

deg <- degree(linkplot, mode="all")
par(bg="white")
set.seed(12524)
plot(linkplot, 
     
     # === vertex
     vertex.color = rgb(0.1,0.7,0.8,0.5),          # Node color
     vertex.frame.color = "gray",                 # Node border color
     vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     vertex.size=(deg*.1)^1.124,                               # Size of the node (default is 15)
     vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
     
     # === vertex label,                   # Character vector used to label the nodes
     vertex.label.color="black",
     vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=.75,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="gray",                           # Edge color
     edge.width=.1,                                 # Edge width, defaults to 1
     edge.arrow.size=.05,                            # Arrow size, defaults to 1
     edge.arrow.width=.05,                           # Arrow width, defaults to 1
     edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
)

