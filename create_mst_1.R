library(sf)
library(tmap)
tmap_mode("view")
library(sfnetworks)
library(tidygraph)
library(units)

N=10
points <- st_sf(id = 1:N,
                geometry = st_sfc(
                  st_point(c(22.000, 39.600)),
                  st_point(c(22.002, 39.610)),
                  st_point(c(22.005, 39.603)),
                  st_point(c(22.010, 39.603)),
                  st_point(c(22.012, 39.610)),
                  st_point(c(22.014, 39.609)),
                  st_point(c(22.015, 39.612)),
                  st_point(c(22.015, 39.610)),
                  st_point(c(22.006, 39.605)),
                  st_point(c(22.007, 39.607))
                ),
                crs = 4326
)


r0<-0.2
R<-4000
Tr<-0.0001    # transmitivity
c = 564.48  # constant c
Q_vector<-c(0.001,0.001,0.001,0.001,0.01,0.001,0.001,0.001,0.001,0.01)
Q_tot<-sum(Q_vector)

#create edges between all points
point_pairs <- t(combn(points$id, 2))
edges_list <- list()
for (i in 1:nrow(point_pairs)) {
  from_id <- point_pairs[i, 1]
  to_id <- point_pairs[i, 2]
  edge_geom <- st_sfc(st_linestring(rbind(
    st_coordinates(points$geometry[from_id]),
    st_coordinates(points$geometry[to_id])
  )), crs = st_crs(points))
  #use distance as weight 
  distance <- as.numeric(st_distance(points$geometry[from_id], points$geometry[to_id]))
  edges_list[[i]] <- st_sf(from = from_id, to = to_id, weight = distance, geometry = edge_geom)
}

edges <- do.call(rbind, edges_list)

#crete sfnetwork
net <- sfnetwork(nodes = points, edges = edges, directed = FALSE)

#igraph mst for minimum spanning tree
mst_graph <- igraph::mst(net)

mst_net <- as_sfnetwork(mst_graph)
mst_edges<-mst_net %>% activate("edges") %>% st_as_sf()
mst_nodes<-mst_net %>% activate("nodes") %>% st_as_sf()

qtm(mst_nodes)+qtm(mst_edges)


adj_matrix <- igraph::as_adjacency_matrix(mst_graph, attr = NULL, sparse = FALSE)





