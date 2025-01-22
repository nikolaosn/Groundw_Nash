
library(tidyr)
library(dplyr)

# Create a data frame from the matrix
edges_df <- expand.grid(from = 1:ncol(qij_mat_nash), to = 1:ncol(qij_mat_nash)) %>%
  mutate(weight = as.vector(qij_mat_nash[(from - 1) * ncol(qij_mat_nash) + to])) %>%
  filter(weight != 0)

edges_df <- edges_df %>%
  filter(from < to)

edges_df$weight<--edges_df$weight

filtered_edges <- edges %>%
  inner_join(edges_df, by = c("from", "to"))

filtered_edges$weight.x<-filtered_edges$weight.y
colnames(filtered_edges)<-c("from", "to", "qij", "UBENij", "geometry")

filtered_edges$positive<-filtered_edges$qij>0

filtered_edges <- filtered_edges %>%
  mutate(
    from_temp = ifelse(!positive, to, from), # Swap from with to if positive is FALSE
    to_temp = ifelse(!positive, from, to),
    from = from_temp,
    to = to_temp
  ) %>%
  select(-from_temp, -to_temp) # Remove temporary columns

new_reversed = st_reverse(filtered_edges[!filtered_edges$positive, ])
filtered_edges <-filtered_edges[filtered_edges$positive, ]
filtered_edges = rbind(filtered_edges, new_reversed)

filtered_edges$qij<-abs(filtered_edges$qij)
filtered_edges$UBENij<-filtered_edges$qij*p

filtered_edges<-filtered_edges[,c(1,2,3,4,6)]

#visualize
qtm(filtered_edges)+qtm(points)

#export as shp
st_write(filtered_edges, "result_edges.shp", delete_dsn = TRUE) 
st_write(points, "result_nodes.shp", delete_dsn = TRUE)

#other plotting options
tmap_mode("plot")
library(tmap)

tm_shape(mst_nodes) + 
  tm_symbols(size = 0.5, col = "black") +
  tm_shape(mst_edges) + 
  tm_lines(lwd = 2, col = "black") +     
  tm_scale_bar(position = c("right", "bottom"))





