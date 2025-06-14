plot_base_network <- function(g, layout_algo = "fr") {

  #tidygraph
  tg <- as_tbl_graph(g)
  
  # node labels
  if (!is.null(igraph::vertex_attr(g)$name)) {
    tg <- tg %>% mutate(label = name)
  } else {
    tg <- tg %>% mutate(label = as.character(1:gorder(.)))
  }
  
  # farm size attribute
  if (!is.null(igraph::vertex_attr(g)$herd_size)) {
    tg <- tg %>% mutate(herd_size = herd_size)
  } else {
    tg <- tg %>% mutate(herd_size = 10)
  }
  
  # edge weight
  if (!is.null(igraph::edge_attr(g)$weight)) {
    edge_mapping <- aes(width = weight)
  } else {
    edge_mapping <- aes()
  }
  
  ggraph(tg, layout = layout_algo) +
    geom_edge_link(edge_mapping, alpha = 0.5, color = "gray50") +
    scale_edge_width(range = c(0.2, 1.5), guide = "none") +
    geom_node_point(aes(size = herd_size, color = label), show.legend = FALSE) +
    geom_node_text(aes(label = label), repel = TRUE, size = 3, color = "black") +
    scale_size(range = c(3, 8)) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10))
}
