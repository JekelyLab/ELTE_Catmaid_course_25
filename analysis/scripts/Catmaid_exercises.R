# Accessing CATMAID via the API with the Natverse package

# source packages and functions -----------------
source("analysis/scripts/packages_and_functions.R")

# loading neurons from CATMAID -----------------

# read the skeleton ids first
neurons_skids <- skids_by_3annotations(
  "episphere", "Sensory neuron", "adult eye"
)

# load neurons
neurons <- nlapply(
  read.neurons.catmaid(
    neurons_skids,
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

# plot a preconfigured background (details in the sourced packages_and_functions.R script)
plot_background_anterior()

# plot the neurons on the same RGL viewer
plot3d(
  neurons,
  WithConnectors = F, soma = T, lwd = 2,
  add = T, alpha = 0.4,
  col = "red"
)

# finding synaptic partners ---------------------

# find synaptic partners
partners <- catmaid_query_connected(
  neurons_skids,
  minimum_synapses = 10,
  pid = 11
)
partners

# parse partner IDs
outgoing_partner_skids <- unique(partners$outgoing$partner)

# load neurons
outgoing_partners <- nlapply(
  read.neurons.catmaid(
    outgoing_partner_skids,
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

# get name of neurons
catmaid_get_neuronnames(
  outgoing_partner_skids,
  pid = 11
)

# plot the partners to the same scene
plot3d(
  outgoing_partners,
  WithConnectors = F, soma = T, lwd = 2,
  add = T, alpha = 1,
  col = blues[8]
)

# add text label
texts3d(
  35000, 40000, 21000,
  text = "PRC",
  col = "black", cex = 2
)

texts3d(
  35000, 62000, 21000,
  text = "IN1",
  col = "black", cex = 2
)

# save rgl view as png
rgl.snapshot("analysis/pictures/eye_with_IN1.png")
close3d()

# network analysis --------------

# create graph with all nodes
nodes <- data.frame(name = c("PRC", "IN1", "celltype3", "celltype4"))
graph <- tbl_graph(nodes = nodes, directed = TRUE)
graph

for (i in 1:3) {
  for (j in 1:3) {
    neurons1 <- paste("celltype", i, sep = "")
    neurons2 <- paste("celltype", j, sep = "")
    # read the skeleton ids first
    skids1 <- skids_by_3annotations(
      neurons1, "connectome", "with_soma"
    )
    skids2 <- skids_by_3annotations(
      neurons2, "connectome", "with_soma"
    )
    # get connectivity
    connectivity <- catmaid_get_connectors_between(
      skids1, skids2,
      pid = 11
    )
    # check the number of synapses from group1 -> group2
    N_syn <- dim(connectivity)[1]
    if(is.null(N_syn)){N_syn=0}
    print(N_syn)
    # Add edges
    graph <- graph |> bind_edges(
      data.frame(from = i, to = j, weight = N_syn)
    )
  }
}

graph

ggraph(graph, layout = "stress") +
  geom_edge_link(arrow = arrow(length = unit(3, "mm")),
                 start_cap = circle(3, "mm"),
                 end_cap = circle(3, "mm")) +
  geom_node_point(size = 10, colour = "orange") +
  geom_node_text(aes(label = name)) +
  theme_graph()

# plot as visNetowrk graph -------------------------


## convert to VisNetwork-list
Conn_graph.visn <- toVisNetworkData(graph)
## copy column "weight" to new column "value" in list "edges"
Conn_graph.visn$edges$value <- Conn_graph.visn$edges$weight


# layout
visNet <- visNetwork(Conn_graph.visn$nodes, Conn_graph.visn$edges) %>%
    visEdges(
      smooth = list(type = "curvedCW", roundness = 0.2),
      scaling = list(min = 2, max = 12),
      color = list(inherit = TRUE, opacity = 0.5),
      arrows = list(to = list(
        enabled = TRUE,
        scaleFactor = 1.2, type = "arrow"
      ))
    ) %>%
    visNodes(
      borderWidth = 0.3,
      color = list(background = "cyan", border = "black"),
      shape = "dot",
      font = list(color = "black", size = 44),
      scaling = list(label = list(enabled = TRUE, min = 34, max = 44))
    ) %>%
    visOptions(highlightNearest = list(
      enabled = TRUE, degree = 1, algorithm = "hierarchical", labelOnly = FALSE)
      )
visNet 
  
# save as html
saveNetwork(visNet, "analysis/pictures/eye_circuit.html")
webshot2::webshot(
  url = "analysis/pictures/eye_circuit.html",
  file = "analysis/pictures/eye_circuit.png",
  cliprect = c(300, 0, 500, 500), zoom = 5
)

# assemble figure --------------------


img_anatomy <- readPNG("analysis/pictures/eye_with_IN1.png")
img_graph <- readPNG("analysis/pictures/eye_circuit.png")

# convert png to image panel
panelA <- ggdraw() + draw_image(img_anatomy, scale = 1) +
  draw_label("PRC and IN1",
             x = 0.4, y = 0.98, fontfamily = "sans", fontface = "plain",
             color = "black", size = 11
             ) 
panelB <- ggdraw() + draw_image(img_graph, scale = 1)

# define layout with textual representation
layout <- "
AB
"

Figure1 <- panelA + panelB +
  plot_layout(design = layout, widths = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 12, face = "plain"))


ggsave("manuscript/figures/Figure1.pdf",
       limitsize = FALSE,
       units = c("px"), Figure1, width = 1600, height = 800
)


ggsave("manuscript/figures/Figure1.png",
       limitsize = FALSE,
       units = c("px"), Figure1, width = 1600, height = 800, bg = "white"
)
