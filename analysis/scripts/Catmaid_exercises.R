#Accessing CATMAID via the API with the Natverse package

#source packages and functions
source("analysis/scripts/packages_and_functions.R")

#read the skeleton ids first
neurons_skids <- skids_by_3annotations(
  "episphere", "Sensory neuron", "adult eye"
)

#load neurons
neurons <- nlapply(
  read.neurons.catmaid(
    neurons_skids,
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

plot_background_anterior()

plot <- plot3d(
  neurons,
  WithConnectors = F, soma = T, lwd = 2,
  add = T, alpha = 0.4,
  col = "red"
)

#find synaptic partners
partners <- catmaid_query_connected(
  neurons_skids, minimum_synapses = 10,
  pid = 11
  )
partners

#parse partner IDs
outgoing_partner_skids <- unique(partners$outgoing$partner)

#load neurons
outgoing_partners <- nlapply(
  read.neurons.catmaid(
    outgoing_partner_skids,
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

#get name of neurons
catmaid_get_neuronnames(
  outgoing_partner_skids, pid = 11
  )

#plot the partners to the same scene
plot <- plot3d(
  outgoing_partners,
  WithConnectors = F, soma = T, lwd = 2,
  add = T, alpha = 1,
  col = blues[8]
)

#add text label
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

#save rgl view as png
rgl.snapshot("analysis/pictures/eye_with_IN1.png")
close3d()

