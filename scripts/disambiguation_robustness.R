# Load libraries

library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(tidygraph)
library(igraph)
library(data.table)
library(ggraph)
library(furrr)


# Load the full SPO edge list: 

load('data/spo_network')

# Generate network measurements for the full network: 

full_net_stats = spo_raw %>% 
  distinct(X1, X2, .keep_all = T) %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph() %>%
  mutate(transitivity = local_transitivity()) %>% 
  mutate(eigen = centrality_eigen()) %>% 
  mutate(degree_total = centrality_degree(mode = 'total')) %>%
  mutate(degree_in = centrality_degree(mode = 'in')) %>%
  mutate(degree_out = centrality_degree(mode = 'out')) %>%
  #mutate(closeness = centrality_closeness(mode = 'total')) %>% 
  mutate(betweenness = centrality_betweenness()) %>% 
  as_tibble() %>% 
  pivot_longer(names_to = 'metric', values_to = 'value', cols = 2:7)

# Function to generate all metrics when passed a dataframe

calculate_metrics = function(.data){
  
  .data %>% 
    distinct(V1, V2, .keep_all = T) %>% 
    graph_from_data_frame() %>% 
    as_tbl_graph() %>%
    mutate(transitivity = local_transitivity()) %>% 
    mutate(eigen = centrality_eigen()) %>% 
    mutate(degree_total = centrality_degree(mode = 'total')) %>%
    mutate(degree_in = centrality_degree(mode = 'in')) %>%
    mutate(degree_out = centrality_degree(mode = 'out')) %>%
    #mutate(closeness = centrality_closeness(mode = 'total')) %>% 
    mutate(betweenness = centrality_betweenness()) %>% 
    as_tibble() %>% 
    pivot_longer(names_to = 'metric', values_to = 'value', cols = 2:7) %>% 
    left_join(full_net_stats, by = c('name', 'metric')) %>% 
    filter(!is.na(value.x)) %>% 
    filter(!is.na(value.y)) %>% 
    group_by(metric) %>% 
    summarise(cor = cor(value.x, value.y, method = 'spearman')) %>% 
    ungroup()
  
}

# Load all sample datasets into R as a single list, and keep just the from and to columns: 

files = list.files('disambiguation_experiment/', full.names = T)

list_of_edge_lists = lapply(files, fread)

for(i in 1:length(list_of_edge_lists)){
  
  list_of_edge_lists[[i]] =  list_of_edge_lists[[i]] %>% select(1,2) %>% mutate(filename = files[i])
  
  
}

# Run the calculate_metrics function on each file in the list: 

samples = future_map_dfr(list_of_edge_lists, calculate_metrics, .progress = T) 

# Add a filename ID: 

samples = samples %>% mutate(filename_id = rep(1:360, each =6))

filenames = files %>% as_tibble() %>% mutate(filename_id = 1:nrow(.))

samples = samples %>% left_join(filenames)

# Some final adjustments

samples = samples %>% 
  mutate(run_info = str_remove(value, "disambiguation_experiment\\/\\/fromto_all_place_mapped_stuart_")) %>% 
  separate(run_info, into = c('removed', 'sample'), sep = "_") %>%  mutate(removed = as.numeric(removed)) %>% 
  mutate(sample = as.numeric(sample))

# Save the file as an R data file: 

save(samples, file = 'data/samples_x')