# Load libraries
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(tidygraph)
library(igraph)
library(data.table)
library(ggraph)
library(snakecase)
library(lubridate)
library(furrr)


# Function to generate all metrics when passed a dataframe

calculate_metrics = function(.data){
  
  .data %>% 
    distinct(X1, X2, .keep_all = T) %>% 
    graph_from_data_frame() %>% 
    as_tbl_graph() %>%
    mutate(transitivity = local_transitivity()) %>% 
    mutate(eigen = centrality_eigen()) %>% 
    mutate(degree_total = centrality_degree(mode = 'total')) %>%
    mutate(degree_in = centrality_degree(mode = 'in')) %>%
    mutate(degree_out = centrality_degree(mode = 'out')) %>%
    mutate(closeness = centrality_closeness(mode = 'total')) %>% 
    mutate(betweenness = centrality_betweenness()) %>% 
    as_tibble() %>% 
    pivot_longer(names_to = 'metric', values_to = 'value', cols = 2:8) %>% 
    left_join(full_net_stats, by = c('name', 'metric')) %>% 
    filter(!is.na(value.x)) %>% 
    filter(!is.na(value.y)) %>% 
    group_by(metric) %>% 
    summarise(cor = cor(value.x, value.y, method = 'spearman')) %>% 
    ungroup()
  
}



robustness_test = function(network, nsim = 1){
  
  #options(dplyr.summarise.inform = FALSE)
  # set up parallel processing
  plan(multisession, workers = availableCores()-1)
  
  # Produce network metrics for full network
  
  full_net_stats = network %>% 
    distinct(X1, X2, .keep_all = T) %>% 
    graph_from_data_frame() %>% 
    as_tbl_graph() %>%
    mutate(transitivity = local_transitivity()) %>% 
    mutate(eigen = centrality_eigen()) %>% 
    mutate(degree_total = centrality_degree(mode = 'total')) %>%
    mutate(degree_in = centrality_degree(mode = 'in')) %>%
    mutate(degree_out = centrality_degree(mode = 'out')) %>%
    mutate(closeness = centrality_closeness(mode = 'total')) %>% 
    mutate(betweenness = centrality_betweenness()) %>% 
    as_tibble() %>% 
    pivot_longer(names_to = 'metric', values_to = 'value', cols = 2:8)
  
  # make this available in global environment
  
  assign("full_net_stats", full_net_stats, envir = .GlobalEnv)
  
  # Empty dataframe for results:
  
  results = tibble(metric = NA, cor = NA, sample = NA, run = NA, type = NA)
  
  
  for(column in c('letter_id', 'year_date', 'folio_or_catalogue')){
  
  removal = as.name(column)
  
  for(i in 1:nsim){
    
    lsEOG<-list()
    
    for(j in 1:99){
      
      
      
      name = paste0('sample_edges', j)
      
      sample_attrs = sample_frac(network %>% 
                                   distinct(!!removal), (100-j)/100) %>% 
        pull(!!removal)
      
      sample_edges = network %>% filter (!!removal %in% sample_attrs) %>% distinct(X1, X2)
      
      sample_edges = sample_edges[,c('X1', 'X2')]
      
      
      
      lsEOG[[name]] <-sample_edges %>% as_tibble()
      
      
      
    }
    
    samples = future_map_dfr(lsEOG, calculate_metrics, .progress = T) %>% 
      mutate(sample = rep(1:99, each = 7)) %>% 
      mutate(run = i) %>% mutate(type = column) 
    
    results=rbind(results, samples)
    
  }
  
}
  return(results)
  
}




# A separate function to test the robustness of node deletion:

robustness_test_nodes = function(network, nsim = 1){
  
  #options(dplyr.summarise.inform = FALSE)
  
  plan(multisession, workers = availableCores()-1)
  
  full_net_stats = network %>% 
    distinct(X1, X2, .keep_all = T) %>% 
    graph_from_data_frame() %>% 
    as_tbl_graph() %>%
    mutate(transitivity = local_transitivity()) %>% 
    mutate(eigen = centrality_eigen()) %>% 
    mutate(degree_total = centrality_degree(mode = 'total')) %>%
    mutate(degree_in = centrality_degree(mode = 'in')) %>%
    mutate(degree_out = centrality_degree(mode = 'out')) %>%
    mutate(closeness = centrality_closeness(mode = 'total')) %>% 
    mutate(betweenness = centrality_betweenness()) %>% 
    as_tibble() %>% 
    pivot_longer(names_to = 'metric', values_to = 'value', cols = 2:8)
  assign("full_net_stats", full_net_stats, envir = .GlobalEnv)
  
  full_node_list = network  %>%
    distinct(X1, X2) %>% 
    graph_from_data_frame(directed = T) %>% 
    as_tbl_graph() %>% 
    activate(nodes) %>% 
    as_tibble()
  
  assign("full_node_list", full_net_stats, envir = .GlobalEnv)
  
  results = tibble(metric = NA, cor = NA, sample = NA, run = NA)
  
    
    
    for(i in 1:nsim){
      
      lsEOG<-list()
      
      for(j in 1:99){
        
        
        
        name = paste0('sample_edges', j)
        
        sample_node_list = full_node_list %>% 
          sample_frac((100-j)/100, replace = F) %>% pull(name)
        
        sample_edges = network %>% filter(X1 %in% sample_node_list | X2 %in% sample_node_list) %>% distinct(X1, X2)
        
        sample_edges = sample_edges[,c('X1', 'X2')]
        
        
        
        lsEOG[[name]] <-sample_edges %>% as_tibble()
        
        
        
      }
      
      samples = future_map_dfr(lsEOG, calculate_metrics, .progress = T) %>% 
        mutate(sample = rep(1:99, each = 7)) %>% 
        mutate(run = i) 
      
      results=rbind(results, samples)
      
    }
    

  return(results)
  
}


