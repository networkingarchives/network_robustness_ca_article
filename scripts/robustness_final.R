# Generate main results for each dataset. Takes several hours, or overnight in the case of the SPO dataset. 

emlo_results = robustness_test(emlo_network, nsim = 40)  
spo_results = robustness_test(spo_network, nsim = 40)
bcc_results = robustness_test(bcc_network, nsim = 40)  

# Generate node removal results
 
bcc_nodes = robustness_test_nodes(bcc_network, nsim = 40)
spo_nodes = robustness_test_nodes(spo_network, nsim = 40)
emlo_nodes = robustness_test_nodes(emlo_network, nsim = 40)

# Bind together into one dataframe and add information on dataset and removal type in case of nodes:

new_results = rbind(spo_results %>% 
                      mutate(dataset = 'spo'), 
                    emlo_results %>% 
                      mutate(dataset = 'emlo'), 
                    bcc_results %>% 
                      mutate(dataset = 'bcc'), 
                    spo_nodes %>% 
                      mutate(type = 'nodes') %>% 
                      mutate(dataset = 'spo'), 
                    emlo_nodes %>% 
                      mutate(type = 'nodes')%>% 
                      mutate(dataset = 'emlo'), 
                    bcc_nodes %>% 
                      mutate(type = 'nodes')%>% 
                      mutate(dataset = 'bcc')) 

# Save the file as an R data file:

save(new_results, file = 'new_results_2')

