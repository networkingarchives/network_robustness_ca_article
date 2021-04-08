

library(shiny)
library(tidyverse)
library(igraph)
library(ggraph)
library(plotly)
library(reader)
options(shiny.maxRequestSize = 30*1024^2)

library(dplyr, warn.conflicts = FALSE)
library(tidygraph)
library(data.table)
library(snakecase)
library(lubridate)
library(furrr)

# Function to generate all metrics when passed a dataframe

calculate_metrics = function(.data, x){
    
    .data %>% 
        distinct(.[1], .[2], .keep_all = T) %>% 
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
        left_join(x, by = c('name', 'metric')) %>% 
        filter(!is.na(value.x)) %>% 
        filter(!is.na(value.y)) %>% 
        group_by(metric) %>% 
        summarise(cor = cor(value.x, value.y, method = 'spearman')) %>% 
        ungroup()
    
}



robustness_test = function(network, columns, nsim = 1, y){

    
    # Empty dataframe for results:
    
    results = tibble(metric = NA, cor = NA, sample = NA, run = NA, type = NA)
    
    
    for(column in columns){
        
        removal = as.name(column)
        
        for(i in 1:nsim){
            
            lsEOG<-list()
            
            for(j in 1:99){
                
                
                
                name = paste0('sample_edges', j)
                
                sample_attrs = sample_frac(network %>% 
                                               distinct(!!removal), (100-j)/100) %>% 
                    pull(!!removal)
                
                sample_edges = network %>% filter (!!removal %in% sample_attrs) %>% distinct(.[1], .[2])
        
                
                
                
                lsEOG[[name]] <-sample_edges %>% as_tibble()
                
                
                
            }
            
            samples = map_dfr(lsEOG, calculate_metrics, x = y) %>% 
                mutate(sample = rep(1:99, each = 7)) %>% 
                mutate(run = i) %>% mutate(type = column) 
            
            results=rbind(results, samples)
            
        }
        
    }
    return(results)
    
}

ui <- fluidPage(
    
    # Application title
    titlePanel("Network Robustness Tool"),
    
    
    sidebarLayout(
        sidebarPanel(p("Upload an edge list (TSV or CSV) to calculate robustness scores."),p("This version is limited to 1000 edges and 20 runs to save on Shiny runtime. Download here and run locally in R to remove this restriction."), p("Edge list should have at least two columns, headed 'from' and 'to'. Additional columns (such as year or source) will be sampled separately and added to the plot."), p("The algorithm removes progressively larger random samples of edges and compares the full ranked metric those calculated using the partial network. All calculations are on an unweighted, directed network"),
            fileInput("file1", "Upload edge list"),
            actionButton('generate', 'Calculate Robustness'),
            selectInput('sims', 'Number of simulations', choices = 1:20) # change choices here to allow more runs
        ),
        
        
        mainPanel(
            plotlyOutput("robustnessPlot")
        )
    )
)

server <- function(input, output, session) {
    
    
    
    
    network_filtered = reactive({ 
        
        file <- input$file1
        
        likely_delim = get.delim(file$datapath, n = 20, delims = c(',', '\t'))
        a = read_delim(file$datapath, delim = likely_delim, col_names = T) %>% 
            filter(!is.na(.[1])) %>%
            filter(!is.na(.[2])) %>% mutate(edge_id = 1:nrow(.)) 
        #%>% 
            #slice(1:1000) # Remove this slice function to lift limit of 1000 edges
        
        
        a   
        
    })
    
    columns_to_use = reactive({
        
        colnames(network_filtered())[-c(1,2)]
    })

    full_net_stats = reactive({
        
        network_filtered() %>% 
        distinct(.[1], .[2], .keep_all = T) %>% 
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
        
    })

    
    
    network = eventReactive(input$generate,{ 
        withProgress(message = 'Calculating Scores', value = 0, {
        robustness_test(network_filtered(), columns = columns_to_use(), nsim = input$sims, y = full_net_stats())  
        })
    })
    
    
    output$robustnessPlot = renderPlotly({
        

        p = network() %>% filter(!is.na(metric)) %>% 
            # filter(dataset == 'emlo') %>% 
            mutate(metric = ifelse(metric == 'degree' | metric == 'degree_total', 'Degree', metric)) %>%
            mutate(metric = ifelse(metric == 'eigen', 'Eigenvector', metric)) %>%
            mutate(metric = ifelse(metric == 'transitivity', 'Transitivity', metric)) %>% 
            mutate(metric = ifelse(metric == 'closeness', 'Closeness', metric))%>% 
            mutate(metric = ifelse(metric == 'betweenness', 'Betweenness', metric)) %>%
            mutate(sample = as.numeric(sample)) %>% 
            rename(`Removal Type` = type) %>%
            group_by(metric, `Removal Type`, sample) %>% 
            summarise(mean = mean(cor), min = min(cor), max = max(cor),sd = sd(cor)) %>% 
            ggplot() + 
            geom_ribbon(aes(x = sample, ymax = mean+sd, ymin = mean-sd), alpha = .6, fill = 'gray75')  + 
            geom_line(aes(x = sample, y = mean), alpha = .8, color = 'blue') +
            labs(x = 'Percentage Removed', y = expression(rho*" (mean)")) + 
            theme_bw() +
           # coord_fixed(100,ylim = c(0,1)) + 
            facet_grid(`Removal Type`~metric) + 
            scale_color_viridis_d()  +
            scale_y_continuous(breaks = c(.5, 1)) +
            scale_x_continuous(breaks = c(50, 100)) + 
            theme(panel.grid.minor =element_blank(), 
                  text = element_text(family = 'Times'))
        
        
        plotly::ggplotly(p)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
