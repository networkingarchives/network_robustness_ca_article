# Network Robustness article repository  

## Code and text for article on network measure robustness

The scripts folder contains code is divided into three parts:

* robustness_data.R, which generates the three standardised edge lists (with additional information) which form the basis of the results. Because the data is from an ongoing project, we prefer not to release the full underlying archive until the project's completion. We have scrambled the node IDs and provide the files which are the output of this first script. The full data files will be published alongside a data paper in due course, and at that point this first script will make the code reproducible from start to finish.

* robustness_code.R - code for the three functions, one to generate a table of correlations for a range of network metrics between a sample and full network , a second which generates a list of sample networks of progressively smaller sizes and applies the first function to the list, and a third which does the same as the second with some adjustments to sample by node rather than by an attribute of an edge.  The code makes use of the C++ backend with data.table and parallel computing with furrr to speed things up. 

* robustness_final.R - loads the three pre-processed datasets, applies the function to them to generate the robustness results, and does some data wrangling to knit the files together in final form to be used for figures in the manuscript.

* disambiguation_robustness.R: a separate script to reproduce the results of the experiment which added random disambiguation errors. The data for this is large (8GB unzipped) and has not been included.

The folder network_robustness_tool contains the source code for a Shiny app which generates robustness results from a user-uploaded edge list. If run locally in R, you can remove the limit of 1000 edges and 20 runs - look for the relevant comments in the app source code. 

robustness_figures.Rmd contains the R markdown for code to generate all figures using ggplot2 - using the results generated by robustness_final.R.

The data folder contains the outputs, generated by the scripts above, necessary to re-create the figures. 
