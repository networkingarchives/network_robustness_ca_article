# Robustness Data

library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(tidygraph)
library(igraph)
library(data.table)
library(ggraph)
library(snakecase)
library(lubridate)
library(furrr)

# Make SPO network from edge list, create include year and folio columns:

spo_raw = read_delim('/Users/Yann/Documents/MOST RECENT DATA/fromto_all_place_mapped_stuart_sorted', delim = '\t', col_names = F )
spo_network = spo_raw %>% 
  dplyr::select(X1, X2, X3, X5, X8) %>% 
  mutate(X8 = str_remove(X8, "\\sf\\.[0-9]{1,}")) %>% 
  mutate(year_date = year(ymd(X3))) %>% 
  select(X1, X2, year_date, letter_id = X5, X8) %>% 
  mutate(folio_or_catalogue = str_remove(X8, "f\\.[0-9]{1}")) %>% 
  mutate(folio_or_catalogue = trimws(folio_or_catalogue, which = 'both')) %>% select(-X8) %>% data.table()

# Make EMLO network. First load full data to extract catalogue info:

work <- read_csv("/Users/Yann/Documents/MOST RECENT DATA/EMLO/work.csv", col_types = cols(.default = "c"))
colnames(work) = to_snake_case(colnames(work))

# Load edge list

emlo_raw = read_delim('/Users/Yann/Documents/MOST RECENT DATA/EMLO/emlo_full_network.dat', delim = '\t', col_names = F)

# Load a list of letters to remove (duplicates and some unknowns) from Github repository:

to_remove = read_csv('https://raw.githubusercontent.com/networkingarchives/de-duplications/master/to_remove_list_with_unknown.csv')

# Make network from edge list with catalogue and year information

emlo_network = emlo_raw %>% 
  filter(!X5 %in% to_remove$value) %>% 
  left_join(work %>%
              mutate(emlo_letter_id_number = as.numeric(emlo_letter_id_number)) %>%
              select(emlo_letter_id_number, original_catalogue_name), 
            by = c('X5' = 'emlo_letter_id_number')) %>%
  filter(original_catalogue_name != 'Bodleian card catalogue') %>% 
  mutate(year_date = year(ymd(X3))) %>% 
  select(X1, X2, year_date, letter_id = X5, folio_or_catalogue = original_catalogue_name )

# Load shelfmark info to join to BCC data:

shelfmarks = read_delim('/Users/Yann/Documents/MOST RECENT DATA/shelfmarks.txt', delim = '\t')

colnames(shelfmarks) = to_snake_case(colnames(shelfmarks))
folio_section = shelfmarks %>% filter(str_detect(shelfmark_and_pagination, "fol"))

bcc_with_f = folio_section %>% 
  separate(shelfmark_and_pagination, into = c('ms', 'folio'), sep = "fol|fols") %>% 
  mutate(ms_name = str_remove(ms, "[0-9]{1,}")) %>% 
  mutate(ms_no = str_extract(ms, "[0-9]{1,}")) %>% 
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Rawl. letters"), "MS Rawl. letters", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Ashmole"), "MS Ashmole", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Cherry"), "MS Cherry", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. letters"), "MS Eng. letters", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Wood F."), "MS Wood F.", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Ashmole"), "MS Ashmole", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. Mis"), "MS Eng. mis", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. mis"), "MS Eng. mis", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Smith"), "MS Smith", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. hist"), "MS Eng. hist", ms_name)) %>%
  mutate(ms_name = str_remove_all(ms_name,"z")) %>%
  mutate(ms_name = str_remove_all(ms_name,", ")) %>%
  mutate(ms_name = str_remove_all(ms_name, "a\\.|b\\.|c\\.|d\\.")) %>% 
  select(work_letter_id, ms_name, ms_no) %>% 
  mutate(folio_or_catalogue = paste0(ms_name, "_", ms_no)) %>% select(work_letter_id, folio_or_catalogue)

# Join this to the BCC portion of EMLO:

bcc_network = emlo_raw %>% 
  filter(!X5 %in% to_remove$value) %>% 
  left_join(work %>%
              mutate(emlo_letter_id_number = as.numeric(emlo_letter_id_number)) %>%
              select(emlo_letter_id_number, original_catalogue_name), 
            by = c('X5' = 'emlo_letter_id_number')) %>%
  filter(original_catalogue_name == 'Bodleian card catalogue') %>% 
  mutate(year_date = year(ymd(X3))) %>% 
  select(X1, X2, year_date, letter_id = X5) %>% left_join(bcc_with_f, by = c('letter_id' = 'work_letter_id'))  

# Scramble the node IDs:

spo_node_ids = spo_network %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(node_id = 1:nrow(.)) %>%
  mutate(name = as.numeric(name))

spo_letter_ids = spo_network %>%
  distinct(letter_id) %>% 
  mutate(sub_letter_id = paste0("L", 1:nrow(.)))

spo_folio_ids = spo_network %>%
  distinct(folio_or_catalogue) %>% 
  filter(!is.na(folio_or_catalogue)) %>% 
  mutate(sub_folio_id = paste0("F", 1:nrow(.)))
  
spo_network = spo_network %>% 
  left_join(spo_node_ids, by = c('X1' = 'name'))%>% 
  left_join(spo_node_ids, by = c('X2' = 'name')) %>% 
  left_join(spo_letter_ids, by = 'letter_id') %>% 
  left_join(spo_folio_ids, by = 'folio_or_catalogue') %>% 
  select(X1 = node_id.x, X2 = node_id.y, year_date, letter_id = sub_letter_id, folio_or_catalogue = sub_folio_id)


emlo_node_ids = emlo_network %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(node_id = 1:nrow(.)) %>%
  mutate(name = as.numeric(name))

emlo_letter_ids = emlo_network %>%
  distinct(letter_id) %>% 
  mutate(sub_letter_id = paste0("L", 1:nrow(.)))

emlo_folio_ids = emlo_network %>%
  distinct(folio_or_catalogue) %>% 
  filter(!is.na(folio_or_catalogue)) %>% 
  mutate(sub_folio_id = paste0("F", 1:nrow(.)))

emlo_network = emlo_network %>% 
  left_join(emlo_node_ids, by = c('X1' = 'name')) %>% 
  left_join(emlo_node_ids, by = c('X2' = 'name')) %>% 
  left_join(emlo_letter_ids, by = 'letter_id') %>% 
  left_join(emlo_folio_ids, by = 'folio_or_catalogue') %>% 
  select(X1 = node_id.x, X2 = node_id.y, year_date, letter_id = sub_letter_id, folio_or_catalogue = sub_folio_id)

bcc_node_ids = bcc_network %>% 
  graph_from_data_frame() %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(node_id = 1:nrow(.)) %>%
  mutate(name = as.numeric(name))

bcc_letter_ids = bcc_network %>%
  distinct(letter_id) %>% 
  mutate(sub_letter_id = paste0("L", 1:nrow(.)))

bcc_folio_ids = bcc_network %>%
  distinct(folio_or_catalogue) %>% 
  filter(!is.na(folio_or_catalogue)) %>% 
  mutate(sub_folio_id = paste0("F", 1:nrow(.)))

bcc_network = bcc_network %>% 
  left_join(bcc_node_ids, by = c('X1' = 'name'))%>% 
  left_join(bcc_node_ids, by = c('X2' = 'name'))  %>% 
  left_join(emlo_letter_ids, by = 'letter_id') %>% 
  left_join(emlo_folio_ids, by = 'folio_or_catalogue') %>% 
  select(X1 = node_id.x, X2 = node_id.y, year_date, letter_id = sub_letter_id, folio_or_catalogue = sub_folio_id)

save(spo_network, file = 'data/spo_network')
save(emlo_network, file = 'data/emlo_network')
save(bcc_network, file = 'data/bcc_network')
