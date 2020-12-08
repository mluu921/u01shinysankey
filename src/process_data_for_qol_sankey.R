library(tidyverse)
library(tidygraph)
library(networkD3)

df <- read_rds('data/processed_full_qol_pro_data.rds')

selected_symptoms <- 'hotfl'
selected_timepoints <- c(1, 5)
selected_initial_response <- 'Extremely'

selected_qol_sankey_data <- function(data, selected_symptoms, selected_timepoints, selected_initial_response) {
  data %>%
    filter(name == selected_symptoms) %>%
    group_by(patientid) %>%
    filter(any(
      time_point_numeric == selected_timepoints[[1]] &
        value == selected_initial_response
    )) %>%
    ungroup() %>%
    filter(time_point_numeric %in% c(selected_timepoints[[1]]:selected_timepoints[[2]]))

}

data <- selected_qol_sankey_data(
  data = df,
  selected_symptoms = selected_symptoms,
  selected_timepoints = selected_timepoints,
  selected_initial_response = selected_initial_response
)

# start processing for sankey ---------------------------------------------

process_data_for_qol_sankey <- function(data) {
  
  temp <- data %>%
    group_nest(patientid, name) %>%
    mutate(data = map(data, ~ add_row(
      .x,
      time_point_numeric = 0,
      time_point = 'temp',
      value = 'temp',
      .before = 1
    )))
  
  temp <- temp %>%
    unnest(data)
  
  temp <- temp %>%
    mutate(value = ifelse(is.na(value), 'No response', value))
  
  time_point <-
    c(
      "temp",
      "Baseline form",
      "6 Month form",
      "12 Month form",
      "18 Month form",
      "24 Month form",
      "30 Month form",
      "36 Month form",
      "42 Month form",
      "48 Month form",
      "54 Month form",
      "60 Month form",
      "66 Month form",
      "72 Month form"
    )
  
  temp <- temp %>%
    mutate(time_point = factor(time_point, levels = time_point, labels = time_point)) %>%
    arrange(patientid, name, time_point)
  
  temp <- temp %>%
    group_by(patientid) %>%
    mutate(
      transition_timepoint = glue::glue('{lag(time_point)} - {time_point}'),
      transition_response =  glue::glue('{lag(value)} - {value}')
    ) %>%
    ungroup() %>%
    filter(!str_detect(transition_timepoint, 'temp')) %>%
    select(-time_point,-value)
  
  temp <- temp %>%
    group_by(name, transition_timepoint, transition_response) %>%
    count()
  
  temp <- temp %>%
    ungroup()
  
  temp <- temp %>%
    separate(transition_timepoint,
             c('source_timepoint', 'target_timepoint'),
             ' - ') %>%
    separate(transition_response,
             c('source_response', 'target_response'),
             ' - ')
  
  time_point <-
    c(
      "Baseline form",
      "6 Month form",
      "12 Month form",
      "18 Month form",
      "24 Month form",
      "30 Month form",
      "36 Month form",
      "42 Month form",
      "48 Month form",
      "54 Month form",
      "60 Month form",
      "66 Month form",
      "72 Month form"
    )
  
  temp <- temp %>%
    mutate(source_timepoint = factor(source_timepoint, levels = time_point, labels = time_point)) %>%
    arrange(name, source_timepoint)
  
  temp <- temp %>%
    mutate(
      source = glue::glue('{source_timepoint} - {source_response}'),
      target = glue::glue('{target_timepoint} - {target_response}')
    )
  
  temp <- temp %>%
    select(
      source, target, n
    ) %>% as_tbl_graph() %>% igraph_to_networkD3()
  
  return(temp)
  
}

sankey_qol_data <- process_data_for_qol_sankey(data)

# test results ------------------------------------------------------------

sankeyNetwork(
  Links = sankey_qol_data$links,
  Nodes = sankey_qol_data$nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = 'name',
  sinksRight = F,
  nodeWidth = 20,
  units = 'Patients',
  fontSize = 15,
  nodePadding = 25,
  margin = c(
    'top' = 50,
    'right' = 0,
    'bottom' = 0,
    'left' = 0
  )
)
