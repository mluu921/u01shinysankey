qol_data <- read_rds('data/processed_full_qol_pro_data.rds')

selected_treatment <- 'Anastrozole'
selected_symptoms <- 'bladlc'
selected_timepoints <- c(1, 5)
selected_initial_response <- 'Moderately'


temp <- selected_qol_sankey_data(
  data = qol_data,
  selected_treatment = selected_treatment,
  selected_symptoms = selected_symptoms,
  selected_timepoints = selected_timepoints,
  selected_initial_response = selected_initial_response
)



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
  
  temp$nodes <- temp$nodes %>%
    mutate(
      group = case_when(
        str_detect(name, 'Not at all') ~ '0',
        str_detect(name, 'Slightly') ~ '1',
        str_detect(name, 'Moderately') ~ '2',
        str_detect(name, 'Quite a bit') ~ '3',
        str_detect(name, 'Extremely') ~ '4',
        str_detect(name, 'No response') ~ '5'
      )
    )
  
  
  return(temp)
  
}

# debugonce(process_data_for_qol_sankey)

process_data_for_qol_sankey(temp)


qolsankeyOutput <- function(id) {
  ns <- NS(id)
  tagList(sankeyNetworkOutput(NS(id, 'sankey_output')))
}

qolsankeyServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
    sankey_data <- reactive({
      process_data_for_qol_sankey(data())
      
    })
    
    my_color <- 'd3.scaleOrdinal() .domain(["0", "1", "2", "3", "4", "5"]) .range(["#feedde", "#fdbe85" , "#fd8d3c", "#e6550d", "#a63603", "#74c476"])'
    
    output$sankey_output <- renderSankeyNetwork({
      sankeyNetwork(
        Links = sankey_data()$links,
        Nodes = sankey_data()$nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        colourScale = my_color,
        NodeGroup = 'group',
        sinksRight = F,
        nodeWidth = 20,
        units = 'Patients',
        fontSize = 15,
        nodePadding = 25,
        iterations = 0,
        margin = c(
          'top' = 50,
          'right' = 0,
          'bottom' = 0,
          'left' = 0
        )
      )
      
    })
    
  })
}


# library(shiny)
# 
# ui <- fluidPage(
#   
# )
# 
# server <- function(input, output, session) {
#   
# }
# 
# shinyApp(ui, server)
# 
# 







