# qol_data <- read_rds('data/processed_full_qol_pro_data.rds')
# 
# selected_treatment <- 'Anastrozole'
# selected_symptoms <- 'bladlc'
# selected_timepoints <- c(1, 5)
# selected_initial_response <- 'Moderately'
# 
# 
# temp <- selected_qol_sankey_data(
#   data = qol_data,
#   selected_treatment = selected_treatment,
#   selected_symptoms = selected_symptoms,
#   selected_timepoints = selected_timepoints,
#   selected_initial_response = selected_initial_response
# )



summary_qol_desc <- function(data) {
  initial_res <- data %>%
    group_by(
      patientid
    ) %>%
    slice(1) %>%
    ungroup() %>%
    select(
      -patientid
    ) %>%
    distinct()
  
  final_time_point <- data %>%
    group_by(patientid) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(time_point) %>%
    distinct(time_point) %>% pull(time_point)
  
  final_text <- data %>%
    group_by(
      patientid
    ) %>%
    slice_tail(n = 1) %>%
    group_by(., value) %>%
    count() %>%
    ungroup() %>%
    mutate(
      prop = scales::percent(n / sum(n), 1)
    ) %>%
    mutate(
      text = glue::glue('{n} ({prop}) responded {value}')
    ) %>%
    summarise(
      text = paste0(text, collapse = ', ')
    ) %>%
    mutate(
      final_timepoint = final_time_point
    ) %>%
    summarise(
      text = glue::glue('{text}, by the {final_timepoint}.')
    )
  
  initial_selected_patients <- unique(data$patientid) %>% length()
  
  
  text <- glue::glue(
    'Among the {initial_selected_patients} patients treated with {initial_res$trt} who responded {initial_res$value} at {initial_res$time_point} for {initial_res$name}; {final_text}'
  )
  
  return(text)
}

summaryqoldescOutput <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns('summary_res'))
  )
}

summaryqoldescServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    summary_res <- reactive({
      
      summary_qol_desc(data())
      
    })
    
    output$summary_res <- renderText({summary_res()})
    
  })
}








