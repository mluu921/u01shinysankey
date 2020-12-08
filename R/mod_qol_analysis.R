selected_qol_sankey_data <- function(data, selected_treatment, selected_symptoms, selected_timepoints, selected_initial_response) {
  data %>%
    filter(trt == selected_treatment) %>%
    filter(name == selected_symptoms) %>%
    group_by(patientid) %>%
    filter(any(
      time_point_numeric == selected_timepoints[[1]] &
        value == selected_initial_response
    )) %>%
    ungroup() %>%
    filter(time_point_numeric %in% c(selected_timepoints[[1]]:selected_timepoints[[2]]))
  
}

qol_analysis_output <- function(id, data) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = 'Inputs',
        width = 12,
        status = 'primary',
        solidHeader = T,
        collapsible = T,
        collapsed = F,
        selectInput(
          ns('selected_treatment'),
          label = h3("1. Selected Treatment"),
          choices = sort(unique(data$trt)),
          selected = 1
        ),
        
        selectInput(
          ns('selected_symptoms'),
          label = h3("2. Selected Symptoms"),
          choices = unique(data$name),
          selected = 1
        ),
        
        sliderInput(
          ns('selected_timepoints'),
          label = h3("3. Selected Cycle"),
          min = 0,
          max = 12,
          value = c(0, 5)
        ),
        
        selectInput(
          ns('selected_initial_response'),
          label = h3("4. Selected Initial Response"),
          choices = c("NA"),
          selected = "NA",
          multiple = F,
          selectize = F
        ),
        hr(),
        actionButton(ns("button_visualize_qol"), "Visualize", icon = icon('bar-chart'))
      )
    ),
    fluidRow(
      box(
        title = 'Summary Description',
        width = 12,
        status = 'primary',
        solidHeader = T,
        collapsible = T,
        collapsed = F,
        h3(summaryqoldescOutput(ns('qol_summary_desc')))
      )
    ),
    fluidRow(
      box(
        title = 'Sankey Diagram',
        width = 12,
        status = 'primary',
        solidHeader = T,
        collapsible = T,
        collapsed = F,
        qolsankeyOutput(ns('qol_sankey'))
      )
    )
  )
}

qol_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
    
    selected_treatment <- reactive({
      data %>% filter(trt == input$selected_treatment)
    })
    
    observeEvent(
      selected_treatment(), {
        
        
        choices <- unique(selected_treatment()$name)
          
        updateSelectInput(
          session, 'selected_symptoms', choices = choices
        )
        
      }
    )
    
    selected_treatment_symptoms <- reactive({
      data %>% filter(trt == input$selected_treatment & name == input$selected_symptoms)
    })
    
    observeEvent(
      selected_treatment_symptoms(), {
        
        max <- max(selected_treatment_symptoms()$time_point_numeric)
        
        updateSliderInput(
          session, 'selected_timepoints', min = 0, max = max
        )
        
      }
    )
    
    selected_treatment_symptom_timepoint <- reactive({

      data %>% 
        filter(trt == input$selected_treatment) %>%
        filter(name == input$selected_symptoms) %>%
        filter(
          time_point_numeric == input$selected_timepoints[[1]]
        )

    })

    observeEvent(
      selected_treatment_symptom_timepoint(), {
        choices <- unique(selected_treatment_symptom_timepoint()$value)

        updateSelectInput(
          session, 'selected_initial_response', choices = choices[choices != 'NA']
        )
      }
    )
    
    dat <- reactive({

      selected_qol_sankey_data(
        data = data,
        selected_treatment = input$selected_treatment,
        selected_symptoms = input$selected_symptoms,
        selected_timepoints = input$selected_timepoints,
        selected_initial_response = input$selected_initial_response
      )

    })
    
    
    dat <- eventReactive(
      input$button_visualize_qol, {
        
        selected_qol_sankey_data(
          data = data,
          selected_treatment = input$selected_treatment,
          selected_symptoms = input$selected_symptoms,
          selected_timepoints = input$selected_timepoints,
          selected_initial_response = input$selected_initial_response
        )
        
      }
    )
    
    qolsankeyServer('qol_sankey', data = dat)

    summaryqoldescServer('qol_summary_desc', data = dat)
    
  })
}









