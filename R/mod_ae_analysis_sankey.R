

list_ae <- c("ABDPN", "ALKPHO", "ALLER", "ALLERH", "ALLERO", "ALOP", "AMYLA", 
             "ANOREX", "APNEA", "ARROTH", "ARTHR", "ARTHRL", "ATAX", "BICARB", 
             "BILIR", "BLADS", "BONPN", "BOWOBS", "BRUIS", "CATAR", "CATHRI", 
             "CFUNC", "CHEPN", "CHYPER", "CHYPO", "CISCH", "CNOTH", "CNSHEM", 
             "COAOTH", "COGND", "COLIT", "COND", "CONFU", "CONJ", "CONST", 
             "COUGH", "CPK", "CREAT", "CTNI", "CTNT", "CVOTH", "DEATH", "DEHYD", 
             "DELUS", "DEPRC", "DERMAT", "DIARCO", "DIARR", "DIZZY", "DRYI", 
             "DRYSKN", "DUOULC", "DYSP", "DYSPAR", "DYSPEP", "DYSPH", "DYSPHES", 
             "DYSPHPH", "DYSUR", "EARAC", "EDEMA", "ENDOTH", "ERYTH", "FATIG", 
             "FEV1", "FEVER", "FISTIN", "FISTRE", "FLAT", "GASTR", "GASULC", 
             "GGT", "GIOTH", "GLAU", "GYNEC", "HALLUC", "HEADAC", "HEARINN", 
             "HEARMID", "HEAROTH", "HEMAT", "HEMATH", "HEMOG", "HEMROTH", 
             "HEPOTH", "HOCALC", "HOGLYC", "HOKAL", "HOMAGN", "HONATR", "HOPHOS", 
             "HOTFL", "HRCHOL", "HRGLYC", "HRKAL", "HRNATR", "HRTRIG", "HRURIC", 
             "HYPOAL", "HYPOT", "HYPOX", "ILEUS", "INCON", "INFEC", "INFECN", 
             "INFECU", "INFOTH", "INSOM", "INVOLM", "IRRIT", "IRRMENS", "LEUK", 
             "LEUKO", "LIBID", "LIPASE", "LYMOTH", "LYMPHO", "LYMPHT", "MELENA", 
             "MEMLOS", "METOTH", "MOODA", "MOUDRY", "MUCRAD", "MUSCW", "MUSKOTH", 
             "MYALG", "MYOSI", "NAILCH", "NAUS", "NECER", "NEMOO", "NEMOT", 
             "NESEN", "NEUOTH", "NEUPN", "NEUT", "OSTEO", "PALPIT", "PANCR", 
             "PELVPN", "PERBEH", "PERIC", "PETECH", "PHLEB", "PHOTSN", "PIGMCH", 
             "PIT", "PLAT", "PLEUEF", "PLEUPN", "PNEUP", "PNEUT", "PNOTH", 
             "PNRAD", "PROTE", "PRURIT", "PTIME", "PULMF", "PULOTH", "QTCINT", 
             "RADDRM", "RADRR", "RASH", "RECPN", "RECTBL", "REGUOTH", "RENAL", 
             "RTOTH", "RTSKIN", "SECMAL", "SEIZ", "SGOT", "SGPT", "SIADH", 
             "SINBRA", "SINTAC", "SKNOTH", "SPEIMP", "STOM", "SUPVEN", "SWEAT", 
             "SXOTH", "SYNCO", "SYNDOTH", "TASTE", "TEARS", "THROM", "TREM", 
             "TRNSRB", "URETO", "URINF", "URINR", "URTIC", "VAGBLD", "VAGDIS", 
             "VAGDRY", "VAGI", "VASEPI", "VENTAR", "VERTI", "VISART", "VISBL", 
             "VISDB", "VISFL", "VISNBL", "VISOTH", "VISPHO", "VOD", "VOICE", 
             "VOMIT", "WNDINF", "WNDNON", "WTGAIN", "WTLOSS")

ae_analysis_output <- function(id) {
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
          choices = c("Anastrozole", "Tamoxifen"),
          selected = 1
        ),
        
        selectInput(
          ns('selected_ae'),
          label = h3("2. Selected AE"),
          choices = list_ae,
          selected = 1
        ),
        
        sliderInput(
          ns('selected_cycle'),
          label = h3("3. Selected Cycle"),
          min = 1,
          max = 18,
          value = c(1, 5)
        ),
        
        selectInput(
          ns('selected_grade'),
          label = h3("4. Selected Initial Grade"),
          choices = c("0-1", "2", "3", "4", "5"),
          selected = "0-1",
          multiple = F,
          selectize = F
        ),
        hr(),
        actionButton(ns("button_visualize"), "Visualize", icon = icon('bar-chart'))
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
        h3(summarydescOutput(ns('summary_res')))
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
        sankeyUI(ns('sankey_diagram'))
      )
    ),
    fluidRow(
      box(
        title = 'TI Distribution',
        width = 6,
        status = 'primary',
        solidHeader = T,
        collapsible = T,
        collapsed = F,
        tidistUI(ns('ti_dist_plot'))
      ),
      box(
        title = 'Grade Duration',
        width = 6,
        status = 'primary',
        solidHeader = T,
        collapsible = T,
        collapsed = F,
        gradedurationUI(ns('grade_duration_plot'))
      )
    )
  )
}

ae_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
    selected_treatment <- reactive({
      data %>% filter(trt == input$selected_treatment)
    })
    
    observeEvent(
      selected_treatment(), {
        
        choices <- unique(selected_treatment()$ae)
        
        desc <- unique(selected_treatment()$description)
        
        names(choices) <- desc
        
        updateSelectInput(
          session, 'selected_ae', choices = choices
        )
        
      }
    )
    
    selected_treatment_ae <- reactive({
      data %>% filter(ae == input$selected_ae & trt == input$selected_treatment)
    })
    
    observeEvent(
      selected_treatment_ae(), {
        
        max <- max(selected_treatment_ae()$ncycle)
        
        updateSliderInput(
          session, 'selected_cycle', min = 1, max = max
        )
      }
    )
    
    selected_treatment_ae_cycle <- reactive({
      
      data %>%
        filter(trt == input$selected_treatment) %>%
        filter(ae == input$selected_ae) %>%
        filter(
          ncycle == input$selected_cycle[[1]]
        )
      
    })
    
    observeEvent(
      selected_treatment_ae_cycle(), {
        choices <- unique(selected_treatment_ae_cycle()$ae_grade)
        
        updateSelectInput(
          session, 'selected_grade', choices = sort(choices[choices != 'OFF'])
        )
      }
    )
    
    dat <- eventReactive(
      input$button_visualize, {
        data %>%
          filter(trt == input$selected_treatment) %>%
          filter(ae == input$selected_ae) %>%
          group_by(patientid) %>%
          filter(any(
            ncycle == input$selected_cycle[[1]] &
              ae_grade == input$selected_grade
          )) %>%
          ungroup() %>%
          filter(ncycle %in% c(input$selected_cycle[[1]]:input$selected_cycle[[2]]))
      }
    )
    
    sankeyServer('sankey_diagram', data = dat)
    
    tidistServer('ti_dist_plot', data = dat)
    
    gradedurationServer('grade_duration_plot', data = dat)
    
    summarydescServer('summary_res', data = dat)
    
  })
}



# 
# library(shiny)
# 
# test_app <- function() {
#   ui <- fluidPage(
#     sidebarLayout(
#       sidebarPanel(
#         ae_analysis_input('ae_analysis')
#       ),
#       mainPanel(
#         ae_analysis_output('ae_analysis')
#       )
#     )
#   )
# 
#   server <- function(input, output, session) {
#     ae_analysis_server('ae_analysis', data)
#   }
# 
#   shinyApp(ui, server)
# 
# }
# 
# 
# test_app()
# 
# 
# 
# 
# 
# 

