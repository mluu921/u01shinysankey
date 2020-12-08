


report_path <- tempfile(fileext = ".Rmd")
file.copy("report.Rmd", report_path, overwrite = TRUE)

render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}


download_report_input <- function(id, label) {
  ns <- NS(id)
  tagList(
    downloadButton(ns('download_report'), label = label)
  )
}

download_report_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        params <- list(n = input$slider)
        callr::r(
          render_report,
          list(input = report_path, output = file, params = params)
        )
      }
    )
    
  })
}











