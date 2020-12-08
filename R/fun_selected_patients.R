

selected_patients <- function(selected_treatment, selected_ae, selected_grade, selected_cycle) {
  
  data %>%
    filter(trt =={{selected_treatment}}) %>%
    filter(ae == {{selected_ae}}) %>%
    group_by(patientid) %>%
    filter(any(
      ncycle == {{selected_cycle}}[[1]] &
        ae_grade == {{selected_grade}}
    )) %>%
    ungroup() %>%
    filter(ncycle %in% c({{selected_cycle}}[[1]]:{{selected_cycle}}[[2]]))
  
}
