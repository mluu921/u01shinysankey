


data <- read_rds(here::here('data/processed_data.rds'))


selected_treatment <- 'Anastrozole'
selected_ae <- 'ARTHRL'
selected_cycle <- c(1, 5)
selected_grade <- 2


temp <- data %>%
  filter(
    trt == selected_treatment
  ) %>%
  filter(
    ae == selected_ae
  ) %>%
  group_by(
    patientid
  ) %>%
  filter(
    any(ncycle == selected_cycle[[1]] & ae_grade == selected_grade)
  ) %>%
  ungroup() %>%
  filter(
    ncycle %in% c(selected_cycle[[1]]:selected_cycle[[2]])
  )




