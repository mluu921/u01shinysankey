data <- read_rds(here::here('data/processed_data.rds'))

keep_ae <- data %>%
  group_by(
    trt, ae, ae_grade
  ) %>%
  count() %>%
  filter(
    ae_grade != '0-1' & ae_grade != 'OFF'
  ) %>%
  group_by(
    trt, ae
  ) %>%
  summarise(
    total = sum(n)
  ) %>%
  arrange(desc(total)) %>%
  ungroup() %>%
  group_nest(trt) %>%
  mutate(
    data = map(data, ~ slice_head(.x, n = 50))
  ) %>%
  unnest(data)

keep_ae <- keep_ae %>% pull(ae)

data <- data %>%
  filter(
    ae %in% keep_ae
  )

write_rds(data, 'data/processed_condensed_data.rds')
