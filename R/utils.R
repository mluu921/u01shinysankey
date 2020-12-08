create_box <- function(title, ...) {
  box(
    title = title,
    width = 12,
    status = 'primary',
    solidHeader = T,
    collapsible = T,
    collapsed = F,
    ...
  )
}


