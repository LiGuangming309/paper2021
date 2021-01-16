dem.state.data2 <- dem.state.data %>%
  pivot_wider(
    names_from = variable,
    values_from = pop_size,
    values_fill = 0
  ) 

dem.state.data3 <- dem.state.data %>%
  spread(key = variable, value = pop_size, fill = 0)

nrow(dem.state.data3)*(ncol(dem.state.data3)-4)-nrow(dem.state.data)
nrow(dem.state.data2)*(ncol(dem.state.data2)-4)-nrow(dem.state.data)
