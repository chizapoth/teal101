# create some data

d1 <- data.frame(
  id = c(1, 2, 3),
  letter_upper = c('A', 'B', 'C')
)

d2 <- data.frame(
  id = c(1, 2, 3),
  letter_lower = c('a', 'b', 'c')
)

d <- teal_data(
  D1 = d1,
  D2 = d2
)

join_keys(d)

# set keys
join_keys(d) <- join_keys(
  join_key('D1', keys = 'id'),
  join_key('D2', keys = 'id'),
  join_key('D1', 'D2', keys = 'id')
)


app <- init(
  data = d,
  modules = modules(
    tm_data_table()
  )
)


shinyApp(app$ui, app$server)
