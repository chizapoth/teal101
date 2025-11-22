# plotly 101

# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("cpsievert/plotly_book")

library(plotly)
# install.packages('listviewer')
library(listviewer)


data(diamonds, package = "ggplot2")
diamonds

# create three visualizations of the diamonds dataset
# if not providing plot type, something will be supplied by plot_ly itself
p <- plot_ly(diamonds, x = ~cut) # barplot
p

plot_ly(diamonds, x = ~cut, y = ~clarity) # heatmap
p <- plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

?plot_ly

plotly_json(p)

b <- plotly_build(p)
b
names(p)
# names(b)
length(b$x$data)

# Extract the `name` of each trace. plotly.js uses `name` to
# populate legend entries and tooltips
purrr::map_chr(b$x$data, "name")
