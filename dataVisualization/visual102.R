# load required library
library(ggplot2)

# make the base graph object
graph_object <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl))

# Add visual elements
graph_object + geom_point()

