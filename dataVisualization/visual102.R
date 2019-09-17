# load required library
library(ggplot2)

# make the base graph object
graph_object <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl))

# Add visual elements
graph_object + geom_point()

# Add linear model liness, per cyl
graph_object + geom_point() +
  geom_smooth(method="lm", se=FALSE)


# lm for the whole dataset
graph_object + geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  geom_smooth(aes(group=1), method="lm", se=FALSE, linetype=2)
