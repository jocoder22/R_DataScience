
# load required library
library(ggplot2)
library(help = "datasets")

# Do basic exploration
str(mtcars)
typeof(mtcars)
class(mtcars)

# plot basic graphs
# Cyclinder: not as a factor
par(mfrow=c(2, 1))
ggplot(mtcars, aes(x=cyl, y=mpg))+
  geom_point()

# Cyclinder: as a factor
ggplot(mtcars, aes(x=factor(cyl), y=mpg))+
  geom_point()


# explore new datasets
class(diamonds)
str(diamonds)

# plot the new graphs
# diamonds_s <- diamonds[c(1:1000), c(1:10)]
diamonds_s <- subset(diamonds, price > 17000, names(diamonds))
d_graph <- ggplot(diamonds_s, aes(x=carat, y=price))
d_graph + geom_point() 
d_graph + geom_point() + geom_smooth()
