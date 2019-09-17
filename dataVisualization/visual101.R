
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

ggplot(mtcars, aes(x=wt, y=mpg))+
  geom_point()

ggplot(mtcars, aes(x=wt, y=mpg, color=disp))+
  geom_point()

ggplot(mtcars, aes(x=wt, y=mpg, size=disp))+
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
d_graph + geom_point(aes(color=clarity)) 


# plot points with aesthetics
ggplot(diamonds_s, aes(x=carat, y=price, color=clarity)) + geom_point()
d_graph + geom_point(aes(color=clarity)) 
d_g <- d_graph + geom_point(alpha=0.3) 
d_g + geom_smooth(se=FALSE)
d_g + geom_point(aes(color=clarity)) 


# plot with color
d_graph2 <- ggplot(diamonds_s, aes(x=carat, y=price, color=cut))
d_graph2 + geom_point() 
d_graph2 + geom_point(alpha=0.6) + geom_smooth()
d_graph2 + geom_smooth()


