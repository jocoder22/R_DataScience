
# load required library
library(ggplot2)

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