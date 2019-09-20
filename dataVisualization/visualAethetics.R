# load required library
library(ggplot2) 


# explore the dataset mtcars
str(mtcars)
head(mtcars)
length(mtcars)
dim(mtcars)



# explore features of dataset mtcars
class(mtcars$am)
class(mtcars$cyl)
class(mtcars$wt)


# plot the timeseries
ggplot(mtcars, aes(x=wt, y=mpg, col=cyl))+
  geom_point(shape=1, size=3)


ggplot(mtcars, aes(x=wt, y=mpg, fill=cyl))+
  geom_point(shape=1, size=3)

ggplot(mtcars, aes(x=wt, y=mpg, fill=cyl))+
  geom_point(shape=1, size=3, alpha=0.6)

