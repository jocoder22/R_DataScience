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


# Map cll to fill
ggplot(mtcars, aes(x=wt, y=mpg, fill=cyl))+
  geom_point(shape=1, size=3)


# add alpha: transparency
ggplot(mtcars, aes(x=wt, y=mpg, fill=cyl))+
  geom_point(shape=1, size=3, alpha=0.6)


# map color to am
ggplot(mtcars, aes(x=wt, y=mpg, fill=cyl, col=am))+
  geom_point(shape=1, size=3, alpha=0.6)



# map size to cylinder
ggplot(mtcars, aes(x=wt, y=mpg, size=cyl))+
  geom_point(shape=1, alpha=0.6, aes(col=am))


# map shape to cylinder
ggplot(mtcars, aes(x=wt, y=mpg, shape=factor(cyl), col=am))+
  geom_point(size=3, alpha=0.6)



# map label to cylinder
ggplot(mtcars, aes(x=wt, y=mpg, label=cyl, col=am))+
  geom_text()