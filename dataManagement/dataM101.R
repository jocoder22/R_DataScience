# load required library
library(tidyr)

# explore dataset
str(iris)
class(iris)
typeof(iris)
head(iris)


# data transformation 
# using gather and separate 
iris_S <- iris %>% 
  gather(Key, Value, -Species) %>%
  separate(Key,c("Type", "TypeValue"), "\\.")

head(iris_S)
str(iris_S)
