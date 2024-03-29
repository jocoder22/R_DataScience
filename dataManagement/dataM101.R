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


# create the full wide dataset 
# create the serial number
iris$Serial <- 1:nrow(iris)
head(iris)
str(iris)
colnames(iris)


# create the full wide dataset 
iris_w <- iris %>% 
  gather(Key, Value, -Species, -Serial) %>%
  separate(Key,c("Type", "TypeValue"), "\\.") %>%
  spread(TypeValue, Value)

head(iris_w)
str(iris_w)


# save datasets
getwd()
filepath <- getwd()
saveRDS(iris_w,file.path(filepath,"dataManagement","irisWide.rds"))
saveRDS(iris_S,file.path(filepath,"dataManagement","irisShort.rds"))
