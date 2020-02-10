
library(data.table)

dengue2013 <- read.csv('data-raw/dengue2013.csv')

usethis::use_data(dengue2013, overwrite = TRUE)


