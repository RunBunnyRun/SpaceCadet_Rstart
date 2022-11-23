# install.packages("tidytuesdayR")
library(dplyr)
tuesdata <- tidytuesdayR::tt_load("2022-02-01")
breed_traits <- tuesdata$breed_traits
select(breed_traits, "Breed")
select(breed_traits, "Coat Type")
select(breed_traits, c(1, 3, 6))
select(breed_traits, c("Breed", "Coat Length"))
