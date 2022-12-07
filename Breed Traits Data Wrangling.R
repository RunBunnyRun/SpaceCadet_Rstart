# install.packages("tidytuesdayR")
#install.packages("dplyr")
#install.packages("janitor)

library(janitor)
library(dplyr)

# tuesdata <- tidytuesdayR::tt_load("2022-02-01")
# breed_traits <- tuesdata$breed_traits

# breed_traits table saved to RDS file to work within limits:
# saveRDS(tuesdata$breed_traits, "breed_traits.rds")

breed_traits <- clean_names(readRDS("breed_traits.rds"))

# select(breed_traits, "Breed")
select(breed_traits, barking_level)
# select(breed_traits, "Coat Type")
select(breed_traits, c(1, 3, 6))
# select(breed_traits, "Breed", "Coat Length")
select(breed_traits, 1,2,6,7,8,9,10)
select(breed_traits, 1,2,6:10)
# select(breed_traits, "Breed", "Affectionate With Family", "Drooling Level":"Openness To Strangers")
select(breed_traits, breed, shedding_level, coat_grooming_frequency)
#Using select and testing clean_names function on selecting columns and running filters

filter(breed_traits, drooling_level == 5)
filter(breed_traits, drooling_level == 5 & coat_length == "Short")
filter(breed_traits, drooling_level == 5 | drooling_level == 3)
filter(breed_traits, drooling_level %in% c(1,3,5))
filter(breed_traits, drooling_level %in% c(3,4,5) & coat_length == "Short")
filter(breed_traits, drooling_level != 5)
filter(breed_traits, !drooling_level %in% c(1,3,5))
#different ways to filter, including a NOT operator (!), and (&), or (|).

# Homework is to choose Dog Breeds that meet all 3 of:
#Most affectionate with family (4,5)
#Best with Young Children (4,5)
#Best with Other Dogs (4,5)
#Then save output to an object called "friendly_dogs"

#filter(breed_traits, affectionate_with_family == 4 | affectionate_with_family == 5)
#filter(breed_traits, affectionate_with_family == 4 | affectionate_with_family == 5 & good_with_young_children == 4 | good_with_young_children == 5 & good_with_other_dogs == 4 | good_with_other_dogs == 5)
#filter(breed_traits, affectionate_with_family %in% c(1,2,3) & good_with_young_children %in% c(1,2,3) & good_with_other_dogs %in% c(1,2,3))
#filter(breed_traits, affectionate_with_family %in% c(1,2,3), good_with_young_children %in% c(1,2,3), good_with_other_dogs %in% c(1,2,3))

filter(breed_traits, affectionate_with_family %in% c(4,5), good_with_young_children %in% c(4,5), good_with_other_dogs %in% c(4,5))
#The above code seems to give me the correct list.

friendly_dogs <- filter(breed_traits, affectionate_with_family %in% c(4,5), good_with_young_children %in% c(4,5), good_with_other_dogs %in% c(4,5))
friendly_dogs

# arrange(breed_traits, breed) |> View()
arrange(breed_traits, desc(breed))
arrange(breed_traits, desc(breed), good_with_other_dogs)
arrange(breed_traits, desc(coat_length), breed)
# Testing different ways to arrange.

drooly_dogs <- breed_traits |>
  filter(drooling_level == 5) |> 
  arrange(breed)
# Testing pipe - filter than arrange via the pipe.
  
noisy_dogs <- breed_traits |> 
  mutate(bark_energy_level = energy_level * barking_level) |>
  select(breed, energy_level, barking_level, bark_energy_level) |>
  arrange(desc(bark_energy_level))
# create a new column based on existing data. Default arrange is in ascending order.

trainable_dogs <- breed_traits |>
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable"
  )) |>
  select(breed, trainability_level, trainability_category) |>
  filter(trainability_category == "Very trainable")
# create a new column, display using filter and select, point code to a new object.

smooth_dogs <- breed_traits |>
  mutate(smooth_coat = if_else(coat_type == "Smooth", TRUE, FALSE)) |>
  select(breed, coat_type, smooth_coat)
# If_else argument vs. case_when. Creating new object is final output.

# Homework - Create a new column that categorises dogs by their drooling levels:
# Light drool = 1 or 2
# Medium drool = 3
# Heavy drool - 4 or 5
# In the same pipeline, filter out light and medium drool dogs.
# In the same pipeline, sort the drooly dogs in descending alphabetical order by breed name.
# Save that output to an object called 'dogs_that_drool'.

dogs_that_drool <- breed_traits |>
  mutate(drool_group = case_when(
    drooling_level <= 2 ~ "Light",
    drooling_level == 3 ~ "Medium",
    drooling_level > 3 ~ "Heavy"
  )) |>
  select(breed, drooling_level, drool_group) |>
  filter(drool_group == "Heavy") |>
  arrange(desc(breed))
# Success

breed_traits |>
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable"
  )) |> 
  group_by(trainability_category) |> 
  summarise(
    avg_energy_lvl = mean(energy_level),
    count = n()
    )
# Testing group_by and summarise functions. Can use mean, median and sum here. n() returns no. of rows.

breed_traits |>
  mutate(trainability_category = case_when(
    trainability_level <= 2 ~ "Not very trainable",
    trainability_level == 3 ~ "Somewhat trainable",
    trainability_level > 3 ~ "Very trainable")) |> 
  count(trainability_category)
# If you just want to count rows in each category.



