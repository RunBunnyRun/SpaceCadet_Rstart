# install.packages("tidytuesdayR")
#install.packages("dplyr")
#install.packages("janitor)

library(janitor)
library(dplyr)
library(tidytuesdayR)
library(ggplot2)

# tuesdata <- tidytuesdayR::tt_load("2022-02-01")
# breed_traits <- tuesdata$breed_traits
#trait_description <- tuesdata$trait_description
# saveRDS(tuesdata$trait_description, "trait_description.rds")
# breed_rank <- tuesdata$breed_rank
# saveRDS(tuesdata$breed_rank, "breed_rank.rds")

# breed_traits table saved to RDS file to work within limits:
# saveRDS(tuesdata$breed_traits, "breed_traits.rds")

breed_traits <- clean_names(readRDS("breed_traits.rds"))
trait_description <- clean_names(readRDS("trait_description.rds"))
breed_rank <- clean_names(readRDS("breed_rank.rds"))

#tuesdata <- tidytuesdayR::tt_load("2022-02-01") |> 
 # saveRDS("all_good_dogs.rds")
all_dogs <- readRDS("all_good_dogs.rds")
# Saving all 3 files of original data under new rds name.

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

# Homework - Which coat type has the highest average (mean) coat grooming frequency?
# Create a table of the dogs with the coat type identified above.
# Do they all have similarly high coat-grooming frequency scores?
# Which is the most common coat type?

grooming_traits <- breed_traits |> 
  select(breed, coat_grooming_frequency, coat_type, coat_length, shedding_level) |> 
  arrange(desc(coat_grooming_frequency))
  
grooming_traits |>
  mutate(groom_freq = case_when(
    coat_grooming_frequency <= 2 ~ "Low",
    coat_grooming_frequency == 3 ~ "Medium",
    coat_grooming_frequency > 3 ~ "High"
  )) 
#|>   arrange(desc(groom_freq))
# This adds a new column with the new category of groom_freq. Can also change order.

# View(grooming_traits)

high_avg_coat_groom <- grooming_traits |> 
  select(breed, coat_grooming_frequency, coat_type) |> 
  filter(coat_grooming_frequency %in% c(4,5)) |>
  group_by(coat_type)
 
high_avg_coat_groom
# Trying to narrow down the info I need into a final table.

coat_highest_groom <- high_avg_coat_groom |> 
  select(breed, coat_grooming_frequency, coat_type) |> 
  filter(coat_grooming_frequency %in% c(4,5)) |>
  group_by(coat_type) |> 
  count(coat_type) |> 
  arrange(n)

coat_highest_groom
# Simple table showing the 'Double' coat type has the highest average coat grooming frequency.

double_coat_dogs <- high_avg_coat_groom |> 
  select(breed, coat_grooming_frequency, coat_type) |> 
  filter(coat_type == "Double")
 
double_coat_dogs
# Table showing dogs with 'Double' coat type and their high (4or5) coat grooming frequency score.


coat_type_totals <- grooming_traits |> 
  select(breed, coat_grooming_frequency, coat_type, coat_length) |>
  arrange(breed, coat_grooming_frequency, desc(coat_type)) |> 
  count(coat_type)
# Table that shows full count for all dogs based on coat type. Double and Smooth are both the highest at 66 dogs.

# Lesson looking at joining different tables:
# tuesdata <- tidytuesdayR::tt_load("2022-02-01") |> 
 # saveRDS("all_good_dogs.rds")
all_dogs <- readRDS("all_good_dogs.rds")

(breed_traits <- all_dogs$breed_traits |> 
    clean_names())

(breed_ranks <- all_dogs$breed_rank |> 
    clean_names())

(traits_with_rank <- left_join(breed_traits, breed_ranks, by = "breed"))
# line above works without but if you want to view in the console, wrap whole line in parenthesis.

(breed_traits <- all_dogs$breed_traits |> 
    clean_names() |> 
  mutate(key = make_clean_names(breed)))
# Clean names tidies up column headings, make clean names tidies up entire column contents.
(breed_ranks <- all_dogs$breed_rank |> 
    clean_names() |> 
  mutate(key = make_clean_names(breed)) |> 
  select(-breed))
# You only need to have one breed column visible when comparing tables so deselect in breed ranks table.
(traits_with_rank <- left_join(breed_traits, breed_ranks, by = "key"))

(traits_with_rank |> 
    filter(is.na(links)) |> 
    nrow())
# This is a way to check if there are any NA values in the row links. This gives us a count of 0.
(traits_with_rank |> 
    filter(is.na(x2013_rank)) |> 
    nrow())
# Running this with a different column to check, gives us a different answer as there are NA values in this column. Count = 19.

(anti_join(breed_traits, breed_ranks, by = "key") |> 
    nrow())
# This checks whether there is a value in column key that doesn't match.

(breed_traits <- all_dogs$breed_traits |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed),
      all_traits_score = rowSums(across(where(is.double)), na.rm = TRUE) |> round()
      ) |> 
    select(-breed))
#The above is example code where the total score across all columns with numbers is added and total is in new column: all_traits_score.
# Breed column has be deselected.


(breed_traits <- all_dogs$breed_traits |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed),
      all_traits_score = rowSums(across(where(is.double)), na.rm = TRUE),
      negative_traits_score = rowSums(across(c(shedding_level:drooling_level, barking_level)), na.rm = TRUE),
      weighted_score = all_traits_score - (2 * negative_traits_score)
    ))
 # Code above creates a few new columns based on named criteria. Sum for a final weighted score added.

(breed_ranks <- all_dogs$breed_rank |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed),
      avg_rank = rowMeans(across(c(x2013_rank:x2020_rank))) |> 
  round()) |> 
  select(-breed))
# Worked through many issues to get the above code to work. 

(traits_with_rank <- left_join(breed_traits, breed_ranks, by = "key"))
# Displays table with newly added columns from both base tables.

(traits_with_rank |> 
    filter(weighted_score > 0) |> 
    ggplot(aes(x = weighted_score, y = avg_rank)) +
    geom_point() +
    theme_minimal())
# Plotting a scatter plot chart to see if there is a clear pattern.

# Homework: Create a new object - Family Friendly Dogs - subjective, you choose categories.
# Of the least family friendly dogs, which was the highest ranked in 2020?
# Of the top 20 ranked dogs in 2015, which are classified as least playful?
# Did those same dogs rank differently in 2018?

(good_traits <-  
    select(breed_traits, c(1:4, 10:14)))
# table with traits I consider good.
(bad_traits <- 
    select(breed_traits, c(1, 5:7)))
# table with traits I consider bad.

(good_dog_traits <- good_traits |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed),
      avg_good_score = rowMeans(across(where(is.double)), na.rm = TRUE) |> round()
    ))
# Gives me new average column of my selected good traits and adding in as new column. Will do the same for bad traits:
(bad_dog_traits <- bad_traits |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed),
      avg_bad_score = rowMeans(across(where(is.double)), na.rm = TRUE) |> round()
    ))

(family_friendly_dogs <- left_join(good_dog_traits, bad_dog_traits, by = "key") |> 
  select(-breed.y))
# Joining the two tables.

(top_family_dogs <- family_friendly_dogs |> 
    mutate(
      good = rowSums(across(c(2:9)), na.rm = TRUE),
      bad = rowSums(across(c(shedding_level:drooling_level)), na.rm = TRUE),
      weighted_friendly = good - (2 * bad)
    ))
# This table gives me my top family friendly dogs using a totally made up weighted formula.
# American Hairless Terrier is best according to my criteria. Plott Hounds are least family friendly.

(dog_ranks <- breed_rank |> 
    clean_names() |> 
    mutate(
      key = make_clean_names(breed)) |> 
    select(-breed))
# Prepping breed_rank to join with new table. I keep making new objects as I'm unsure about the pipe function. Need to practise more.
(ranked_family_dogs <- left_join(top_family_dogs, dog_ranks, by = "key"))
# Successful join of tables with extra columns to determine homework answers.

# Of the least family friendly dogs, which was the highest ranked in 2020?
# Of the top 20 ranked dogs in 2015, which are classified as least playful?
# Did those same dogs rank differently in 2018?

(rankings <- ranked_family_dogs |>
  mutate(dogrank = case_when(
    weighted_friendly < 13 ~ "Low",
    weighted_friendly > 21 ~ "High"
  )) |> 
  select(key, good, bad, weighted_friendly, dogrank, playfulness_level, x2013_rank:x2020_rank))
# Table identifying the top and lowest weighted scores.

# Highest ranked least family friendly dog in 2020 is Bulldogs.
# Least playful highest ranked dog from 2015 is equal: Cavalier King Charles Spaniel and Shih Tzu.
# These two dogs had the same playfulness score in 2018. CKCS was same at 18, ST went from rank 19 to 20.

# Although I answered the questions in the end, this task was tricky and I need more focus time to streamline my code.
# Need to use the pipe more and I didn't use R to give me the answer, rather just got the answers from the table I made.
# Should have used the filter and arrange functions more. Need to practice these.



 