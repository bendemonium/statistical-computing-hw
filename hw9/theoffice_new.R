library(tidyverse)
library(tidymodels)
library(schrute)

data(theoffice)

# count the number of lines for each character
theoffice_characters <- theoffice |> 
  summarize(count = n(),
            .by = c(season, 
                    episode, 
                    imdb_rating, 
                    director, 
                    writer, 
                    character)
  )

# find out which characters to classify as "other" because they occur in 6 or fewer episodes

characters_other <- theoffice_characters |>
  count(character) |>
  filter(n <= 4) |>
  pull(character)

# change to the other category and get the percentage of lines by each character per episode
theoffice_characters <- theoffice_characters |>
  mutate(character = if_else(character %in% characters_other, 
                             true = "other", 
                             false = character)) |>
  summarize(count = sum(count),
            .by = c(season, 
                    episode, 
                    imdb_rating, 
                    director, 
                    writer, 
                    character)) |>
  mutate(percentage = count/sum(count)) |>
  select(-count) 

# pivot so that each character gets their own column
theoffice_characters_wider <- theoffice_characters |>
  pivot_wider(names_from = character,
              values_from = percentage,
              names_prefix = "dialog_share_",
              values_fill = 0)

# split up writers

theoffice_characters_directors  <- theoffice_characters_wider |> 
  separate_longer_delim(cols = writer, 
                        delim = ";")

# make rare writers an "other" category

writers_other <- theoffice_characters_directors |>
  count(writer) |> 
  filter(n <= 4) |>
  pull(writer)

# pivot wider the writers, and do a count so we know how many "other" writers there are.
theoffice_characters_directors_writers <- theoffice_characters_directors |>
  mutate(writer = if_else(writer %in% writers_other,
                          true = "other",
                          false = writer)) |>
  mutate(writer_num = 1) |>
  group_by(across(c(-writer_num))) |>
  summarize(writer_num = sum(writer_num), 
            .groups = "drop") %>%
  pivot_wider(names_from = writer,
              values_from = writer_num,
              names_prefix = "writer_",
              values_fill = 0)


# fill in all NAs with zeros
# get the season percentage
theoffice_new <- theoffice_characters_directors_writers |>
  group_by(season) |>
  mutate(season_percentage = episode/max(episode),
         season = factor(season), 
         .after = season,
         director = factor(director)) |>
  ungroup()

library(janitor)

theoffice_new <- theoffice_new |>
  clean_names()

theoffice_new |>
  glimpse()

save(theoffice_new, file = "Homework/Homework 9/theoffice_new.Rda")

 