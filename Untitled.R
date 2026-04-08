# 1. Install and load necessary packages
# (If you don't have tidyverse installed, remove the '#' from the next line)
# install.packages("tidyverse")

library(tidyverse)

# 2. Load the data
# Make sure the 'Gallery_Library.csv' file is in your working directory
datos <- read_csv("Gallery_Library.csv")

# ==============================================================================
# INITIAL DATA EXPLORATION
# ==============================================================================

# View the general structure of the data (column names and data types)
glimpse(datos)

# View a basic statistical summary
summary(datos)

# ==============================================================================
# PLOT 1: EVOLUTION OF PUBLICATIONS PER YEAR
# ==============================================================================
# This chart will help us see if interest in this field has grown over time.

grafico_anios <- datos %>%
  # Remove rows with no publication year
  filter(!is.na(`Publication Year`)) %>% 
  # Count how many publications there are for each year
  count(`Publication Year`) %>% 
  
  # Create the plot
  ggplot(aes(x = `Publication Year`, y = n)) +
  geom_col(fill = "#4C72B0", alpha = 0.8) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) + # Add the number on top of the bar
  theme_minimal() +
  labs(
    title = "Evolution of Publications per Year",
    subtitle = "Literature trend on invertebrates, pain, and welfare",
    x = "Publication Year",
    y = "Number of Publications"
  )

# Show the plot
print(grafico_anios)

# ==============================================================================
# PLOT 2: TOP 15 MOST FREQUENT TOPICS
# ==============================================================================
# Since the "Topics" column contains several topics separated by commas, 
# we need to separate them to count them individually.

grafico_temas <- datos %>%
  # Remove rows without topics
  filter(!is.na(Topics)) %>% 
  # Separate the Topics column into multiple rows (using the comma as separator)
  separate_rows(Topics, sep = ",") %>% 
  # Remove extra whitespaces at the beginning and end
  mutate(Topics = str_trim(Topics)) %>% 
  # Count the frequency of each topic and sort from highest to lowest
  count(Topics, sort = TRUE) %>% 
  # Take only the top 15 topics
  slice_max(order_by = n, n = 15) %>% 
  
  # Create the plot
  ggplot(aes(x = reorder(Topics, n), y = n)) +
  geom_col(fill = "#55A868", alpha = 0.8) +
  coord_flip() + # Flip the bars so the texts are easier to read
  theme_minimal() +
  labs(
    title = "Top 15 Most Frequent Topics in the Literature",
    x = "Category / Topic",
    y = "Number of Articles"
  )

# Show the plot
print(grafico_temas)

# ==============================================================================
# PLOT 3 (Optional): MOST PROLIFIC AUTHORS
# ==============================================================================
# Authors are separated by semicolons (;)

grafico_autores <- datos %>%
  filter(!is.na(Authors)) %>%
  separate_rows(Authors, sep = ";") %>%
  mutate(Authors = str_trim(Authors)) %>%
  count(Authors, sort = TRUE) %>%
  slice_max(order_by = n, n = 10) %>% # Top 10 authors
  
  ggplot(aes(x = reorder(Authors, n), y = n)) +
  geom_col(fill = "#C44E52", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Most Prolific Authors",
    x = "Author",
    y = "Number of Articles"
  )

# Show the plot
print(grafico_autores)

