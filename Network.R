# 1. Install necessary packages (remove the '#' if you don't have them)
install.packages(c("tidyverse", "igraph", "ggraph"))

library(tidyverse)
library(igraph)
library(ggraph)

# 2. Load the data and create a unique ID for each article
datos <- read_csv("Gallery_Library.csv") %>%
  mutate(ID_Articulo = row_number()) # Add a row number as ID

# 3. Clean and separate the authors
autores_df <- datos %>%
  select(ID_Articulo, Authors) %>%
  filter(!is.na(Authors)) %>%
  separate_rows(Authors, sep = ";") %>% # Authors are separated by semicolons
  mutate(Authors = str_trim(Authors))   # Remove extra spaces

# 4. Filter to avoid a giant, incomprehensible "cloud" of names
# We will keep only authors who have published at least 2 articles in this dataset.
autores_frecuentes <- autores_df %>%
  count(Authors) %>%
  filter(n >= 2) %>%
  pull(Authors)

# 5. Create the "links" (Edges) of the network (who worked with whom)
# We join the table with itself based on the article ID
coautores <- autores_df %>%
  filter(Authors %in% autores_frecuentes) %>%
  inner_join(autores_df %>% filter(Authors %in% autores_frecuentes), 
             by = "ID_Articulo", relationship = "many-to-many") %>%
  # Keep only unique pairs (prevents A-B and B-A from counting double, or A joining A)
  filter(Authors.x < Authors.y) %>% 
  # Count how many times they published together (this will be the "thickness" of the line)
  count(Authors.x, Authors.y, name = "weight")

# 6. Create the mathematical network object (Graph)
red_autores <- graph_from_data_frame(d = coautores, directed = FALSE)

# 7. Plot the network with ggraph
set.seed(123) # So the shape of the graph is always the same when running the code

ggraph(red_autores, layout = "fr") + # 'Fruchterman-Reingold' layout (separates nodes well)
  # Draw the lines (edges)
  geom_edge_link(aes(edge_width = weight), alpha = 0.4, color = "darkgray") +
  # Draw the points (nodes/authors)
  geom_node_point(color = "tomato", size = 5, alpha = 0.9) +
  # Add author names
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5, color = "black", 
                 bg.color = "white", bg.r = 0.15) + # 'bg.color' adds a white outline for readability
  # Adjust the scale of line thickness
  scale_edge_width(range = c(0.5, 2)) +
  # Clean the background
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40")) +
  labs(
    title = "Researcher Co-authorship Network",
    subtitle = "Authors with at least 2 publications and their collaboration links",
    edge_width = "Number of collaborations"
  )

