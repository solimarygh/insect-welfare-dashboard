# 1. Install necessary packages for text mining and word clouds
# (Remove the '#' if you don't have them installed)
 install.packages(c("tidyverse", "tidytext", "ggwordcloud"))

library(tidyverse)
library(tidytext)      # For text mining (tokenization)
library(ggwordcloud)   # For plotting word clouds in ggplot2

# 2. Load the data
datos <- read_csv("Gallery_Library.csv")
head (datos)
levels (as.factor(datos$Topics))

# ==============================================================================
# PLOT 4: EVOLUTION OF THE TOP 5 TOPICS OVER TIME
# ==============================================================================

# First, we identify the Top 5 topics (excluding "Article Type" categories)
top_temas <- datos %>%
  filter(!is.na(Topics)) %>%
  separate_rows(Topics, sep = ",") %>%
  mutate(Topics = str_trim(Topics)) %>%
  # Exclude tags that are just article types to focus on real subjects
  filter(!str_detect(Topics, "Article Type")) %>%
  count(Topics, sort = TRUE) %>%
  slice_max(order_by = n, n = 6) %>%
  filter (Topics != "Invertebrate")%>% # Estou asumindo que todos sao de invertebrados..
  pull(Topics)

# Now we plot how these 5 topics evolved over the years
grafico_evolucion_temas <- datos %>%
  filter(!is.na(`Publication Year`), !is.na(Topics)) %>%
  separate_rows(Topics, sep = ",") %>%
  mutate(Topics = str_trim(Topics)) %>%
  filter(Topics %in% top_temas) %>% # Filter only the top 5
  count(`Publication Year`, Topics) %>%
  
  ggplot(aes(x = `Publication Year`, y = n, color = Topics, group = Topics)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") + # Use a distinct color palette
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Evolution of Top 5 Research Topics Over Time",
    subtitle = "Tracking the growth of specific subjects (e.g., Welfare, Sentience, Farming)",
    x = "Publication Year",
    y = "Number of Publications",
    color = "Topic"
  )

# Show the plot
print(grafico_evolucion_temas)


# ==============================================================================
# PLOT 5: WORD CLOUD (AGRUPANDO PALAVRAS SEMELHANTES)
# ==============================================================================

grafico_nuvem_palavras <- datos %>%
  filter(!is.na(Title)) %>%
  # 1. Separar as frases em palavras
  unnest_tokens(word, Title) %>%
  # 2. Remover "stop words" (the, and, in, etc.)
  anti_join(stop_words, by = "word") %>%
  # 3. Remover números e palavras genéricas de artigos
  filter(!word %in% c("evaluation", "effects", "effect", "study", "studies"),
         !str_detect(word, "^[0-9]+$")) %>% 
  
  # 4. AGRUPANDO AS PALAVRAS (NOVA ETAPA)
  mutate(word = case_when(
    word %in% c("animal", "animals") ~ "animals",
    word %in% c("insect", "insects") ~ "insects",
    word %in% c("invertebrate", "invertebrates") ~ "invertebrates",
    # Agrupando também as grafias americanas e britânicas:
    word %in% c("anesthesia", "anesthetics", "anesthetic", 
                "anaesthesia", "anaesthetics", "anaesthetic") ~ "anesthesia",
    # O comando 'TRUE ~ word' significa: "se a palavra não for nenhuma das acima, deixe como está"
    TRUE ~ word 
  )) %>%
  
  # 5. Contar a frequência (agora com as palavras unificadas)
  count(word, sort = TRUE) %>%
  # Pegar o Top 60
  slice_max(order_by = n, n = 70) %>%
  
  # 6. Criar o gráfico
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud_area(shape = "circle") +
  scale_size_area(max_size = 60) + # Aumentei um pouco o tamanho máximo para dar mais destaque
  scale_color_gradient(low = "#4C72B0", high = "#C44E52") + 
  theme_minimal() +
  labs(
    title = "Word Cloud of Article Titles",
    subtitle = "Most frequently used terms (Grouped concepts)"
  )

# Exibir o gráfico
print(grafico_nuvem_palavras)
