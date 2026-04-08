# Instalar paquetes si faltan
# install.packages(c("tidyverse", "visNetwork", "igraph", "viridis"))
install.packages("flexdashboard")
library(tidyverse)
library(visNetwork)
library(igraph)
library(viridis)

# Cargar datos
datos <- read_csv("Gallery_Library.csv") %>%
  mutate(ID_Articulo = row_number())

# ==============================================================================
# 1. RED INTERACTIVA DE CO-OCURRENCIA DE TEMAS (CORREGIDO)
# ==============================================================================

temas_df <- datos %>%
  select(ID_Articulo, Topics) %>%
  filter(!is.na(Topics)) %>%
  separate_rows(Topics, sep = ",") %>%
  mutate(Topics = str_trim(Topics)) %>%
  # Excluir los tipos de artículos
  filter(!str_detect(Topics, "Article Type"))

# Encontrar los temas más frecuentes
temas_frecuentes <- temas_df %>%
  count(Topics) %>%
  filter(n >= 3) %>%
  pull(Topics)

# Crear los enlaces (EDGES)
edges_temas <- temas_df %>%
  filter(Topics %in% temas_frecuentes) %>%
  inner_join(temas_df %>% filter(Topics %in% temas_frecuentes), 
             by = "ID_Articulo", relationship = "many-to-many") %>%
  filter(Topics.x < Topics.y) %>%
  count(Topics.x, Topics.y, name = "value") %>%
  rename(from = Topics.x, to = Topics.y) %>%
  mutate(title = paste0("<p><b>Appeared together:</b> ", value, " times</p>"))

# Crear los nodos (NODES) - ¡Aquí quitamos la lista de colores problemática!
conteo_temas <- temas_df %>% count(Topics)
nombres_temas <- unique(c(edges_temas$from, edges_temas$to))

nodes_temas <- tibble(id = nombres_temas, label = nombres_temas) %>%
  left_join(conteo_temas, by = c("id" = "Topics")) %>%
  mutate(
    title = paste0("<p><b>Topic:</b> ", id, "<br><b>Total articles:</b> ", n, "</p>"),
    value = n # El tamaño del nodo dependerá de su frecuencia
  )

# Graficar la red de temas
visNetwork(nodes_temas, edges_temas, 
           main = "Topic Co-occurrence Network",
           submain = "Which concepts are investigated together?") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visEdges(color = list(color = "lightgray", highlight = "black")) %>%
  # AQUI APLICAMOS EL COLOR DE FORMA GLOBAL A TODOS LOS NODOS:
  visNodes(
    color = list(background = "#88CFA6", border = "#2F784D", highlight = "#FFD700"),
    font = list(color = "black", strokeWidth = 1.5, strokeColor = "white")
  ) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             nodesIdSelection = TRUE) %>%
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, hover = TRUE)


# ==============================================================================
# 2. EVOLUCIÓN DE LA MADUREZ DEL CAMPO (TIPOS DE ARTÍCULOS)
# ==============================================================================

grafico_madurez <- datos %>%
  filter(!is.na(Topics), !is.na(`Publication Year`)) %>%
  select(`Publication Year`, Topics) %>%
  separate_rows(Topics, sep = ",") %>%
  mutate(Topics = str_trim(Topics)) %>%
  # Filtrar SOLO las etiquetas que nos dicen el "Tipo de Artículo"
  filter(str_detect(Topics, "Article Type")) %>%
  # Limpiar el nombre para que el gráfico sea más legible
  mutate(Topics = str_remove(Topics, "Article Type - ")) %>%
  
  # 1. PRIMERO: Agrupamos los menos comunes bajo la etiqueta "Other"
  mutate(Topics = fct_lump(Topics, n = 5)) %>% 
  
  # 2. SEGUNDO: Filtramos para ELIMINAR los "Other" (!= significa "diferente de")
  filter(Topics != "Other") %>%
  
  count(`Publication Year`, Topics) %>%
  
  # Crear el gráfico de barras apiladas
  ggplot(aes(x = `Publication Year`, y = n, fill = Topics)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "plasma") + 
  theme_minimal() +
  labs(
    title = "Evolution of the Field: Article Types over Time",
    subtitle = "Tracking the transition from theoretical to empirical studies (Top 5 Types)",
    x = "Publication Year",
    y = "Number of Articles",
    fill = "Article Type"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))

# Mostrar el gráfico
print(grafico_madurez)

