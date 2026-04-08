# 1. Instalar los paquetes necesarios (quita el '#' si no los tienes)
# install.packages(c("tidyverse", "visNetwork", "igraph"))

library(tidyverse)
library(visNetwork)
library(igraph)

# 2. Cargar los datos y crear un ID único para cada artículo
datos <- read_csv("Gallery_Library.csv") %>%
  mutate(ID_Articulo = row_number()) # Agrega un número de fila como ID

# 3. Limpiar y separar los autores
autores_df <- datos %>%
  select(ID_Articulo, Authors) %>%
  filter(!is.na(Authors)) %>%
  separate_rows(Authors, sep = ";") %>% 
  mutate(Authors = str_trim(Authors))   

# 4. Filtrar a los autores más frecuentes (al menos 2 publicaciones)
autores_frecuentes <- autores_df %>%
  count(Authors) %>%
 # filter(n >= 2) %>%
  pull(Authors)

# 5. Crear los enlaces (EDGES) en el formato que requiere visNetwork
# (Necesita columnas llamadas 'from', 'to' y opcionalmente 'value' para el grosor)
edges <- autores_df %>%
  filter(Authors %in% autores_frecuentes) %>%
  inner_join(autores_df %>% filter(Authors %in% autores_frecuentes), 
             by = "ID_Articulo", relationship = "many-to-many") %>%
  filter(Authors.x < Authors.y) %>% 
  count(Authors.x, Authors.y, name = "value") %>%
  rename(from = Authors.x, to = Authors.y) # visNetwork exige estos nombres

# 6. Crear los nodos (NODES) en el formato que requiere visNetwork
# Obtenemos todos los autores únicos que sobrevivieron en la tabla de 'edges'
nombres_unicos <- unique(c(edges$from, edges$to))

nodes <- tibble(
  id = nombres_unicos,
  label = nombres_unicos,                    # El texto que aparece junto al nodo
  title = paste("Autor:", nombres_unicos)    # El texto que aparece al hacer "hover" (pasar el ratón)
) %>%
  # AQUI RESALTAMOS A LOS AUTORES ESPECÍFICOS
  mutate(
    # Cambiamos el color
    color = ifelse(id %in% c("Fischer, Bob", "Barrett, Meghan"), 
                   "#FFD700",  # Color Oro/Gold para los resaltados
                   "tomato"),  # Color por defecto para los demás
    
    # Hacemos los nodos resaltados un poco más grandes
    size = ifelse(id %in% c("Fischer, Bob", "Barrett, Meghan"), 35, 20),
    
    # Opcional: Hacer el texto de los resaltados un poco más grande
    font.size = ifelse(id %in% c("Fischer, Bob", "Barrett, Meghan"), 20, 14)
  )

# 7. Graficar la red interactiva
visNetwork(nodes, edges, 
           main = "Interactive Researcher Co-authorship Network",
           submain = "Highlighted: Fischer, Bob & Barrett, Meghan") %>%
  # Usar el algoritmo Fruchterman-Reingold de igraph para distribuir los nodos
  visIgraphLayout(layout = "layout_with_fr") %>%
  # Configurar las líneas (enlaces)
  visEdges(color = list(color = "darkgray", highlight = "black"), smooth = FALSE) %>%
  # Configurar el estilo del texto de los nodos
  visNodes(font = list(color = "black", strokeWidth = 2, strokeColor = "white")) %>%
  # Opciones de interactividad:
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), # Resalta a los coautores al hacer clic/hover
    nodesIdSelection = TRUE # Agrega un menú desplegable para buscar autores por nombre
  ) %>%
  # Controles de navegación y zoom
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)

