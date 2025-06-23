library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(DT)
library(car)
library(caret)
library(tidyr)
library(sf)
library(leaflet)
library(scales)
library(rmarkdown)
library(readr)
library(knitr)
library(pagedown)
library(tinytex)
library(forcats)
library(patchwork)
library(rpart)
library(rpart.plot)
library(naniar)
# Carga de datos desde los archivos subidos
datos <- list(
  nacimientos = read_excel("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/Basefinal.xlsx", sheet = "Nacimientos"),
  fecundidad = read_excel("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/Basefinal.xlsx", sheet = "Fecundidad"),
  proyecciones = read_excel("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/Basefinal.xlsx", sheet = "Indicadores", col_names = TRUE),
  ingreso = read_excel("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/Basefinal.xlsx", sheet = "Ingreso"),
  epf_ingresos = read_excel("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/Basefinal.xlsx", sheet = "Ingresosdisponibles"),
  epf_poblacion = read_excel("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/Basefinal.xlsx", sheet = "teripoblacion1")
)

epf <- read_delim("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/epf_persona.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Edicion y limpieza de epf

hogar_base <- epf %>%
  filter(sprincipal == 1) %>%
  select(
    folio,
    ing_disp_hog_hd_pc,
    edad, sexo, edue, edunivel,
    npersonas, cse, macrozona,
    estrato_muestreo, ecivil
  )

# Paso 1: construir número de hijos por hogar
hijos_por_folio <- epf |>
  filter(parentesco %in% c(3, 4, 5)) |>
  group_by(folio) |>
  summarise(num_hijos = n(), .groups = "drop")

# Paso 2: unir con hogar_base y limpiar ingreso sin winsorizar
hogares_model <- hogar_base |>
  left_join(hijos_por_folio, by = "folio") |>
  mutate(
    num_hijos = replace_na(num_hijos, 0),
    tiene_hijos = ifelse(num_hijos > 0, 1, 0),
    ing_disp_hog_hd_pc = as.numeric(gsub(",", ".", ing_disp_hog_hd_pc))
  )


#### arbol

hogares_model <- hogares_model|>
  mutate(
    grupo_hijos = case_when(
      num_hijos == 0 ~ "0 hijos",
      num_hijos %in% 1:2 ~ "1–2 hijos",
      num_hijos >= 3 ~ "3+ hijos"
    ) |> factor(levels = c("0 hijos", "1–2 hijos", "3+ hijos"))
  )

# Crear índices para la partición estratificada
# p = 0.7 indica 70% para entrenamiento
# list = FALSE devuelve un vector de índices en lugar de una lista
set.seed(123)
trainIndex <- createDataPartition(
  y = hogares_model$grupo_hijos, # Usa la variable dependiente para la estratificación
  p = 0.7,
  list = FALSE
)

# Crear los conjuntos de datos de entrenamiento y prueba
train_data <- hogares_model[trainIndex, ]
test_data  <- hogares_model[-trainIndex, ]

# 1. Entrena el árbol con un 'cp' muy pequeño
#    Un 'cp' de 0.001 o incluso más pequeño (ej., 0.0001) asegura que el árbol crezca casi por completo.
#    Esto es necesario para que rpart explore todas las posibles podas.
arbol_completo <- rpart(grupo_hijos ~ npersonas + ing_disp_hog_hd_pc + edad + edue + sexo,
                        data = train_data,
                        method = "class",
                        control = rpart.control(cp = 0.0001)) # ¡CP muy bajo para empezar!



#Encontrar el CP usando la regla 1-SE (más robusta)
# El 'xstd' es el error estándar del 'xerror'.
# Buscamos el cp para el árbol más simple cuyo error está dentro de 1 desviación estándar del mínimo.
min_xerror_idx <- which.min(arbol_completo$cptable[,"xerror"])
min_xerror <- arbol_completo$cptable[min_xerror_idx,"xerror"]
se_xerror <- arbol_completo$cptable[min_xerror_idx,"xstd"]

# Encontrar el CP con el menor error de validación cruzada (xerror)
cp_min_error <- arbol_completo$cptable[which.min(arbol_completo$cptable[,"xerror"]),"CP"]

cp_1se_rule <- arbol_completo$cptable[
  arbol_completo$cptable[,"xerror"] <= (min_xerror + se_xerror), "CP"
]

# Si hay varios, elige el más grande (árbol más simple)
if (length(cp_1se_rule) > 0) {
  cp_1se_rule <- max(cp_1se_rule)
} else {
  cp_1se_rule <- cp_min_error
}

arbol_podado_optimo <- prune(arbol_completo, cp = cp_1se_rule)

preds_test_podado <- predict(arbol_podado_optimo, test_data, type = "class")
conf_mat <- confusionMatrix(preds_test_podado, test_data$grupo_hijos)

# Obtener la importancia de las variables
var_importance <- arbol_podado_optimo$variable.importance

# Verificar si hay variables importantes (puede que el árbol no use todas las variables)
if (length(var_importance) == 0) {
  print("")
} else {
  # Convertir a un dataframe para facilitar la visualización con ggplot2
  importance_df <- data.frame(
    Variable = names(var_importance),
    Importance = as.numeric(var_importance)
  )
}
  
# Ordenar por importancia descendente para el gráfico
importance_df <- importance_df

# Crea carpeta para gráficos persistentes si no existe
if (!dir.exists("figs_pdf")) dir.create("figs_pdf")

# Guardar el gráfico del árbol como imagen PNG (para el reporte)
ruta_arbol_png <- file.path("figs_pdf", "arbol_decision.png")  # Asegúrate de que la carpeta 'www' existe

png(filename = ruta_arbol_png, width = 1600, height = 900, res = 300)
rpart.plot::rpart.plot(arbol_podado_optimo,
                       type = 2,
                       extra = 104,
                       fallen.leaves = FALSE,
                       box.palette = "GnBu",
                       shadow.col = "gray",
                       main = "Árbol de decisión")
dev.off()


nombres_bonitos <- c(
  ing_disp_hog_hd_pc = "Ingreso per cápita",
  npersonas = "Tamaño del hogar",
  edue = "Escolaridad",
  edad = "Edad",
  sexo = "Sexo"
)

importance_df_bonito <- importance_df |> 
  mutate(Variable = ifelse(
    Variable %in% names(nombres_bonitos),
    nombres_bonitos[Variable],
    Variable
  ))
importance_df_bonito$Variable
# Selecciona solo las variables que vas a usar en el modelo del VIF
vif_vars <- train_data %>%
  select(ing_disp_hog_hd_pc, npersonas, edad, edue, sexo, cse, macrozona) %>%
  drop_na() # Elimina cualquier NA de estas variables para el VIF

aux_model <- lm(npersonas ~ ing_disp_hog_hd_pc + edad + edue + sexo , data = vif_vars)

# Calcula el VIF para el modelo auxiliar
vif_values <- vif(aux_model)


# Cargar shapefile de regiones (utilizar geojson simplificado desde GADM, si se tiene)
# Aquí se asume que existe un archivo "regiones_chile.geojson" con nombre de columna "Region"
regiones_chile <- st_read("C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/regiones.json", quiet = TRUE)

# Lista de indicadores relevantes
indicadores_relevantes <- c(
  "Tasa global de fecundidad",
  "Edad media de la fecundidad",
  "Tasa bruta de natalidad",
  "Número de nacimientos",
  "Edad media",
  "Índice de envejecimiento",
  "Tasa de crecimiento natural"
)

# Preparar base de proyecciones
proy_df <- datos$proyecciones |>
  rename_with(~ trimws(.x)) |>
  mutate(across(all_of(indicadores_relevantes), as.numeric)) |>
  mutate(Año = as.numeric(Año))

# Preparar base de ingreso
ingreso_df <- datos$ingreso |>
  rename_with(~ gsub("\\s+", " ", .x)) |>
  mutate(Año = as.numeric(Año),
         `Ingreso medio nominal ($)` = as.numeric(`Ingreso medio nominal ($)`))

epf_ingresos_df <- datos$epf_ingresos |> 
  rename_with(~ gsub("\\s+", "_", .x)) |> 
  filter(GRUPO_QUINTIL_DE_HOGARES != "Total") |>  # Elimina la fila TOTAL
  mutate(across(where(is.character), as.factor))

epf_poblacion_df <- datos$epf_poblacion |> 
  rename_with(~ gsub("\\s+", "_", .x)) |> 
  filter(CARACTERÍSTICAS_DE_LA_POBLACIÓN != "TOTAL") |>  # <-- esta línea elimina la fila TOTAL
  mutate(across(where(is.character), as.factor))

epf_poblacion_long <- epf_poblacion_df |> 
  rename(Grupo_Etario = CARACTERÍSTICAS_DE_LA_POBLACIÓN) |> 
  pivot_longer(cols = c(TOTAL, MUJERES, HOMBRES),
               names_to = "Sexo",
               values_to = "Poblacion")

# Redondear en epf_ingresos_df
tabla_epf_ingresos_df <- epf_ingresos_df |> 
  mutate(across(where(is.numeric), ~ round(.x)))

# Redondear en epf_poblacion_df
tabla_epf_poblacion_df <- epf_poblacion_df |> 
  mutate(across(where(is.numeric), ~ round(.x)))

tabla_epf_ingresos_df <- tabla_epf_ingresos_df |> 
  rename(
    Quintil = GRUPO_QUINTIL_DE_HOGARES,
    Hogares = HOGARES_NÚMERO,
    Porcentaje = HOGARES_PORCENTAJE
    # y así según lo que consideres relevante
  )

tabla_epf_poblacion_df <- tabla_epf_poblacion_df |> 
  rename(
    Grupo_Etario = CARACTERÍSTICAS_DE_LA_POBLACIÓN
    # El resto ya tiene nombres claros
  )

niveles_edad <- c(
  "0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
  "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59",
  "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 o MÁS"
)

epf_piramide <- epf_poblacion_df |>
  rename(Grupo_Etario = CARACTERÍSTICAS_DE_LA_POBLACIÓN) |>
  filter(Grupo_Etario %in% niveles_edad) |>  # Solo si es necesario filtrar
  mutate(
    Grupo_Etario = factor(Grupo_Etario, levels = niveles_edad),
    HOMBRES = -round(HOMBRES / 1000),
    MUJERES = round(MUJERES / 1000)
  ) |>
  pivot_longer(cols = c(HOMBRES, MUJERES), names_to = "Sexo", values_to = "Poblacion")


df_n <- datos$nacimientos  # Tu tabla original

# Seleccionar solo las columnas de grupos etarios
df_edades <- df_n |>
  select(Año, starts_with("Nacimientos de mujeres"))

# Transformar a formato largo
df_long <- df_edades |>
  pivot_longer(cols = -Año, names_to = "GrupoEdad", values_to = "Nacimientos")
df_long
# Definir grupos menores
grupos_menores <- c(
  "Nacimientos de mujeres de 50 años y más",
  "Nacimientos de mujeres de menores de 15 años",
  "Nacimientos de mujeres de edad no especificada",
  "Nacimientos de mujeres de 45 a 49 años"
)
# Dividir en dos datasets
df_principales <- df_long |> filter(!GrupoEdad %in% grupos_menores)
df_menores <- df_long |> filter(GrupoEdad %in% grupos_menores)
df_menores <- df_menores |>
  mutate(GrupoEdad = case_when(
    GrupoEdad == "Nacimientos de mujeres menores de 15 años" ~ "Menores de 15 años",
    GrupoEdad == "Nacimientos de mujeres de 45 a 49 años" ~ "45 a 49 años",
    GrupoEdad == "Nacimientos de mujeres de 50 años y más" ~ "50 años y más",
    GrupoEdad == "Nacimientos de mujeres de edad no especificada" ~ "Edad no especificada",
    TRUE ~ GrupoEdad  # Para dejar cualquier otro valor sin cambio
  ))

df_principales <- df_principales |>
  mutate(GrupoEdad = case_when(
    GrupoEdad == "Nacimientos de mujeres de 15 a 19 años" ~ "15 a 19 años",
    GrupoEdad == "Nacimientos de mujeres de 20 a 24 años" ~ "20 a 24 años",
    GrupoEdad == "Nacimientos de mujeres de 25 a 29 años" ~ "25 a 29 años",
    GrupoEdad == "Nacimientos de mujeres de 30 a 34 años" ~ "30 a 34 años",
    GrupoEdad == "Nacimientos de mujeres de 35 a 39 años" ~ "35 a 39 años",
    GrupoEdad == "Nacimientos de mujeres de 40 a 44 años" ~ "40 a 44 años",
    TRUE ~ GrupoEdad  # Cualquier otro valor queda igual
  ))




# UI del dashboard
ui <- dashboardPage(
  # Cabecera del dashboard con título e ícono
  dashboardHeader(
    title = tags$span(
      icon("flag", class = "text-danger"),  # Ícono de bandera en rojo
      tags$b("Fec-Mat CL")                   # Título en negrita
    )
  ),
  
  # Barra lateral con menú de navegación y sección "Acerca de"
  dashboardSidebar(
    sidebarMenu(
      # Estilos CSS personalizados para el menú interno "Acerca de"
      HTML('
        <style>
          /* Cambia el fondo y estilo del menú "Acerca de" */
          .sidebar .treeview-menu.acerca-bg {
            background-color: #f4f4f4 !important;
            border-left: 2px solid #3c8dbc;
            padding: 10px 15px;
            margin-top: 4px;
          }
        </style>
        
        <!-- Menú desplegable "Acerca de" con información del equipo -->
        <li class="treeview">
          <a href="#">
            <i class="fa fa-info-circle"></i> <span><b>Acerca de</b></span>
            <span class="pull-right-container"><i class="fa fa-angle-left pull-right"></i></span>
          </a>
          <ul class="treeview-menu acerca-bg" style="font-size: 13px;">
            <li style="margin-bottom: 4px; color: black;"><i class="fa fa-user text-info"></i> <b>Cristóbal Belmar Osorio.</b></li>
            <li style="margin-bottom: 4px; color: black;"><i class="fa fa-user text-info"></i> <b>César Sandoval Mondaca.</b></li>
            <li style="margin-bottom: 4px; color: black;"><i class="fa fa-envelope text-warning"></i> cristobal.belmar@alu.ucm.cl</li>
            <li style="margin-bottom: 4px; color: black;"><i class="fa fa-envelope text-warning"></i> cesar.sandoval@alumnos.ucm.cl</li>
            <li style="margin-bottom: 4px; color: black;"><i class="fa fa-book text-success"></i> Business Intelligence (IES-414)</li>
            <li style="color: black;"><i class="fa fa-chalkboard-teacher"></i> <b>Docente:</b>  José Zúñiga Núñez.</li>
          </ul>
        </li>
      '),
      
      # Opciones principales del menú de navegación con pestañas y sus íconos
      menuItem("Resumen", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Fecundidad por edad", tabName = "fecundidad", icon = icon("baby")),
      menuItem("Factores sociodemográficos", tabName = "factores", icon = icon("users")),
      menuItem("Proyecciones", tabName = "proyecciones", icon = icon("calendar")),
      menuItem("Ingreso regional", tabName = "ingreso", icon = icon("map")),
      menuItem("Árbol de decisión", tabName = "arbol", icon = icon("tree")),
      menuItem("Reporte y recomendaciones", tabName = "conclusiones", icon = icon("clipboard-check"))
    )
  ),
  
  # Cuerpo principal del dashboard con las pestañas y su contenido
  dashboardBody(
    tabItems(
      # Pestaña "Contacto" con información general y datos del autor
      tabItem(tabName = "contacto",
              h3("Información de contacto"),
              p("Este dashboard fue desarrollado como parte de un proyecto académico."),
              p(HTML("<b>Autor:</b> Tu Nombre")),
              p(HTML("<b>Correo:</b> <a href='mailto:tucorreo@ejemplo.com'>tucorreo@ejemplo.com</a>")),
              p(HTML("<b>Institución:</b> Nombre de tu universidad o centro de estudios")),
              br(),
              p("Última actualización: ", Sys.Date())
      ),
      
      # Pestaña "Resumen" con indicadores clave y gráficos principales
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("box_tgf"),    # Caja de valor para Tasa Global de Fecundidad
                valueBoxOutput("box_edad"),   # Caja de valor para Edad media maternidad
                valueBoxOutput("box_ultimo")  # Caja de valor con otro indicador relevante
              ),
              fluidRow(
                box(title = "Evolución TGF", width = 6, plotlyOutput("plot_tgf")),   # Gráfico evolución TGF
                box(title = "Edad media maternidad", width = 6, plotlyOutput("plot_edad")) # Gráfico edad media
              )
      ),
      
      # Pestaña "Fecundidad por edad" con distribución de nacimientos por grupos de edad
      tabItem(tabName = "fecundidad",
              h3("Distribución de nacimientos por edad de la madre"),
              fluidRow(
                box(title = "Grupos principales", width = 6, plotlyOutput("plot_nacimientos_edades")),
                box(title = "Grupos menos frecuentes", width = 6, plotlyOutput("plot_nacimientos_menores"))
              )
      ),
      
      # Pestaña "Factores sociodemográficos" con análisis de ingresos y estructura poblacional
      tabItem(tabName = "factores",
              h3("Factores sociodemográficos asociados"),
              fluidRow(
                box(title = "Ingreso disponible per cápita por quintil", width = 6, plotlyOutput("plot_epf_ingresos")),
                box(title = "Pirámide poblacional por sexo y edad", width = 6, plotlyOutput("plot_epf_poblacion"))
              ),
              fluidRow(
                box(title = "Tabla: Estructura de ingresos", width = 6, DTOutput("tabla_epf_ingresos")),
                box(title = "Tabla: Composición por edad y sexo", width = 6, DTOutput("tabla_epf_poblacion"))
              )
      ),
      
      # Pestaña "Proyecciones" con gráficos interactivos y selector de indicador
      tabItem(tabName = "proyecciones",
              h3("Proyecciones de fecundidad y contexto demográfico (1992–2050)"),
              selectInput("indicador_proy", "Selecciona un indicador:",
                          choices = indicadores_relevantes,
                          selected = "Tasa global de fecundidad"),
              plotlyOutput("plot_proyecciones")
      ),
      
      # Pestaña "Ingreso regional" con mapa interactivo para selección por año
      tabItem(tabName = "ingreso",
              h3("Ingreso medio nominal por región (2010–2021)"),
              selectInput("anio_ingreso", "Selecciona un año:",
                          choices = sort(unique(ingreso_df$Año)),
                          selected = max(ingreso_df$Año)),
              leafletOutput("mapa_ingreso_region", height = 600)
      ),
      
      # Pestaña "Árbol de decisión" con visualización y análisis del modelo predictivo
      tabItem(tabName = "arbol",
              h3("Árbol de decisión: Clasificación por número de hijos"),
              p("Esta sección presenta un modelo predictivo basado en árbol de decisión, entrenado con datos de la IX Encuesta de Presupuestos Familiares (EPF), que permite identificar los principales factores sociodemográficos asociados al número de hijos por hogar."),
              br(),
              fluidRow(
                box(title = "Árbol de decisión podado", width = 6, imageOutput("imagen_arbol", height = "400px")),
                box(title = "Importancia de variables", width = 6, plotlyOutput("importancia_variables"))
              ),
              br(),
              fluidRow(
                box(
                  title = "Análisis de multicolinealidad (VIF)", width = 6, status = "info", solidHeader = TRUE,
                  uiOutput("texto_vif_html")),
                box(
                  title = "Desempeño del árbol de decisión", width = 6, status = "success", solidHeader = TRUE,
                  uiOutput("resumen_confusion"))
              )
      ),
      
      # Pestaña "Conclusiones" con descarga de reportes y recomendaciones estratégicas
      tabItem(tabName = "conclusiones",
              h2("Centro de reportes y recomendaciones"),
              p("Desde aquí puedes descargar un informe completo con los principales hallazgos del análisis de fecundidad y maternidad en Chile. Este reporte está diseñado para entregar evidencia clara y procesable para la toma de decisiones."),
              br(),
              fluidRow(
                box(
                  title = "📄 Descargar Reporte en PDF",
                  width = 6,
                  status = "danger",
                  solidHeader = TRUE,
                  icon = icon("file-pdf"),
                  p("Versión imprimible del reporte con estructura ejecutiva y gráficos embebidos."),
                  downloadButton("descargar_pdf", "Descargar PDF", class = "btn-danger btn-block")
                ),
                box(
                  title = "🌐 Descargar Reporte en HTML",
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  icon = icon("file-code"),
                  p("Versión interactiva navegable del reporte, ideal para exploración en pantalla."),
                  downloadButton("descargar_html", "Descargar HTML", class = "btn-success btn-block")
                ),
                box(
                  title = "📘 Informe ejecutivo",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  p("Este informe contiene el objetivo estrategico, la metodología empleada. Precaucion descargar archivo *preambulo.tex* para una correcta ejecución."),
                  downloadButton("descargar_resumen", "Descargar resumen", class = "btn-info btn-block")
                )
              ),
              br(),
              box(
                title = "💡 Recomendaciones estratégicas para el CEO",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                htmlOutput("recomendaciones_ceo")
              )
      )
    )
  )
)
# SERVER del dashboard
server <- function(input, output, session) {
  
  output$box_tgf <- renderValueBox({
    tgf <- tail(na.omit(datos$fecundidad$`Tasa Global de Fecundidad (TGF)`), 1)
    valueBox(round(tgf, 2), subtitle = "Último TGF registrado", icon = icon("venus-mars"), color = "purple")
  })
  
  output$box_edad <- renderValueBox({
    edad <- tail(na.omit(datos$fecundidad$`Edad media de las madres`), 1)
    valueBox(round(edad, 1), subtitle = "Edad media maternidad (último año)", icon = icon("female"), color = "blue")
  })
  
  output$box_ultimo <- renderValueBox({
    ultimo <- max(datos$fecundidad$Año, na.rm = TRUE)
    valueBox(ultimo, subtitle = "Último año disponible", icon = icon("calendar"), color = "green")
  })
  
  output$plot_tgf <- renderPlotly({
    df <- datos$fecundidad
    p <- ggplot(df, aes(x = Año, y = `Tasa Global de Fecundidad (TGF)`)) +
      geom_line(color = "#9b59b6", size = 1.2) +
      labs(x = "Año", y = "TGF") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_edad <- renderPlotly({
    df <- datos$fecundidad
    p <- ggplot(df, aes(x = Año, y = `Edad media de las madres`)) +
      geom_line(color = "#3498db", size = 1.2) +
      labs(x = "Año", y = "Edad promedio") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_nacimientos_edades <- renderPlotly({
    p <- ggplot(df_principales, aes(x = Año, y = Nacimientos, color = GrupoEdad)) +
      geom_line(size = 1) +
      labs(title = "", x = "Año", y = "Nacimientos") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_nacimientos_menores <- renderPlotly({
    p <- ggplot(df_menores, aes(x = Año, y = Nacimientos, color = GrupoEdad)) +
      geom_line(size = 1.2) +
      labs(title = "",
           x = "Año",
           y = "Nacimientos") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  # Gráfico de ingresos
  output$plot_epf_ingresos <- renderPlotly({
    df <- epf_ingresos_df
    p <- ggplot(df, aes(x = GRUPO_QUINTIL_DE_HOGARES, 
                        y = INGRESO_DISPONIBLE_PROMEDIO_MENSUAL_PER_CÁPITA,
                        fill = GRUPO_QUINTIL_DE_HOGARES)) +
      geom_bar(stat = "identity") +
      labs(x = "Quintil", y = "Ingreso ($)") +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Tabla de ingresos
  output$tabla_epf_ingresos <- renderDT({
    datatable(tabla_epf_ingresos_df, 
              options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Gráfico de estructura poblacional
  output$plot_epf_poblacion <- renderPlotly({
    p <- ggplot(epf_piramide, aes(x = Poblacion, y = Grupo_Etario, fill = Sexo, text = paste("Grupo etario:", Grupo_Etario, "<br>Población:", abs(Poblacion), "mil"))) +
      geom_bar(stat = "identity") +
      scale_x_continuous(labels = abs, name = "Población (en miles)") +
      scale_fill_manual(values = c("HOMBRES" = "darkgreen", "MUJERES" = "orchid")) +
      labs(title = "", y = "Grupo etario") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Tabla de población
  output$tabla_epf_poblacion <- renderDT({
    datatable(tabla_epf_poblacion_df,
              options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$imagen_arbol <- renderPlot({
    rpart.plot::rpart.plot(arbol_podado_optimo,
                           type = 2,
                           extra = 104,
                           fallen.leaves = FALSE,
                           box.palette = "GnBu",
                           shadow.col = "gray",
                           main = "")
  })
  
  output$importancia_variables <- renderPlotly({
    p <- ggplot(importance_df_bonito, aes(x = reorder(Variable, Importance), y = Importance,
                                          text = paste("Variable:", Variable, "<br>Importancia:", round(Importance, 2)))) +
      geom_bar(stat = "identity", fill = "#3688C4") +
      coord_flip() +
      labs(title = "", x = "Variable", y = "Importancia") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$texto_vif_html <- renderUI({
    valores <- round(vif_values, 2)
    
    # Aplica renombramiento elegante
    nombres_mostrados <- nombres_bonitos[names(valores)]
    
    # Construir tabla en HTML
    tabla <- paste0(
      "<table class='table table-bordered' style='width:50%;'>
      <thead><tr><th>Variable</th><th>VIF</th></tr></thead><tbody>",
      paste0(
        "<tr><td>", nombres_mostrados, "</td><td><b>", valores, "</b></td></tr>",
        collapse = ""
      ),
      "</tbody></table>"
    )
    
    # Texto explicativo
    explicacion <- HTML(paste0(
      "<p style='font-size:14px'><i class='fa fa-check-circle text-success'></i> Todos los VIF están por debajo de 5, lo cual indica <b>baja colinealidad</b> entre los predictores.<br>"
    ))
    
    # Renderizar
    tagList(
      HTML(tabla),
      explicacion
    )
  })
  
  output$resumen_confusion <- renderUI({
    acc <- round(conf_mat$overall["Accuracy"], 3)
    kappa <- round(conf_mat$overall["Kappa"], 3)
    sen <- round(conf_mat$byClass[, "Sensitivity"], 3)
    esp <- round(conf_mat$byClass[, "Specificity"], 3)
    clases <- rownames(conf_mat$byClass)
    
    HTML(paste0(
      "<p><i class='fa fa-check-circle text-success'></i> <b>Accuracy global:</b> ", acc, "<br>",
      "<i class='fa fa-project-diagram text-primary'></i> <b>Kappa:</b> ", kappa, "</p>",
      "<table class='table table-striped table-bordered' style='width:70%; font-size:13px'>
      <thead><tr>
        <th>Clase</th><th>Sensibilidad</th><th>Especificidad</th>
      </tr></thead><tbody>",
      paste0("<tr><td>", clases, "</td><td>", sen, "</td><td>", esp, "</td></tr>", collapse = ""),
      "</tbody></table>"
    ))
  })
  
  output$mapa_ingreso_region <- renderLeaflet({
    ingreso_año <- ingreso_df |> filter(Año == input$anio_ingreso)
    
    mapa_data <- regiones_chile |>
      left_join(ingreso_año, by = c("Region" = "Región"))
    
    pal <- colorNumeric("Blues", domain = mapa_data$`Ingreso medio nominal ($)`)
    
    leaflet(mapa_data) |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor = ~pal(`Ingreso medio nominal ($)`),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(Region, ": $", format(`Ingreso medio nominal ($)`, big.mark = ".")),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) |>
      addLegend(pal = pal, values = ~`Ingreso medio nominal ($)`,
                title = "Ingreso promedio ($)", position = "bottomright")
  })
  
 
  
  
  # Descargar reporte en formato HTML
  output$descargar_html <- downloadHandler(
    filename = function() {
      paste0("reporte-fecundidad-", Sys.Date(), ".html")
    },
    content = function(file) {
      # Define nombre temporal de salida dentro de carpeta "reportes"
      output_ht_temp <- file.path("reportes", paste0("Informe_", Sys.Date(), ".html"))
      # Muestra una notificación al usuario
      showModal(modalDialog("Generando reporte HTML, por favor espere...", footer=NULL))
      # Crea la lista de parámetros para pasar al Rmd
      params_list <- list(
        indicador_seleccionado = input$indicador_proy,
        anio_seleccionado = input$anio_ingreso,
        datos_reporte = list(
          proy_df = proy_df,
          df_principales = df_principales,
          df_menores = df_menores,  
          epf_piramide = epf_piramide,
          epf_ingresos_df = epf_ingresos_df,                     # opcional si usas ese gráfico
          tabla_epf_ingresos_df = tabla_epf_ingresos_df,         # opcional
          tabla_epf_poblacion_df = tabla_epf_poblacion_df,       # opcional
          regiones_chile = regiones_chile,                       # opcional si haces mapa
          ingreso_df = ingreso_df, 
          fecundidad = datos$fecundidad,
          importancia_vars_df = importance_df,                 # <- NUEVO
          path_arbol_png = ruta_arbol_png,                     # <- NUEVO
          confusion_matrix = as.data.frame(conf_mat$table),    # <- NUEVO para tabla
          conf_stats = as.list(conf_mat$overall),              # <- Opcional: accuracy, Kappa, etc.
          by_class_stats = as.data.frame(conf_mat$byClass),    # <- Opcional: por clase
          importance_df_bonito = importance_df_bonito
        )
      )
      
      # Renderiza el archivo Rmd
      rmarkdown::render(
        input = "C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/reporte.Rmd",         # Ruta al Rmd
        output_format = "html_document", # Formato de salida
        output_file = output_ht_temp,    # Ruta del archivo final
        params = params_list,          # Lista de parámetros
        envir = new.env(parent = globalenv()) # Entorno de ejecución
      )
      
      # Copia el archivo a donde shiny lo espera entregar
      file.copy(output_ht_temp, file, overwrite = TRUE)
      
      # Cierra la notificación
      removeModal()
    }
  )
  
  # Descargar reporte en formato PDF
  output$descargar_pdf <- downloadHandler(
    filename = function() {
      paste0("reporte-fecundidad-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      showModal(modalDialog("Generando reporte PDF, esto puede tardar...", footer = NULL))
      
      # Crea carpeta para gráficos persistentes si no existe
      if (!dir.exists("figs_pdf")) dir.create("figs_pdf")
      
      # Crea carpeta para salida de reportes si no existe
      if (!dir.exists("reportes")) dir.create("reportes")
      
      # Define nombre temporal de salida dentro de carpeta "reportes"
      output_pdf_temp <- file.path("reportes", paste0("tmp_reporte_", Sys.Date(), ".pdf"))
      
      # Lista de parámetros que pasas al Rmd
      params_list <- list(
        indicador_seleccionado = input$indicador_proy,
        anio_seleccionado = input$anio_ingreso,
        datos_reporte = list(
          proy_df = proy_df,
          df_principales = df_principales,
          df_menores = df_menores,
          epf_piramide = epf_piramide,
          epf_ingresos_df = epf_ingresos_df,
          tabla_epf_ingresos_df = tabla_epf_ingresos_df,
          tabla_epf_poblacion_df = tabla_epf_poblacion_df,
          regiones_chile = regiones_chile,
          ingreso_df = ingreso_df,
          fecundidad = datos$fecundidad,
          importancia_vars_df = importance_df,                 # <- NUEVO
          path_arbol_png = ruta_arbol_png,                     # <- NUEVO
          confusion_matrix = as.data.frame(conf_mat$table),    # <- NUEVO para tabla
          conf_stats = as.list(conf_mat$overall),              # <- Opcional: accuracy, Kappa, etc.
          by_class_stats = as.data.frame(conf_mat$byClass),    # <- Opcional: por clase
          importance_df_bonito = importance_df_bonito
        )
      )
      
      # Renderiza el Rmd al archivo temporal
      rmarkdown::render(
        input = "C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/reporte.Rmd",
        output_format = "pdf_document",
        output_file = output_pdf_temp,
        params = params_list,
        envir = new.env(parent = globalenv())
      )
      
      # Copia el archivo a donde shiny lo espera entregar
      file.copy(output_pdf_temp, file, overwrite = TRUE)
      
      removeModal()
    })
  output$recomendaciones_ceo <- renderUI({
    HTML(paste0(
      "<ul>",
      "<li><strong>Focalizar políticas familiares</strong> en grupos de hogares pequeños con alto ingreso disponible, ya que tienden a presentar menor número de hijos.</li>",
      "<li><strong>Impulsar campañas educativas</strong> y de planificación reproductiva especialmente en mujeres de 25 a 34 años, rango etario con mayor participación en nacimientos.</li>",
      "<li><strong>Atender a la transición demográfica</strong> observada en el envejecimiento de la población y el retraso en la edad promedio de la maternidad.</li>",
      "<li><strong>Utilizar el árbol de decisión</strong> como herramienta para segmentar hogares y anticipar tendencias reproductivas con base en variables clave como escolaridad, edad y tamaño del hogar.</li>",
      "</ul>"
    ))
  })
  output$descargar_resumen <- downloadHandler(
    filename = function() {
      paste0("Resumen_Ejecutivo_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Define nombre temporal de salida dentro de carpeta "reportes"
      output_rep_temp <- file.path("reportes", paste0("Informe_", Sys.Date(), ".pdf"))
      # Ruta al Rmd estático
      rmarkdown::render(
        input = "C:/Users/cesar/OneDrive/Escritorio/TrabajoFecundidadyHogares/Dashboard/informeCEO.Rmd",
        output_format = "pdf_document",
        output_file = output_rep_temp,
        envir = new.env(parent = globalenv())
      )
      # Copia el archivo a donde shiny lo espera entregar
      file.copy(output_rep_temp, file, overwrite = TRUE)
      
      removeModal()
    })
}

shinyApp(ui, server)

