
# MÓDULO 8 - RETO 2
# DASHBOARD DE CIENCIA DE DATOS
# Análisis demográfico de países de Latinoamérica


# 1. CARGAR PAQUETES
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 2. CARGAR BASES DE DATOS DEPURADAS

mortalidad <- read_excel(
  "../Base_Datos_Depurada/mortalidad_LATAM.xlsx"
)

fertilidad <- read_excel(
  "../Base_Datos_Depurada/fertilidad_LATAM.xlsx"
)

poblacion <- read_excel(
  "../Base_Datos_Depurada/poblacion_LATAM.xlsx"
)

# 3. TRANSFORMAR LAS BASES DE FORMATO ANCHO A LARGO

mortalidad_largo <- mortalidad %>%
  pivot_longer(
    cols = 3:ncol(mortalidad),
    names_to = "anio",
    values_to = "mortalidad"
  ) %>%
  mutate(anio = as.numeric(anio))


fertilidad_largo <- fertilidad %>%
  pivot_longer(
    cols = 3:ncol(fertilidad),
    names_to = "anio",
    values_to = "fertilidad"
  ) %>%
  mutate(anio = as.numeric(anio))


poblacion_largo <- poblacion %>%
  pivot_longer(
    cols = 3:ncol(poblacion),
    names_to = "anio",
    values_to = "poblacion"
  ) %>%
  mutate(anio = as.numeric(anio))

# 4. UNIR LAS TRES BASES

datos_dashboard <- mortalidad_largo %>%
  select(geo, name, anio, mortalidad) %>%
  left_join(
    fertilidad_largo %>%
      select(geo, anio, fertilidad),
    by = c("geo", "anio")
  ) %>%
  left_join(
    poblacion_largo %>%
      select(geo, anio, poblacion),
    by = c("geo", "anio")
  )


# 5. INTERFAZ DEL DASHBOARD

ui <- fluidPage(
  
  titlePanel("Dashboard demográfico de Latinoamérica"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "pais",
        "Selecciona un país:",
        choices = paises_latam,
        selected = "Mexico"
      ),
      
      selectInput(
        "indicador",
        "Selecciona un indicador:",
        choices = c(
          "Población" = "poblacion",
          "Fertilidad" = "fertilidad",
          "Mortalidad infantil" = "mortalidad"
        ),
        selected = "poblacion"
      ),
      
      sliderInput(
        "periodo",
        "Periodo:",
        min = 1950,
        max = 2025,
        value = c(1950, 2025),
        step = 1,
        sep = ""
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Evolución",
          plotOutput("grafico_evolucion")
        ),
        
        tabPanel(
          "Comparación",
          plotOutput("grafico_comparacion")
        ),
        
        tabPanel(
          "Datos",
          tableOutput("tabla_datos")
        )
      )
    )
  )
)


# 6. LÓGICA DEL DASHBOARD


server <- function(input, output) {
  
  
  # ----------------------------------------------------------
  # 6.1 FILTRAR DATOS DEL PAÍS SELECCIONADO
  # ----------------------------------------------------------
  
  datos_pais <- reactive({
    
    datos_dashboard %>%
      filter(
        name == input$pais,
        anio >= input$periodo[1],
        anio <= input$periodo[2]
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 6.2 GRÁFICO DE EVOLUCIÓN
  # ----------------------------------------------------------
  
  output$grafico_evolucion <- renderPlot({
    
    datos <- datos_pais()
    
    ggplot(
      datos,
      aes(
        x = anio,
        y = .data[[input$indicador]]
      )
    ) +
      
      geom_line() +
      
      labs(
        title = paste(
          "Evolución de",
          input$indicador,
          "en",
          input$pais
        ),
        x = "Año",
        y = input$indicador
      ) +
      
      theme_minimal()
    
  })
  
  
  # ----------------------------------------------------------
  # 6.3 COMPARACIÓN ENTRE PAÍSES
  # ----------------------------------------------------------
  
  output$grafico_comparacion <- renderPlot({
    
    datos <- datos_dashboard %>%
      filter(
        name %in% input$paises_comparar,
        anio >= input$periodo[1],
        anio <= input$periodo[2]
      )
    
    ggplot(
      datos,
      aes(
        x = anio,
        y = .data[[input$indicador]],
        group = name
      )
    ) +
      
      geom_line(aes(linetype = name)) +
      
      labs(
        title = paste(
          "Comparación de",
          input$indicador
        ),
        x = "Año",
        y = input$indicador,
        linetype = "País"
      ) +
      
      theme_minimal()
    
  })
  
  # 6.4 TABLA DE DATOS

  output$tabla_datos <- renderTable({
    
    datos_pais() %>%
      select(
        name,
        anio,
        poblacion,
        fertilidad,
        mortalidad
      )
    
  })
  
}


# 7. EJECUTAR DASHBOARD


shinyApp(
  ui = ui,
  server = server
)