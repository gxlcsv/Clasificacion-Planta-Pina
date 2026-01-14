# ==============================================================================
# 1. PREPARACIÓN Y LIBRERÍAS
# ==============================================================================
if (!require("rsconnect")) install.packages("rsconnect")

library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(bslib)
library(DT)

# Generar manifiesto para Posit Connect (solo en modo local interactivo)
if (interactive()) {
  rsconnect::writeManifest(appPrimaryDoc = "app.R")
}

# ==============================================================================
# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- page_navbar(
  title = "Gestión Agrícola: Plantas",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Espaciador y Logo en la parte superior derecha
  nav_spacer(),
  nav_item(
    tags$img(
      src = "logo.jpg", 
      height = "60px", 
      style = "margin-right: 15px; margin-top: 5px;"
    )
  ),
  
  nav_panel("Dashboard Operativo",
            layout_sidebar(
              sidebar = sidebar(
                title = "Panel de Control",
                # Selector de archivos dinámico desde la carpeta data/
                selectInput("select_gpkg", "Seleccionar Archivo GPKG:", 
                            choices = list.files("data/", pattern = "\\.gpkg$")),
                # Este selector se genera en el server
                uiOutput("ui_selector_bloque"),
                hr(),
                actionButton("zoom_btn", "Re-centrar Mapa", 
                             icon = icon("location-crosshairs"), 
                             class = "btn-primary w-100")
              ),
              
              layout_columns(
                col_widths = c(12, 7, 5),
                row_heights = c("450px", "400px"),
                
                card(
                  full_screen = TRUE,
                  card_header("Distribución Espacial (Satélite)"),
                  leafletOutput("mapa_plantas")
                ),
                
                card(
                  card_header("Estado de Plantas: Conteo por Bloque"),
                  plotlyOutput("grafica_barras")
                ),
                
                card(
                  card_header("KPI: Porcentaje de Mortalidad"),
                  plotlyOutput("grafica_gauge")
                )
              )
            )
  ),
  
  nav_panel("Datos Detallados",
            card(
              card_header("Resumen de Inventario (Tabla de Atributos)"),
              DTOutput("tabla_maestra")
            )
  )
)

# ==============================================================================
# 3. LÓGICA DEL SERVIDOR (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  # 3.1. Lectura reactiva de datos
  datos <- reactive({
    req(input$select_gpkg)
    ruta <- file.path("data", input$select_gpkg)
    
    # Validar que el archivo existe antes de leer
    if (!file.exists(ruta)) return(NULL)
    
    df <- st_read(ruta, quiet = TRUE) %>% 
      st_transform(4326)
    
    # Normalizar nombres a minúsculas para evitar conflictos
    names(df) <- tolower(names(df))
    return(df)
  })
  
  # 3.2. Generación dinámica del selector de bloques
  output$ui_selector_bloque <- renderUI({
    df <- datos()
    req(df)
    choices <- c("Todos", sort(unique(df$name)))
    selectInput("bloque_filtro", "Filtrar por Bloque:", choices = choices, selected = "Todos")
  })
  
  # 3.3. Mapa Base
  output$mapa_plantas <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -83.5, lat = 10.2, zoom = 12)
  })
  
  # 3.4. Actualización del Mapa (con validación de filtro)
  observe({
    req(datos(), input$bloque_filtro)
    df <- datos()
    
    if (input$bloque_filtro != "Todos") {
      df <- df %>% filter(name == input$bloque_filtro)
    }
    
    req(nrow(df) > 0) # Asegurar que hay datos tras el filtro
    
    bbox <- st_bbox(df)
    pal <- colorNumeric(palette = "YlOrRd", domain = datos()$perc_mort)
    
    leafletProxy("mapa_plantas", data = df) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(perc_mort),
        weight = 1.5, color = "white", fillOpacity = 0.7,
        popup = ~paste0(
          "<b>Bloque: </b>", name, "<br><hr>",
          "<b>Plantas: </b>", numpoints, "<br>",
          "<b>Mortalidad: </b>", round(perc_mort, 2), "%"
        ),
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
      ) %>%
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })
  
  # 3.5. Botón de Re-centrar
  observeEvent(input$zoom_btn, {
    req(datos())
    bbox <- st_bbox(datos())
    leafletProxy("mapa_plantas") %>% 
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })
  
  # 3.6. Gráfica de Barras (Corregida)
  output$grafica_barras <- renderPlotly({
    req(datos(), input$bloque_filtro)
    df_plot <- as.data.frame(datos())
    
    if (input$bloque_filtro != "Todos") {
      df_plot <- df_plot %>% filter(name == input$bloque_filtro)
    }
    
    plot_ly(df_plot, x = ~name, y = ~viva, type = 'bar', name = 'Vivas', marker = list(color = '#2ecc71')) %>%
      add_trace(y = ~muerta, name = 'Muertas', marker = list(color = '#e74c3c')) %>%
      add_trace(y = ~alerta, name = 'Alertas', marker = list(color = '#f1c40f')) %>%
      layout(yaxis = list(title = "Cantidad"), barmode = 'group', xaxis = list(title = "Bloque"))
  })
  
  # 3.7. Gauge de Mortalidad (Corregida)
  output$grafica_gauge <- renderPlotly({
    req(datos(), input$bloque_filtro)
    df <- as.data.frame(datos())
    
    if (input$bloque_filtro == "Todos") {
      val <- mean(df$perc_mort, na.rm = TRUE)
    } else {
      val <- df %>% filter(name == input$bloque_filtro) %>% pull(perc_mort)
    }
    
    # Manejo de casos vacíos
    if(length(val) == 0) val <- 0
    
    plot_ly(
      type = "indicator", mode = "gauge+number", value = val,
      gauge = list(
        axis = list(range = list(NULL, 100), ticksuffix = "%"),
        steps = list(
          list(range = c(0, 15), color = "#d4edda"),
          list(range = c(15, 30), color = "#fff3cd"),
          list(range = c(30, 100), color = "#f8d7da")
        ),
        bar = list(color = "#2c3e50")
      )
    )
  })
  
  # 3.8. Tabla Maestra
  output$tabla_maestra <- renderDT({
    req(datos())
    df_tabla <- st_drop_geometry(datos())
    datatable(df_tabla, 
              options = list(pageLength = 10, scrollX = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')), 
              rownames = FALSE)
  })
}

# Lanzar Aplicación
shinyApp(ui, server)