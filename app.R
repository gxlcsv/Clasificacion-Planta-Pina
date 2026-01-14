# ==============================================================================
# 1. PREPARACIÓN Y LIBRERÍAS
# ==============================================================================
pkg <- c("shiny", "leaflet", "sf", "plotly", "dplyr", "bslib", "DT", "writexl", "rsconnect")
new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new_pkg)) install.packages(new_pkg)

library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(bslib)
library(DT)
library(writexl)

if (interactive()) {
  rsconnect::writeManifest(appPrimaryDoc = "app.R")
}

# ==============================================================================
# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- page_navbar(
  title = "Gestión Agrícola: Plantas",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("Dashboard Operativo",
            layout_sidebar(
              sidebar = sidebar(
                width = 350,
                title = "Panel de Control",
                selectInput("select_gpkg", "Seleccionar Archivo GPKG:", 
                            choices = list.files("data/", pattern = "\\.gpkg$"),
                            width = "100%"),
                uiOutput("ui_selector_bloque"),
                hr(),
                actionButton("zoom_btn", "Re-centrar Mapa", 
                             icon = icon("location-crosshairs"), 
                             class = "btn-primary w-100")
              ),
              
              layout_columns(
                value_box(
                  title = "Plantas Vivas",
                  value = textOutput("txt_vivas"),
                  showcase = icon("leaf"),
                  theme = "success"
                ),
                value_box(
                  title = "Plantas Muertas",
                  value = textOutput("txt_muertas"),
                  showcase = icon("skull"),
                  theme = "danger"
                ),
                value_box(
                  title = "Mortalidad Promedio",
                  value = textOutput("txt_mort"),
                  showcase = icon("percent"),
                  theme = "warning"
                )
              ),
              
              layout_columns(
                col_widths = c(12, 7, 5),
                row_heights = c("500px", "400px"),
                card(full_screen = TRUE, card_header("Distribución Espacial"), leafletOutput("mapa_plantas")),
                card(card_header("Estado de Plantas"), plotlyOutput("grafica_barras")),
                card(card_header("KPI Mortalidad"), plotlyOutput("grafica_gauge"))
              )
            )
  ),
  
  nav_panel("Datos Detallados",
            card(
              card_header(
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                    "Resumen de Inventario",
                    div(
                      downloadButton("download_csv", "CSV", class = "btn-sm btn-outline-secondary"),
                      downloadButton("download_excel", "Excel", class = "btn-sm btn-outline-success")
                    )
                )
              ),
              DTOutput("tabla_maestra")
            )
  ),
  
  nav_spacer(),
  nav_item(tags$img(src = "logo.jpg", height = "60px", style = "margin-top: -5px;"))
)

# ==============================================================================
# 3. LÓGICA DEL SERVIDOR (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  # 3.1. Lectura de datos
  datos <- reactive({
    req(input$select_gpkg)
    ruta <- file.path("data", input$select_gpkg)
    if (!file.exists(ruta)) return(NULL)
    df <- st_read(ruta, quiet = TRUE) %>% st_transform(4326)
    names(df) <- tolower(names(df))
    return(df)
  })
  
  datos_filtrados <- reactive({
    req(datos(), input$bloque_filtro)
    df <- datos()
    if (input$bloque_filtro != "Todos") df <- df %>% filter(name == input$bloque_filtro)
    return(df)
  })
  
  output$ui_selector_bloque <- renderUI({
    req(datos())
    selectInput("bloque_filtro", "Filtrar por Bloque:", choices = c("Todos", sort(unique(datos()$name))))
  })
  
  # 3.2. Formateo de tabla
  tabla_final_df <- reactive({
    req(datos_filtrados())
    datos_filtrados() %>%
      st_drop_geometry() %>%
      select(
        `Bloque` = name,
        `Área Total (ha)` = area,
        `Área Efectiva (ha)` = area_efectiva,
        `Cantidad Total` = numpoints,
        `Densidad (Plantas/ha)` = densidad,
        `Plantas Vivas` = viva,
        `Plantas Muertas` = muerta,
        `Plantas en Alerta` = alerta,
        `Porcentaje de Mortalidad` = perc_mort
      )
  })
  
  # 3.3. Indicadores y Gráficos
  output$txt_vivas <- renderText({ format(sum(datos_filtrados()$viva, na.rm=T), big.mark=",") })
  output$txt_muertas <- renderText({ format(sum(datos_filtrados()$muerta, na.rm=T), big.mark=",") })
  output$txt_mort <- renderText({ paste0(round(mean(datos_filtrados()$perc_mort, na.rm=T), 2), "%") })
  
  output$mapa_plantas <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% setView(lng = -83.5, lat = 10.2, zoom = 10)
  })
  
  observe({
    df <- datos_filtrados()
    req(nrow(df) > 0)
    pal <- colorNumeric(palette = "YlOrRd", domain = datos()$perc_mort)
    
    leafletProxy("mapa_plantas", data = df) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~pal(perc_mort), weight = 1.5, color = "white", fillOpacity = 0.7,
                  popup = ~paste0(
                    "<b>Bloque: </b>", name, "<br><hr>",
                    "<b>Total Plantas: </b>", numpoints, "<br>",
                    "<b>Vivas: </b>", viva, "<br>",
                    "<b>Muertas: </b>", muerta, "<br>",
                    "<b>Alertas: </b>", alerta, "<br>",
                    "<b>Mortalidad: </b>", round(perc_mort, 2), "%"
                  )) %>%
      flyToBounds(st_bbox(df)[["xmin"]], st_bbox(df)[["ymin"]], st_bbox(df)[["xmax"]], st_bbox(df)[["ymax"]])
  })
  
  # CORRECCIÓN: Botón Re-centrar ahora usa el dataset completo (datos())
  observeEvent(input$zoom_btn, {
    req(datos())
    bbox <- st_bbox(datos())
    leafletProxy("mapa_plantas") %>% 
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })
  
  # GRÁFICO CON BARRA DE ALERTAS RESTAURADA
  output$grafica_barras <- renderPlotly({
    req(tabla_final_df())
    plot_ly(tabla_final_df(), x = ~Bloque, y = ~`Plantas Vivas`, type = 'bar', name = 'Vivas', marker = list(color = '#2ecc71')) %>%
      add_trace(y = ~`Plantas Muertas`, name = 'Muertas', marker = list(color = '#e74c3c')) %>%
      add_trace(y = ~`Plantas en Alerta`, name = 'Alertas', marker = list(color = '#f1c40f')) %>%
      layout(barmode = 'group', xaxis = list(title = "Bloque"), yaxis = list(title = "Cantidad"))
  })
  
  # GAUGE CON COLORES (Arco rojo a partir de 20%)
  output$grafica_gauge <- renderPlotly({
    req(tabla_final_df())
    val <- mean(tabla_final_df()$`Porcentaje de Mortalidad`, na.rm = TRUE)
    plot_ly(type = "indicator", mode = "gauge+number", value = val,
            gauge = list(
              axis = list(range = list(NULL, 100), ticksuffix = "%"),
              bar = list(color = "#2c3e50"),
              steps = list(
                list(range = c(0, 10), color = "#d4edda"),
                list(range = c(10, 20), color = "#fff3cd"),
                list(range = c(20, 100), color = "#f8d7da") # Arco rojo (alerta)
              ),
              threshold = list(
                line = list(color = "red", width = 4),
                thickness = 0.75,
                value = 20
              )
            ))
  })
  
  # 3.4. Tabla y Descargas
  output$tabla_maestra <- renderDT({
    datatable(tabla_final_df(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = c(2, 3, 5, 9), digits = 2)
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste0("Inventario_", input$select_gpkg, "_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(tabla_final_df(), file, row.names = FALSE) }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() { paste0("Inventario_", input$select_gpkg, "_", Sys.Date(), ".xlsx") },
    content = function(file) { write_xlsx(tabla_final_df(), file) }
  )
}


shinyApp(ui, server)
