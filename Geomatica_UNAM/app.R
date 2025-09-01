library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)

t1 = read_rds("t1.rds")

ui <- page_sidebar(
  title = div(
    tags$a(
      href = "https://www.geografia.unam.mx/geoigg/",
      target = "_blank",
      img(src = "ig_logo.svg", height = "30px"),
      class = "logo-link"
    ),
    tags$strong("Espacios de Excepción - Análisis Territorial"),
    style = "display: flex; align-items: center; gap: 10px;"
  ),
  tags$head(
    tags$style(HTML("
      .logo-link {
        text-decoration: none;
        transition: opacity 0.3s;
      }
      .logo-link:hover {
        opacity: 0.8;
      }
    "))
  ),
  useShinyjs(),
  sidebar = sidebar(
    width = 300,
    # Navegación secuencial tipo storytelling
    radioButtons("seccion", 
                 "Navegación:",
                 choices = list(
                   "1. Introducción Teórica" = "intro",
                   "2. El Concepto de Agamben" = "concepto", 
                   "3. Datos y Evidencia" = "datos",
                   "4. Análisis Territorial" = "analisis",
                   "5. Mecanismos de Perpetuación" = "mecanismos",
                   "6. Metodología y Contacto" = "contacto"
                 ),
                 selected = "intro"),
    
    # Progreso visual
    br(),
    div(
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
      h6("Progreso del análisis", class = "text-muted"),
      progressBar(
        id = "progreso",
        value = 16.67, # Se actualizará según la sección
        status = "info",
        size = "xs"
      )
    )
  ),
  
  # Contenido principal que cambia según la sección
  uiOutput("contenido_principal")
)

server <- function(input, output, session) {
  
  # Actualizar barra de progreso según la sección
  observe({
    progreso_valores <- list(
      "intro" = 16.67,
      "concepto" = 33.33,
      "datos" = 50,
      "analisis" = 66.67,
      "mecanismos" = 83.33,
      "contacto" = 100
    )
    
    updateProgressBar(
      session = session,
      id = "progreso", 
      value = progreso_valores[[input$seccion]]
    )
    runjs(paste0("document.getElementById('barra_progreso').style.width = '", progreso_valores[[input$seccion]], "';"))
  })
  
  observeEvent(input$next_intro, {
    updateRadioButtons(session, "seccion", selected = "concepto")
  })
  
  observeEvent(input$next_concepto, {
    updateRadioButtons(session, "seccion", selected = "datos")
  })
  
  observeEvent(input$back_concepto, {
    updateRadioButtons(session, "seccion", selected = "intro")
  })
  
  observeEvent(input$next_datos, {
    updateRadioButtons(session, "seccion", selected = "analisis")
  })
  
  observeEvent(input$back_datos, {
    updateRadioButtons(session, "seccion", selected = "concepto")
  })
  
  observeEvent(input$next_analisis, {
    updateRadioButtons(session, "seccion", selected = "mecanismos")
  })
  
  observeEvent(input$back_analisis, {
    updateRadioButtons(session, "seccion", selected = "datos")
  })
  
  observeEvent(input$next_mecanismos, {
    updateRadioButtons(session, "seccion", selected = "contacto")
  })
  
  observeEvent(input$back_mecanismos, {
    updateRadioButtons(session, "seccion", selected = "analisis")
  })
  
  observeEvent(input$back_contacto, {
    updateRadioButtons(session, "seccion", selected = "mecanismos")
  })
  
  # Reiniciar análisis
  observeEvent(input$reiniciar, {
    updateRadioButtons(session, "seccion", selected = "intro")
  })
  
  
  # Contenido principal reactivo
  output$contenido_principal <- renderUI({
    switch(input$seccion,
           
           # Sección 1: Introducción
           "intro" = div(
             card(
               card_header(
                 icon("book-open"), " Introducción: Los Espacios de Excepción"
               ),
               card_body(
                 p("De acuerdo al portal de datos abiertos de la CDMX, ", 
                   tags$a(href = "https://datos.cdmx.gob.mx/dataset/victimas-en-carpetas-de-investigacion-fgj", 
                          target = "_blank",
                          "Víctimas en carpetas de investigación FGJ"), 
                   ", existen 315 delitos en 16 categorías. Existen delitos que a través del tiempo presentan cierta tendencia ya sea por la capacidad de denunciar (formas más eficientes) o porque las problemáticas se vuelven más complejas de detener
                   algunos ejemplos son: la categoría de Delitos de bajo impacto, Hechos no delictivos y VIolación"),
                 gt::gt_output(outputId = "t_1"),
                 p("Los", strong("espacios de excepción"), "son territorios donde se suspende el orden jurídico normal, creando zonas ambiguas donde la ley se aplica precisamente a través de su", em("no-aplicación"), "."),
                 
                 br(),
                 div(
                   class = "alert alert-info",
                   icon("lightbulb"), " ",
                   strong("Concepto clave:"), " No es ausencia total de ley, sino una inclusión a través de la exclusión."
                 ),
                 
                 h5("Características principales:"),
                 tags$ul(
                   tags$li("Suspensión temporal del orden jurídico"),
                   tags$li("Violencia latente o manifiesta"),
                   tags$li("Ambigüedad legal permanente"),
                   tags$li("Control territorial específico")
                 ),
                 
                 br(),
                 div(
                   class = "d-flex justify-content-end",
                   actionButton("next_intro", "Siguiente: El Concepto →", 
                                class = "btn-primary")
                 )
               )
             )
           ),
           
           # Sección 2: Concepto de Agamben
           "concepto" = div(
             card(
               card_header(
                 icon("user-graduate"), " Giorgio Agamben y la Teoría"
               ),
               card_body(
                 div(
                   class = "row",
                   div(
                     class = "col-md-8",
                     h4("Marco Teórico", class = "text-primary"),
                     p("Giorgio Agamben desarrolló este concepto especialmente en sus obras:"),
                     tags$ul(
                       tags$li(strong("Homo Sacer (1995):"), " Introduce la figura del 'homo sacer' - vida que puede ser matada pero no sacrificada"),
                       tags$li(strong("Estado de excepción (2003):"), " Analiza cómo el estado de excepción se ha vuelto paradigma de gobierno")
                     ),
                     
                     br(),
                     div(
                       class = "alert alert-warning",
                       h6("Paradoja fundamental:"),
                       p("El espacio de excepción está", em("incluido"), "en el orden jurídico precisamente a través de su", em("exclusión"), "de él.")
                     )
                   ),
                   div(
                     class = "col-md-4",
                     div(
                       class = "card bg-light",
                       div(class = "card-body text-center",
                           icon("quote-left", class = "fa-2x text-muted mb-2"),
                           p(em("\"El estado de excepción no es ni exterior ni interior al ordenamiento jurídico\""), 
                             class = "small"),
                           p(strong("- Giorgio Agamben"), class = "text-muted small")
                       )
                     )
                   )
                 ),
                 
                 br(),
                 div(
                   class = "d-flex justify-content-between",
                   actionButton("back_concepto", "← Anterior", class = "btn-secondary"),
                   actionButton("next_concepto", "Siguiente: Datos →", class = "btn-primary")
                 )
               )
             )
           ),
           
           # Sección 3: Datos
           "datos" = div(
             card(
               card_header(
                 icon("chart-bar"), " Datos y Evidencia Empírica"
               ),
               card_body(
                 h4("Identificación de Espacios de Excepción", class = "text-primary"),
                 p("Los datos utilizados corresponden a las víctimas de la FGJ Datos abiertos CDMX, periodo, para esté análisis se consideraron
       solo los delitos que representan el 70% de las víctimas acumuladas por año. Para identificar estos delitosestos espacios utilizamos indicadores que revelan la suspensión del orden normal"),
                 div(class = "row",
                     div(class = "col-md-6",
                         h5("Indicadores Primarios:"),
                         tags$ul(
                           tags$li("Tasas de criminalidad anómalas"),
                           tags$li("Ausencia de servicios estatales"),
                           tags$li("Control territorial no-estatal"),
                           tags$li("Índices de abandono urbano")
                         )
                     ),
                     div(class = "col-md-6",
                         h5("Indicadores Secundarios:"),
                         tags$ul(
                           tags$li("Estigmatización espacial"),
                           tags$li("Segregación socioeconómica"),
                           tags$li("Deterioro infraestructural"),
                           tags$li("Redes criminales territorializadas")
                         )
                     )
                 ),

                 # g1 en columna de width 12
                 div(class = "row",
                     div(class = "col-12",
                         h3("Histórico de delitos por carpeta"),
                         img(src = "g1.png", class = "img-fluid")
                     )
                 ),
                 
                 # g2 en fila completa
                 div(class = "row",
                     div(class = "col-12",
                         h3("Histórico Por categoría"),
                         img(src = "g2.png", class = "img-fluid")
                     )
                 ),
                 
                 # p1 y p2 en el mismo row, dos columnas width 6 cada uno
                 h4("Delitos que se mantienen en proporción"),
                 div(class = "row",
                     div(class = "col-6",
                         img(src = "p1.png", class = "img-fluid")
                     ),
                     div(class = "col-6",
                         img(src = "p2.png", class = "img-fluid")
                     )
                 ),
                 
                 br(),
                 div(
                   class = "d-flex justify-content-between",
                   actionButton("back_datos", "← Anterior", class = "btn-secondary"),
                   actionButton("next_datos", "Siguiente: Análisis →", class = "btn-primary")
                 )
               )
             )
           ),
           
           # Sección 4: Análisis
           "analisis" = div(
             card(
               card_header(
                 icon("map"), " Análisis Territorial"
               ),
               card_body(
                 h4("Mapeo de Espacios de Excepción", class = "text-primary"),
                 p("El análisis territorial revela patrones espaciales que confirman la teoría de Agamben:"),
                 
                 div(class = "alert alert-success",
                     h6("Hallazgos principales:"),
                     tags$ul(
                       tags$li("Concentración geográfica de la excepción"),
                       tags$li("Patrones de continuidad territorial"),
                       tags$li("Relación con infraestructura urbana"),
                       tags$li("Correlación con variables socioeconómicas")
                     )
                 ),
                 
                 # Placeholder para mapas y análisis
                 leafletOutput(outputId = "mapa_historico"),
                 br(),
                 div(
                   class = "d-flex justify-content-between",
                   actionButton("back_analisis", "← Anterior", class = "btn-secondary"),
                   actionButton("next_analisis", "Siguiente: Mecanismos →", class = "btn-primary")
                 )
               )
             )
           ),
           
           # Sección 5: Mecanismos
           # Sección 5: Mecanismos
           "mecanismos" = div(
             card(
               card_header(
                 icon("cogs"), " Mecanismos de Perpetuación"
               ),
               card_body(
                 h4("¿Cómo se perpetúan estos espacios?", class = "text-primary"),
                 p("Los espacios de excepción no son fenómenos temporales, sino que desarrollan mecanismos de auto-reproducción:"),
                 
                 div(class = "row",
                     div(class = "col-md-6",
                         div(class = "card border-danger mb-3",
                             div(class = "card-header bg-danger text-white",
                                 icon("exclamation-triangle"), " Retroalimentación Negativa"
                             ),
                             div(class = "card-body",
                                 p("El robo genera abandono estatal y ciudadano, creando más oportunidades para el robo.", class = "small")
                             )
                         ),
                         div(class = "card border-warning mb-3",
                             div(class = "card-header bg-warning",
                                 icon("graduation-cap"), " Aprendizaje Territorial"
                             ),
                             div(class = "card-body",
                                 p("Los espacios 'enseñan' las mejores técnicas y oportunidades criminales.", class = "small")
                             )
                         )
                     ),
                     div(class = "col-md-6",
                         div(class = "card border-primary mb-3",
                             div(class = "card-header bg-primary text-white",
                                 icon("network-wired"), " Redes Espacializadas"
                             ),
                             div(class = "card-body",
                                 p("Grupos criminales que controlan y reproducen territorios específicos.", class = "small")
                             )
                         ),
                         div(class = "card border-secondary mb-3",
                             div(class = "card-header bg-secondary text-white",
                                 icon("tag"), " Estigmatización Espacial"
                             ),
                             div(class = "card-body",
                                 p("Los lugares quedan marcados, dificultando intervenciones positivas.", class = "small")
                             )
                         )
                     )
                 ),
                 
                 br(),
                 div(class = "alert alert-danger",
                     h6(icon("sync-alt"), " Ciclo de Perpetuación:"),
                     p("Estos mecanismos se refuerzan mutuamente, creando un ciclo difícil de romper donde la excepción se normaliza y se reproduce espacialmente.")
                 ),
                 
                 # Diagrama conceptual simple
                 div(
                   style = "text-align: center; padding: 20px; background-color: #f8f9fa; border-radius: 8px;",
                   h6("Ciclo de Retroalimentación", class = "text-muted"),
                   div(
                     style = "font-size: 14px; color: #6c757d;",
                     "Violencia → Abandono → Oportunidad → Violencia"
                   )
                 ),
                 
                 br(),
                 div(
                   class = "d-flex justify-content-between",
                   actionButton("back_mecanismos", "← Anterior", class = "btn-secondary"),
                   actionButton("next_mecanismos", "Siguiente: Metodología →", class = "btn-primary")
                 )
               )
             )
           ),
           
           # Sección 6: Metodología y Contacto
           "contacto" = div(
             card(
               card_header(
                 icon("info-circle"), " Metodología y Contacto"
               ),
               card_body(
                 div(class = "row",
                     div(class = "col-md-8",
                         h5("Marco Metodológico:"),
                         tags$ul(
                           tags$li(strong("Enfoque teórico:"), " Basado en la obra de Giorgio Agamben sobre espacios de excepción"),
                           tags$li(strong("Análisis espacial:"), " Uso de SIG para identificar patrones territoriales"),
                           tags$li(strong("Indicadores cuantitativos:"), " Datos de criminalidad, servicios públicos y variables socioeconómicas"),
                         ),
                         h5("Fuentes de Datos:"),
                         tags$ul(
                           tags$li("Fiscalía General de Justicia, considerando ano hecho 2019 al 2023, se excluye el 2024 por que no está completo")
                         ),
                         
                         div(class = "alert alert-success",
                             h5("Transparencia por convicción, no por obligación."),
                             p("Cada algoritmo, cada gráfico y cada conclusión de esta investigación puede ser inspeccionada, cuestionada y mejorada. Creemos que el conocimiento científico debe ser accesible, verificable y construido colaborativamente, permitiendo que cualquier persona pueda revisar, replicar y perfeccionar nuestro trabajo."),
                             # p("Usamos software libre porque la ciencia libre construye sociedades libres. Porque la ciencia que no se puede verificar, no es ciencia."),
                             
                             # Sección del código fuente
                             div(class = "d-flex align-items-center flex-wrap mb-3",
                                 div(class = "me-3 mb-2",
                                     p(class = "mb-0", "Consulta el código completo:"),
                                     a(href = "https://github.com/useReconomist/INSTITUTO-DE-GEOGRAF-A-UNAM-DIP-GEO", 
                                       target = "_blank",
                                       class = "btn btn-outline-dark btn-sm",
                                       icon("github", lib = "font-awesome"),
                                       " Ver en GitHub")
                                 ),
                                 div(class = "mb-2",
                                     p(class = "mb-1 small", "Escanea el código QR:"),
                                     img(src = "qr.svg", 
                                         style = "max-width: 90px; height: auto;",
                                         alt = "Código QR para acceder al repositorio")
                                 )
                             ),
                             
                             # Sección de contacto
                             div(class = "border-top pt-2",
                                 p(class = "mb-0",
                                   "¿Dudas? Contacta a los autores: ",
                                   a(href = "mailto:noe.osorio@aiesec.net,ecostat.nog@gmail.com", 
                                     class = "text-decoration-none",
                                     icon("envelope", lib = "font-awesome"),
                                     " Enviar correo")
                                 )
                             )
                         )
                     ),
                     
                     div(class = "col-md-4",
                         div(class = "card bg-light",
                             div(class = "card-header",
                                 icon("envelope"), " Contacto"
                             ),
                             div(class = "card-body",
                                 h6("Autores"),
                                 p(icon("user"), "Erandy",icon("envelope"), href = "mailto:participante4@email.com",icon("twitter"),icon("linkedin"), class = "small"),
                                 p(icon("user"), "Manuel",icon("envelope"), href = "mailto:participante4@email.com",icon("twitter"),icon("linkedin"), class = "small"),
                                 p(icon("user"), "Verónica",icon("envelope"), href = "mailto:participante4@email.com",icon("twitter"),icon("linkedin"), class = "small"),
                                 p(icon("user"), "Noé Osorio García",icon("envelope"), href = "mailto:participante4@email.com",icon("twitter"),icon("linkedin"), class = "small"),
                                 hr(),
                                 p(icon("university"), "UNAM, Instituto de Geografía", class = "small"),
                                 p(icon("book-open"),"XVIII DIPLOMADO EN GEOMÁTICA",class="small"),
                                 p(icon("envelope"), "diplogeo@geografia.unam.mx", class = "small")
                             )
                         ),

                         div(class = "card bg-primary text-white",
                             div(class = "card-body text-center",
                                 icon("check-circle", class = "fa-2x mb-2"),
                                 h6("Análisis Completado"),
                                 p("Has recorrido todo el análisis territorial sobre espacios de excepción.", class = "small")
                             )
                         )
                     )
                 ),
                 div(
                   class = "d-flex justify-content-between",
                   actionButton("back_contacto", "← Anterior", class = "btn-secondary"),
                   actionButton("reiniciar", "Reiniciar Análisis", class = "btn-outline-primary",
                                onclick = "Shiny.setInputValue('seccion', 'intro');")
                 )
               )
             )
           )
    )
  })
  
  output$t_1=render_gt({
    t1 %>%
      arrange(desc(`Año 2023`)) %>%
      gt(rowname_col = "categoria_delito") %>%
      fmt_integer(columns = starts_with("Año")) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_stub(  # Cambio clave: usar cells_stub() en lugar de cells_body()
          rows = categoria_delito %in% c("DELITO DE BAJO IMPACTO",
                                         "HECHO NO DELICTIVO",
                                         "VIOLACIÓN")
        )
      ) %>%
      tab_stubhead(label = md("**Categoría delito**")) %>%
      cols_nanoplot(
        columns = starts_with("Año"),
        autohide = FALSE,
        new_col_name = "nanoplots",
        new_col_label = md("*Progression*")
      ) %>%
      tab_spanner(
        label = "Víctimas por año",
        columns = starts_with("Año")
      ) %>%
      cols_align(align = "right", columns = nanoplots)
  })
  
  output$mapa_historico = renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      addMiniMap()
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
