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
library(gt)
library(sf)
library(duckdb)
library(leaflet.extras)
library(leaflet.extras2)
library(echarts4r)
library(scales)

# con_global <- DBI::dbConnect(duckdb)
t1 = read_rds("t1.rds")
colonias = sf::st_read("colonias_conapo_cdmx.gpkg",quiet=T) %>% sf::st_transform(crs=4326)
mun = sf::st_read("mun_cdmx.gpkg",quiet=T) %>% sf::st_transform(crs=4326)

# write_rds(x = victimas_app %>% as_tibble(),"Geomatica_UNAM/base_final.rds")
victimas_app = read_rds(file = "base_final.rds") 

peligroson = c("18603","18606","17164","17424","17568","18337","18586","18615")

# Perdonen el UI, todo debería ir en el www
ui <- page_sidebar(
  useBusyIndicators(),
  title = div(
    tags$a(
      href = "https://www.geografia.unam.mx/geoigg/",
      target = "_blank",
      img(src = "ig_logo.svg", height = "30px"),
      class = "logo-link"
    ),
    tags$strong(
      "Geovisualizador: Violencia Directa y Espacios de Exclusion",
      style = "color: white; font-weight: bold;"
    ),
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
      
      /* Estilos para la barra superior - múltiples selectores */
      .navbar, .navbar-brand, .bslib-page-title, 
      .navbar-light, .bg-light, .navbar-expand {
        background-color: #5a9c9e !important;
        color: white !important;
        font-weight: bold !important;
      }
      
      /* Contenedor principal del título */
      .container-fluid {
        background-color: #5a9c9e !important;
      }
      
      /* Header completo */
      header {
        background-color: #5a9c9e !important;
        color: white !important;
        font-weight: bold !important;
      }
      
      /* Todo el contenido del header */
      header * {
        color: white !important;
        font-weight: bold !important;
      }
      
      /* Selector más específico para el título de la página */
      .bslib-sidebar-layout > .navbar {
        background-color: #5a9c9e !important;
        border-color: #5a9c9e !important;
      }
    "))
  ),
  useShinyjs(),
  sidebar = sidebar(
    width = 280,

    radioButtons("seccion", 
                 "Navegación:",
                 choices = list(
                   "1. Introducción" = "intro",
                   "2. Marco teórico" = "concepto", 
                   "3. Datos y Evidencia" = "datos",
                   "4. Análisis Espacial" = "analisis",
                   "5. Conclusiones" = "conclusiones",
                   "6. Metodología y Contacto" = "contacto"
                 ),
                 selected = "intro"),
    
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
  
  showModal(
    modalDialog(
      div(
        style = "text-align: center; position: relative; z-index: 2; padding: 20px;",
        tags$img(src = "pleca_head.png", width = "100%", height = "auto"),
        tags$div(
          HTML("
<strong>Geovisualizador:<br> Violencia Directa y Espacios de Exclusion</strong><br><br>

Explora los patrones de violencia urbana en la CDMX a través de análisis espaciales basados en datos abiertos de la FGJ
 y movilidad inmediata <br>
<br>
        ")#<em>Plataforma preliminar</em>
        )
      ),
      easyClose = TRUE,
      footer = modalButton("ok"),
      # Aquí aplicamos el CSS al modal completo
      tags$head(
        tags$style(HTML("
        .modal-content {
          background-image: url('fondo_volcanes.png') !important;
          background-size: cover !important;
          background-position: center !important;
          background-repeat: no-repeat !important;
          position: relative !important;
        }
        .modal-content::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background-color: rgba(255, 255, 255, 0.8);
          z-index: 1;
        }
        .modal-header, .modal-body, .modal-footer {
          position: relative;
          z-index: 2;
        }
      "))
      )
    )
  )
  

  observe({
    progreso_valores <- list(
      "intro" = 16.67,
      "concepto" = 33.33,
      "datos" = 50,
      "analisis" = 66.67,
      "conclusiones" = 83.33,
      "contacto" = 100
    )
    
    updateProgressBar(
      session = session,
      id = "progreso", 
      value = progreso_valores[[input$seccion]]
    )
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
    updateRadioButtons(session, "seccion", selected = "conclusiones")
  })
  
  observeEvent(input$back_analisis, {
    updateRadioButtons(session, "seccion", selected = "datos")
  })
  
  observeEvent(input$next_conclusiones, {
    updateRadioButtons(session, "seccion", selected = "contacto")
  })
  
  observeEvent(input$back_conclusiones, {
    updateRadioButtons(session, "seccion", selected = "analisis")
  })
  
  observeEvent(input$back_contacto, {
    updateRadioButtons(session, "seccion", selected = "conclusiones")
  })
  
  # Reiniciar análisis
  observeEvent(input$reiniciar, {
    updateRadioButtons(session, "seccion", selected = "intro")
  })
  
  rv <- reactiveValues(
    current_click = NULL,
    buffer_data = NULL,
    bbox_coords = NULL
  )
  
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
                   algunos ejemplos son: las categorías de Delitos como 'Bajo impacto', 'Hechos no delictivos' y 'Violación' exhiben tendencias temporales que sugieren la existencia de ventanas de oportunidad criminal."),
                 gt::gt_output(outputId = "t_1"),
                 p("Los", strong("espacios de excepción"), "no solo se manifiestan geográficamente, sino también temporalmente. Los patrones de criminalidad revelan cómo ciertos momentos del día, días de la semana y épocas del año se convierten en 'tiempos de excepción' donde la vulnerabilidad ciudadana se intensifica."),   
                 p("
                 El análisis identificó aquellos ",strong("delitos que muestran una tendencia sostenida de crecimiento en el número de víctimas año tras año"),". Se encontraron 14 delitos clasificados como 'prioritarios' debido a que presentan incrementos consistentes en su incidencia anual.
                   "),
                 selectInput(inputId = "id_delito",choices = unique(victimas_app$delito),label = "Selecciona un delito",multiple = FALSE,selected = "VIOLENCIA FAMILIAR" ),
                 echarts4rOutput(outputId = "plot_1"),
                 h4("Distribución espacial por año"),
                 fluidRow(
                   class = "justify-content-center",
                   column(12,plotOutput("maps_year"))
                 ),
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
                 icon("city"), " Producción del espacio y violencia"
               ),
               card_body(
                 div(
                   class = "row",
                   div(
                     class = "col-md-12",
                     h4("Marco Teórico", class = "text-primary"),
                     p("La violencia urbana es un fenómeno complejo que trasciende los actos delictivos visibles, integrando manifestaciones directas con condiciones sistémicas profundamente vinculadas a la reproducción del sistema económico hegemónico y la producción social del espacio urbano. Esta violencia no se limita a crímenes o inseguridad percibida, sino que abarca procesos estructurales como marginalización, desinversión y estigmatización socioterritorial."),
                     
                     div(
                       class = "alert alert-info",
                       h6("Urbanismo neoliberal:"),
                       p("Impone la lógica del libre mercado sobre la ciudad, privatizando espacios y segmentándolos según criterios de rentabilidad. Esta dinámica profundiza desigualdades sociales y territoriales, donde la violencia actúa como instrumento para imponer órdenes políticos, económicos y simbólicos específicos.")
                     ),
                     
                     br(),
                     
                     # Violencia sistémica-objetiva
                     div(
                       class = "card border-warning mb-3",
                       div(
                         class = "card-header bg-warning text-dark",
                         h5(icon("cogs"), " Violencia sistémica-objetiva", class = "mb-0")
                       ),
                       div(
                         class = "card-body",
                         p("Esta forma de violencia estructural no se manifiesta de manera directa o visible, sino que está inscrita en las condiciones materiales, institucionales y estructurales que permiten la reproducción del orden económico y social dominante."),
                         p(strong("Características principales:")),
                         tags$ul(
                           tags$li("Es una violencia normalizada y silenciosa"),
                           tags$li("Forma el 'ambiente' donde se naturalizan las relaciones de dominación"),
                           tags$li("Se basa en clase, género, raza y territorio")
                         ),
                         p("Desde la geografía crítica, se expresa en segregación urbana, precarización de servicios y vivienda, exclusión territorial y desigualdad socioespacial. El espacio urbano neoliberal se convierte en escenario donde la violencia sistémica actúa instrumentalmente para producir desigualdad y asegurar la reproducción del capital.")
                       )
                     ),
                     
                     # Violencia directa-subjetiva
                     div(
                       class = "card border-danger mb-3",
                       div(
                         class = "card-header bg-danger text-white",
                         h5(icon("exclamation-triangle"), " Violencia directa-subjetiva", class = "mb-0")
                       ),
                       div(
                         class = "card-body",
                         p("Representa la expresión visible y corporal de la violencia a través de actos coercitivos y materiales que manifiestan el funcionamiento de un sistema que, ante crisis o resistencia, impone su orden mediante represión y control físico."),
                         div(
                           class = "alert alert-light",
                           p(strong("Importante:"), " No es irracional ni aislada, sino una forma racionalizada de dominación que sostiene el urbanismo neoliberal cuando los mecanismos de consenso fallan.")
                         ),
                         p(strong("Manifestaciones:")),
                         tags$ul(
                           tags$li("Violencia de género, racismo, xenofobia que legitiman exclusión"),
                           tags$li("Violencia del crimen organizado como territorialización"),
                           tags$li("Control social en espacios marginados")
                         )
                       )
                     ),
                     
                     # Seguridad
                     div(
                       class = "card border-info mb-3",
                       div(
                         class = "card-header bg-info text-white",
                         h5(icon("shield-alt"), " Seguridad", class = "mb-0")
                       ),
                       div(
                         class = "card-body",
                         p("Se ha posicionado como eje fundamental en la configuración del espacio urbano contemporáneo, impulsada por la expansión del crimen organizado y creciente percepción social de inseguridad."),
                         p("Desde una visión crítica y espacial, la seguridad no es únicamente un conjunto de acciones institucionales, sino una práctica que reorganiza relaciones sociales y territoriales."),
                         div(
                           class = "bg-light p-2 rounded",
                           p(strong("Contradicciones:"), " Las políticas de seguridad pública muestran contradicciones entre el discurso internacional y prácticas nacionales/locales que priorizan vigilancia policial y contención del delito.")
                         )
                       )
                     )
                   )
                 ),
                 
                 br(),
                 
                 # Conceptos clave expandidos
                 div(
                   class = "alert alert-primary",
                   h5("Conceptos clave:", class = "alert-heading"),
                   div(
                     class = "row",
                     div(
                       class = "col-md-6",
                       tags$ul(
                         tags$li(strong("Producción social del espacio:"), " Proceso dinámico mediante el cual el espacio urbano se configura a partir de relaciones sociales, económicas, políticas y culturales"),
                         tags$li(strong("Urbanismo neoliberal:"), " Modelo basado en libre mercado que impulsa privatización y gentrificación, configurando ciudades segmentadas"),
                         tags$li(strong("Seguridad urbana:"), " Políticas y prácticas que gestionan violencia, convirtiéndose en dispositivo de control social")
                       )
                     ),
                     div(
                       class = "col-md-6",
                       tags$ul(
                         tags$li(strong("Marginalización socioterritorial:"), " Proceso de exclusión de grupos y territorios de la participación plena urbana"),
                         tags$li(strong("Violencia sistémica-objetiva:"), " Violencia estructural silenciosa inscrita en condiciones materiales que reproducen el sistema hegemónico"),
                         tags$li(strong("Violencia directa-subjetiva:"), " Manifestación visible y coercitiva que incluye agresiones físicas y control directo")
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
                     div(class = "col-md-12",
                         h5("Indicadores Primarios:"),
                         tags$ul(
                           tags$li("Incremento constante"),
                           tags$li("Tasa anual por delito"),
                           tags$li("Permanencia espacial en la zona")
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
                 div(
                   class = "d-flex justify-content-between",
                   actionButton("back_datos", "← Anterior", class = "btn-secondary"),
                   actionButton("next_datos", "Siguiente: Análisis →", class = "btn-primary")
                 )
               )
             )
           ),
           
           # Sección 4: Análisis
           "analisis" = {
             output$mapa_historico <- renderLeaflet({
               leaflet(options = leafletOptions(minZoom = 10)) %>% 
                 addWMSTiles(baseUrl = "http://gaiamapas1.inegi.org.mx/mdmCache/service/wms?",
                             layers = "MapaBaseTopograficov61_sinsombreado",
                             group = "INEGI") %>% 
                 addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
                 addProviderTiles(providers$CartoDB, group = "Carto") %>%
                 addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
                 setView(lng = -99.1359976, lat = 19.432411, zoom = 10) %>%
                 addPolygons(data=mun,weight = .5,color = "black",opacity = 1,fill=NA) %>% 
                 addPolygons(data=colonias,weight = .5,color = "black",opacity = 1,fill=NA) %>% 
                 addPolygons(data=colonias %>% filter(ID_COL%in%peligroson),weight = .5,color = "red",opacity = 1,fill="red") %>% 
                 addLayersControl(baseGroups = c("Carto","INEGI","OSM","WorldImagery"),
                                  overlayGroups = c("H3","Calor"), 
                                  position = "topleft") %>% 
                 setMaxBounds( lng1 = -99.36492 
                               , lat1 = 19.04824
                               , lng2 = -98.94030
                               , lat2 = 19.59276) %>% 
                 addControl(html = "<div style='background-color: white; padding: 8px; border-radius: 4px; font-size: 12px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>
               <strong>Da click en el mapa <br> y luego en 'calcular'<br>para conocer la <br>distribución de delitos</strong>
               </div>",
                            position = "topleft")
             })
             
             div(
             style = "height: 100vh;",
             # Botones de navegación en la parte superior
             div(
               class = "d-flex justify-content-between mb-2",
               actionButton("back_analisis", "← Anterior", class = "btn-secondary"),
               actionButton("next_analisis", "Siguiente: Conclusiones →", class = "btn-primary")
             ),
             # Layout de dos columnas
             div(
               class = "row g-2",
               style = "height: calc(100vh - 80px);",
               # Columna 1: Mapa en card con botón flotante (50% del ancho)
               div(
                 class = "col-6",
                 style = "height: 100%; position: relative;",
                 card(
                   full_screen = TRUE,
                   card_header(
                     icon("map"), " Análisis Territorial"
                   ),
                   card_body(
                     # Botón calcular flotante en esquina superior derecha
                     div(
                       style = "position: absolute; top: 10px; right: 15px; z-index: 1000;",
                       div(
                         style = "font-weight: bold;",
                         fluidRow(
                           column(6,
                                  radioButtons(inputId = "modo_compara",
                                               label = "Analizar por:", inline = TRUE,
                                               choices = c("Distancia","Tiempo")
                                  ),
                                  actionButton("calcular", "Calcular", class = "btn-success")
                           ),
                           column(6,
                                  uiOutput(outputId = "deriva_modo_compara")
                           )
                         )
                       )
                     ),
                     leafletOutput(outputId = "mapa_historico", height = "100%"),
                     class = "p-0",
                     style = "position: relative;"
                   ),
                   style = "height: 100%;"
                 )
               ),
               # Columna 2: Solo gráficos en cards (50% del ancho)
               div(
                 class = "col-6",
                 style = "height: 100%;",
                 navset_card_tab(
                   id = "tabs_graficos",
                   nav_panel(
                     title = tagList(icon("chart-line"), " Histórico"),
                     gt_output("grafico1"),
                     value = "historico"
                   ),
                   nav_panel(
                     title = " Correlación", 
                     plotOutput("grafico3",
                                height = "100%",
                                width = "100%"),
                     value = "correlacion"
                   ),
                   full_screen = TRUE
                 )
               )
             )
           )},
           # Sección 5: Mecanismos
           "conclusiones" = div(
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
                     class = "text-center",
                     style = "margin: 20px 0;",
                     img(src = "CICLO.jpg", 
                         alt = "Ciclo de violencia", 
                         style = "max-width: 100%; height: auto; border-radius: 8px;")
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
                             p("Cada algoritmo, cada gráfico y cada conclusión de esta investigación puede ser inspeccionada, cuestionada y mejorada. Creemos que el conocimiento científico debe ser accesible, verificable y construido colaborativamente, permitiendo que cualquier persona pueda revisar, replicar y perfeccionar nuestro trabajo.
                               a través del softare libre. Consulta el código completo en:"),
                             # p("Usamos software libre porque la ciencia libre construye sociedades libres. Porque la ciencia que no se puede verificar, no es ciencia."),
                             
                             # Sección del código fuente
                             div(class = "d-flex align-items-center flex-wrap mb-3",
                                 div(class = "me-3 mb-2",
                                     a(href = "https://github.com/useReconomist/INSTITUTO-DE-GEOGRAF-A-UNAM-DIP-GEO", 
                                       target = "_blank",
                                       class = "btn btn-outline-dark btn-sm",
                                       icon("github", lib = "font-awesome"),
                                       " Ver en GitHub")
                                 ),
                                 div(class = "mb-2",
                                     p(class = "mb-1 small", "accede al código"),
                                     img(src = "qr.svg", 
                                         style = "max-width: 90px; height: auto;",
                                         alt = "Código QR para acceder al repositorio")
                                 )
                             ),
                             
                             # Sección de contacto
                             div(class = "border-top pt-2",
                                 p(class = "mb-0",
                                   "¿Dudas? Contacta a los autores: ",
                                   a(href = "mailto:ecostat.nog@gmail.com,jmespejol@gmail.com,erandijj@gmail.com,vmflores.geo@gmail.com", 
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
                                 p(
                                   icon("user"), " Erandi Jiménez Jacques ", 
                                   a(icon("envelope"), href = "mailto:erandijj@gmail.com", style = "margin: 0 5px;"),
                                   class = "small"
                                 ),
                                 p(
                                   icon("user"), " Jesús Manuel Espejo Lemarroy ", 
                                   a(icon("envelope"), href = "mailto:jmespejol@gmail.com", style = "margin: 0 5px;"),
                                   class = "small"
                                 ),
                                 p(
                                   icon("user"), " Verónica Mares Flores ", 
                                   a(icon("envelope"), href = "mailto:vmflores.geo@gmail.com", style = "margin: 0 5px;"),
                                   class = "small"
                                 ),
                                 p(
                                   icon("user"), " Noé Osorio García ", 
                                   a(icon("envelope"), href = "mailto:ecostat.nog@gmail.com", style = "margin: 0 5px;"),
                                   a(icon("twitter"), href = "https://x.com/NoeOsorioPK", target = "_blank", style = "margin: 0 5px;"),
                                   a(icon("linkedin"), href = "https://www.linkedin.com/in/noe-osorio-garcia-979a1818a/", target = "_blank", style = "margin: 0 5px;"),
                                   class = "small"
                                 ),
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
                                         "VIOLACIÓN"))
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
  
  rv <- reactiveValues(
    current_click = NULL,
    buffer_data = NULL,
    bbox_coords = NULL,
    delitos_zona=NULL
  )

  observeEvent(input$mapa_historico_click, {
    click <- input$mapa_historico_click
    rv$current_click <- click

    leafletProxy("mapa_historico") %>%
      clearGroup("current_click") %>%
      clearGroup("marcador") %>% 
      addMarkers(lng = click$lng, lat = click$lat,
                 group = "current_click")
  })
  
  consulta_serie = reactive({
    victimas_app %>% 
      dplyr::filter(delito==input$id_delito)
  })
  
  colonias_react = reactive({
  colonias
  })
  
  serie_completa = reactive({
    victimas_app 
  })
  
  output$plot_1 = renderEcharts4r({
    
    data_filtered <- consulta_serie() %>% 
      count(semana_hecho = lubridate::floor_date(fecha_hecho,unit="year"))
    
    data_filtered %>%
      e_charts(semana_hecho) %>% 
      e_bar(n) %>% 
      e_visual_map(
        min = 0,
        max = max(data_filtered$n),
        calculable = TRUE
      ) %>% 
      e_legend(FALSE) %>% 
      e_title(paste0("Histórico anual: ",input$id_delito),
              subtext = "Periodo del 2019 al 2023") %>% 
      e_tooltip(trigger = "axis")
  })
  
  output$maps_year = renderPlot({
    consulta_serie() %>% 
      count(anio_hecho,ID_COL) %>% 
      left_join(colonias_react() %>% select(ID_COL),by="ID_COL") %>% 
      ggplot()+
      geom_sf(aes(fill=n,geometry=geom,col=n),alpha=.7)+
      geom_sf(data=mun,fill=NA,color="black")+
      viridis::scale_fill_viridis()+
      viridis::scale_color_viridis()+
      facet_wrap(~anio_hecho,ncol=5)+
      ggthemes::theme_map()+
      theme(strip.text = element_text(color="black",face = "bold",size = 15))
    
  })
  

  # Función de cálculo mejorada
  output$deriva_modo_compara <- renderUI({
    if (input$modo_compara == "Tiempo") {
      # Mostrar radio buttons para tiempo (10, 15, 20, 25, 30 minutos)
      tagList(
        radioButtons(
          inputId = "modo_seleccion",
          label = "Selecciona Modalidad:",
          choices = c("Caminando" = "walking", "En vehículo" = "driving-traffic"),
          selected = "walking",
          inline = TRUE
        ),
        
        radioButtons(
          inputId = "tiempo_seleccion",
          label = "Selecciona tiempo (minutos):",
          choices = c("5" =5,"10" = 10, "15" = 15),
          selected = 5,
          inline = TRUE
        )
      )
    } else if (input$modo_compara == "Distancia") {
      # Mostrar slider para distancia (100 a 3000, de 200 en 200)
      sliderInput(
        inputId = "distancia_seleccion",
        label = "Selecciona distancia:",
        min = 100,
        max = 3000,
        value = 500,
        step = 100,
        post = " metros"
      )
    }
  })
  
  
  observeEvent(input$calcular, {
    
    req(input$seccion)
    req(rv$current_click)
    
    
    showNotification("Generando análisis", type = "default", duration = 3)
    
    punto <- st_point(c(rv$current_click$lng, rv$current_click$lat)) %>% 
      st_sfc(crs = 4326)
    
    isocrona =   poligono_resultado <- if (input$modo_compara == "Distancia") {
      # Para distancia: usar st_buffer directamente
      req(input$distancia_seleccion)
      punto %>%
        st_transform(32614) %>%  # UTM Zone 14N
        st_buffer(as.numeric(input$distancia_seleccion)) %>%
        st_transform(4326)
    } else {
      req(input$tiempo_seleccion)
      
      mapboxapi::mb_isochrone(
        location = punto %>% sf::st_as_sf(),
        time = as.numeric(input$tiempo_seleccion),
        profile = input$modo_seleccion
      )
      
    }

    
    h3_informacion <- isocrona %>% 
      h3jsr::polygon_to_cells(res = 10) %>% .[[1]] %>% 
      as_tibble() %>% 
      rename(h3_9 = value) %>% 
      mutate(geometry = h3jsr::cell_to_polygon(h3_9)) %>% 
      sf::st_as_sf()
    
    # Verificar que h3_informacion tenga celdas válidas
    req(nrow(h3_informacion) > 0)
    
    # Preparar los datos del conteo
    rv$delitos_zona <- serie_completa() %>% 
      filter(h3 %in% h3_informacion$h3_9) 
    
    # Hacer el join y manejar valores faltantes
    valores_coro <- h3_informacion %>% 
      left_join(rv$delitos_zona%>%
                  count(h3, name = "n"), by = c("h3_9" = "h3")) %>%
      mutate(n = ifelse(is.na(n), 0, n))  # Reemplazar NA con 0
    
    # Verificar que tengamos al menos algunas celdas H3 válidas
    if(nrow(rv$delitos_zona%>%
            count(h3)) == 0) {
      # Si no hay coincidencias, mostrar mensaje o usar valores por defecto
      showNotification("La zona seleccionada no tiene datos disponibles", type = "warning")
      valores_coro$n <- rep(0, nrow(valores_coro))
    }
    
    paleta = colorNumeric(palette = "inferno", 
                          domain = c(0, max(valores_coro$n, na.rm = TRUE)), 
                          na.color = "gray")
    
    bbox <- sf::st_bbox(h3_informacion)
    
    leafletProxy("mapa_historico") %>%
      clearGroup("current_click") %>%
      clearGroup("marcador") %>%
      clearGroup("isocrona") %>%
      clearGroup("H3") %>%
      clearGroup("Calor") %>%
      addMarkers(data = punto,
                 group = "current_click") %>% 
      addPolygons(data = isocrona, 
                  weight = 1, 
                  group = "isocrona") %>% 
      addPolygons(data = valores_coro,
                  weight = 1,
                  group = "H3",
                  label = ~paste("Carpetas, Víctimas:", n),
                  color = ~paleta(n),
                  fillColor = ~paleta(n),
                  opacity = .7,
                  fillOpacity = .7)
    
    # Agregar heatmap solo si hay datos válidos
    if(nrow(rv$delitos_zona %>% count(h3)) > 0) {
      leafletProxy("mapa_historico") %>%
        leaflet.extras::addHeatmap(
          data = serie_completa() %>% 
            mutate(h3 = as.character(h3)) %>% 
            filter(h3 %in% h3_informacion$h3_9) %>% 
            sf::st_as_sf(),
          radius = 15, 
          group = "Calor"
        )
    }
    
    # Ajustar vista
    leafletProxy("mapa_historico") %>%
      flyToBounds(lng1 = bbox[[1]], 
                  lat1 = bbox[[2]], 
                  lng2 = bbox[[3]], 
                  lat2 = bbox[[4]])
  })
  
  output$grafico1 = render_gt({
    req(input$calcular)
    req(rv$delitos_zona)
  
    rv$delitos_zona%>% 
      count(anio_hecho,delito) %>% 
      arrange(anio_hecho,delito) %>% 
      pivot_wider(id_cols = delito,
                  names_from = anio_hecho,
                  values_from = n,
                  values_fill = 0,
                  names_prefix = "Año ") %>%
      gt(rowname_col = "delito") %>% 
      tab_stubhead(label = md("**Delito**")) %>% 
      cols_nanoplot(autohide = FALSE,
                    autoscale = FALSE,
                    columns = starts_with("Año "),
                    new_col_name = "nanoplots",
                    new_col_label = md("*Progression*")
      ) %>% 
      cols_align(align = "right", columns = nanoplots) %>% 
      cols_align(align = "right", columns = delito)
    
  })
  
  output$grafico3 = renderPlot({
    req(input$calcular)
    req(rv$delitos_zona)
    
    ggcorrplot::ggcorrplot(corr = cor(rv$delitos_zona %>%  
                                        mutate(mes_6=lubridate::floor_date(fecha_hecho,unit="quarter")) %>% 
                                        group_by(mes_6,delito) %>% 
                                        summarise(total=n(),.groups = "drop") %>% 
                                        pivot_wider(id_cols = mes_6,
                                                    names_from = delito,
                                                    values_from = total,
                                                    values_fill = 0) %>% 
                                        select(-mes_6)), 
                           hc.order = TRUE, 
                           type = "lower",
                           lab = TRUE)+
      scale_x_discrete(labels = label_wrap(15)) +
      scale_y_discrete(labels = label_wrap(15)) +
      theme(axis.text = element_text(color="black"))
    
    
  }) 
  
}


shinyApp(ui = ui, server = server)

