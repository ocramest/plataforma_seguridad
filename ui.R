library(shiny)
library(leaflet)
library(plotly)
library(shinybusy)
library(waiter)
library(shinyjs)


ui <- fluidPage(
  useShinyjs(),
  use_waiter(), 
  waiter_show_on_load(html = spin_fading_circles()),
    
    tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css"),
              tags$script(src = "message-handler.js"),
              tags$title("Plataforma de Seguridad Jalisco"),
              tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')
              ),
    
    tags$style(type="text/css", 
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
   ),
   
   add_busy_spinner(onstart = F, spin = "fading-circle", color = "#E34F70"),
  
    fluidRow(id = "top_row_title",
             column(width = 3,
              tags$div(class = "title",
                        tags$h2(class = "last_month", "CONCENTRADO ESTATAL")
              )
             )
    ),
      
    fluidRow(id = "top_row",
        
        
        column(width = 3,
               tags$div(class = "top_cols",
                        tags$img(class = "top_img", src="Asset 1PSEG.svg"),
                        tags$h4(class = "last_month", textOutput("last")),
                        tags$h4(class = "last_month2", textOutput("crime_total")),
                        tags$h4(class = "last_month", "Carpetas de investigación")
                        )
               ),
        
        column(width = 3,
               tags$div(class = "top_cols",
                        tags$img(class = "top_img", src="Asset 2PSEG.svg"),
                        tags$h4(class = "last_month", "Tasa"),
                       tags$h4(class = "last_month2", textOutput("crime_rate")),
                       tags$h4(class = "last_month", "Carpetas/100 mil habitantes")
                       )
               ),
        
        column(width = 3,
               tags$div(class = "top_cols",
                        tags$img(class = "top_img", src="Asset 3PSEG.svg"),
                        tags$h4(class = "last_month", "Promedio (12 meses previos)"),
                       tags$h4(class = "last_month2", textOutput("avg")),
                       tags$h4(class = "last_month", textOutput("period"))
                       )
               ),
        
        column(width = 3,
               tags$div(class = "top_cols",
                        tags$img(class = "top_img", src="Asset 4PSEG.svg"),
                       tags$h4(class = "last_month", "Delito con mayor tasa"),
                       tags$h4(class = "last_month", textOutput("crime_highest")),
                       tags$h4(class = "last_month2", textOutput("highest_quantity"))
                       )
               )
        ),
  
    fluidRow(id = "filters_row_title",
             column(width = 3,
                    tags$div(class = "title2",
                             tags$h2(class = "title_cols", "LABORATORIO DE DATOS")
                    )
             )
    ),
    
    fluidRow(id = "filters_row",
             
             
        
             column(width = 2, offset = 1,
                    tags$div(class = "filters_cols",
                             uiOutput("goods")
                             )
                    ),
             
             column(width = 2,
                    tags$div(class = "filters_cols",
                             uiOutput("crimes"),
                             #uiOutput("vlnc"),
                             #uiOutput("ntntn")#,
                             # selectInput("tool", multiple = T,
                             #             label = "Instrumento",
                             #             choices = c("Arma de fuego",
                             #                         "Objeto punzocortante",
                             #                         "Explosivos")
                             #             )
                             )
                    ),
             
             column(width = 2,
                    tags$div(class = "filters_cols",
                             uiOutput("dates"),
                             uiOutput("type"),
                             #uiOutput("typ")
                             )
                    ),
             column(width=2,
                    tags$div( class = "graph_cols", id = "filter_mun_area",
                              selectInput("mun_area",
                                          label = "Filtro geográfico",
                                          choices = c("Municipios", "Área Metropolitana"),
                                          selected = c("Municipios"),
                                          multiple = FALSE
                                          ),
                              conditionalPanel("input.mun_area=='Municipios'",
                                               tags$div(class = "filters_cols",
                                                        uiOutput("muns"),
                                                        conditionalPanel(
                                                          condition = "input.mun != ''",
                                                          uiOutput("neighborhoods")
                                                          )
                                                        )
                                               ), 
                              conditionalPanel("input.mun_area=='Área Metropolitana'",
                                               tags$div(class="filters_cols",
                                                        uiOutput("areas")
                                                        )
                                               )
                              )
                    ),
             
             # tags$div(class = "graph_cols", id = "graph_selection",
             #          selectInput("graph_type",
             #                      label = "Tipo de gráfica",
             #                      choices = c("Dispersión", "Barras", "Histograma", "Densidad"),
             #                      selected = "Dispersión"),
             #          conditionalPanel("input.graph_type == 'Dispersión'",
             #                           uiOutput("ui_x_axis")
             #          ),
             #          conditionalPanel("input.graph_type != 'Dispersión'",
             #                           uiOutput("ui_x_axis2")
             #          ),
             #          conditionalPanel("input.graph_type == 'Dispersión'",
             #                           uiOutput("y_axis_ui")
             #          ),
             #          conditionalPanel("input.graph_type == 'Dispersión' & (input.x_axis == 'Fecha' | input.x_axis == 'Mes' | input.x_axis == 'Año')",
             #                           radioButtons("pnts_lns",
             #                                        label = "",
             #                                        choices = c("Puntos", "Puntos + líneas"),
             #                                        selected = "Puntos + líneas")
             #          ),
             #          checkboxInput("zeros",
             #                        label = "Incluir ceros",
             #                        value = F),
             #          conditionalPanel("input.graph_type != 'Barras'",
             #                           checkboxInput("cb_groups",
             #                                         label = "Desglosar",
             #                                         value = F),
             #                           uiOutput("groupslist")
             #          ),
             #          conditionalPanel("input.graph_type == 'Dispersión' & input.cb_groups == false &
             #                                  (input.x_axis == 'Fecha' | input.x_axis == 'Mes')",
             #                           uiOutput("ymeans")
             #          )
             #          
             #          
             # )
             
             column(width = 1,
                    tags$div(class = "filters_cols",
                             
                             )
                    ),
             
             column(width = 1,
                    tags$div(class = "filters_cols",
                             # actionButton("rmv_fltrs",
                             #                "Quitar filtros",
                             #                width = "14rem")
                    )
             )
             
             ),
  
  fluidRow(id = "dwnld_row",
           
           # column(width = 2, 
           #        downloadButton("mun_amg",
           #                     "Comparativo municipal (AMG)",
           #                     width = "20rem")
           # ),
           column(width = 2, offset = 3,
                  
                  actionButton("apply",
                               "Aplicar filtros",
                               icon = icon("play"),
                               width = "14rem")
           ),
           
           column(width = 2,
                  
                  actionButton("refresh",
                               "Reiniciar",
                               icon = icon("refresh"),
                               width = "14rem")
           ),
           
           column(width = 2,
                  downloadButton("dwnld",
                                 "Descargar (.csv)",
                                 width = "14rem")
                  )
           ),
  
    # fluidRow(id = "lab_row_title",
    #          column(width = 3,
    #                 tags$div(class = "title2",
    #                          tags$h2(class = "title_cols",  "Laboratorio de datos")
    #                 )
    #          )
    # ),
     
    
    fluidRow(id = "graph_row",
             
             column(width = 10,
                    tags$div(class = "graph_cols",
                             conditionalPanel("input.dimension[0]>767",
                              plotlyOutput("graph", height = "60rem")
                             )
                             )
                    ),
             
             column(width = 2,
                    tags$div(class = "graph_cols", id = "graph_selection",
                             conditionalPanel("input.dimension[0]>767",
                               selectInput("graph_type",
                                           label = "Tipo de gráfica",
                                           choices = c("Dispersión", "Barras", "Histograma", "Densidad"),
                                           selected = "Dispersión"),
                               conditionalPanel("input.graph_type == 'Dispersión'",
                                                uiOutput("ui_x_axis")
                                                ),
                               conditionalPanel("input.graph_type != 'Dispersión'",
                                                uiOutput("ui_x_axis2")
                                                ),
                               conditionalPanel("input.graph_type == 'Dispersión'",
                                                uiOutput("y_axis_ui")
                                                ),
                               conditionalPanel("input.graph_type == 'Dispersión' & (input.x_axis == 'Fecha' | input.x_axis == 'Mes' | input.x_axis == 'Año')",
                                                radioButtons("pnts_lns",
                                                             label = "",
                                                             choices = c("Puntos", "Puntos + líneas"),
                                                             selected = "Puntos + líneas")
                               ),
                               conditionalPanel("input.graph_type != 'Barras'",  
                                 checkboxInput("zeros",
                                               label = "Incluir ceros",
                                               value = F)
                                 ),
                               conditionalPanel("input.graph_type != 'Barras'",
                                                checkboxInput("cb_groups",
                                                              label = "Desglosar",
                                                              value = F),
                                                uiOutput("groupslist")
                                                ),
                               conditionalPanel("input.graph_type == 'Dispersión' & input.cb_groups == false &
                                                (input.x_axis == 'Fecha' | input.x_axis == 'Mes') & 
                                                input.y_axis == 'Total'",
                                                 uiOutput("ymeans")
                               )
                             )

                             
                    )
             )
             ),

  
    fluidRow(id = "map_row",
             
             column(width = 10,
                    tags$div(class = "map_cols",
                             leafletOutput("map", height = "60rem")
                             )
                    ),
             
             column(width = 2,
                    tags$div(class = "map_cols", id = "map_selection",
                             radioButtons("mapviz",
                                          "Visualización del mapa",
                                          choices = c("Conglomerados",
                                                      "Mapa de calor"),
                                          selected = "Conglomerados"),
                             conditionalPanel(condition = "input.mapviz == 'Conglomerados'",
                                              checkboxInput("map_legend",
                                                            "Mostrar leyenda")
                                              ),
                             conditionalPanel(condition = "input.map_legend",
                                              selectInput("legend_var",
                                                          "Variable",
                                                          c("Delito", "Bien afectado",
                                                            "Día de la semana"),
                                                          "Delito")
                                              )
                             )
                    ),
    ),
    
    fluidRow(id = "test_row",
    )
)
