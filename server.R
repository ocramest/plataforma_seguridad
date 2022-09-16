# Librerías: ----
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(leaflet.extras)
library(plotly)
library(scales)
library(stringr)
library(waiter)
library(pals)
library(shinyjs)
library(zoo)


# Server: ----
function(input, output, session){
  
# Cálculos iniciales ----
  
  pob <- read_rds("data/conapo_mun.rds") %>%
    group_by(mun, year) %>%
    summarise(pob = sum(pob))
  
  data <- read_rds("data/data_prueba.rds") %>%
    dplyr::select(fecha, delito, x, y, colonia, municipio, clave_mun, hora, 
                  violencia, comision, victimas, tipo, weekday) %>%
    mutate(municipio = as.character(municipio),
           colonia = as.character(colonia),
           bien_afectado = 
             case_when(tolower(delito) %in% c("lesiones dolosas", "homicidio doloso",
                                              "feminicidio", "parricidio") ~ "La vida y la integridad corporal",
                       tolower(delito) == "violencia familiar" ~ "La familia",
                       tolower(delito) %in% c("abuso sexual infantil", "violacion") ~ "La libertad y la seguridad sexual",
                       TRUE ~ "El patrimonio"),
           delito = str_to_sentence(delito),
           zona_geografica= case_when(
             municipio %in% c("EL SALTO", "GUADALAJARA", "IXTLAHUACAN DE LOS MEMBRILLOS",
                              "JUANACATLAN", "SAN PEDRO TLAQUEPAQUE", "TLAJOMULCO DE ZUÑIGA",
                              "TONALA", "ZAPOPAN", "ZAPOTLANEJO") ~ "AMG",
             TRUE ~ "Interior"
           )
    ) 
  
  mesEsp <- function(fecha){ 
    case_when(month(fecha) == 1 ~ "Ene", 
              month(fecha) == 2 ~ "Feb", 
              month(fecha) == 3 ~ "Mar", 
              month(fecha) == 4 ~ "Abr", 
              month(fecha) == 5 ~ "May", 
              month(fecha) == 6 ~ "Jun", 
              month(fecha) == 7 ~ "Jul", 
              month(fecha) == 8 ~ "Ago", 
              month(fecha) == 9 ~ "Sep", 
              month(fecha) == 10 ~ "Oct", 
              month(fecha) == 11 ~ "Nov", 
              month(fecha) == 12 ~ "Dic") 
  }
  
  last_month <- month(max(data$fecha))
  last_year <- year(max(data$fecha))
  first_last_day <- min(data$fecha[month(data$fecha)==last_month & year(data$fecha) == last_year])
  first_past_day <- min(data$fecha[month(data$fecha)==last_month & year(data$fecha) == last_year-1])
  pob_jal <- sum(pob$pob[pob$year == last_year], na.rm = T)
  
  data_highest <- data %>%
    filter(month(data$fecha) == last_month & year(data$fecha) == last_year) %>%
    group_by(delito) %>%
    summarise(total = n()) %>%
    ungroup()
  
  data_last_12 <- data %>%
    filter(fecha >= first_past_day & fecha < first_last_day) %>%
    mutate(month_year = paste0(month(fecha),"-", year(fecha))) %>%
    group_by(month_year) %>%
    summarise(total = n()) %>%
    ungroup()
  
# Diseño de filtros ----  
  
  output$dates <- renderUI({
    dateRangeInput("date",
                   label = "Periodo",
                   start = min(data$fecha[month(data$fecha) == last_month & year(data$fecha) == last_year-1]),
                   end = max(data$fecha[month(data$fecha) == last_month & year(data$fecha) == last_year]),
                   min = min(data$fecha),
                   max = max(data$fecha),
                   separator = "-",
                   format = "dd/mm/yyyy",
                   language = "es",
                   weekstart = 1)
  })

  output$goods <- renderUI({
    selectInput("good",
                       label =  "Bien jurídico afectado",
                       choices = sort(unique(data$bien_afectado)),
                multiple = T
    )
  })
  
  output$crimes <- renderUI({
    
    if(is.null(input$good)){
      selectInput("crime", multiple = T,
                  label = "Delitos",
                  choices = sort(unique(data$delito))
      )
    } else {
      selectInput("crime", multiple = T,
                  label = "Delitos",
                  choices = sort(unique(data$delito[data$bien_afectado %in% input$good]))
                    
      )
    }
  })
 

  
  observe({
    input$good
    input$crime
    
    output$vlnc <- renderUI({
      
      conditionalPanel(condition = "input.good == 'El patrimonio' | 
                               (!input.crime.some(r=>['Homicidio doloso', 'Violencia familiar',
                               'Lesiones dolosas', 'Violacion', 'Abuso sexual infantil',
                               'Feminicidio'].includes(r)) & input.crime != '')",
                       
                       checkboxGroupInput("violence",
                                          label = "Forma de acción",
                                          choices = c("Con violencia", "Sin violencia")
                       )
      )
      
    })
    
    output$ntntn <- renderUI({
      
      conditionalPanel(condition = "input.good == 'La vida y la integridad corporal' | 
                             (!input.crime.some(r=>['Robo a negocio', 'Robo de autopartes',
                                              'Robo casa habitacion', 'Robo a persona',
                                              'Violencia familiar', 'Abuso sexual infantil',
                                              'Robo a vehiculos particulares', 'Robo a int de vehiculos',
                                              'Robo a carga pesada', 'Robo de motocicleta',
                                              'Robo a cuentahabientes', 'Violacion',
                                              'Robo a bancos'].includes(r)) & input.crime != '')",
                       
                       checkboxGroupInput("intention",
                                          label = "Comisión",
                                          choices = c("Dolosa", "Culposa")
                       )
      )
      
    })
    
    
    # output$type <- renderUI({
    #   
    #   conditionalPanel(condition = "input.good == 'La vida y la integridad corporal' | 
    #                          (!input.crime.some(r=>['Robo a negocio', 'Robo de autopartes',
    #                                           'Robo casa habitacion', 'Robo a persona',
    #                                           'Violencia familiar', 'Abuso sexual infantil',
    #                                           'Robo a vehiculos particulares', 'Robo a int de vehiculos',
    #                                           'Robo a carga pesada', 'Robo de motocicleta',
    #                                           'Robo a cuentahabientes', 'Violacion',
    #                                           'Robo a bancos'].includes(r)) & input.crime != '')",
    #                    
    #                    selectInput("class",
    #                                label = "Tipo",
    #                                choices = c("Carpetas",
    #                                            "Víctimas (sólo gráfica)"),
    #                                selected = "Carpetas")
    #   )
    #   
    # })
    # 
    # output$typ <- renderUI({
    #   
    #   conditionalPanel(condition = "input.crime == 'Homicidio doloso'",
    #                    
    #                    checkboxGroupInput("typ3",
    #                                       label = "Situación",
    #                                       choices = c("Agresión directa", "Fosa")
    #                                       
    #                    )
    #   )
    #   
    # })
    
  })

  
  
  output$muns <- renderUI({
    
    selectInput("mun", multiple = T,
                label = "Municipio",
                choices = sort(unique(data$municipio)),
                selected = NULL
    )
  })
  
  output$areas <- renderUI({
    selectInput("area", multiple = T,
                label = "Zona Geográfica",
                choices = c("AMG", "Interior"),
                selected = NULL
    )
  })
  
  output$neighborhoods <- renderUI({
    selectInput("neighborhood", multiple = T, 
                label = "Colonia",
                choices = sort(unique(data$colonia[data$municipio %in% input$mun]))
    )
  })
  
  output$y_axis_ui <- renderUI({
    inputs <- if(length(unique(filtered()$municipio))==1){
      c("Fecha",
        "Mes", "Año",
        "Municipio", "Colonia",
        "Bien afectado", "Delito","Total")
    } else {
      c("Fecha",
        "Mes", "Año",
        "Municipio","Bien afectado", "Delito",
        "Total")
    }
    selectInput("y_axis",
                label = "Eje y",
                choices =  inputs[inputs != input$x_axis],
                selected = ifelse(input$x_axis != "Total", "Total", "Fecha")
    )
  })
  
  # Este eje es para el gráfico de dispersión (líneas/puntos)
  output$ui_x_axis <- renderUI({
    selectInput("x_axis",
                label = "Eje x",
                choices = if(length(unique(filtered()$municipio))==1){
                  c("Fecha", 
                    "Mes", "Año",
                    "Municipio", "Colonia",
                    "Bien afectado", "Delito",
                    "Total")} else {
                      c("Fecha",  
                        "Mes", "Año",
                        "Municipio",
                        "Bien afectado", "Delito",
                        "Total")}
                ,
                selected = ifelse(input$graph_type!="Dispersión", input$x_axis2, "Mes")) # Lo que queremos es que se mantenga la selección de la variable cuando cambiemos de tipo de gráfico
  })
  
  # Este eje es para los gráficos de barras, histograma y densidad
  output$ui_x_axis2 <- renderUI({
    selectInput("x_axis2",
                label = "Agrupación",
                choices = if(length(unique(filtered()$municipio))==1){
                  c("Fecha", "Mes", "Año",
                    "Municipio", "Zona geográfica", "Colonia",
                    "Bien afectado", "Delito")} else {
                      c("Fecha", "Mes", "Año",
                        "Municipio", "Zona geográfica",
                        "Bien afectado", "Delito")}
                ,
                selected = ifelse(input$graph_type=="Dispersión", input$x_axis, "Mes"))
  })
  
 
  
  
  
    
    
    
    output$ymeans <- renderUI({
      
      checkboxInput("means",
                    label = "Incluir promedios anuales",
                    value = if(input$graph_type != "Dispersión" | input$cb_groups | !(input$x_axis %in% c("Fecha", "Mes")) |
                                                                                      input$y_axis != "Total"
                               ){
                      FALSE
                    } else { TRUE }
      )
    })
 
 
  

# Elementos de la barra superior: ----
  
  output$last <- renderText({
    str_to_sentence(paste(mesEsp(month(last_month)), last_year))
  })
  
  output$period <- renderText({
    paste(str_to_sentence(mesEsp(month(last_month))), last_year-1,
                          "-",
          ifelse(last_month==1,paste(str_to_sentence(mesEsp(month(12))),last_year-1),
            paste(str_to_sentence(mesEsp(month(last_month-1))), last_year))
    )
  })
  
  output$crime_total <- renderText({
      comma(nrow(data[month(data$fecha) == last_month & year(data$fecha) == last_year,]))
  })
  
  output$crime_rate <- renderText({
    round(nrow(data[month(data$fecha) == last_month & year(data$fecha) == last_year,])*100000/pob_jal)
  })
  
  output$avg <- renderText({
    comma(round(mean(data_last_12$total)))
  })
  
  output$crime_highest <- renderText({
    str_to_sentence(data_highest$delito[which.max(data_highest$total)])
  })
  
  output$highest_quantity <- renderText({
    round(max(data_highest$total)*100000/pob_jal)
  })
  
  
  waiter_hide()


  
# Base filtrada: ----

delay(50, click("apply"))
  
filtered <- eventReactive(input$apply, {
  
  
  data %>%
    filter(fecha >= min(input$date), fecha <= max(input$date),
           if(!is.null(input$good)) bien_afectado %in% input$good else bien_afectado != "",
           if(!is.null(input$crime)) delito %in% input$crime else delito != "",
           if(!is.null(input$violence)) violencia %in% input$violence else violencia %in% violencia,
           if(!is.null(input$intention)) comision %in% input$intention else comision %in% comision,
           #if(!is.null(input$typ3)) tipo %in% input$typ3 else tipo %in% tipo,
           
           
           if(input$mun_area=="Área Metropolitana" & !is.null(input$area)) zona_geografica %in% input$area else zona_geografica != "",
           if(input$mun_area=="Municipios" & !is.null(input$mun)) municipio %in% input$mun else municipio != "",
           if(input$mun_area=="Municipios"&!is.null(input$neighborhood)) colonia %in% input$neighborhood else colonia != "")
  
  
})

output$groupslist <- renderUI({
  grouplist <- c("Delito", "Bien afectado",
                 "Municipio", "Zona geográfica",
                 "Día de la semana",
                 "Mes del año","Año")
  
  if(length(unique(filtered()$mun))==1){
    grouplist <- c(grouplist, "Colonia")
  }
  
  
  
  conditionalPanel(condition = "input.cb_groups == true",
                   selectInput("groups",
                               label = "",
                               choices = if(input$graph_type == "Dispersión"){
                                 grouplist[which(grouplist!=input$x_axis & grouplist != input$y_axis)]} else{
                                   grouplist[which(grouplist!=input$x_axis2 & grouplist != input$y_axis)]},
                               selected = if(input$graph_type == "Dispersión"){
                                 grouplist[which(grouplist!=input$x_axis & grouplist != input$y_axis)][1]} else {
                                   grouplist[which(grouplist!=input$x_axis2 & grouplist != input$y_axis)][1]})
  )
})

filtered_means <- eventReactive(input$apply, {
  
  
  data %>%
    filter(if(!is.null(input$good)) bien_afectado %in% input$good else bien_afectado != "",
           if(!is.null(input$crime)) delito %in% input$crime else delito != "",
           if(!is.null(input$violence)) violencia %in% input$violence else violencia %in% violencia,
           if(!is.null(input$intention)) comision %in% input$intention else comision %in% comision,
           #if(!is.null(input$typ3)) tipo %in% input$typ3 else tipo %in% tipo,
           
           
           if(input$mun_area=="Área Metropolitana" & !is.null(input$area)) zona_geografica %in% input$area else zona_geografica != "",
           if(input$mun_area=="Municipios" & !is.null(input$mun)) municipio %in% input$mun else municipio != "",
           if(input$mun_area=="Municipios"&!is.null(input$neighborhood)) colonia %in% input$neighborhood else colonia != "")
})
  
# filtered <- reactive({
#   
# 
#     data %>%
#     filter(fecha >= min(input$date), fecha <= max(input$date),
#            if(!is.null(input$good)) bien_afectado %in% input$good else bien_afectado != "",
#            if(!is.null(input$crime)) delito %in% input$crime else delito != "",
#            if(!is.null(input$mun)) municipio %in% input$mun else municipio != "",
#            if(!is.null(input$neighborhood)) colonia %in% input$neighborhood else colonia != "",
#            if(!is.null(input$violence)) violencia %in% input$violence else violencia %in% violencia,
#            if(!is.null(input$intention)) comision %in% input$intention else comision %in% comision,
#            #if(!is.null(input$typ3)) tipo %in% input$typ3 else tipo %in% tipo,
#            if(!is.null(input$area)) zona_geografica %in% input$area else zona_geografica != "")
# 
#   
# })
#   
# filtered_means <- reactive({
#     
#     
#     data %>%
#       filter(if(!is.null(input$good)) bien_afectado %in% input$good else bien_afectado != "",
#              if(!is.null(input$crime)) delito %in% input$crime else delito != "",
#              if(!is.null(input$mun)) municipio %in% input$mun else municipio != "",
#              if(!is.null(input$neighborhood)) colonia %in% input$neighborhood else colonia != "",
#              if(!is.null(input$violence)) violencia %in% input$violence else violencia %in% violencia,
#              if(!is.null(input$intention)) comision %in% input$intention else comision %in% comision,
#              #if(!is.null(input$typ3)) tipo %in% input$typ3 else tipo %in% tipo,
#              if(!is.null(input$area)) zona_geografica %in% input$area else zona_geografica != "")
#     
#   
#     
#   })  


  
# Botones: ----
  


  # Reiniciar:
  
  observeEvent(input$refresh, {
    session$reload();
  })
  

  # Quitar filtros:
  
  # observeEvent(input$rmv_fltrs, {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'Filtros borrados')
  #   
  #   output$dates <- renderUI({
  #     dateRangeInput("date",
  #                    label = "Periodo",
  #                    start = min(data$fecha[month(data$fecha) == last_month & year(data$fecha) == last_year-1]),
  #                    end = max(data$fecha[month(data$fecha) == last_month & year(data$fecha) == last_year]),
  #                    min = min(data$fecha),
  #                    max = max(data$fecha),
  #                    separator = "-",
  #                    format = "dd/mm/yyyy",
  #                    language = "es",
  #                    weekstart = 1)
  #   })
  #   
  #   output$goods <- renderUI({
  #     selectInput("good",
  #                        label =  "Bien jurídico afectado",
  #                        choices = unique(data$bien_afectado),
  #                 multiple = T
  #     )
  #   })
  #   
  #   output$crimes <- renderUI({
  #     
  #     if(is.null(input$good)){
  #       selectInput("crime", multiple = T,
  #                   label = "Delitos",
  #                   choices = unique(tolower(data$delito))
  #       )
  #     } else {
  #       selectInput("crime", multiple = T,
  #                   label = "Delitos",
  #                   choices = unique(tolower(data$delito[data$bien_afectado %in% input$good]))
  #                   
  #       )
  #     }
  #   })
  #   
  #   output$muns <- renderUI({
  #     selectInput("mun", multiple = T,
  #                 label = "Municipio",
  #                 choices = unique(data$municipio),
  #                 selected = NULL
  #     )
  #   })
  #   
  #   output$neighborhoods <- renderUI({
  #     selectInput("neighborhood", multiple = T, 
  #                 label = "Colonia",
  #                 choices = unique(data$colonia[data$municipio %in% input$mun])
  #     )
  #   })
  #   
  #   
  #   updateCheckboxGroupInput(session,
  #                            inputId = "violence",
  #                            label = "Forma de acción",
  #                            choices = c("Con violencia", "Sin violencia")
  #   )
  #   
  #   
  #   updateCheckboxGroupInput(session,
  #                            inputId = "intention",
  #                            label = "Comisión",
  #                            choices = c("Dolosa", "Culposa")
  #   )
  #   
  #   updateSelectInput(session,
  #                     inputId = "tool",
  #                     label = "Instrumento",
  #                     choices = c("Arma de fuego",
  #                                 "Objeto punzocortante",
  #                                 "Explosivos"))
  #   
  #   updateSelectInput(session,
  #                     "class",
  #                     label = "Tipo",
  #                     choices = c("Carpetas",
  #                                 "Víctimas"),
  #                     selected = "Carpetas")
  #   
  # })
  
  
  # Descargar:
  
  output$dwnld <- downloadHandler(
    
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file){
      
      filtered() %>%
        select(-c(violencia, comision, victimas, tipo, weekday)) %>%
        write_excel_csv(file)
    }
  )
  

  
  #visualizaciones predefinidas
  
  # output$mun_amg <- downloadHandler(
  #   graph_1 <- filtered() %>% filter(zona_geografica=="AMG") %>%
  #     group_by(municipio) %>%
  #     summarise(Total=n()) %>%
  #     ggplot() + geom_bar(stat = "identity",
  #                         aes(x=reorder(municipio, -Total), 
  #                             y=Total, fill=municipio)) +
  #     geom_text(aes(x=reorder(municipio, -Total),
  #                   y=Total, label=comma(Total)),
  #               vjust=1, size=1.8) +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #           legend.position = "none") +
  #     labs(x="Municipio"),
  #   
  #   filename = function() {
  #     "comparativa_municipal_AMG.png"
  #   },
  #   
  #   content = function(file) {
  # 
  #       device <- function(..., width, height) {
  #         grDevices::png(..., width = width, height = height,
  #                        res = 300, units = "in")
  #       }
  #       ggsave(plot = graph_1, device = device, 
  #              filename = "comparativa_municipal_AMG.png")
  #   })

  
  # observeEvent(input$mun_amg, {
  #   input$graph_type <- "Barras"
  #   output$x_axis2 <- "Municipio"
  #   filtered() <- filtered() %>% 
  #     filter(zona_geografica="AMG")
  #   # filtered() %>% filter(zona_geografica="AMG") %>%
  #   #   group_by(municipio) %>%
  #   #   summarise(Total=n()) %>%
  #   #   ggplot() + geom_bar(stat = "identity",
  #   #                       aes(x=municipio, y=Total, fill=municipio)) +
  #   #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })
  
  
# Gráfica: ----
  
  output$graph <- renderPlotly({
    
    
    if(nrow(filtered())==0){
      graph <- ggplot() +
        annotate(geom="text", x=2, y=2,
                 label="No existen datos para los parámetros seleccionados",
                 size=4) +
        theme_void()
    } else {

    
    show_modal_spinner()
    
    xaxis <- case_when(input$x_axis == "Fecha" ~ "date",
                       input$x_axis == "Mes" ~ "month",
                       input$x_axis == "Año" ~ "year",
                       input$x_axis == "Municipio" ~ "mun",
                       input$x_axis == "Colonia" ~ "neighborhood",
                       input$x_axis == "Bien afectado" ~ "good",
                       input$x_axis == "Delito" ~ "crime",
                       input$x_axis == "Total" ~ "total"
    )
    
    x2axis <- case_when(input$x_axis2 == "Fecha" ~ "date",
                        input$x_axis2 == "Mes" ~ "month",
                        input$x_axis2 == "Año" ~ "year",
                        input$x_axis2 == "Municipio" ~ "mun",
                        input$x_axis2 == "Colonia" ~ "neighborhood",
                        input$x_axis2 == "Bien afectado" ~ "good",
                        input$x_axis2 == "Delito" ~ "crime",
                        input$x_axis2 == "Total" ~ "total",
                        input$x_axis2 == "Zona geográfica" ~ "zona_geografica"
    )
    
    yaxis <- case_when(input$y_axis == "Fecha" ~ "date",
                       input$y_axis == "Mes" ~ "month",
                       input$y_axis == "Año" ~ "year",
                       input$y_axis == "Municipio" ~ "mun",
                       input$y_axis == "Colonia" ~ "neighborhood",
                       input$y_axis == "Bien afectado" ~ "good",
                       input$y_axis == "Delito" ~ "crime",
                       input$y_axis == "Total" ~ "total"
    )
    
    group <- case_when(input$groups == "Delito" ~ "crime",
                       input$groups == "Bien afectado" ~ "good",
                       input$groups == "Municipio" ~ "mun",
                       input$groups == "Zona geográfica" ~ "zona_geografica",
                       input$groups == "Colonia" ~ "neighborhood",
                       input$groups == "Día de la semana" ~ "weekday",
                       input$groups == "Mes del año" ~ "month_of_year",
                       #input$groups == "Situación" ~ "type",
                       input$groups == "Año" ~ "year2"
                       
    )
    
    if (input$graph_type=="Dispersión") {
      
      if(input$cb_groups==T){
        
        groups <- c(xaxis, yaxis, group)
        
      } else {
        
        groups <- c(xaxis, yaxis)
        
      } 
      
    } else if ((input$graph_type=="Histograma" | input$graph_type == "Densidad") & input$cb_groups) {
      
      groups <- c(x2axis, group)
      
    } else {
      
      groups <- c(x2axis)
      
    }
    
    
    base <- filtered() %>%
      select(-c("x", "y", "clave_mun")) %>%
      mutate(month_of_year = month(fecha, label = T, abbr = F),
             year = year(fecha),
             year2 = as.character(factor(year(fecha))),
             # weekday = factor(weekdays(fecha, F), 
             #                  levels = c("lunes", "martes", "miércoles",
             #                             "jueves", "viernes", "sábado", "domingo")),
             month = ymd(paste0(year, "-", month_of_year, "-", "01")
             )
      ) %>%
      setNames(c("date", "crime", "neighborhood", "mun",
                 "time", "violence", "com", "victims",
                 "type","weekday", "good", "zona_geografica", "month_of_year", "year", "year2",
                 "month"))  %>%
      group_by_at(groups[groups!="total"]) %>% 
      #summarise(total= ifelse(input$class == "Carpetas", n(), sum(victims)))
      summarise(total = n())
    
    if(input$zeros==T){
      
      if(input$cb_groups & input$graph_type != "Barras"){
        
        if("date" %in% groups){
          if(!("year2" %in% groups)){
            cmplt <- base %>%
              ungroup() %>%
              expand(date = seq.Date(from = min(input$date), to = max(input$date), by = "day"),
                     .data[[groups[which(groups!="total"&groups!="date")][1]]]) %>%
              mutate(comp = 0) %>%
              full_join(base) %>%
              mutate(total = ifelse(is.na(total), 0, total)) %>%
              select(-comp) 
          } else {
            cmplt <- base %>%
              ungroup() %>%
              expand(date = seq.Date(from = min(input$date), to = max(input$date), by = "day"),
                     year2 = levels(factor(seq(year(min(input$date)), year(max(input$date)), by = 1)))) %>%
              mutate(comp = 0) %>%
              full_join(base) %>%
              mutate(total = ifelse(is.na(total), 0, total)) %>%
              select(-comp)
          }
        } else if("month" %in% groups){
          if(!("year2" %in% groups)){
            cmplt <- base %>%
              ungroup() %>%
              expand(month = seq.Date(ymd(paste0(year(min(input$date)), "-", month(min(input$date)), "-", "01")),
                                      ymd(paste0(year(max(input$date)), "-", month(max(input$date)), "-", "01")),
                                      by = "month"),
                     .data[[groups[which(groups!="total"&groups!="month")][1]]]) %>%
              mutate(comp = 0) %>%
              full_join(base) %>%
              mutate(total = ifelse(is.na(total), 0, total)) %>%
              select(-comp)
          } else {
            cmplt <- base %>%
              ungroup() %>%
              expand(month = seq.Date(ymd(paste0(year(min(input$date)), "-", month(min(input$date)), "-", "01")),
                                      ymd(paste0(year(max(input$date)), "-", month(max(input$date)), "-", "01")),
                                      by = "month"),
                     year2 = levels(factor(seq(year(min(input$date)), year(max(input$date)), by = 1)))) %>%
              mutate(comp = 0) %>%
              full_join(base) %>%
              mutate(total = ifelse(is.na(total), 0, total)) %>%
              select(-comp)
          }
        } else if("year" %in% groups){
          cmplt <- base %>%
            ungroup() %>%
            expand(year = seq(year(min(input$date)), year(max(input$date)), by = 1),
                   .data[[groups[which(groups!="total"&groups!="year")][1]]]) %>%
            mutate(comp = 0) %>%
            full_join(base) %>%
            mutate(total = ifelse(is.na(total), 0, total)) %>%
            select(-comp)
        } else if("year2" %in% groups){
          cmplt <- base %>%
            ungroup() %>%
            expand(year2 = levels(factor(seq(year(min(input$date)), year(max(input$date)), by = 1))),
                   .data[[groups[which(groups!="total"&groups!="year2")][1]]]) %>%
            mutate(comp = 0) %>%
            full_join(base) %>%
            mutate(total = ifelse(is.na(total), 0, total)) %>%
            select(-comp)
        } else {
          cmplt <- base %>%
            ungroup() %>%
            expand(.data[[groups[which(groups!="total")][1]]],
                   .data[[groups[which(groups!="total")][2]]]) %>%
            mutate(comp = 0) %>%
            full_join(base) %>%
            mutate(total = ifelse(is.na(total), 0, total)) %>%
            select(-comp)
        }
        
      } else {
        
        if("date" %in% groups){
          cmplt <- base %>%
            ungroup() %>%
            expand(date = seq.Date(from = min(input$date), to = max(input$date), by = "day")) %>%
            mutate(comp = 0) %>%
            full_join(base) %>%
            mutate(total = ifelse(is.na(total), 0, total)) %>%
            select(-comp)
        } else if("month" %in% groups){
          cmplt <- base %>%
            ungroup() %>%
            expand(month = seq.Date(ymd(paste0(year(min(input$date)), "-", month(min(input$date)), "-", "01")),
                                    ymd(paste0(year(max(input$date)), "-", month(max(input$date)), "-", "01")),
                                    by = "month")) %>%
            mutate(comp = 0) %>%
            full_join(base) %>%
            mutate(total = ifelse(is.na(total), 0, total)) %>%
            select(-comp)
        } else if("year" %in% groups){
          cmplt <- base %>%
            ungroup() %>%
            expand(year = seq(year(min(input$date)), year(max(input$date)), by = 1)) %>%
            mutate(comp = 0) %>%
            full_join(base) %>%
            mutate(total = ifelse(is.na(total), 0, total)) %>%
            select(-comp)
        } else {
          cmplt <- base %>%
            ungroup() %>%
            expand(.data[[groups[which(groups!="total")][1]]]) %>%
            mutate(comp = 0) %>%
            full_join(base) %>%
            mutate(total = ifelse(is.na(total), 0, total)) %>%
            select(-comp)
        }
        
      }
    } else{
      cmplt <- base
    }

    
    
    # Gráfica:
    
    # Si es de dispersión: solo los promedios aparecerán en este gráfico
    if(input$graph_type=="Dispersión"){
      
      # Si hay desglose: nota: los promedios no aparecerán en el desgloce
      
      if(input$cb_groups){
        
        graph <- ggplot(cmplt,
                        aes_string(x=xaxis, y=yaxis, color = group)
        )
        # Si no hay desglose: solo aquí van a aparecer los promedios anuales. 
        
      } else {
        if(input$means==F){
        
        graph <- ggplot(cmplt,
                        aes_string(x=xaxis, y=yaxis)
        )
        } else if(input$means==T){
          if(xaxis=="month"){
            base_means <- filtered_means() %>%
              select(-c("x", "y", "clave_mun")) %>%
              mutate(month_of_year = month(fecha, label = T, abbr = F),
                     year = year(fecha),
                     month = ymd(paste0(year, "-", month_of_year, "-", "01")
                     )
              ) %>%
              setNames(c("date", "crime", "neighborhood", "mun",
                         "time", "violence", "com", "victims",
                         "type","weekday", "good", "zona_geografica", "month_of_year", "year",
                         "month"))  %>%
              #group_by(month, year) %>% 
              #summarise(total= ifelse(input$class == "Carpetas", n(), sum(victims))) %>%
              #summarise(total = n()) %>%
              group_by(year) %>%
              summarise(promedio=n()/12
                ) 
              
            graph <- ggplot(cmplt,
                            aes_string(x=xaxis, y=yaxis)
                            ) 
                              
            if (any(base_means$year==2017)) { 
                                graph <- graph + geom_hline(data=base_means %>% filter(year==2017),
                                         aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),
                                             colour="2017"#, colour = "2017"
                                             ),
                                         linetype="longdash"#,colour="#440154FF"
                                        )
                                } 
            if (any(base_means$year==2018)) {
                                  graph <- graph + geom_hline(data=base_means %>% filter(year==2018),
                                        aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),
                                            colour="2018"#, colour = "2018"
                                            ),
                                         linetype="longdash"#,colour="#39568CFF"
                                        )
                                } 
            if (any(base_means$year==2019)) {
                                    
                                  graph <- graph + geom_hline(data=base_means %>% filter(year==2019),
                                        aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),#, colour = "2019"
                                            colour="2019"),#, colour="#1F968BFF",
                                        linetype="longdash"
                                        ) 
                                  } 
            if  (any(base_means$year==2020)) {
                                    graph <- graph + geom_hline(data=base_means %>% filter(year==2020),
                                        aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),#, colour = "2020"
                                            colour="2020"), 
                                         linetype="longdash"#, colour="#73D055FF"
                                    )
            }
            if  (any(base_means$year==2021)) {
              graph <- graph + geom_hline(data=base_means %>% filter(year==2021),
                                          aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),#, colour = "2020"
                                              colour="2021"), 
                                          linetype="longdash"#, colour="#73D055FF"
              )
            }
            graph <- graph +
              scale_color_manual(values = c("#1F968BFF", "#39568CFF", "#440154FF", "#00953B", "#C2B20E")) +
              labs(colour="")
              
              
            
          } else if(xaxis=="date"){
            base_means <- filtered_means() %>%
              select(-c("x", "y", "clave_mun")) %>%
              mutate(month_of_year = month(fecha, label = T, abbr = F),
                     year = year(fecha),
                     month = ymd(paste0(year, "-", month_of_year, "-", "01")
                     )
              ) %>%
              setNames(c("date", "crime", "neighborhood", "mun",
                         "time", "violence", "com", "victims",
                         "type","weekday", "good","zona_geografica", "month_of_year", "year",
                         "month")) %>%
              #summarise(total= ifelse(input$class == "Carpetas", n(), sum(victims))) %>%
              group_by(year) %>%
              summarise(promedio=n()/365.25) 
            
            graph <- ggplot(cmplt,
                            aes_string(x=xaxis, y=yaxis)
            ) 
            
            if (any(base_means$year==2017)) { 
              graph <- graph + geom_hline(data=base_means %>% filter(year==2017),
                                          aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),
                                              colour="2017"#, colour = "2017"
                                          ),
                                          linetype="longdash"#,colour="#440154FF"
              )
            } 
            if (any(base_means$year==2018)) {
              graph <- graph + geom_hline(data=base_means %>% filter(year==2018),
                                          aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),
                                              colour="2018"#, colour = "2018"
                                          ),
                                          linetype="longdash"#,colour="#39568CFF"
              )
            } 
            if (any(base_means$year==2019)) {
              
              graph <- graph + geom_hline(data=base_means %>% filter(year==2019),
                                          aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),#, colour = "2019"
                                              colour="2019"),#, colour="#1F968BFF",
                                          linetype="longdash"
              ) 
            } 
            if  (any(base_means$year==2020)) {
              graph <- graph + geom_hline(data=base_means %>% filter(year==2020),
                                          aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),#, colour = "2020"
                                              colour="2020"), 
                                          linetype="longdash"#, colour="#73D055FF"
              )
            } 
            if  (any(base_means$year==2021)) {
              graph <- graph + geom_hline(data=base_means %>% filter(year==2021),
                                          aes(yintercept = promedio, text=paste(round(promedio, 2), "\n",year),#, colour = "2020"
                                              colour="2021"), 
                                          linetype="longdash"#, colour="#73D055FF"
              )
            }
            graph <- graph +
              scale_color_manual(values = c("#1F968BFF", "#39568CFF", "#440154FF", "#00953B", "#C2B20E")) +
              labs(colour="")
            
          }
          
        }
                
      }
      
      # Si en los ejes se elige "Mes":
      # p
      if(xaxis=="month"){
        
        graph <- graph +
          scale_x_date(date_breaks = "1 month",
                       date_labels = "%m %Y")
        
        
      } else if (yaxis=="month"){
        
        graph <- graph +
          scale_y_date(date_breaks = "1 month" ,
                       date_labels = "%m %Y")
        
      }
      
      # Si en los ejes se elige "Año" y no se incluyen los ceros:
      
      if (xaxis=="year" & input$zeros==FALSE){
        
        graph <- graph +
          scale_x_continuous(breaks=seq(min(cmplt$year), max(cmplt$year), 1))
        
      } else if (yaxis=="year" & input$zeros==FALSE){
        
        graph <- graph +
          scale_y_continuous(breaks=seq(min(cmplt$year), max(cmplt$year), 1))
      }
      
      # Si se elige incluir líneas:
      
      if (input$pnts_lns == "Puntos + líneas" & (xaxis=="year" | xaxis=="date" | xaxis=="month")){
        
        graph <- graph +
          geom_line(size=.35)
        
      }
      
      # Jitter o puntos, dependiendo del desglose y de las variables de tiempo:
      
      if (input$cb_groups == T){
        
        if (xaxis=="year" | xaxis=="date"){
          graph <- graph +
            geom_point(size=1.5, alpha=.35, 
                       aes(text = paste(.data[[group]],
                                        "<br>", .data[[yaxis]],
                                        "<br>", .data[[xaxis]])
                       )
            )
          
        } else if (xaxis=="month"){
          graph <- graph +
            geom_point(size=1.5, alpha=.35, 
                       aes(text = paste(.data[[group]],
                                        "<br>", .data[[yaxis]],
                                        "<br>", 
                                        paste0(year(.data[[xaxis]]), "-",
                                               mesEsp(month(.data[[xaxis]]))
                       )
                       )
            ))
        } else {
          
          graph <- graph +
            geom_jitter(size=1.5, alpha=.35,
                        width = 0.10, height = 0, 
                        aes(text = paste(.data[[group]],
                                         "<br>", .data[[yaxis]],
                                         "<br>", .data[[xaxis]])
                        ))
        }
        
      } else {
        
        if(xaxis != "month"){
        
          graph <- graph +
            geom_point(size=1.5, color = "#FBBB27", 
                       aes(text = paste(.data[[yaxis]],
                                        "<br>", .data[[xaxis]]
                       )
                       )
            )
          
        } else {
          graph <- graph +
            geom_point(size=1.5, color = "#FBBB27", 
                       aes(text = paste(.data[[yaxis]],
                                        "<br>", 
                                        paste0(year(.data[[xaxis]]), "-",
                                               mesEsp(month(.data[[xaxis]]))
                       )
                       )
            ))
        }
      }
      
      # Ajustes finales:
      
      graph <- graph +
        labs(x = input$x_axis,
             y = input$y_axis,
             color = "") +
        theme(axis.text = element_text(size = 8, color = 'black'),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title = element_text(size = 13, color = 'black', face = 'bold'),
              panel.grid.major.y  = element_line(colour = "grey", size = .001,
                                                 linetype = "dotted"),
              panel.grid.minor.y  = element_blank(),
              panel.grid.major.x = element_line(colour = "grey", size = .001,
                                                linetype = "dotted"),
              panel.grid.minor.x = element_blank(),
              panel.background = element_rect(fill = 'ghostwhite'),
              legend.key = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
              legend.background = element_rect(fill = 'ghostwhite'),
              legend.title = element_text(size = 11, 
                                          color = 'black',
                                          face = 'bold',
                                          hjust = .5),
              legend.text = element_text(size = 9.5, 
                                         color = 'black'),
              plot.background = element_rect(fill = "ghostwhite"),
              axis.line = element_line(colour = 'grey'), 
              axis.ticks = element_line(colour = 'grey')
        )
      
      # Si el gráfico es histograma:
      
    } else if (input$graph_type=="Histograma"){
      
      graph <- ggplot(cmplt, aes(x=total))
      
      # Si se desglosa:
      
      if (input$cb_groups== T){
        
        graph <- graph + 
          geom_histogram(aes_string(fill=group, text = group), alpha=.25, position = "identity"
          )
        
        
        # Si no se desglosa:
        
      } else {
        
        graph <- graph +
          geom_histogram(fill="#FBBB27", alpha=.25, position = "identity"
          )
        
        
      }
      
      
      # Ajustes finales:
      
      graph <- graph +
        labs(
          #x = ifelse(input$class == "Carpetas", "Carpetas de investigación", "Víctimas"),
          x = "Carpetas de investigación", 
             y = paste0("Frecuencia (", input$x_axis2, ")"),
             fill = "") +
        theme(axis.text = element_text(size = 8, color = 'black'),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title = element_text(size = 13, color = 'black', face = 'bold'),
              panel.grid.major.y  = element_line(colour = "grey", size = .001,
                                                 linetype = "dotted"),
              panel.grid.minor.y  = element_blank(),
              panel.grid.major.x = element_line(colour = "grey", size = .001,
                                                linetype = "dotted"),
              panel.grid.minor.x = element_blank(),
              panel.background = element_rect(fill = 'ghostwhite'),
              legend.key = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
              legend.background = element_rect(fill = 'ghostwhite'),
              legend.title = element_text(size = 11, 
                                          color = 'black',
                                          face = 'bold',
                                          hjust = .5),
              legend.text = element_text(size = 9.5, 
                                         color = 'black'),
              plot.background = element_rect(fill = "ghostwhite"),
              axis.line = element_line(colour = 'grey'), 
              axis.ticks = element_line(colour = 'grey')
        )
      
      # Si es gráfica de barras:
      
    } else if (input$graph_type=="Barras"){
      
      if (x2axis=="month"){
        
        graph <- ggplot(cmplt, aes_string(x=x2axis)) +
          geom_bar(stat = "identity", 
                   aes(y=total, fill = factor(as.yearmon(.data[[x2axis]])),
                       text = paste(total,
                                    "<br>", 
                                    paste0(year(.data[[x2axis]]), "-",
                                           mesEsp(month(.data[[x2axis]]
                                           ))
                                    )
                       )
                   )
          ) +
          scale_x_date(date_breaks = "1 month", 
                       date_labels = "%m %Y") #+
          #theme(legend.position = "none")
        
      } else {
        graph <- ggplot(cmplt, aes_string(x=x2axis)) +
          geom_bar(stat = "identity", 
                   aes(y=total, fill = factor(.data[[x2axis]]),
                       text = paste(total,
                                    "<br>", .data[[x2axis]]
                       )
                   )
          )
      } 
      
      
      
      # Ajustes finales:
      
      graph <- graph +
        labs(x = input$x_axis2,
             #y = ifelse(input$class == "Carpetas", "Carpetas de investigación", "Víctimas"),
             y = "Carpetas de investigación",
             fill = "") +
        theme(axis.text = element_text(size = 8, color = 'black'),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title = element_text(size = 13, color = 'black', face = 'bold'),
              panel.grid.major.y  = element_line(colour = "grey", size = .001,
                                                 linetype = "dotted"),
              panel.grid.minor.y  = element_blank(),
              panel.grid.major.x = element_line(colour = "grey", size = .001,
                                                linetype = "dotted"),
              panel.grid.minor.x = element_blank(),
              panel.background = element_rect(fill = 'ghostwhite'),
              legend.key = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
              legend.background = element_rect(fill = 'ghostwhite'),
              legend.title = element_text(size = 11, 
                                          color = 'black',
                                          face = 'bold',
                                          hjust = .5),
              legend.position = "none",
              legend.text = element_text(size = 9.5, 
                                         color = 'black'),
              plot.background = element_rect(fill = "ghostwhite"),
              axis.line = element_line(colour = 'grey'), 
              axis.ticks = element_line(colour = 'grey')
        )
      
      # Si es gráfica de densidad:
      
    } else if (input$graph_type=="Densidad"){
      
      graph <- ggplot(cmplt, aes(x=total))
      
      # Si se desglosa:
      
      if (input$cb_groups== T){
        
        graph <- graph + 
          geom_density(aes_string(fill=group, text = group), alpha=.25, position = "identity")
        
        # Si no se desglosa:
      } else {
        
        graph <- graph +
          geom_density(fill="#FBBB27", alpha=.25, position = "identity")
        
      }
      
      # Ajustes finales:
      
      graph <- graph +
        labs(
            #x = ifelse(input$class == "Carpetas", "Carpetas de investigación", "Víctimas"),
             x = "Carpetas de investigación",
             y = paste0("Densidad (", input$x_axis2, ")"),
             fill = "") +
        theme(axis.text = element_text(size = 8, color = 'black'),
              axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title = element_text(size = 13, color = 'black', face = 'bold'),
              panel.grid.major.y  = element_line(colour = "grey", size = .001,
                                                 linetype = "dotted"),
              panel.grid.minor.y  = element_blank(),
              panel.grid.major.x = element_line(colour = "grey", size = .001,
                                                linetype = "dotted"),
              panel.grid.minor.x = element_blank(),
              panel.background = element_rect(fill = 'ghostwhite'),
              legend.key = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
              legend.background = element_rect(fill = 'ghostwhite'),
              legend.title = element_text(size = 11, 
                                          color = 'black',
                                          face = 'bold',
                                          hjust = .5),
              legend.text = element_text(size = 9.5, 
                                         color = 'black'),
              plot.background = element_rect(fill = "ghostwhite"),
              axis.line = element_line(colour = 'grey'), 
              axis.ticks = element_line(colour = 'grey')
        )
      
    }
    
    remove_modal_spinner()  
    # Gráfica final:
     }
    
    ggplotly(graph, tooltip = "text") %>% layout(legend = list(orientation = "h", x = .05, y =-.35))
    
    
    
  })
  
  
# Mapa: ----
  
  
  output$map <- renderLeaflet({
    
    if (nrow(filtered())==0 | all(is.na(filtered()$y))){
      leaflet() %>% addTiles()
    } else {

    c <- 3.572919
    b <- 8/(exp(-.016*c)-exp(-4*c))
    a <- 7
    rm <- max(diff(range(filtered()$x, na.rm = T)), diff(range(filtered()$y, na.rm = T)))


    llmap <- leaflet() %>%
      #addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(opacity = .9)) %>%
      addTiles() %>%
      setView(lng = .5*max(filtered()$x, na.rm = T)+.5*min(filtered()$x, na.rm = T),
              lat = .5*max(filtered()$y, na.rm = T)+.5*min(filtered()$y, na.rm = T),
              zoom = a + b*exp(-rm*c))

    llmap
    }
  })

  observe({

    show_modal_spinner()



    map_viz <- input$mapviz

    map_filtered <- filtered() %>%
      drop_na(x) %>%
      mutate(dia_semana = weekday,
               # factor(weekdays(fecha, F),
               #                levels = c("lunes", "martes", "miércoles",
               #                           "jueves", "viernes", "sábado", "domingo")),
             label=paste(sep = "<br/>",
                         paste0("<B>Información</B> "),
                         paste0("Día de la semana: ", dia_semana),
                         paste0("Fecha: ", day(fecha), "/", month(fecha), "/", year(fecha)),
                         paste0("Delito: ",delito)#,
                         #paste0("Víctimas: ", victimas)
             )) %>% select(-weekday)

    lgnd <- case_when(input$legend_var == "Delito" ~ "delito",
                      input$legend_var == "Bien afectado" ~ "bien_afectado",
                      input$legend_var == "Día de la semana" ~ "dia_semana")

    pal <- colorFactor(glasbey(),
                       select(map_filtered,
                              .data[[lgnd]])[,1],
                       n = nrow(unique(select(map_filtered,
                                  .data[[lgnd]])))
                       )



    map_base <- leafletProxy("map") %>%
      clearMarkerClusters() %>%
      clearHeatmap() %>%
      clearControls()

    if(map_viz == "Mapa de calor"){

      updateCheckboxInput(session,
                          "map_legend",
                          "Mostrar leyenda",
                          FALSE)

      map_base %>%
        addHeatmap(lng = map_filtered$x, lat = map_filtered$y, radius = 10,
                   blur = 8, max = 15, cellSize = 15, minOpacity = .35)

    } else if (map_viz == "Conglomerados"){
      
    
        
      if(input$map_legend){
        map_base %>%
          addCircleMarkers(lng = map_filtered$x, lat = map_filtered$y,
                           radius = 3.5, popup = map_filtered$label,
                           color = pal(select(map_filtered, .data[[lgnd]])[,1]),
                           clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE
                           )) %>% 
          addLegend("topright", pal = pal, values = select(map_filtered, .data[[lgnd]])[,1],
                                          title = paste0("<B>",input$legend_var,"<B>"))
      } else {
        map_base %>%
          addCircleMarkers(lng = map_filtered$x, lat = map_filtered$y,
                           radius = 3.5, popup = map_filtered$label,
                           color = pal(select(map_filtered, .data[[lgnd]])[,1]),
                           clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE
                           ))
      }
         

    }

    remove_modal_spinner()
  })


  # observe ({
  #   if (nrow(filtered())>0 & any(!is.na(filtered()$y))){
  # 
  #   leg <- input$map_legend
  # 
  #   map_filtered <- filtered() %>%
  #     drop_na(x) %>%
  #     mutate(dia_semana = weekday,
  #              # factor(weekdays(fecha, F),
  #              #                   levels = c("lunes", "martes", "miércoles",
  #              #                              "jueves", "viernes", "sábado", "domingo")),
  #            label=paste(sep = "<br/>",
  #                        paste0("<B>Información</B> "),
  #                        paste0("Día de la semana: ", dia_semana),
  #                        paste0("Fecha: ", day(fecha), "/", month(fecha), "/", year(fecha)),
  #                        paste0("Delito: ",delito),
  #                        paste0("Víctimas: ", victimas)
  #            )) %>% select(-weekday)
  # 
  #   lgnd <- case_when(input$legend_var == "Delito" ~ "delito",
  #                     input$legend_var == "Bien afectado" ~ "bien_afectado",
  #                     input$legend_var == "Día de la semana" ~ "dia_semana")
  # 
  #   pal <- colorFactor(glasbey(),
  #                      select(map_filtered,
  #                             .data[[lgnd]])[,1],
  #                      n = nrow(unique(select(map_filtered,
  #                                             .data[[lgnd]])))
  #   )
  # 
  # 
  #   if (input$map_legend){
  #     leafletProxy("map")  %>%
  #       addLegend("topright", pal = pal, values = select(map_filtered, .data[[lgnd]])[,1],
  #               title = paste0("<B>",input$legend_var,"<B>"))
  #   } else {
  #     leafletProxy("map") %>%
  #       clearControls()
  #   }
  #   
  # 
  #   }
  # })




# Pruebas: ----
  # 
  # output$tesi <- renderDataTable({
  # 
  # })
  # 
  # output$tesi1 <- renderText({
  # })

}
  
