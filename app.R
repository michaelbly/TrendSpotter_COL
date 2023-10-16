options(shiny.maxRequestSize = 30*1024^2)

source("functions/1_setup.R")
source("functions/2_import_datasets.R")
source("functions/3_recoding_cati.R")
cati_with_composites <- recoding_cari(cati_response)

library(shinyWidgets)                                                             # additional UI options for shiny
library(shinythemes)    
library(shinydashboard)
library(shinyauthr)
library(shinymanager)
library(shiny)
library(fresh)
library(bs4Dash)
conflicts_prefer(bs4Dash::dashboardPage)
conflicts_prefer(bs4Dash::dashboardHeader)
conflicts_prefer(bs4Dash::dashboardSidebar)
conflicts_prefer(bs4Dash::dashboardBody)
conflicts_prefer(bs4Dash::sidebarMenu)
conflicts_prefer(bs4Dash::menuItem)
conflicts_prefer(bs4Dash::tabItems)
conflicts_prefer(bs4Dash::tabItem)
conflicts_prefer(bs4Dash::menuSubItem)
conflicts_prefer(bs4Dash::actionButton)




####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI
#UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI
#UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI 
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  title = "Limpieza de Datos",
  fullscreen = F,
  skin = "light",
  
  
  #######################################################################################################
  #HEADER
  header = dashboardHeader(
    title = dashboardBrand(
      title = "COLOMBIA | Context",
      color = "secondary",
      href = "https://es.wfp.org/paises/colombia",
      image = "https://cdn.wfp.org/guides/ui/v1.0.0/assets/logos/emblem/wfp-logo-emblem-white.png",
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("sliders"),
    fixed = FALSE),
  
  
  #######################################################################################################
  #SIDEBAR
  bs4Dash::dashboardSidebar(
    skin = "light",
    status = "secondary",
    width = 8,
    elevation = 9,
    collapsed = T,
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      
      menuItem("Seguridad Alimentaria", tabName = "seguridad_alimentaria", icon = icon("wheat-awn"), startExpanded = F,
               menuSubItem("Consumo de Alimentos", tabName = "subtab_consumo", icon = icon("bowl-food")),
               menuSubItem("Hambre", tabName = "subtab_hambre", icon = icon("wheat-awn-circle-exclamation")),
               menuSubItem("Estrategias Medios de Vida", tabName = "subtab_lcs", icon = icon("person-digging"))
      ),      
      menuItem("Resumen de Datos", tabName = "resumen", icon = icon("magnifying-glass"))
      
    ) #CLOSE SIDEBARMENU
  ), #CLOSE DASHBOARDSIDEBAR
  
  
  controlbar = dashboardControlbar(
    id= "controlbar",
    collapsed = TRUE,
    width = "330px",
    skin = "light",
    controlbarMenu(
      controlbarItem("Elija un periodo de tiempo y el nivel de administración de la visualización:",
                     fileInput("dataset_file", label = "Cargue tu base de datos en CSV", accept = c(".csv"), buttonLabel = "Browse...",
                               placeholder = "Ningún archivo seleccionado"),
                     fileInput("odk_file", label = "Cargue tu ODK en CSV", accept = c(".csv"), buttonLabel = "Browse...",
                               placeholder = "Ningún archivo seleccionado")
                     
      ) #CLOSE CONTROLBARITEM
    ) #CLOSE CONTROLBARMENU
  ), #CLOSE DASHBOARDCONTROLBAR
  
  
  #######################################################################################################
  #DASHBOARDBODY
  #######################################################################################################
  dashboardBody(

  ########################################
  #CHANGE SKIN OF DASHBOARD
  #######################################
    use_theme(create_theme(
      bs4dash_status(
        primary = "#FFF",
        secondary = "#636458"),
      bs4dash_color(gray_900 = "#FFF", white = "#C5C889"),
      bs4dash_sidebar_light(
        bg = "#C5C889",
        color = "#FFF",
        hover_color = "#FFF",
        submenu_bg = "#C5C889", 
        submenu_color = "#FFF",
        submenu_hover_color = "#FFF"),
      bs4dash_status(light = "#C5C889"),
      bs4dash_vars(
        navbar_light_color = "#FFF", # color of titles in sidebar
        navbar_light_active_color = "#FFF",
        navbar_light_hover_color = "#FFF",
        body_bg = "#C5C889"),
      bs4dash_layout(
        main_bg = "#faf6eb"),
      bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#C5C889"))),
    
    
  #######################################################################################################
  #FIRST PAGE - FOOD SECURITY
  #######################################################################################################
  tabItems(
    bs4TabItem(tabName = "subtab_consumo",
               fluidRow(
                 style = "margin-top: 0px; margin-bottom: 0px; margin-left: 0px;",
                 column(width = 5,
                        style = "margin-top: 0px; margin-bottom: 10px; margin-left: 0px;",
                        h3("Consumo de Alimentos", style = "color: #434348;"),
                        tags$div(
                          style = "color: #434348; text-align: justify;",
                          tags$span(
                            HTML("El <strong>Food Consumption Score (FCS) </strong> del PMA evalúa el consumo de alimentos de un hogar basándose en el número de días de la semana anterior en que los miembros del hogar han comido ocho tipos diferentes de alimentos (es decir, proteínas, verduras, frutas, cereales, legumbres, productos lácteos, azúcar y aceite). A continuación, estos datos se ponderan en función del valor nutricional relativo de los grupos de alimentos consumidos. Por ejemplo, los grupos de alimentos que contienen alimentos densos desde el punto de vista nutricional, como los productos animales, tienen más peso que los que contienen alimentos menos densos desde el punto de vista nutricional, como los tubérculos. <br>En función de la puntuación final del consumo de alimentos, cada hogar se clasifica en una de las tres categorías de gravedad: <strong><span style='color: #6D545D;'>pobre</span>, <span style='color: #F5E8B7;'>limitrofe</span> o <span style='color: #C5C889;'>aceptable</span></strong>.")
                          )
                        )),
                 column(width = 7,
                        highchartOutput("pie_chart_fcs", height = "75vh", width = "60vw"))
               ),
               
               br(),
               
               fluidRow(
                 style = "margin-top: 30px; margin-bottom: 0px; margin-left: 0px;",
                 column(width = 5,
                        style = "margin-top: -30px; margin-bottom: -10px; margin-left: 0px;",
                        highchartOutput("map_fcs", width = "100%", height = "500px")
                        ),
                 
                 column(width = 7,
                        style = "margin-top: 20px; margin-left: 0px;",
                        highchartOutput("area_chart_fcs", width = "100%", height = "450px")
                 )
               ),
               
               fluidRow(
                 style = "margin-top: 30px; margin-bottom: 90px; margin-left: 0px;",
                 column(width = 5,
                        style = "margin-top: 20px; margin-bottom: -10px; margin-left: 0px;",
                        highchartOutput("spider_fcs", width = "100%", height = "500px")
                        ),
                 column(width = 7,
                        style = "margin-top: 80px; margin-left: 0px;",
                        highchartOutput("line_fcs", width = "100%", height = "400px")
                 )
                 
               ),
               
               tags$hr(style="border-color: #636458;"),
               
               
               fluidRow(
                 column(width = 5,
                        style = "margin-top: 80px; margin-bottom: 0px; margin-left: 0px;",
                        h3("Estrategias de Consumo:", style = "color: #434348;"),
                        tags$div(
                          style = "color: #434348; text-align: justify;",
                          tags$span(
                            HTML("Otro indicador que evalúa la situación de consumo de alimentos de un hogar es el <strong>Coping Strategy Index (CSI) </strong>. El CSI evalúa en qué medida los hogares utilizan estrategias de afrontamiento perjudiciales cuando no tienen suficientes alimentos o dinero para comprarlos. <br>Se pregunta a cada hogar cuántos días de los últimos siete han tenido que emplear una de las cinco estrategias basadas en el consumo debido a la falta de alimentos o de dinero para comprarlos: 1) comer menos alimentos preferidos, 2) reducir el número de comidas diarias, 3) reducir el tamaño de las porciones de las comidas, 4) comprar alimentos a crédito o pedirlos prestados y 5) reducir las raciones de alimentos de los adultos para que los niños tengan más alimentos para comer.")
                          )
                        )),
                 column(width = 6,
                        style = "margin-top: 70px; margin-bottom: -10px; margin-left: 30px;",
                        highchartOutput("stackbar_csi", height = "60vh", width = "110%")
               )
               ),
               
               fluidRow(
                 style = "margin-top: 30px; margin-bottom: 0px; margin-left: 0px;",
                 column(width = 8,
                        style = "margin-top: 30px; margin-bottom: -10px; margin-left: 0px; margin-right: 0px;",
                        highchartOutput("linechart_csi", height = "80vh", width = "85%")
                        ),
                 
                 column(width = 4,
                        style = "margin-top: 30px; margin-bottom: -10px; margin-left: 0px;",
                        highchartOutput("stacked_barchart_csi", height = "80vh", width = "100%")
                 )
                 
               )
               
    )
  )
    
  ) #CLOSE DASHBOARDBODY

) #CLOSE UI









####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
#SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVE
#SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVE
#SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVE
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
server <- function(input, output) {
  
##############################################################
#FIRST PAGE - PIE CHART FOR FCS FOR DETERMINET PERIOD
#############################################################
pie_chart_maker_fcs <- function(){
  
  pie_chart <- cati_with_composites %>% 
    mutate(num_variable = fcs_category) %>%
    count(num_variable, name = "Frequency") %>%
    mutate(y = round(100* (Frequency / sum(Frequency)),2)) %>%
    rename(name = num_variable) %>%
    select(-Frequency)
  
  rd_bu_palette <- c("#C5C889", "#F0DFAD", "#6D545D")
  
  highchart() %>%
    hc_add_series(type = "pie", data = pie_chart, hcaes(x = name, y = y), innerSize = "50%", marginLeft = -40, marginTop = -20) %>%
    hc_colors(rd_bu_palette) %>%
    hc_title(
      text = NULL) %>%
    hc_tooltip(
      valueSuffix = "% de hogares",
      pointFormat = "{point.name}: <b>{point.y}</b>") %>%
    hc_plotOptions(series = list(stacking = FALSE, allowPointSelect = TRUE, dataLabels = list(
      style = list(fontSize = "15px")  # Adjust the font size here
    )))
}

output$pie_chart_fcs <- renderHighchart({
  pie_chart_maker_fcs()
})
  

#################################################################
#FIRST PAGE - AREA CHART FOR FCS TENDENCIES
#################################################################
area_chart_maker_fcs <- function(){
  
area_chart <- cati_with_composites %>% 
  mutate(num_variable = fcs_category) %>%
  group_by(quarter) %>%
  count(num_variable, name = "Frequency") %>%
  mutate(y = round(100* (Frequency / sum(Frequency)),2)) %>%
  rename(name = num_variable) %>%
  select(-Frequency)

area_chart %>%
  hchart(type = "area",
    hcaes(x = quarter, y = y, group = name)) %>%
  hc_xAxis(title = list(text = "Trimestre")) %>%
  hc_yAxis(
    title = list(text = ""),
    max = 100,  # Set the maximum value for the y-axis
    labels = list(formatter = JS("function () {return this.value + '%';}"))) %>%
  hc_plotOptions(
    area = list(
      stacking = "normal", # Stacking mode
      marker = list(enabled = FALSE))) %>%
  hc_colors(c("#C5C889", "#F0DFAD", "#6D545D")) %>%
  hc_tooltip(
    valueSuffix = "% de hogares")
}

output$area_chart_fcs <- renderHighchart({
  area_chart_maker_fcs()
})

  

##############################################################
#FIRST PAGE - MAP SHOWING BORDERLINE OR POOR FOOD CONSUMPTION
#############################################################
map_maker_fcs <- function(){
  
line_cart_consumo <- cati_with_composites %>%
  group_by(department_name_map) %>%
  summarise("Consumo Pobre" = as.numeric(round(100*mean(sa1_poor_or_borderline, na.rm = TRUE), 0))) 

hcmap(
  "countries/co/co-all",
  data = line_cart_consumo,
  value = "Consumo Pobre",
  joinBy = c("name", "department_name_map"),
  name = "Consumo Pobre o Limitrofe",
  dataLabels = list(enabled = FALSE),
  borderColor = "#FAFAFA",
  borderWidth = 0.1,
  tooltip = list(
    valueDecimals = 0,
    valueSuffix = "% de hogares")) %>%
  hc_legend(enabled = F) %>%
  hc_colorAxis(minColor = "#C5C889", maxColor = "#434348") %>%
  hc_title(text = "% de hogares con un consumo pobre o limitrofe", margin = -5, align = "left",   style = list(fontSize = "16px"))
}

output$map_fcs <- renderHighchart({
  map_maker_fcs()
})




##############################################################
#FIRST PAGE - SPIDER CHART FOR CONSUMPTION BY TYPE
#############################################################
spider_maker_fcs <- function(){
  
spider_colombian <- cati_with_composites %>%
  filter(migration_status == "Colombianos") %>%
  summarise("Cereales" = as.numeric(round(mean(FCSStap, na.rm = TRUE), 1)),
            "Leguminosas" = as.numeric(round(mean(FCSPulse, na.rm = TRUE), 1)),
            "Leche" = as.numeric(round(mean(FCSDairy, na.rm = TRUE), 1)),
            "Carne" = as.numeric(round(mean(FCSPr, na.rm = TRUE), 1)),
            "Vegetales" = as.numeric(round(mean(FCSVeg, na.rm = TRUE), 1)),
            "Frutas" = as.numeric(round(mean(FCSFruit, na.rm = TRUE), 1))
  ) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "nr_dias") %>%
  arrange(desc(nr_dias)) %>%
  rename("Nr dias" = nr_dias)


spider_migrante <- cati_with_composites %>%
  filter(migration_status == "Migrantes") %>%
  summarise("Cereales" = as.numeric(round(mean(FCSStap, na.rm = TRUE), 1)),
            "Leguminosas" = as.numeric(round(mean(FCSPulse, na.rm = TRUE), 1)),
            "Leche" = as.numeric(round(mean(FCSDairy, na.rm = TRUE), 1)),
            "Carne" = as.numeric(round(mean(FCSPr, na.rm = TRUE), 1)),
            "Vegetales" = as.numeric(round(mean(FCSVeg, na.rm = TRUE), 1)),
            "Frutas" = as.numeric(round(mean(FCSFruit, na.rm = TRUE), 1))
  ) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "nr_dias") %>%
  arrange(desc(nr_dias)) %>%
  rename("Nr dias" = nr_dias)

highchart() %>%
  hc_chart(type = 'line', polar = TRUE) %>%
  hc_xAxis(categories = spider_colombian$name,
           labels = list(
             style = list(fontSize = '10px'))) %>%
  hc_add_series(
    spider_colombian$`Nr dias`,
    name = 'Colombianos',
    dataLabels = list(enabled = TRUE),
    color = '#636458',
    lineWidth = 2) %>%
  hc_add_series(
    spider_migrante$`Nr dias`,
    name = 'Migrantes',
    dataLabels = list(enabled = TRUE),
    color = '#C5C889',
    lineWidth = 2) %>%
  hc_yAxis(
    gridLineInterpolation = 'circle',
    min = 0,
    max = 6.2,
    labels = list(
      formatter = JS("function() {
        return this.value + 'd';}"))) %>%
  hc_tooltip(valueSuffix = " dias") %>%
  hc_legend(itemStyle = list(fontSize = '16px')) %>%
  hc_title(text = "Número de días que los miembros del hogar comen distintos tipos de alimentos", margin = -5, align = "left",   style = list(fontSize = "16px"))

}

output$spider_fcs <- renderHighchart({
  spider_maker_fcs()
})




##############################################################
#FIRST PAGE - LINE CHART FOR TENDENCIES OF CONSUMPTION BY FOOD TYPE
#############################################################
line_maker_fcs <- function(){
  
line_cart_consumo <- cati_with_composites %>%
  group_by(quarter) %>%
  summarise("Cereales" = as.numeric(round(mean(FCSStap, na.rm = TRUE), 1)),
            "Leguminosas" = as.numeric(round(mean(FCSPulse, na.rm = TRUE), 1)),
            "Leche" = as.numeric(round(mean(FCSDairy, na.rm = TRUE), 1)),
            "Carne" = as.numeric(round(mean(FCSPr, na.rm = TRUE), 1)),
            "Vegetales" = as.numeric(round(mean(FCSVeg, na.rm = TRUE), 1)),
            "Frutas" = as.numeric(round(mean(FCSFruit, na.rm = TRUE), 1))) 


highchart() %>%
  hc_chart(type = 'line') %>%
  hc_xAxis(categories = line_cart_consumo$quarter) %>%
  hc_add_series(
    line_cart_consumo$Cereales, name = 'Cereales', color = '#66545e', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_consumo$Carne, name = 'Carne', color = '#a39193', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_consumo$Leche, name = 'Productos Lacteos', color = '#aa6f73', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_consumo$Vegetales, name = 'Vegetales ', color = '#eea990', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_consumo$Frutas, name = 'Frutas ', color = '#f6e0b5', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_yAxis(
    gridLineInterpolation = 'circle', min = 0, max = 7,
    labels = list(
      formatter = JS("function() {return this.value + 'd';}"))) %>%
  hc_tooltip(valueSuffix = " dias")
}

output$line_fcs <- renderHighchart({
  line_maker_fcs()
})




##############################################################
#STACKED BAR CHART SHOWING NUMBER OF MEALS
#############################################################
stackedbar_maker_csi <- function(){
  
  analysis_lcs <-  cati_with_composites %>%
    summarise("Comer comida menos preferida" = as.numeric(round(mean(rCSILessQlty, na.rm = TRUE), 1)),
              "Comprar a credito" = as.numeric(round(mean(rCSIBorrow, na.rm = TRUE), 1)),
              "Reducir numero comidas" = as.numeric(round(mean(rCSIMealNb, na.rm = TRUE), 1)),
              "Reducir tamaño porciones" = as.numeric(round(mean(rCSIMealSize, na.rm = TRUE), 1)),
              "Reducir consumo de adultos" = as.numeric(round(mean(rCSIMealAdult, na.rm = TRUE), 1))) %>%
    pivot_longer(cols = everything(), names_to = "CSI_Estrategias", values_to = "nr_dias") %>%
    arrange(desc(nr_dias)) %>%
    rename("%" = nr_dias) 
  
  colors <- colorRampPalette(c("#434348", "#C5C889"))(length(analysis_lcs$CSI_Estrategias))
  
  hchart(analysis_lcs, type = "bar", hcaes(x = `CSI_Estrategias`, y = `%`), pointWidth = 43) %>%
    hc_tooltip(
      valueSuffix = " dias", pointFormat = "{point.name}: <b>{point.y}</b>") %>%
    hc_xAxis(
      labels = list(style = list(color = "black", fontSize = "14px")), title = NULL) %>%
    hc_yAxis(
      labels = list(style = list(color = "black")), title = "") %>%
    hc_plotOptions(
      bar = list(colorByPoint = TRUE, colors = colors))
}

output$stackbar_csi <- renderHighchart({
  stackedbar_maker_csi()
})




##############################################################
#LINE CHART FOR TENDENCIES OF EMPLOYMENT OF CSI STRATEGIES
#############################################################
linechart_maker_csi <- function(){
  
line_cart_csi <- cati_with_composites %>%
  group_by(quarter) %>%
  summarise("Comer comida menos preferida" = as.numeric(round(mean(rCSILessQlty, na.rm = TRUE), 1)),
            "Comprar a credito" = as.numeric(round(mean(rCSIBorrow, na.rm = TRUE), 1)),
            "Reducir numero comidas" = as.numeric(round(mean(rCSIMealNb, na.rm = TRUE), 1)),
            "Reducir tamaño porciones" = as.numeric(round(mean(rCSIMealSize, na.rm = TRUE), 1)),
            "Reducir consumo de adultos" = as.numeric(round(mean(rCSIMealAdult, na.rm = TRUE), 1)))


highchart() %>%
  hc_chart(type = 'line') %>%
  hc_xAxis(categories = line_cart_csi$quarter) %>%
  hc_add_series(
    line_cart_csi$`Comer comida menos preferida`, name = 'Comida menos preferido', color = '#66545e', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_csi$`Comprar a credito`, name = 'Comprar comida a credito', color = '#a39193', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_csi$`Reducir numero comidas`, name = 'Reducir numero comidas', color = '#aa6f73', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_csi$`Reducir tamaño porciones`, name = 'Reducir tamaño porciones ', color = '#eea990', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_add_series(
    line_cart_csi$`Reducir consumo de adultos`, name = 'Reducir consumo de adultos ', color = '#f6e0b5', lineWidth = 2, marker = list(enabled = F)) %>%
  hc_yAxis(
    gridLineInterpolation = 'circle', min = 0, max = 6,
    labels = list(
      formatter = JS("function() {return this.value + 'd';}"))) %>%
  hc_tooltip(valueSuffix = " dias")
}

output$linechart_csi <- renderHighchart({
  linechart_maker_csi()
})




##############################################################
#STACKED BAR CHART SHOWING NUMBER OF MEALS
#############################################################
stacked_barchahrt_maker_csi <- function(){
  
stacked_bar <- cati_with_composites %>% 
  mutate(num_variable = HHMealNb) %>%
  count(num_variable, name = "Frequency") %>%
  mutate(y = round(100* (Frequency / sum(Frequency)),0)) %>%
  rename(name = num_variable) %>%
  select(-Frequency) %>%
  pivot_wider(names_from = name, values_from = y)

stacked_bar$category <- ""

highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Número de comidas el dia de ayer", margin = 20, align = "left", style = list(fontSize = "16px")) %>%
  hc_xAxis(categories = stacked_bar$category, labels = list(enabled = FALSE)) %>%
  hc_yAxis(max = 100, labels = list(format = "{value}%")) %>%
  hc_plotOptions(
    column = list(stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y}%",
                    style = list(fontSize = "14px"),
                    textShadow = "none"))) %>%
  hc_add_series(
    name = "1 Comida", data = stacked_bar$`1 MEAL`, color = "#434348") %>%
  hc_add_series(
    name = "2 Comidas", data = stacked_bar$`2 MEALS`, color = "#838568") %>%
  hc_add_series(
    name = "3 Comidas", data = stacked_bar$`3 OR MORE MEALS`, color = "#C5C889") %>%
  hc_tooltip(valueSuffix = "% de hogares", headerFormat = '<span style="font-size: 10px">numero de comidas:</span><br/>') %>%
  hc_legend(itemStyle = list(fontSize = '13px'))  # Increase the legend item font size
}

output$stacked_barchart_csi <- renderHighchart({
  stacked_barchahrt_maker_csi()
})



  
} #CLOSE SERVER



shinyApp(ui, server)

