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
                 column(width = 5,
                        style = "margin-top: 0px; margin-bottom: -20px; margin-left: 0px;",
                        h3("Consumo de Alimentos", style = "color: #434348;"),
                        tags$div(
                          style = "color: #434348; text-align: justify;",
                          tags$span(
                            HTML("El <strong>Food Consumption Score (FCS)</strong> del PMA evalúa el consumo de alimentos de un hogar basándose en el número de días de la última semana en que los miembros del hogar han comido determinados tipos de alimentos (por ejemplo, proteínas, verduras, frutas, etc.). Cada tipo de alimento tiene un peso específico basado en su valor nutricional y cada hogar se clasifica en una de las tres categorías de gravedad: pobre, limitrofe o aceptable. <br> <br> Otro indicador que evalúa la situación del consumo de alimentos de un hogar es el <strong>Coping Strategies Index (CSI)</strong>, que examina el número de días a la semana que un hogar emplea estrategias relacionadas con el consumo (por ejemplo, reducir el número de comidas al día, reducir las porciones de comida, etc.) porque no tiene suficientes alimentos o dinero para comprarlos.")
                          )
                        )),
                 column(width = 7,
                        highchartOutput("pie_chart_fcs", height = "80vh", width = "60vw"))
               ),
               
               br(),
               
               fluidRow(
                 column(width = 5,
                        style = "margin-top: -70px; margin-bottom: -10px; margin-left: 0px;",
                        highchartOutput("map_fcs", width = "100%", height = "500px")
                        ),
                 
                 column(width = 7,
                        style = "margin-top: 40px; margin-left: 0px;",
                        highchartOutput("area_chart_fcs", width = "100%", height = "400px")
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
  
  rd_bu_palette <- c("#6A9C89", "#F5E8B7", "#CD5C08")
  
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
    title = list(text = "% de hogares"),
    max = 100,  # Set the maximum value for the y-axis
    labels = list(formatter = JS("function () {return this.value + '%';}"))) %>%
  hc_plotOptions(
    area = list(
      stacking = "normal", # Stacking mode
      marker = list(enabled = FALSE))) %>%
  hc_colors(c("#6A9C89", "#F5E8B7", "#CD5C08")) %>%
  hc_tooltip(
    valueSuffix = "%")
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




  
} #CLOSE SERVER



shinyApp(ui, server)

