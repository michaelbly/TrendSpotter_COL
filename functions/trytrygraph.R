#################################################################
#AREA CHART FOR FCS TENDENCIES
#################################################################
pie_chart <- cati_with_composites %>% 
  mutate(num_variable = fcs_category) %>%
  group_by(quarter) %>%
  count(num_variable, name = "Frequency") %>%
  mutate(y = round(100* (Frequency / sum(Frequency)),2)) %>%
  rename(name = num_variable) %>%
  select(-Frequency)


# Create a stacked area chart
pie_chart %>%
  hchart(
    type = "area",
    hcaes(x = quarter, y = y, group = name)) %>%
  hc_title(text = "Tendencias de consumo") %>%
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





##############################################################
#PIE CHART FOR FCS FOR DETERMINET PERIOD
#############################################################
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
  hc_plotOptions(
    series = list(
      stacking = FALSE, allowPointSelect = TRUE))






##############################################################
#SPIDER CHART FOR CONSUMPTION BY TYPE
#############################################################
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
  hc_title(text = "Número de dias de consumo:") %>%
  hc_xAxis(categories = spider_colombian$name,
           labels = list(
             style = list(fontSize = '16px'))) %>%
  hc_add_series(
    spider_colombian$`Nr dias`,
    name = 'Colombianos',
    dataLabels = list(enabled = TRUE),
    color = '#ebe65e',
    lineWidth = 4) %>%
  hc_add_series(
    spider_migrante$`Nr dias`,
    name = 'Migrantes',
    dataLabels = list(enabled = TRUE),
    color = '#23731c',
    lineWidth = 4) %>%
  hc_yAxis(
    gridLineInterpolation = 'circle',
    min = 0,
    max = 6.5,
    labels = list(
      formatter = JS("function() {
        return this.value + 'd';}"))) %>%
  hc_tooltip(valueSuffix = " dias") %>%
  hc_legend(itemStyle = list(fontSize = '16px'))  # Increase the legend font size)







##############################################################
#LINE CHART FOR TENDENCIES OF CONSUMPTION BY FOOD TYPE
#############################################################
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






##############################################################
#MAP SHOWING BORDERLINE OR POOR FOOD CONSUMPTION
#############################################################
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
  hc_title(text = "% de hogares con consumo pobre o limitrofe")
  





##############################################################
#STACKED BAR CHART SHOWING NUMBER OF MEALS
#############################################################
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
  hc_title(text = "Número de comidas el dia de ayer") %>%
  hc_xAxis(categories = stacked_bar$category, labels = list(enabled = FALSE)) %>%
  hc_yAxis(max = 100, labels = list(format = "{value}%")) %>%
  hc_plotOptions(
    column = list(stacking = "normal",
      dataLabels = list(
      enabled = TRUE,
      format = "{y}%",
      style = list(fontSize = "16px"),
      textShadow = "none"))) %>%
  hc_add_series(
    name = "1 Comida", data = stacked_bar$`1 MEAL`, color = "#CD5C08") %>%
  hc_add_series(
    name = "2 Comidas", data = stacked_bar$`2 MEALS`, color = "#F5E8B7") %>%
  hc_add_series(
    name = "3 Comidas", data = stacked_bar$`3 OR MORE MEALS`, color = "#6A9C89") %>%
  hc_tooltip(valueSuffix = "% de hogares", headerFormat = '<span style="font-size: 10px">numero de comidas:</span><br/>') %>%
  hc_legend(itemStyle = list(fontSize = '14px'))  # Increase the legend item font size





##############################################################
#BAR CHART WITH AVERAGE DAYS CSI STRATEGIES
#############################################################
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
    labels = list(style = list(color = "lightgrey", fontSize = "14px")), title = NULL) %>%
  hc_yAxis(
    labels = list(style = list(color = "lightgrey")), title = "") %>%
  hc_title(
    text = "Número promedio de días en los últimos 7 días en que los hogares tuvieron que emplear una estrategia de consumo:",  # Add a title
    style = list(color = "lightgrey", align = "left"))  %>%
  hc_plotOptions(
    bar = list(colorByPoint = TRUE, colors = colors))






##############################################################
#LINE CHART FOR TENDENCIES OF EMPLOYMENT OF CSI STRATEGIES
#############################################################
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
    gridLineInterpolation = 'circle', min = 0, max = 7,
    labels = list(
      formatter = JS("function() {return this.value + 'd';}"))) %>%
  hc_tooltip(valueSuffix = " dias")
