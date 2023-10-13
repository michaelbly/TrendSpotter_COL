cati_response <- read.csv("Input/cati_dataset_061023.csv", sep = ";"
                     , comment.char = "", strip.white = TRUE,
                     stringsAsFactors = TRUE, encoding="UTF-8-BOM")


# filter only surveys after June 2021
cati_response$date_assessment <- strptime(as.character(cati_response$ObsDate), "%Y-%m-%d")
cati_response$date_assessment <- format(as.Date(cati_response$ObsDate), "%B-%y")



#group months together to trimesters
cati_response <- cati_response %>% mutate(quarter = case_when(
  cati_response$date_assessment %in% c("abril-20", "mayo-20", "junio-20")  ~ "2020_2",
  cati_response$date_assessment %in% c("julio-20", "agosto-20", "sepiembre-20")  ~ "2020_3",
  cati_response$date_assessment %in% c("octubre-20", "noviembre-20", "diciembre-20")  ~ "2020_4",
  cati_response$date_assessment %in% c("enero-21", "febrero-21", "marzo-21")  ~ "2021_1",
  cati_response$date_assessment %in% c("abril-21", "mayo-21", "junio-21")  ~ "2021_2",
  cati_response$date_assessment %in% c("julio-21", "agosto-21", "sepiembre-21")  ~ "2021_3",
  cati_response$date_assessment %in% c("octubre-21", "noviembre-21", "diciembre-21")  ~ "2021_4",
  cati_response$date_assessment %in% c("enero-22", "febrero-22", "marzo-22")  ~ "2022_1",
  
  cati_response$date_assessment %in% c("abril-22", "mayo-22", "junio-22")  ~ "2022_2",
  cati_response$date_assessment %in% c("julio-22", "agosto-22", "sepiembre-22")  ~ "2022_3",
  cati_response$date_assessment %in% c("octubre-22", "noviembre-22", "diciembre-22")  ~ "2022_4",
  cati_response$date_assessment %in% c("enero-23", "febrero-23", "marzo-23")  ~ "2023_1",
  cati_response$date_assessment %in% c("abril-23", "mayo-23", "junio-23")  ~ "2023_2",
  cati_response$date_assessment %in% c("julio-23", "agosto-23", "sepiembre-23")  ~ "2023_3",
  cati_response$date_assessment %in% c("octubre-23", "noviembre-23", "diciembre-23")  ~ "2023_4",
  
  cati_response$date_assessment %in% c("enero-24", "febrero-24", "marzo-24")  ~ "2024_1",
  cati_response$date_assessment %in% c("abril-24", "mayo-24", "junio-24")  ~ "2024_2",
  cati_response$date_assessment %in% c("julio-24", "agosto-24", "sepiembre-24")  ~ "2024_3",
  cati_response$date_assessment %in% c("octubre-24", "noviembre-24", "diciembre-24")  ~ "2024_4",
  
  cati_response$date_assessment %in% c("enero-25", "febrero-25", "marzo-25")  ~ "2025_1",
  cati_response$date_assessment %in% c("abril-25", "mayo-25", "junio-25")  ~ "2025_2",
  cati_response$date_assessment %in% c("julio-25", "agosto-25", "sepiembre-25")  ~ "2025_3",
  cati_response$date_assessment %in% c("octubre-25", "noviembre-25", "diciembre-25")  ~ "2025_4"))


cati_response$departamento <- cati_response$ADM2_NAME
to_alphanumeric_lowercase <- function (x)
{tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))}
cati_response$departamento <- to_alphanumeric_lowercase(cati_response$departamento)


######################################################
#CREATE REGIONS
cati_response <- cati_response %>% dplyr::mutate(regiones_sampling = case_when(
  cati_response$departamento %in% c("antioquia", "caldas", "quindino", "risaralda") ~ "antioquia_ec",
  
  cati_response$departamento %in% c("san_andres", "atlantic", "bolivar", "cesar", "cordoba", "la_guijira", 
                               "magdalena", "sucre") ~ "caribe",
  
  cati_response$departamento %in% c("cuindinamarca", "huila", "tolima", "bogota") ~ "centro",
  
  cati_response$departamento %in% c("boyaca", "santander","norte_santander") ~ "oriente",
  
  cati_response$departamento %in% c("cauca", "choco", "valle_de_cauca", "narino") ~ "pacifica",
  
  cati_response$departamento %in% c("atn", "vichada", "vaupes", "amazonas", "guainia", "guivave", "chaqueta", "puntomayo", "cesarane", 
                               "meta", "arauca") ~ "sur"))



#######################################################
#CREATE VARIABLE SHOWING IF MIGRANT OR COLOMBIAN
cati_response <- cati_response %>% mutate(migration_status = case_when(
  cati_response$RESPMigrationStatus %in% c("COLOMBIAN RETURNED", "INTERNALLY DISPLACED PEOPLE", "RESIDENT") ~ "Colombianos",
  cati_response$RESPMigrationStatus %in% c("COLOMBIAN RETURNED", "MIGRANT/REFUGEE WITH DUAL VENEZUELAN-COLOMBIAN NATIONALITY", "VENEZUELAN MIGRANT/REFUGEE") ~ "Migrantes"))



######################################################
#ALIGN NAMES OF DATA WITH HIGHCHARTER MAP DATA
cati_response <- cati_response %>% dplyr::mutate(department_name_map = case_when(
  cati_response$departamento == "antioquia" ~ "Antioquia",
  cati_response$departamento == "caldas" ~ "Caldas",
  cati_response$departamento == "quindino" ~ "Quindío",
  cati_response$departamento == "risaralda" ~ "Risaralda",
  cati_response$departamento == "cuindinamarca" ~ "Cundinamarca",
  cati_response$departamento == "huila" ~ "Huila",
  cati_response$departamento == "tolima" ~ "Tolima",
  cati_response$departamento == "bogota" ~ "Bogota",
  cati_response$departamento == "boyaca" ~ "Boyacá",
  cati_response$departamento == "santander" ~ "Santander",
  cati_response$departamento == "norte_santander" ~ "Norte de Santander",
  cati_response$departamento == "cauca" ~ "Cauca",
  cati_response$departamento == "choco" ~ "Chocó",
  cati_response$departamento == "valle_de_cauca" ~ "Valle del Cauca",
  cati_response$departamento == "narino" ~ "Nariño",
  cati_response$departamento == "vichada" ~ "Vichada",
  cati_response$departamento == "vaupes" ~ "Vaupés",
  cati_response$departamento == "amazonas" ~ "Amazonas",
  cati_response$departamento == "guainia" ~ "Guainía",
  cati_response$departamento == "guivave" ~ "Guaviare",
  cati_response$departamento == "chaqueta" ~ "Caquetá",
  cati_response$departamento == "puntomayo" ~ "Putumayo",
  cati_response$departamento == "cesarane" ~ "Casanare",
  cati_response$departamento == "meta" ~ "Meta",
  cati_response$departamento == "arauca" ~ "Arauca",
  cati_response$departamento == "san_andres" ~ "San Andrés y Providencia",
  cati_response$departamento == "atlantic" ~ "Atlántico",
  cati_response$departamento == "bolivar" ~ "Bolívar",
  cati_response$departamento == "cesar" ~ "Cesar",
  cati_response$departamento == "cordoba" ~ "Córdoba",
  cati_response$departamento == "la_guijira" ~ "La Guajira",
  cati_response$departamento == "magdalena" ~ "Magdalena",
  cati_response$departamento == "sucre" ~ "Sucre"))




