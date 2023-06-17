library(httr)
library(geojsonio)
library(sf)
library(leaflet)
library(tidyverse)


catalogo_municipios <- read.csv("datos/raw/catalogo_municipios/AGEEML_20236122149393.csv")


# FUNCIÓN PARA OBTENER CÓDIGO DE ENTIDAD CON NOMBRE
getCodigoEstado <- function(nombre_ent) {
  codes <- unique(catalogo_municipios$CVE_ENT[catalogo_municipios$NOM_ENT %in% nombre_ent])
  return(codes)
}
# FUNCIÓN PARA OBTENER CÓDIGO MUNICIPAL CON CÓDIGO DE ENTIDAD
# Y NOMBRE DE MUNICIPIO
getCodigoMunicipio <- function(codigo_ent,nombres_mun=NULL){
  if(is.null(nombres_mun)){
    codes <- unique(catalogo_municipios$CVE_MUN[catalogo_municipios$CVE_ENT == codigo_ent])
  }else{
    codes <- unique(catalogo_municipios$CVE_MUN[(catalogo_municipios$CVE_ENT == codigo_ent) & (catalogo_municipios$NOM_MUN %in% nombres_mun)])
  }
  return(codes)
}


# PROBABLEMENTE NO NECESARIAS. ÚTILES PARA HACER 
# VISUALIZACIONES A NIVEL MUNICIPAL
descargaLimitesMunicipales <- function(entidad){
  # DIRECTORIO DE DESCARGA
  lim_mun_path <- "datos/raw/limites_mun"
  ifelse(!dir.exists(lim_mun_path),dir.create(lim_mun_path),print("Hi"))
  lista_cve_mun <- unique(catalogo_municipios[catalogo_municipios$CVE_ENT==entidad,]$CVE_MUN)
  
  # LIGA DE DESCARGA
  base_url <- "https://gaia.inegi.org.mx/wscatgeo/geo/mgem/"
  ent_str <- sprintf("%02d", entidad)
  
  for (clave in lista_cve_mun) {
    # Formatear el código para que tenga tres dígitos
    code_str <- sprintf("%03d", clave)
    
    # Pegar el url y el código municipal
    url <- paste0(base_url,ent_str,code_str)
    
    # Definir el nombre del archivo
    filename <- paste0(ent_str, code_str, ".geojson")
    filepath <- file.path(lim_mun_path, filename)
    
    # Hacer el GET del url
    response <- GET(url)
    
    # Checar si la conexión fue exitosa
    if (status_code(response) == 200) {
      # Guardar la respuesta al archivo con el nombre definido anteriormente
      writeBin(rawToChar(content(response, as = "raw")), filepath)
      cat(paste0("Data downloaded successfully for code ", ent_str,code_str, ".\n"))
    } else {
      cat(paste0("Error downloading data for code ",ent_str, code_str, ". Status code:", status_code(response), "\n"))
    }
    
    # Los archivos se guardan con un espacio adicional que no permite su lectura.
    # Este código borra el espacio y vuelve a guardar el archivo.
    geojson_string <- readLines(filepath)
    geojson_string_trimmed <- trimws(geojson_string, "right")
    writeLines(geojson_string_trimmed, filepath)
  }
}
makeMapaMunicipal <- function(entidad,lista_codigos_municipales,directorio) {
  ent_str <- sprintf("%02d", entidad)
  map <- leaflet() %>% addTiles()
  
  for (code in lista_codigos_municipales) {
    code_str <- sprintf("%03d", code)
    # read the corresponding geojson file
    filename <- paste0(directorio,"/",ent_str, code_str, ".geojson")
    data <- read_sf(filename)
    
    # add the geojson layer to the map with the municipality name as the tooltip
    map <- map %>% addPolygons(data = data, fillColor = "blue",
                               stroke = TRUE, weight = 1, opacity = 1,
                               fillOpacity = 0.5, color = "white",
                               label = data$nom_agem)
  }
  return(map)
}


# FUNCIÓN PARA DESCARGAR DATOS URBANOS LISTA
descargaDatosAGEBUrb <- function(entidad){
  # DIRECTORIO DE DESCARGA
  data_ageb_path <- "datos/raw/agebs"
  if (!dir.exists(data_ageb_path)) {
    dir.create(data_ageb_path)
  }
  
  ent_str <- sprintf("%02d",entidad)
  agebs_base_url <- "https://gaia.inegi.org.mx/wscatgeo/geo/agebu/"
  url <- paste0(agebs_base_url,ent_str)
  response <- GET(url)
  filename <- paste0("ageb",ent_str,".geojson")
  filepath <- file.path(data_ageb_path, filename)
  print(filepath)
  if (status_code(response) == 200) {
    # Guardar la respuesta al archivo con el nombre definido anteriormente
    writeBin(rawToChar(content(response, as = "raw")), filepath)
  } else {
    cat(paste0("Error downloading data for state ", ent_str, ". Status code:", status_code(response_prueba), "\n"))
  }
  geojson_string <- readLines(filepath)
  geojson_string_trimmed <- trimws(geojson_string, "right")
  writeLines(geojson_string_trimmed, filepath)
  cat(paste0("Data downloaded successfully for state ", ent_str, ".\n"))
}
# FUNCIÓN PARA DESCARGAR DATOS RURALES SÓLO NECESITA ADAPTACIONES
descargaDatosAGEBRural <- function(entidad,codigos_mun,directorio){
  ent_str <- sprintf("%02d",entidad)
  agebs_base_url <- "https://gaia.inegi.org.mx/wscatgeo/geo/agebr/"
  url <- paste0(agebs_base_url,ent_str,"/",code_str)
  response <- GET(url)
  filename <- paste0("ageb",ent_str,code_str,".geojson")
  filepath <- file.path(lim_mun_path, filename)
  geojson_string <- readLines(filepath)
  geojson_string_trimmed <- trimws(geojson_string, "right")
  writeLines(geojson_string_trimmed, filepath)
  
  for (code in codigos_mun) {
    code_str <- sprintf("%03d", code)
    url <- paste0(agebs_base_url,ent_str,"/",code_str)
    print(url)
    response <- GET(url)
    filename <- paste0("ageb",ent_str,code_str,".geojson")
    filepath <- file.path(lim_mun_path, filename)
    print(filepath)
    
    if (status_code(response) == 200) {
      # Guardar la respuesta al archivo con el nombre definido anteriormente
      writeBin(rawToChar(content(response, as = "raw")), filepath)
      cat(paste0("Data downloaded successfully for code ", code_str, ".\n"))
    } else {
      cat(paste0("Error downloading data for code ", code_str, ". Status code:", status_code(response_prueba), "\n"))
    }
    
    geojson_string <- readLines(filepath)
    geojson_string_trimmed <- trimws(geojson_string, "right")
    writeLines(geojson_string_trimmed, filepath)
  }
}
# FUNCIÓN PARA HACER MAPAS DE AGEBS A NIVEL ESTATAL LISTA
makeAGEBMap <- function(entidad,fill_opacity){
  ent_str <- sprintf("%02d",entidad)
  data_completo <- data.frame()
  filepath <- paste0('datos/raw/agebs/ageb',ent_str,'.geojson')
  data <- read_sf(filepath)
  data_completo <- rbind(data_completo,data)
  
  #%>% addTiles()
  map <- leaflet() %>%  addProviderTiles("CartoDB.Positron") %>% addPolygons(data = data_completo, fillColor = "blue",
                                                                             stroke = TRUE, weight = .2, opacity = 1,
                                                                             fillOpacity = fill_opacity, color = "white",
                                                                             popup = data_completo$cve_ageb)
  return(map)
}
