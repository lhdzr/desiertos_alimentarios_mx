library(tidyverse)
source("scripts/funciones_descarga_inegi.R")

# DESCARGA DE DATOS GEOGRAFICOS DE LOS AGEBS EN CADA ESTADO
for (entidad in 1:32) {
  descargaDatosAGEBUrb(entidad)
}

#DESCARGA DE DATOS ÍNDICE DE MARGINACIÓN URBANA
IMU_2020 <- readxl::read_xls("datos/raw/imu_2020/IMU_2020/IMU_2020.xls", sheet = "IMU_2020") %>% 
  select(CVE_AGEB,IM_2020,GM_2020,IMN_2020)

# INCORPORACIÓN DE TODOS LOS AGEB A UN MISMO DATAFRAME
agebs_mx <- data.frame()
for (entidad in 1:32) {
  ent_str <- sprintf("%02d",entidad)
  filepath <- paste0("datos/raw/agebs/ageb",ent_str,".geojson")
  agebs_mx <- rbind(agebs_mx,read_sf(filepath))
  print(paste("Entidad",ent_str,"agregada."))
}

# CRUZAR DATOS DE AGEBS CON DATOS DE INDICE DE MARGINACIÓN URBANA
agebs_mx <- agebs_mx %>%
  mutate(CVE_AGEB = paste0(cve_agee,cve_agem,cve_loc,cve_ageb)) %>%
  left_join(IMU_2020,by="CVE_AGEB") %>% 
  mutate(GM_2020 = factor(GM_2020, 
                          levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"), 
                          ordered = TRUE),
         centroid = st_centroid(geometry),
         pobtot = as.numeric(pobtot),
         tvivhab = as.numeric(tvivhab))




# LEER DATOS DENUE
paths <-c("datos/raw/denue/denue_00_46111_shp/conjunto_de_datos/denue_inegi_46111_.shp",
          "datos/raw/denue/denue_00_46112-46311_shp/conjunto_de_datos/denue_inegi_46112-46311_.shp",
          "datos/raw/denue/denue_00_72_2_0522_shp/conjunto_de_datos/denue_inegi_72_2.shp")
denue <- data.frame()

for (path in paths) {
  units <- read_sf(path) %>%
    select(id,clee,nom_estab,raz_social,
           codigo_act,nombre_act,per_ocu,
           cve_ent,cve_mun,ageb,manzana,
           latitud,longitud,geometry)
  denue <- rbind(denue,units)
  rm(units)
  print(paste("Path",path,"added."))
}

# MANTENER ÚNICAMENTE LOS CÓDIGOS DE ACTIVIDAD DESEADOS
denue <- denue %>%
  filter(grepl("^(7225|4611)", codigo_act))

# CAMBIAR NOMBRES DE ENCODING, AGREGAR VARIABLE DE ALIMENTO CONVENCIONAL
Encoding(denue$nom_estab) <- "latin1"
Encoding(denue$nombre_act) <- "latin1"
denue$alimento_convencional <- 1

# DEFINIR CÓMO SE DETERMINARÁ SI UN NEGOCIO VENDE 
# ALIMENTOS CONVENCIONALES O ALIMENTOS MARGINALES
actividades_no_saludables <- c("461160","461170","461190","461110",
                               "722514","722511","722513","722512", 
                               "722517","722518","722519","722515","722516")
denue$alimento_convencional[denue$codigo_act %in% actividades_no_saludables] <- 0 




# UNIÓN DE DATOS AGEB Y DENUE
st_crs(agebs_mx) == st_crs(denue)
denue <- st_transform(denue, crs = 4326)

agebs_mx$buffer <- st_buffer(agebs_mx$centroid, dist = 1000)

buffers_agebs <- agebs_mx %>%
  as.data.frame() %>%
  select(CVE_AGEB,buffer) %>%
  st_as_sf()

# INTERSECCIÓN ENTRE BUFFERS DE AGEBS Y UNIDADES ECONÓMICAS (UEs)
agebs_denue <- st_join(buffers_agebs,denue,join = st_intersects) %>%
  as.data.frame() %>%
  select(CVE_AGEB,id,alimento_convencional)

agebs_denue_count <- agebs_denue  %>% 
  group_by(CVE_AGEB) %>%
  summarise(unidades_totales = n(),
            unidades_saludables = sum(alimento_convencional)) %>%
  mutate(proporcion_saludable = round(unidades_saludables / unidades_totales, 2))

agebs_mx <- agebs_mx %>%
  merge(agebs_denue_count, by = 'CVE_AGEB') #%>%
  #merge(censo, by = 'CVE_AGEB',all.x = TRUE)

agebs_mx <- agebs_mx %>%
  mutate(indice_acceso = case_when(
    GM_2020 == "Muy bajo" ~ 5,
    GM_2020 == "Bajo" ~ 4,
    GM_2020 == "Medio" ~ 3,
    GM_2020 == "Alto" ~ 2,
    GM_2020 == "Muy alto" ~ 1
  ),indice_acceso = indice_acceso * 0.5,
  indice_acceso = ifelse(is.na(proporcion_saludable),indice_acceso, indice_acceso + proporcion_saludable))

agebs_marginados <- agebs_mx %>% filter(GM_2020 %in% c("Medio", "Alto", "Muy alto"))

agebs_desiertos <- agebs_marginados[agebs_marginados$unidades_saludables==0,]
agebs_pantanos <- agebs_marginados[agebs_marginados$proporcion_saludable<0.5,]
agebs_oasis <- agebs_mx %>% 
  filter(GM_2020 %in% c("Bajo", "Muy bajo")) %>%
  filter(proporcion_saludable>0.5)

# VISUALIZACIONES

barplot(table(agebs_denue_count$unidades_totales), 
        las =2,
        main = "Cantidad de AGEBS según su \naccesibilidad a alimentos",
        xlab = "Cantidad de unidades económicas minoristas de alimento alrededor del AGEB",
        ylab = "Frecuencia")

barplot(table(agebs_denue_count$proporcion_saludable), 
        las =2,
        main = "Cantidad de AGEBS según su \naccesibilidad a alimentos saludables",
        xlab = "Proporción de unidades económicas saludables alrededor del AGEB",
        ylab = "Frecuencia")

# MAPA DE ÍNDICE DE ACCESO ALIMENTARIO
pal <- colorNumeric("YlOrRd", agebs_mx$indice_acceso,reverse = TRUE)

indice_map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_mx, fillColor = ~pal(indice_acceso),
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_mx$cve_ageb,
                             "<strong>MUN:</strong>",agebs_mx$nom_agem,
                             "<br><strong>Grado de Marginación:</strong>",agebs_mx$GM_2020,
                             "<br><strong>Unidades económicas <br>cercanas:</strong>",agebs_mx$unidades_totales,
                             "<br><strong>Opciones saludables:</strong>",agebs_mx$unidades_saludables,
                             "<br><strong>Índice de acceso:</strong>",agebs_mx$indice_acceso)) %>%
  addLegend(position = "bottomright", pal = pal, values = agebs_mx$indice_acceso, 
            title = "Acceso a alimento saludable", opacity = 1)
indice_map

pal_prueba <- colorNumeric("YlOrRd", prueba$indice_acceso,reverse = TRUE)

prueba_map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = prueba, fillColor = ~pal(indice_acceso),
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",prueba$cve_ageb,
                             "<strong>MUN:</strong>",prueba$nom_agem,
                             "<br><strong>Grado de Marginación:</strong>",prueba$GM_2020,
                             "<br><strong>Unidades económicas <br>cercanas:</strong>",prueba$unidades_totales,
                             "<br><strong>Opciones saludables:</strong>",prueba$unidades_saludables,
                             "<br><strong>Índice de acceso:</strong>",prueba$indice_acceso)) %>%
  addLegend(position = "bottomright", pal = pal, values = prueba$indice_acceso, 
            title = "Acceso a alimento saludable", opacity = 1)
prueba_map

dir.create("datos/processed/indice")


write_sf(agebs_mx,"datos/processed/indice/iaa_mx.shp")

# write.csv(agebs_denue, "datos/processed/agebs_denue.csv")
prueba <- read_sf("datos/processed/indice/iaa_mx.shp")
names_agebs <- c( "CVE_AGEB","cve_agee","nom_agee",
                  "cve_agem","nom_agem","cve_loc",
                  "cvegeo","pobtot","pobmas",
                  "pobfem","tvivhab","cve_ageb",
                  "IM_2020","GM_2020","IMN_2020",
                  "unidades_totales","unidades_saludables",
                  "proporcion_saludable","centroid",
                  "buffer","indice_acceso")
names(prueba) <- names_agebs
