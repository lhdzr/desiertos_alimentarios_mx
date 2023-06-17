library(tidyverse)
source("scripts/funciones_descarga_inegi.R")

# DESCARGA DE DATOS GEOGRAFICOS DE LOS AGEBS EN CADA ESTADO
for (entidad in 1:32) {
  descargaDatosAGEBUrb(entidad)
}

#DESCARGA DE DATOS ÍNDICE DE MARGINACIÓN URBANA
IMU_2020 <- readxl::read_xls("datos/raw/imu_2020/IMU_2020/IMU_2020.xls", sheet = "IMU_2020") %>% 
  filter(ENT==19) %>%
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
  print(paste("Path",path,"added."))
}

denue <- denue %>%
  filter(grepl("^(7225|4611)", codigo_act))

Encoding(denue$nom_estab) <- "latin1"
Encoding(denue$nombre_act) <- "latin1"

denue$alimento_convencional <- 1
