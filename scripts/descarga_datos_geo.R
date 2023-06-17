source("tesina/funciones_descarga_inegi.R")

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
  agebs_ent <- read_sf(filepath)
  agebs_mx <- rbind(agebs_mx,agebs_ent)
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

