library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx2)
library(openxlsx)
#install.packages("taskscheduleR") #es para automatizar la ejecución del script



#Cargo todos los datos


base_operativos <- read_excel("base_2023.xlsx", 
                          sheet = "Base operativos", col_types = c("text",                                                                              "text", "date", "numeric", "numeric", 
                                                                                "numeric", "numeric", "text", "text", 
                                                                                "numeric", "text", "text", "text", 
                                                                                "text", "text", "text", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "text", "text", "text", "numeric", 
                                                                                "numeric", "numeric", "text", "numeric", 
                                                                                "text", "text", "text", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "text", "text", "text", "text", "numeric", 
                                                                                "numeric"))







  base_datos <- read_excel("base_datos.xlsx", 
                             sheet = "Sheet 1", col_types = c("text", 
                                                                           "date", "numeric", "numeric", "numeric", 
                                                                           "numeric", "text", "text", "numeric", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "text", "text",  "text", "numeric"))


base_drive <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQFHTPBWJIeGYX9Gwxq4fxsa-hhVGj2aWxoPuGKkNDug0j8ZFPWc_foAWWBWmxTmuwTLJEN307QRFuH/pub?output=csv", encoding = "utf-8")




colnames(base_drive)<- c("marca_temp",
                         "nombre_sup",
                         "id",
                         "fecha_op",
                         "marco_op",
                         "provincia",
                         "municipio",
                         "participantes de la dappte por areas",
                         "especialidades aportadas por otras dependencias provinciales",
                         "especialidades aportadas por otras dependencias municipales",
                         "otras areas u organismos participantes del operativo" ,
                         "Vacunación_Calendario Nacional_Personas", 
                         "Vacunación_Calendario Nacional_Dosis de vacunas aplicadas", 
                         "Vacunación_Calendario Nacional_Calendarios completados", 
                         "Vacunación_Calendario Nacional_Libretas AUH", 
                         "Vacunación_COVID-19_Dosis de vacunas aplicadas", 
                         "Promoción_Consejerías_Personas", 
                         "Promoción_Talleres_Talleres realizados", 
                         "Promoción_Talleres_Personas", 
                         "Atención médica_Clínica médica_Personas", 
                         "Atención médica_Clínica médica_Patología crónica", 
                         "Atención médica_Clínica médica_Patología aguda", 
                         "Atención médica_Clínica médica_Control de salud", 
                         "Atención médica_Clínica médica_Derivaciones", 
                         "Atención médica_Clínica médica - Externos_Personas", 
                         "Atención médica_Pediatría_Personas", 
                         "Atención médica_Pediatría_Patología aguda", 
                         "Atención médica_Pediatría_Control de niño sano", 
                         "Atención médica_Pediatría_Libretas AUH", 
                         "Atención médica_Pediatría_Derivaciones", 
                         "Atención médica_Pediatría - Externos_Personas", 
                         "Atención médica_SSYR_Personas", 
                         "Atención médica_SSYR_Implantes subdérmicos", 
                         "Atención médica_SSYR_Otro método anticonceptivo", 
                         "Atención médica_SSYR_Consejerías en salud sexual", 
                         "Atención médica_SSYR_PAP", 
                         "Atención médica_SSYR_Prestaciones", 
                         "Atención médica_SSYR_Ext. Implantes subdérmicos", 
                         "Atención médica_SSYR_Derivaciones", 
                         "Atención médica_SSYR - Externos_Personas", 
                         "Atención médica_SSYR - Externos_Implantes subdérmicos",
                         "Imágenes_Mamografías_Personas", 
                         "Imágenes_Mamografías_Placas RX", 
                         "Imágenes_Placas RX_Personas", 
                         "Imágenes_Placas RX_Placas RX", 
                         "Imágenes_Imágenes_Derivaciones", 
                         "Oftalmología_Oftalmología_Personas", 
                         "Oftalmología_Oftalmología_Derivaciones", 
                         "Oftalmología_Oftalmología_Lentes recetados", 
                         "Enfermería_Enfermería_Personas", 
                         "Enfermería_Enfermería_Prestaciones", 
                         "Enfermería_Enfermería_Libretas AUH", 
                         "Enfermería_Enfermería_Derivaciones", 
                         "Odontología_Odontología_Personas", 
                         "Odontología_Odontología_Prestaciones", 
                         "Odontología_Odontología_Derivaciones", 
                         "Salud mental_Salud mental_Personas", 
                         "Salud mental_Salud mental_Orientaciones",
                         "Salud mental_Salud mental_Consultorías en padecimientos subjetivos", 
                         "Salud mental_Salud mental_Consultorías en consumo problemático", 
                         "Salud mental_Salud mental_Consultorías en violencias (de género u otras)", 
                         "Salud mental_Salud mental_Consultorías en problemáticas en el crecimiento y desarrollo", 
                         "Salud mental_Salud mental_Derivaciones",
                         "Salud mental_Talleres_Talleres realizados", 
                         "Salud mental_Talleres_Personas",
                         "Observaciones generales del desarrollo del operativo",
                         "Facilitadores y obstaculizadores identificados",
                         "Cuestiones logisticas de traslado y alojamiento",
                         "Sugerencias y propuestas")



#Pongo en condiciones las bases

base_operativos <- base_operativos[,c(1:17)]

base_datos <- base_datos[,c(1:18)]

base_drive_inf_final <- base_drive[,c(1:11,66:69)]

base_drive <- base_drive[,c(3,4,8:69)]

base_drive$fecha_op <- dmy(base_drive$fecha_op)

base_drive <- base_drive %>% mutate(id = paste0(id,"-",day(fecha_op)))



#saco los que estan suspendidos
op_susp <- base_operativos %>% filter(estado == "Suspendido")
for(e in unique(op_susp$id)) {
  base_drive <- base_drive %>% dplyr::filter(id != e)
}




#saco los datos que ya tengo



for(e in unique(base_datos$id)) {
  base_drive <- base_drive %>% dplyr::filter(id != e)
}
 

#reemplazo na
convertir_na <- function(datos,valor){                     
  cant_na <- matrix(nrow = ncol(datos),ncol = 2)
  colnames(cant_na) <- c("variable","cant")
  for(i in seq_along(datos)) { cant_na[i,1] <- colnames(datos[i]) #con este veo la cantidad de los na
  cant_na[i,2] <- sum(is.na(datos[i]))
  }
  print(cant_na)
  corredor <- 1:ncol(datos)
  zz<-matrix()
  for(k in corredor) {
    if(cant_na[k,2]>0){zz<-rbind(zz,cant_na[k,1])}
  }
  zz<-zz[-1]
  pos_na<- apply(is.na(datos), 2, which)#con este veo la posición de na
  for(l in zz){for(q in pos_na[[l]]){
    datos[[q,l]]<- "0"
  }}
  return(datos)#necesario para que me copie los datos en x
}

base_drive<- convertir_na(datos = base_drive, valor = 0)


dejar_solo_num<-function(datos,fila_desde,fila_hasta,col_desde,col_hasta){
  for(i in c(fila_desde:fila_hasta)){ for(j in c(col_desde:col_hasta)) {
    datos[[i,j]]<-gsub("a","",datos[[i,j]])
    datos[[i,j]]<-gsub("á","",datos[[i,j]])
    datos[[i,j]]<-gsub("b","",datos[[i,j]])
    datos[[i,j]]<-gsub("c","",datos[[i,j]])
    datos[[i,j]]<-gsub("d","",datos[[i,j]])
    datos[[i,j]]<-gsub("e","",datos[[i,j]])
    datos[[i,j]]<-gsub("é","",datos[[i,j]])
    datos[[i,j]]<-gsub("f","",datos[[i,j]])
    datos[[i,j]]<-gsub("g","",datos[[i,j]])
    datos[[i,j]]<-gsub("h","",datos[[i,j]])
    datos[[i,j]]<-gsub("i","",datos[[i,j]])
    datos[[i,j]]<-gsub("í","",datos[[i,j]])
    datos[[i,j]]<-gsub("j","",datos[[i,j]])
    datos[[i,j]]<-gsub("k","",datos[[i,j]])
    datos[[i,j]]<-gsub("l","",datos[[i,j]])
    datos[[i,j]]<-gsub("m","",datos[[i,j]])
    datos[[i,j]]<-gsub("n","",datos[[i,j]])
    datos[[i,j]]<-gsub("ñ","",datos[[i,j]])
    datos[[i,j]]<-gsub("o","",datos[[i,j]])
    datos[[i,j]]<-gsub("ó","",datos[[i,j]])
    datos[[i,j]]<-gsub("p","",datos[[i,j]])
    datos[[i,j]]<-gsub("q","",datos[[i,j]])
    datos[[i,j]]<-gsub("r","",datos[[i,j]])
    datos[[i,j]]<-gsub("s","",datos[[i,j]])
    datos[[i,j]]<-gsub("t","",datos[[i,j]])
    datos[[i,j]]<-gsub("u","",datos[[i,j]])
    datos[[i,j]]<-gsub("ú","",datos[[i,j]])
    datos[[i,j]]<-gsub("ü","",datos[[i,j]])
    datos[[i,j]]<-gsub("v","",datos[[i,j]])
    datos[[i,j]]<-gsub("w","",datos[[i,j]])
    datos[[i,j]]<-gsub("x","",datos[[i,j]])
    datos[[i,j]]<-gsub("y","",datos[[i,j]])
    datos[[i,j]]<-gsub("z","",datos[[i,j]])
    datos[[i,j]]<-gsub("A","",datos[[i,j]])
    datos[[i,j]]<-gsub("Á","",datos[[i,j]])
    datos[[i,j]]<-gsub("B","",datos[[i,j]])
    datos[[i,j]]<-gsub("C","",datos[[i,j]])
    datos[[i,j]]<-gsub("D","",datos[[i,j]])
    datos[[i,j]]<-gsub("F","",datos[[i,j]])
    datos[[i,j]]<-gsub("E","",datos[[i,j]])
    datos[[i,j]]<-gsub("É","",datos[[i,j]])
    datos[[i,j]]<-gsub("F","",datos[[i,j]])
    datos[[i,j]]<-gsub("G","",datos[[i,j]])
    datos[[i,j]]<-gsub("H","",datos[[i,j]])
    datos[[i,j]]<-gsub("I","",datos[[i,j]])
    datos[[i,j]]<-gsub("Í","",datos[[i,j]])
    datos[[i,j]]<-gsub("J","",datos[[i,j]])
    datos[[i,j]]<-gsub("K","",datos[[i,j]])
    datos[[i,j]]<-gsub("L","",datos[[i,j]])
    datos[[i,j]]<-gsub("M","",datos[[i,j]])
    datos[[i,j]]<-gsub("N","",datos[[i,j]])
    datos[[i,j]]<-gsub("Ñ","",datos[[i,j]])
    datos[[i,j]]<-gsub("O","",datos[[i,j]])
    datos[[i,j]]<-gsub("Ó","",datos[[i,j]])
    datos[[i,j]]<-gsub("P","",datos[[i,j]])
    datos[[i,j]]<-gsub("Q","",datos[[i,j]])
    datos[[i,j]]<-gsub("R","",datos[[i,j]])
    datos[[i,j]]<-gsub("S","",datos[[i,j]])
    datos[[i,j]]<-gsub("T","",datos[[i,j]])
    datos[[i,j]]<-gsub("U","",datos[[i,j]])
    datos[[i,j]]<-gsub("Ú","",datos[[i,j]])
    datos[[i,j]]<-gsub("V","",datos[[i,j]])
    datos[[i,j]]<-gsub("W","",datos[[i,j]])
    datos[[i,j]]<-gsub("X","",datos[[i,j]])
    datos[[i,j]]<-gsub("Y","",datos[[i,j]])
    datos[[i,j]]<-gsub("Z","",datos[[i,j]])
    datos[[i,j]]<-gsub(" ","",datos[[i,j]])
  }
  }
  return(datos)
}


base_drive <- dejar_solo_num(datos = base_drive,col_desde = 18, col_hasta = 18, fila_desde = 1, fila_hasta = nrow(base_drive))


#le saco las columnas que identifican el operativo que no sean el id porque ya no las necesito

base_drive <-base_drive[,-2] 

#le pego los datos de la base_operativos y reordeno y divido para cada cosa

base_drive_op <- left_join(base_drive, base_operativos)

base_drive_op <-base_drive_op[,c(1,64:79,2:63)]

base_drive_datos <- base_drive_op[,c(1:17,22:75)]



#lo paso a formato tidy


#tengo que pasar todas las clumnas a caracter
for(z in c(18:71)){   #esto es importante hacerlo con for y no con un conjunto como argumento en el corchete porque se hace mierda sino
  base_drive_datos[,z] <- as.character(base_drive_datos[,z])
}



base_drive_datos <- base_drive_datos %>% pivot_longer(cols = c(-id,-estado, -fecha, -dia, -mes, -anio, -semana, -`marco del operativo`, -provincia, -`id operativo`, -`municipio departamento comuna`, -`localidad barrio`,-lugar,-direccion,-`link georeferenciacion`,-latitud,-longitud),
                                                    names_to = "des",
                                                    values_to = "cantidad",
                                                    values_drop_na = TRUE)


base_drive_datos <- separate(base_drive_datos,des,into = c("especialidad", "descrip", "uni_medida"), sep = "_",)

#Saco todo lo que no sea número de la base y los ceros

base_drive <- dejar_solo_num(datos = base_drive,col_desde = 21, col_hasta = 21, fila_desde = 1, fila_hasta = nrow(base_drive))

base_drive_datos <- base_drive_datos %>% filter(cantidad > 0)


#le saco estado, latitud y longitud

base_drive_datos <-base_drive_datos[,c(-2,-16,-17)]

colnames(base_drive_datos) <- colnames(base_datos)

base_datos_act <- rbind(base_datos,base_drive_datos)



# actualizo los informes de los supervisores ------------------------------


base_drive_inf_final<- base_drive_inf_final %>% filter(`Observaciones generales del desarrollo del operativo`!= "")

base_drive_inf_final$fecha_op <- dmy(base_drive_inf_final$fecha_op)


base_drive_inf_final <- base_drive_inf_final %>% mutate(id = paste0(id,"-",day(fecha_op)))


base_drive_inf_final<- left_join(base_drive_inf_final, base_operativos, by = "id")

base_drive_inf_final<- base_drive_inf_final[,c(3,17:31,8:15,1,2)]

base_drive_inf_final <- base_drive_inf_final %>% filter(semana>max(unique(base_operativos$semana)-2))





#Guardo los datos


write.xlsx(base_operativos,"base_operativos.xlsx")

write.xlsx(base_datos_act, "base_datos.xlsx")

write.xlsx(base_drive_inf_final, "base_final_op.xlsx")



for (i in unique(base_drive_datos$id)) {
  if (i %in% base_operativos$id) {
    mensaje <- paste("Operativo", i, "cargado correctamente")
  } else {
    mensaje <- paste("Operativo", i, "cargado incompleto")
  }
  
  # Imprimir el mensaje para cada elemento
  print(mensaje)
}

base_operativos_filt <- base_operativos %>% filter(semana==max(unique(base_operativos$semana))) %>% filter(estado!="Suspendido")

for (e in base_operativos_filt$id) {
  if (e %in% base_datos_act$id) {
    mensaje2 <- paste("Operativo", e, "todo ok")
  } else {
    mensaje2 <- paste("Operativo", e, "Falta cargar")
  }
  
  # Imprimir el mensaje para cada elemento
  print(mensaje2)
}
