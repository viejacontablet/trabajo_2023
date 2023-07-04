library(readxl) 
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(crosstalk)

# DIRECTORIO Y CARGA DE DATOS ---------------------------------------------




base_t <- read_excel("base_datos.xlsx", sheet = "Sheet 1")
base_t$fecha <- as_date(base_t$fecha)
dappt_base <- base_t

base_r_completa <- read_excel("base_2023.xlsx", 
                     sheet = "Base operativos")
base_r <- base_r_completa[,-2]

base_camp <- read_excel("base_camp.xlsx", sheet = "Sheet 1")

base_final_op <- read_excel("base_final_op.xlsx")


metas_anual <- read_excel("base_2023.xlsx", sheet = "Metas_fisicas_anual")
metas_trim <- read_excel("base_2023.xlsx", sheet = "Metas_trimestre")

 #cargo esta función que la voy a usar
convertir_na <- function(datos,valor){                     
  cant_na <- matrix(nrow = ncol(datos),ncol = 2)
  colnames(cant_na) <- c("variable","cant")
  for(i in seq_along(datos)) { cant_na[i,1] <- colnames(datos[i]) #con este veo la cantidad de los na
  cant_na[i,2] <- sum(is.na(datos[i]))
  }
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




# ARMADO BASE_DAPPTE ----------------------------------------------------


## Semanas dia inicio y fin--------------

sem_unicas <-  unique(dappt_base$semana)#creo un vector con las semanas unicas del archivo


mat_aux<-matrix(sem_unicas,ncol = 1)#esto lo hago para obtener el numero de semanas unicas (filas del vector semanas únicas) para crear la tabla sem_fechas
a <-nrow(mat_aux)

sem_fechas1 <- tibble(semana = unique(dappt_base$semana),#armo una tabla con las semanas que tenga dos columnas con formato fecha
                      sem_i = as_date(rep(0,a)),
                      sem_f = as_date(rep(0,a))
)
for (s in sem_unicas) {#sirve para pegarle el principio y el final de los operativos en cada semana.
  
  aaa <- dappt_base %>% 
    filter(semana == s) %>% 
    summarise (prim_dia = min(fecha))
  
  sem_fechas1 [which(sem_unicas==s),2] <- aaa
  
  bbb <- dappt_base %>%
    dplyr::filter(semana == s) %>%
    summarise (ult_dia = max(fecha))
  
  sem_fechas1 [which(sem_unicas==s),3] <- bbb
}

dappt_base <-left_join(dappt_base, sem_fechas1)


## Coordenadas-----------------


colnames(base_r) <-gsub(" ","_",colnames(base_r))

base_aux <- base_r[,1] 


base_aux <- cbind(base_aux,base_r$latitud)
colnames(base_aux) <- c("id", "lat")


id_coor<-data.frame(str_split (base_aux$lat,",",simplify = TRUE))
id_coor <- cbind(id_coor,base_aux$id)

colnames(id_coor) <- c("lat", "lon","id")

id_coor$lon<-  as.numeric(id_coor$lon)
id_coor$lat<-  as.numeric(id_coor$lat)

dappt_base<-left_join(dappt_base,id_coor)

dappt_base2 <- dappt_base[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22)]



dappt_base$lat <- as.character(dappt_base$lat)
dappt_base$lon <- as.character(dappt_base$lon)

#esto es lo mismo de las semanas, pero lo deje porque se usa para otra cosa también
sem_unicas <-  unique(dappt_base$semana)

sem_fechas <- tibble (sem_a = rep(0,52),
                      sem_i = as_date(rep(0,52)),
                      sem_f = as_date(rep(0,52)) )  

sem_unicas <- na.omit(sem_unicas)
sem_unicas

for (s in sem_unicas) {
  
  aaa <- dappt_base %>% 
    dplyr::filter(semana == s) %>% 
    summarise (prim_dia = min(fecha))
  
  sem_fechas [s,2] <- aaa
  
  bbb <- dappt_base %>%
    dplyr::filter(semana == s) %>%
    summarise (ult_dia = max(fecha))
  
  sem_fechas [s,3] <- bbb
}

sem_fechas <- sem_fechas %>% 
  filter(sem_i > 2021 & sem_f > 2021) %>% 
  mutate(sem_a = sem_unicas)

prim_dia <- max (dappt_base$sem_i)
ult_dia <- max (dappt_base$sem_f)
ult_semana <- max (dappt_base$semana)


dappt_base$operativo <- case_when(
  NA ~ "OTROS",
  dappt_base$operativo %in% c("ACTIVIDADES DAPPT","DAT","DAPPT","DAPPTE") ~ "DAPPTE",
  dappt_base$operativo == "OPERATIVO VERANO" ~ "OP. VERANO",
  dappt_base$operativo == "OPERATIVOS ESPECIALES" ~ "OP. ESPECIAL",
  TRUE ~ dappt_base$operativo
)


dappt_base$sem_i <- as_date(dappt_base$sem_i)
dappt_base$sem_f <- as_date(dappt_base$sem_f)
dappt_base$lat <- as.numeric(dappt_base$lat)
dappt_base$lon <- as.numeric(dappt_base$lon)


## Fechas de los operativos-------------------

oper_unicos <-  unique(dappt_base$id_op)#creo un vector con las semanas unicas del archivo


mat_aux<-matrix(oper_unicos,ncol = 1)#esto lo hago para obtener el numero de semanas unicas (filas del vector semanas únicas) para crear la tabla sem_fechas
a <-nrow(mat_aux)

op_fechas1 <- tibble(id_op = unique(dappt_base$id_op),#armo una tabla con las semanas que tenga dos columnas con formato fecha
                     op_i = as_date(rep(0,a)),
                     op_f = as_date(rep(0,a))
)
for (s in oper_unicos) {#sirve para pegarle el principio y el final de los operativos en cada semana.
  
  aaa <- dappt_base %>% 
    filter(id_op == s) %>% 
    summarise (prim_dia = min(fecha))
  
  op_fechas1 [which(oper_unicos==s),2] <- aaa
  
  bbb <- dappt_base %>%
    filter(id_op == s) %>%
    summarise (ult_dia = max(fecha))
  
  op_fechas1 [which(oper_unicos==s),3] <- bbb
}
# SEMANAL ---------------------------------------------------

#Semana actual------------------------------

filt_sem_act <- max(unique(base_r$semana))

### Datos para gráficos----------------------

#para los dos primeros graficos(personas por día y proporción de especialidades)

base_t$cantidad <- as.numeric(base_t$cantidad)

base_tus<- base_t %>%
  filter(semana == filt_sem_act) %>% 
  filter(uni_medida == "Personas") %>%
  group_by(dia,fecha,id_op,operativo,especialidad) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(id_ope = paste0(id_op,"-",operativo)) %>% #para que en el filtro me quede tmb el dispositivo
  mutate(Día = format(fecha,"%A"))#esto hace una columna con los días de la semana


base_tus<-base_tus[,-3] #borro el ip_op solo

base_tus_aux<- base_tus%>% dplyr::group_by(fecha, id_ope)%>% #esto es para que el grafico de personas por días, creo una segunda columna con esa información.
  summarise(Cantidad = sum(cantidad))

base_tus<- left_join(base_tus,base_tus_aux)

firstCap <- function(x) { #esta es una función que pasa de minuscula a nombres propios
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), 
        substring(s, 2),
        sep="", collapse=" ")
}
base_tus$Día <-unlist(lapply(base_tus$Día, FUN=firstCap))#Pongo la primera letra en mayuscula

base_tus$Día = factor(base_tus$Día,c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))



base_tus$id_ope<-factor(base_tus$id_ope)
base_tus$especialidad<-factor(base_tus$especialidad)

#para el grafico de la cantidad de operativos

oper_dia_sem_act <- base_r_completa %>% filter(semana == max(unique(base_r_completa$semana))) %>% group_by(fecha) %>%
  summarise(Cantidad = n()) %>%
  mutate(Día = format(fecha,"%A"))


oper_dia_sem_act <-oper_dia_sem_act[,c(2,3)]

oper_dia_sem_act$Día <-unlist(lapply(oper_dia_sem_act$Día, FUN=firstCap))#Pongo la primera letra en mayuscula

oper_dia_sem_act$Día = factor(oper_dia_sem_act$Día,c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))






base_tus_sd <- SharedData$new(base_tus) 

### Datos para tablas--------------


base_presta <- base_t%>% filter(semana==filt_sem_act) %>%
  group_by(id_op,especialidad,descrip,uni_medida) %>%
  summarise(cantidad=sum(cantidad))



base_presta_sd <- SharedData$new(base_presta)

## Listado de op
op_ultsem<- base_r[,c(6:13)]

op_ultsem<-op_ultsem%>% dplyr::filter(semana==filt_sem_act)#filtro la ultima semana

colnames(op_ultsem)<-gsub(" ", "_", colnames(op_ultsem))#quito espacios de las columnas

op_ultsem_dias<- op_ultsem%>% dplyr::group_by(id_operativo) %>% #cuento la cantidad de dias de cada operativo
  summarise(dias_totales = n())

op_ultsem<-distinct(op_ultsem)#quito los duplicados, como ya saque las columnas que tienen que ver con el día de operativo me quedan solo los id de operativos distintos y las ubicaciones distintas

op_ultsem <- left_join(op_ultsem,op_ultsem_dias) #le pego la columna con la cantidad de operativos por día
op_ultsem <- op_ultsem[,c(4,9,2,3,5:7)] #ordeno y quito columnas




### Grafico1:  personas  por día --------------------------------------------
colgraf<-c("#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80")

sda<-ggplot(base_tus_sd,mapping = aes(x=Día, y=Cantidad))+
  geom_col(position = "identity",
           #just = 0.5,
           width = 0.5
  ) + 
  scale_y_continuous("")+
  scale_x_discrete("",limits = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")) +
  scale_fill_manual(values = colgraf)+
  scale_color_manual(values = colgraf)+
  theme_classic()


sda<- ggplotly(sda) %>% layout(title = "",  showlegend = F) %>% 
  config(modeBarButtonsToRemove=c('zoom2d', 'pan2d', 'select2d',
                                  'lasso2d', 'zoomIn2d', 'zoomOut2d',
                                  'autoScale2d','resetScale2d'))


sda <- sda %>% layout(title = "",  showlegend = F)


### Gráfico 2: proporcion por especialidad -------------------------


oper<-unique(base_tus$id_ope)
oper<-factor(oper)
corr<-nlevels(oper)
oper<-as.data.frame(oper)
oper$oper<-as.character(oper$oper)
oper<-cbind(oper,c(1:nrow(oper)))


for(y in 1:corr){
  baseaux <- base_tus %>% filter(id_ope==oper[[y,1]]) %>%
    group_by(especialidad) %>%
    summarise(aux=sum(cantidad))
  oper[y,2]<-nrow(baseaux)
}


col_operativos<-c("#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80")



fig <- base_tus_sd %>% plot_ly(labels = ~especialidad, values = ~cantidad,
                               marker = list(colors = col_operativos,
                                             line = list(color = '#FFFFFF', width = 5)),
                               textinfo = 'label+percent')
fig <- fig %>% add_pie(hole = 0.3)

grafico5_plotly <- fig %>% layout(title = "",  showlegend = F)




### Grafico 3: operativos por día ----------------------------------

colgrafo<-c("#000000", "#000000","#000000","#000000","#000000","#000000","#000000")

grafico_opdia<-ggplot(oper_dia_sem_act,mapping = aes(x=Día, y= Cantidad))+
  geom_col(fill = "#000000",
           #just = 0.5,
           width = 0.5
  ) + 
  scale_y_continuous("")+
  scale_x_discrete("",limits = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")) +
  scale_fill_manual(values = colgrafo)+
  scale_color_manual(values = colgrafo)+
  theme_classic()

grafico_opdia<- ggplotly(grafico_opdia) %>% layout(title = "",  showlegend = F) %>% 
  config(modeBarButtonsToRemove=c('zoom2d', 'pan2d', 'select2d',
                                  'lasso2d', 'zoomIn2d', 'zoomOut2d',
                                  'autoScale2d','resetScale2d'))

grafico_opdia <- grafico_opdia %>% layout(title = "",  showlegend = F)




# Semana pasada-------------------------------------------------

filt_sem <-max(unique(base_r$semana))-1
### Datos para gráficos----------------------

#para los dos primeros graficos(personas por día y proporción de especialidades)

base_tus2<- base_t %>%
  filter(semana == filt_sem) %>% 
  filter(uni_medida == "Personas") %>%
  group_by(dia,fecha,id_op,operativo,especialidad) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(id_ope = paste0(id_op,"-",operativo)) %>% #para que en el filtro me quede tmb el dispositivo
  mutate(Día = format(fecha,"%A"))#esto hace una columna con los días de la semana
#para que en el filtro me quede tmb el dispositivo





base_tus2<-base_tus2[,-3]

base_tus_aux2<- base_tus2%>% dplyr::group_by(fecha, id_ope)%>% #esto es para que el grafico de personas por días, creo una segunda columna con esa información.
  summarise(Cantidad=sum(cantidad))

base_tus2<- left_join(base_tus2,base_tus_aux2)

base_tus2$Día <-unlist(lapply(base_tus2$Día, FUN=firstCap))#Pongo la primera letra en mayuscula

base_tus2$Día = factor(base_tus2$Día,c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))


base_tus2$id_ope<-factor(base_tus2$id_ope)
base_tus2$especialidad<-factor(base_tus2$especialidad)

#para campaña

colnames(base_camp)<- gsub(" ","_",colnames(base_camp))#arreglo los nombres 

base_camp_aux2 <- base_camp%>% filter(semana==filt_sem) %>% #armo la base con los datos de esta semana
  group_by(dia,fecha,id_operativo,marco_del_operativo)%>%
  summarise(dosis_campania = sum(cantidad)) %>%
  mutate(id_ope=paste0(id_operativo,"-",marco_del_operativo)) %>% #para que en el filtro me quede tmb el dispositivo
  mutate(dia_esc = format(fecha,"%A"))#esto hace una columna con los días de la semana
#para que en el filtro me quede tmb el dispositivo

base_camp_aux2<-base_camp_aux2[,-3]


aux_basetus<-matrix(c(0:0),nrow = nrow(base_tus2),ncol = 1)#esta matris la creo por si no tengo datos para que no se rompa todo
colnames(aux_basetus)<-"dosis_campania"
aux_basetus<- as.data.frame(aux_basetus)


if(is.na(base_camp_aux2[1,1])){base_tus2<-cbind(base_tus2,aux_basetus)}else{
  base_tus2 <- left_join(base_tus2,base_camp_aux2)
}#este if es por si no tengo datos esta semana para que no se rompa todo




#para el grafico de la cantidad de operativos

oper_dia_sem_act2 <- base_r_completa %>% filter(semana == max(unique(base_r_completa$semana))-1) %>% group_by(fecha) %>%
  summarise(Cantidad = n()) %>%
  mutate(Día = format(fecha,"%A"))


oper_dia_sem_act2 <-oper_dia_sem_act2[,c(2,3)]

oper_dia_sem_act2$Día <-unlist(lapply(oper_dia_sem_act2$Día, FUN=firstCap))#Pongo la primera letra en mayuscula

oper_dia_sem_act2$Día = factor(oper_dia_sem_act2$Día,c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))


base_tus_sd2 <- SharedData$new(base_tus2) 

### Datos para tablas--------------


base_presta2 <- base_t%>% filter(semana==filt_sem) %>%
  group_by(id_op,especialidad,descrip,uni_medida) %>%
  summarise(cantidad=sum(cantidad))



base_presta_sdsemanapasada <- SharedData$new(base_presta2)


## Listado de op

op_ultsem2<- base_r[,c(6:13)]

op_ultsem2<-op_ultsem2%>% dplyr::filter(semana==filt_sem)#filtro la ultima semana

colnames(op_ultsem2)<-gsub(" ", "_", colnames(op_ultsem2))#quito espacios de las columnas

op_ultsem_dias<- op_ultsem2%>% dplyr::group_by(id_operativo) %>% #cuento la cantidad de dias de cada operativo
  summarise(dias_totales = n())

op_ultsem2<-distinct(op_ultsem2)#quito los duplicados, como ya saque las columnas que tienen que ver con el día de operativo me quedan solo los id de operativos distintos y las ubicaciones distintas

op_ultsem2 <- left_join(op_ultsem2,op_ultsem_dias) #le pego la columna con la cantidad de operativos por día
op_ultsem2 <- op_ultsem2[,c(4,9,2,3,5:7)] #ordeno y quito columnas





### Grafico1:  personas  por día --------------------------------------------
colgraf<-c("#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80")

sda2<-ggplot(base_tus_sd2,mapping = aes(x=Día, y=Cantidad))+
  geom_col(position = "identity",
           #just = 0.5,
           width = 0.5
  ) + 
  scale_y_continuous("")+
  scale_x_discrete("",limits = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")) +
  scale_fill_manual(values = colgraf)+
  scale_color_manual(values = colgraf)+
  theme_classic()


sda2<- ggplotly(sda2) %>% layout(title = "",  showlegend = F) %>% 
  config(modeBarButtonsToRemove=c('zoom2d', 'pan2d', 'select2d',
                                  'lasso2d', 'zoomIn2d', 'zoomOut2d',
                                  'autoScale2d','resetScale2d'))


sda2 <- sda2 %>% layout(title = "",  showlegend = F)


### Gráfico 2: proporcion por especialidad -------------------------


oper2<-unique(base_tus2$id_ope)
oper2<-factor(oper2)
corr2<-nlevels(oper2)
oper2<-as.data.frame(oper2)
oper2$oper2<-as.character(oper2$oper2)
oper2<-cbind(oper2,c(1:nrow(oper2)))


for(y in 1:corr2){
  baseaux <- base_tus2 %>% filter(id_ope==oper2[[y,1]]) %>%
    group_by(especialidad) %>%
    summarise(aux=sum(cantidad))
  oper2[y,2]<-nrow(baseaux)
}



col_operativos<-c("#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80","#2F3237", "#7c7d80")



fig2 <- base_tus_sd2 %>% plot_ly(labels = ~especialidad, values = ~cantidad,
                                 marker = list(colors = col_operativos,
                                               line = list(color = '#FFFFFF', width = 5)),
                                 textinfo = 'label+percent')
fig2 <- fig2 %>% add_pie(hole = 0.3)

grafico5_plotly2 <- fig2 %>% layout(title = "",  showlegend = F)




### Grafico 3: Operativos por día----------------------------------

colgrafo<-c("#000000", "#000000","#000000","#000000","#000000","#000000","#000000")

grafico_opdia2<-ggplot(oper_dia_sem_act2,mapping = aes(x=Día, y=Cantidad))+
  geom_col(position = "identity", fill = "#000000",
           #just = 0.5,
           width = 0.5
  ) + 
  scale_y_continuous("")+
  scale_x_discrete("",limits = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")) +
  scale_fill_manual(values = colgrafo)+
  scale_color_manual(values = colgrafo)+
  theme_classic()

grafico_opdia2<- ggplotly(grafico_opdia2) %>% layout(title = "",  showlegend = F) %>% 
  config(modeBarButtonsToRemove=c('zoom2d', 'pan2d', 'select2d',
                                  'lasso2d', 'zoomIn2d', 'zoomOut2d',
                                  'autoScale2d','resetScale2d'))

grafico_opdia2 <- grafico_opdia2 %>% layout(title = "",  showlegend = F)





#INFORME SUPERVISORES--------------------------------------


firstCap <- function(x) { #esta es una función que pasa de minuscula a nombres propios
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), 
        substring(s, 2),
        sep="", collapse=" ")
}







# FALTANTES ---------------------------------------------------------------

#armo la tabla con los faltantes


base_r2 <- base_r_completa

base_r2 <- base_r2 %>% filter(estado!="Suspendido") %>% filter(estado != "Sin actividad")

base_r2 <- base_r2[,c(-2)]

a <- unique(base_t$id)#vector con todos los operativos que tengo
base_r2$fecha <-ymd(base_r2$fecha)#arreglo el formato de la fecha


for(t in a){
  base_r2<-base_r2 %>% filter(id != t)
} #saco todos los operativos que tengo de la base_r, me quedan los faltanes


falt_ultsem <- base_r2 %>% filter(semana==max(unique(base_r2$semana)))
falt_ultsem <- falt_ultsem[,c(1,2,7:12)]#armo la tabla con solo los faltantes

fech_act <-Sys.Date()

falt_ultsem<-falt_ultsem %>% filter(fecha<=fech_act)

### Value box operativos-------------------------

oper_ultfecha <- max(base_t$fecha, na.rm = TRUE)

asdf = seq(from = oper_ultfecha , to = Sys.Date(), by = 'day')

spam2<-length(asdf)-1#ESTO LO VOY A TENER QUE ARREGLAR PARA CUANDO CAMBIE EL MES



### Gráfico 1: faltantes actual----------------------

ult_sem<-base_r %>% filter(semana == max(unique(base_r$semana)))

r<-unique(ult_sem$id)
r<-matrix(r, ncol=1)
r<-nrow(r)

ult_sem_t<-base_t %>% filter(semana == max(unique(base_t$semana)))

z<-unique(ult_sem_t$id)
z<-matrix(z, ncol=1)
z<-nrow(z)

cant_falt <- matrix(c("No recibidos","Datos recibidos",(r-z),z),nrow=2,ncol=2)

colnames(cant_falt)<-c("desc","cant")

cant_falt<-as.data.frame(cant_falt)

#esto es para que cambie todo de color


a = seq(from =ymd(max(base_t$fecha, na.rm = TRUE)) , to = Sys.Date(), by = 'day')

#a = seq(from =ymd(max(base_t$fecha)) , to = Sys.Date()-1, by = 'day')

ultactual_opera<-length(a)-1

if(ultactual_opera>=2){col_operativos <- c( "#670010","#ff8478",  "#9367bd", "#a35f52", "#008b8b", "#2ca02c","#f781d4", "#9c9a9a","#a4b59e",
                                            "#17bdcf", "#57a8e2", "#bb6df2" )}else{
                                              col_operativos <- c( "#038554","#02ed95",  "#adf7db", "#9cb8ad", "#adf7db", "#9cb8ad","#adf7db", "#9cb8ad","#adf7db",
                                                                   "#9cb8ad","#adf7db", "#bb6df2" )}

cant_falt$cant <- as.numeric(cant_falt$cant)


cant_falt <- cant_falt[order(cant_falt$cant,decreasing = TRUE), ]

fig <- cant_falt %>% plot_ly(labels = ~desc, values = ~cant,
                             marker = list(colors = col_operativos,
                                           line = list(color = '#FFFFFF', width = 5)),
                             textinfo = 'label+percent')
fig <- fig %>% add_pie(hole = 0.3)

graficofaltantes_plotly <- fig %>% layout(title = "",  showlegend = F,
                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



### Gráfico 2: faltantes total---------------------
falt_total <- base_r_completa %>% group_by(estado) %>%
  summarise(cant = n())


falt_total<-convertir_na(falt_total,"0")

falt_total <-falt_total %>% filter(estado != 0) %>% filter(estado != "-")

falt_total$estado <- gsub(pattern = "Averiguar", replacement = "Faltantes", falt_total$estado)

falt_total <- falt_total[order(falt_total$cant,decreasing = TRUE), ]


fig <- falt_total %>% plot_ly(labels = ~estado, values = ~cant,
                             marker = list(colors = col_operativos,
                                           line = list(color = '#FFFFFF', width = 5)),
                             textinfo = 'label+percent')


fig <- fig %>% add_pie(hole = 0.3)

graficofaltantestotal_plotly <- fig %>% layout(title = "",  showlegend = F,
                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



### Tablas---------------------------

colnames(falt_ultsem) <- c("id","fecha","dispositivo","provincia","id_operativo","municipio_departamento_comuna","localidad_barrio","lugar") 

a<-falt_ultsem$id#hago un vector con los id de los dias de  op
falt_ultsem <- falt_ultsem[,c(5,2:4,6:8)] #reordeno porque quiero que id op qeude segundo 
rownames(falt_ultsem)<- a #lo pongo como nombre, esto lo hago para reemplazar el id que me hace automatico DT




falt_ultsem_sd <- SharedData$new(falt_ultsem) #el shared data para los faltantes semanales

b<-base_r2$id #aca estoy armando la tabla de los faltantes totales
base_r2 <- base_r2[,c(9,2,7,8,10:12)]
rownames(base_r2)<- b
colnames(base_r2) <- c("id","fecha","dispositivo","provincia","id_operativo","municipio_departamento_comuna","localidad_barrio","lugar")#arreglo los nombres que tienen espacios


### Campaña de vacunación------------------------------

#Valuebox

#para obtener la ultima fecha de actulización y cuantos días pasaron para que se ponga en rojo

base_camp$fecha<- ymd(base_camp$fecha)

ultfechacamp<-max(base_camp$fecha)


a = seq(from =ultfechacamp , to = Sys.Date(), by = 'day')

spam1<-length(a)-1


#armo una base que tenga la cantidad de operativos de los cuales tengo datos por día

base_camp_aux<-base_camp %>% group_by(fecha, id_operativo)%>%
  summarise(cant_dias=n())

base_camp_aux<-base_camp_aux%>% group_by(fecha)%>%
  summarise(cant_dias=n())
#esto es para generar una matriz que tenga el rango de días desde que empezó la campaña hasta el día actual.

# defining start date
start_date <- as.Date("2022/10/01")

# defining end date
end_date <- Sys.Date()

# generating range of dates
range <- seq(start_date, end_date,"days")
#range<-ymd(range)

matrixaux<-data.frame(range)
colnames(matrixaux)<-c("fecha")


#pego las dos base y me queda la cantidad de operativos con datos por día hasta la fecha con los que no tengo nada tmb
base_camp_aux<-left_join(matrixaux,base_camp_aux)

base_camp_aux<- convertir_na(base_camp_aux,0)

base_camp_aux$cant_dias<-as.numeric(base_camp_aux$cant_dias)

#esto if es para que me cambie de color el grafico cuando pasan muchos días



if(spam1>=7){graf_campfalt<-ggplot(base_camp_aux,mapping = aes(x=fecha, y=cant_dias))+
  geom_col(fill="#670010") + 
  theme_classic()+ 
  scale_x_date("")+
  scale_y_continuous("Registros")}else{
    graf_campfalt<-ggplot(base_camp_aux,mapping = aes(x=fecha, y=cant_dias))+
      geom_col(fill="#038554") + 
      theme_classic()+ 
      scale_x_date("")+
      scale_y_continuous("Registros")
  }

graf_campfalt<- ggplotly(graf_campfalt) %>% layout(title = "",  showlegend = F) %>% 
  config(modeBarButtonsToRemove=c('zoom2d', 'pan2d', 'select2d',
                                  'lasso2d', 'zoomIn2d', 'zoomOut2d',
                                  'autoScale2d','resetScale2d'))
graf_campfalt <- graf_campfalt %>% layout(title = "",  showlegend = F)




### Metodos anticonceptivos-----------------

metodos <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQhUJ_P05qmjRHFDibfuCgASOmxYnaljEiRlNbDCuh7sVxjbuH0kbVv2qZ_uEwyrh3w0uBhr8wN2xG6/pub?gid=152054715&single=true&output=csv",encoding = "utf-8")

metodos$Marca.temporal<-lubridate::dmy_hms(metodos$Marca.temporal)

metodos<- metodos%>% mutate(mes = month(metodos$Marca.temporal))



metodos_red<-metodos[,c(3,4,5,6,7,8,9)]
colnames(metodos_red)<-c("Provincia",	"Municipio/localidad",	"Levonorgestrel + Etinilestradiol (*)",	"Desogestrel (*)",	"Medroxiprogesterona",	"CHIP ISD",	"Gestodeno + Etinilestradiol (*)")


month(Sys.Date())

metodos <-metodos%>% filter(mes == month(Sys.Date()))


entradas_metodos <- nrow(metodos) #esto es para el velocimetro


if(-Inf == max(metodos$Marca.temporal)) {ultimoreg_met<-Sys.Date()-7} else{ultimoreg_met<-max(metodos$Marca.temporal)}

ultimoreg_met<-as.Date(ultimoreg_met,format="%d/%m/%Y")

asdf = seq(from =ultimoreg_met , to = Sys.Date(), by = 'day')

spam<-length(asdf)-1

### Derivaciones------------------------------

#value box

derivaciones <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS4-N7bqVJLOsfwMdosKjkX9s9LC6Gz0KDu8qhMKHtVHP70DzDua2vWGp2dER6JQy8-LLr62vSmDBcG/pub?gid=0&single=true&output=csv", encoding = "UTF-8")

derivaciones$Fecha.corta<-dmy(derivaciones$Fecha.corta)

deriv_ult<-max(derivaciones$Fecha.corta, na.rm = TRUE)

deriv_u = seq(from =deriv_ult , to = Sys.Date(), by = 'day')

deriv_u<-length(deriv_u)-1

#gráfico

cant_segui<-derivaciones%>%group_by(Fecha.corta)%>%
  summarise(cant=n())

if(deriv_u>=7){graf_seguifalt<-ggplot(cant_segui,mapping = aes(x=Fecha.corta, y=cant))+
  geom_col(fill="#670010") + 
  scale_x_date("")+
  scale_y_continuous("Seguimientos")
theme_classic()}else{
  graf_seguifalt<-ggplot(cant_segui,mapping = aes(x=Fecha.corta, y=cant))+
    geom_col(fill="#038554") + 
    theme_classic()+ 
    scale_y_continuous("Seguimientos")+ 
    scale_x_date("")
}

graf_seguifalt<- ggplotly(graf_seguifalt) %>% layout(title = "",  showlegend = F) %>% 
  config(modeBarButtonsToRemove=c('zoom2d', 'pan2d', 'select2d',
                                  'lasso2d', 'zoomIn2d', 'zoomOut2d',
                                  'autoScale2d','resetScale2d'))
graf_seguifalt <- graf_seguifalt %>% layout(title = "",  showlegend = F)



# METAS FISICAS ----------------------------



metas_anual$ejecutado_hasta <- as.numeric(metas_anual$ejecutado_hasta)
metas_anual$porc_ejec <- round(as.numeric(metas_anual$porc_ejec),4)*100

metas_trim$ejec_1_trim <- as.numeric(metas_trim$ejec_1_trim)
metas_trim$ejec_2_trim <- as.numeric(metas_trim$ejec_2_trim)
metas_trim$ejec_3_trim <- as.numeric(metas_trim$ejec_3_trim)
metas_trim$ejec_4_trim <- as.numeric(metas_trim$ejec_4_trim)

metas_trim$porc_ejec1 <- round(as.numeric(metas_trim$porc_ejec1),4)*100
metas_trim$porc_ejec2 <- round(as.numeric(metas_trim$porc_ejec2),4)*100
metas_trim$porc_ejec3 <- round(as.numeric(metas_trim$porc_ejec3),4)*100
metas_trim$porc_ejec4 <- round(as.numeric(metas_trim$porc_ejec4),4)*100

## Metas Anuales ------------------------------

metas_anual <-  metas_anual %>% 
  select(c("descripcion_medicion","descripcion_unidad_de_medida", "programado_acumulado_anual",
           "ejecutado_hasta", "porc_ejec"))


metas_ejec <- ifelse(all(all(is.na(metas_trim$ejec_2_trim)), all(metas_trim$porc_ejec2==0)),"1er",
                     ifelse(all(all(is.na(metas_trim$ejec_3_trim)), all(metas_trim$porc_ejec3==0)),"2do",
                            ifelse(all(all(is.na(metas_trim$ejec_4_trim)), all(metas_trim$porc_ejec4==0)),
                                   "3er","4to")))

# Calculo las sumas de las distintos trimestres para automatizar la actualización en las metas anuales.-
metas_trim <- metas_trim %>% 
  mutate("1er" = prog_1_trim) %>% 
  mutate("2do" = prog_1_trim + prog_2_trim) %>% 
  mutate("3er" = prog_1_trim + prog_2_trim + prog_3_trim) %>% 
  mutate("4to" = prog_1_trim + prog_2_trim + prog_3_trim + prog_4_trim)

metas_p_agregar <- metas_trim %>% 
  select("1er","2do","3er","4to")

metas_anual <- metas_anual %>% 
  cbind(metas_p_agregar)

metas_anual <- metas_anual %>% 
  select(1:5,metas_ejec) %>% 
  relocate(acumulado=metas_ejec,.after = programado_acumulado_anual) %>% 
  mutate(porc_ejec = round((ejecutado_hasta/acumulado),4)*100)

# Valores para los 3 valores de los Gauge (velocímetros)
verde <- sum(metas_anual$porc_ejec >= 90, na.rm = T)
amarillo <- sum(metas_anual$porc_ejec >=60 & metas_anual$porc_ejec < 90, na.rm = T)
rojo <- sum(metas_anual$porc_ejec < 60, na.rm = T)


## Metas Trimestrales -----------------------------------

# 1er trim

metas_trim1 <- metas_trim %>% 
  select("descripcion_medicion","descripcion_unidad_de_medida",
         "prog_1_trim","ejec_1_trim","porc_ejec1")


# 2do trim

metas_trim2 <- metas_trim %>% 
  select("descripcion_medicion","descripcion_unidad_de_medida",
         "prog_2_trim","ejec_2_trim","porc_ejec2")


# 3er trim

metas_trim3 <- metas_trim %>% 
  select("descripcion_medicion","descripcion_unidad_de_medida",
         "prog_3_trim","ejec_3_trim","porc_ejec3")


# 4to trim

metas_trim4 <- metas_trim %>% 
  select("descripcion_medicion","descripcion_unidad_de_medida",
         "prog_4_trim","ejec_4_trim","porc_ejec4")



# RESUMEN GRAFICO ---------------------------------

graf_0 <- dappt_base %>% 
  select(fecha, dia, mes,anio, semana, operativo,especialidad,uni_medida,cantidad) 

graf_1 <- graf_0 %>% 
  group_by(fecha,operativo) %>% 
  summarise(cant= n()) 

#reemplazo cantidad por 1 para todos porque al agrupar me sumaba el mismo operativo varias veces ya que
#son diferentes las columnas especialidad y uni_medida
graf_1$cant <- 1 

#cantidad de dias que participamos en los diferentes operativos (por tipo de operativo)
diasx_oper <- graf_1 %>% 
  group_by(operativo) %>% 
  summarise(canti= n()) %>% 
  mutate(operativo = fct_reorder(operativo, -canti))



## Grafico1 - torta ----------------

col_operativos <- c(  "#9367bd", "#1f77b4", "#008b8b", "#2ca02c","#f781d4", "#9c9a9a","#a4b59e",
                      "#17bdcf", "#57a8e2", "#d62727", "#e17f0e","#b7b81f","#bb6df2","#87ebfa","#9f46e8", "#a35f52" )

fig <- diasx_oper %>% plot_ly(labels = ~operativo, values = ~canti,
                              marker = list(colors = col_operativos))
fig <- fig %>% add_pie(hole = 0.3)

grafico1_plotly <- fig %>% layout(title = "",  showlegend = F,
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))






## Grafico 2: operativos por mes (posición fill todo al 100%).--------------------------------

operas <- dappt_base %>% 
  select(mes,operativo,sem_i,sem_f) %>% 
  dplyr::group_by(mes,operativo) %>% 
  unique()

unique(operas$operativo)

operas$operativo <- factor(operas$operativo, 
                           labels = c("CARPAS SALUDABLES","DAPPTE","DICEI","EETB", "TECNOPOLIS", "HAY EQUIPO", "HOSPITALES NACIONALES", "JUEGOS EVITA", "ESTAR", "MDS"))
#ggplotly(grafico2)

operas <- operas %>% 
  group_by(mes, operativo) %>% 
  summarise(cant= n()) 

grafico2 <- ggplot(operas, aes(mes, cant, fill=operativo)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = col_operativos)+
  labs(x = "Mes", y = "Cantidad de Operativos", fill= "Operativos",) +
  scale_x_continuous (limit = c(0.5,max(operas$mes)+1), breaks= seq(0, max(operas$mes, na.rm = TRUE), 1))+
  theme_classic()

grafico2

ggplotly(grafico2)



##Grafico 3: Persnas anual ----------------
dappt_base <- dappt_base %>% 
  mutate(prestaciones = paste(dappt_base$especialidad,dappt_base$descrip,
                              dappt_base$uni_medida, sep="  //  "))

dappt_base$prestaciones2 <-  case_when(#forma alternativa de hacer el if cuando tengo que hacer muchos if, sobretodo si es con caracter
  NA ~ "Otros",
  str_detect(dappt_base$prestaciones, "Casos positivos  //  Personas") ~ "Otras prestaciones",
  str_detect(dappt_base$prestaciones, "Casos positivos por criterio clínico") ~ "Personas",
  str_detect(dappt_base$prestaciones, "COVID-19  //  Dosis de vacunas aplicadas") ~ "Personas",
  str_detect(dappt_base$prestaciones, "Personas") ~ "Personas",
  TRUE ~ "Otras prestaciones"
)

dappt_base$cantidad <- as.numeric(dappt_base$cantidad)

presta <- dappt_base %>%
  filter(prestaciones2 == "Personas") %>%
  group_by(semana, especialidad, prestaciones2) %>%
  summarise(total = sum(cantidad)) %>%
  ungroup()




#para cubrir el bache de las semanas 11 a la 15 donde no hubo actividad.-



#Grafico 3: Personas atendidas totales por semana en cada especialidad
grafico3 <- ggplot(presta, aes(semana, total, fill = especialidad, ))+ #shape= especialidad
  #geom_point(size = 2, alpha = 0.7, inherit.aes = T, na.rm = T)+
  #scale_shape_manual(values=c(21,22,23))+
  #geom_line(size = 0.5)+ #linetype = especialidad
  geom_col(position = "stack",)+ #Dodge  #Fill #Identity
  #geom_text(aes(label=presta_total$tot), hjust=-0.1,size = 2.8, position = "stack")+ #usar este para poner total
  labs(x = "Semana", y = "Personas Atendidas", fill= "Especialidad",) + # shape = "Especialidad"
  #coord_flip()+
  theme_classic()






## Value boxs-------------
base_r$id_operativo

cant_op <- nrow(as.data.frame(unique(base_r$id_operativo)))#cantidad de operativos

base_r_completa_sin_susp <- base_r_completa %>% filter(estado != "Suspendido")

cant_dias_op <-nrow(base_r_completa_sin_susp)#cantidad de dias de operativos

cant_pers <- base_t %>% dplyr::filter(uni_medida == "Personas") %>%
  summarise(sum(cantidad))#cantidad de participantesde los operativos

cant_espe <- unique(base_t$especialidad)
cant_espe <- matrix(cant_espe,ncol = 1)
cant_espe <-nrow(cant_espe)#cantidad de especialidades



pres_atenmedic <- base_t %>% dplyr::filter(especialidad == c("Atención médica")) %>% dplyr::filter(uni_medida == c("Personas")) %>%
  summarise(sum(cantidad))

pres_enf <- base_t %>% dplyr::filter(especialidad == c("Enfermería")) %>% dplyr::filter(uni_medida == c("Prestaciones")) %>%
  summarise(sum(cantidad))


pres_imag <- base_t %>% dplyr::filter(especialidad == c("Imágenes")) %>% dplyr::filter(uni_medida == c("Placas RX")) %>%
  summarise(sum(cantidad))

pres_odon <- base_t %>% dplyr::filter(especialidad == c("Odontología")) %>% dplyr::filter(uni_medida == c("Prestaciones")) %>%
  summarise(sum(cantidad))

pres_oft <- base_t %>% dplyr::filter(especialidad == c("Oftalmología")) %>% dplyr::filter(uni_medida == c("Lentes recetados")) %>%
  summarise(sum(cantidad))

pres_prom <- base_t %>% dplyr::filter(especialidad == c("Promoción")) %>%dplyr::filter(uni_medida == c("Personas")) %>%
  summarise(sum(cantidad))



pres_sment <- base_t %>% dplyr::filter(especialidad == c("Salud mental")) %>%
  summarise(sum(cantidad))

pres_sment_aux <- base_t %>% dplyr::filter(especialidad == c("Salud mental")) %>% dplyr::filter(uni_medida == c("Personas")) %>%
  summarise(sum(cantidad))

pres_sment <- pres_sment - pres_sment_aux

pres_test <- base_t %>% dplyr::filter(especialidad == c("Testeo COVID-19")) %>% dplyr::filter(uni_medida == c("Personas")) %>%
  summarise(sum(cantidad))

prest_vac <- base_t %>% dplyr::filter(especialidad == c("Vacunación")) %>% dplyr::filter(uni_medida == c("Dosis de vacunas aplicadas")) %>%
  summarise(sum(cantidad))

prest_vac_aux1 <- base_t %>% dplyr::filter(especialidad == c("Vacunación")) %>% dplyr::filter(uni_medida == c("Libretas AUH")) %>%
  summarise(sum(cantidad))

prest_vac_aux2 <- base_t %>% dplyr::filter(especialidad == c("Vacunación")) %>% dplyr::filter(uni_medida == c("Calendarios completados")) %>%
  summarise(sum(cantidad))

prest_vac <- prest_vac + prest_vac_aux1 + prest_vac_aux2


cant_prest <- sum(pres_atenmedic,pres_enf,pres_imag,pres_odon,pres_oft,pres_prom,pres_sment,pres_test,prest_vac)



# DATOS HISTORICOS --------------------------------------------------------


dappt_base_sem <- dappt_base %>% 
  group_by(id_op, semana, operativo , especialidad, descrip, uni_medida) %>% 
  summarise(totales = sum(cantidad, na.rm = T))

dappt_base_sem <-left_join(dappt_base_sem, op_fechas1) #le agrego las fechas de inicio y fin del operativo




dappt_base_sem<- dappt_base_sem[,c(1,8,9,2:7)]



#esta parte es para que me quede el id del operativo como id de la tabla

