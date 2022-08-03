library(tidyverse)
library(haven)
library(readr)
library(lubridate)
library(summarytools)
library(openxlsx)
library(data.table)

setwd("C:/Users/laura/Dropbox/MAPA/11-Manejo de datos/11-Analisis/Final")

Sys.setenv("HOME"= "c:\\Users\\laura")
Sys.setenv("R_ZIPCMD"= "c:\\Rtools\\bin\\zip")
#localiza el directorio del repositorio a partir de la carpeta dropbox del usuario e importa los scripts R
home.dir <- Sys.getenv("HOME")
home.dir.arr <- strsplit(home.dir,"/")[[1]]
user.dir <- paste(home.dir.arr[1:(length(home.dir.arr)-1)],collapse="/")
#dropbox.dir <- "../../../repostorio_r_IECS"
dropbox.dir <- paste(user.dir,"Dropbox/repostorio_r_IECS",sep="/")

source(paste(dropbox.dir,'fx.REDCap.R',sep="/"))
source(paste(dropbox.dir,'fx.lenguajes.R',sep="/"),encoding = "UTF-8")

source(paste(dropbox.dir,'funciones.tablas.R',sep="/"))
source(paste(dropbox.dir,'fx.REDCap.Frecuencias.R',sep="/"))

lenguaje_por_defecto="ES"

# Proyecto ADHERENCIA
api_key_AD <- "AE3F77A593016DAE692212F922F53F63" 
api_url_AD <- 'http://datos.iecs.org.ar/redcap1/api/'

connection_AD <- fx.REDCap.connection.data(api_url_AD, api_key_AD)

df_plano_AD <- fx.REDCap.api.get.record(connection_AD,c(exportDataAccessGroups = 'true'))
df_plano_AD$redcap_data_access_group <- "Global_AD"
df_metadata_AD <- fx.REDCap.api.get.metadata(connection_AD)
df_labels_AD<- fx.REDCap.apply.metadata(df_plano_AD,df_metadata_AD)
base_AD <- df_plano_AD %>% filter(is.na(redcap_repeat_instance))

#Filtra participantes elegibles
base_AD <- base_AD %>% filter(tam_23 == 1)

#Arma base por participante para el formulario repetible de Farmacia Intervención
base_AD_repet <- df_plano_AD %>% filter(!is.na(redcap_repeat_instance))
base_AD_repet <- base_AD_repet %>% dplyr:: select(starts_with("farm"))
base_AD_repet %>% 
        group_by(farm_id) %>% 
        mutate(n = row_number()) %>% 
        {data.table::dcast(data = setDT(.), farm_id ~ n, value.var = c("farm_id",             "farm_instancia_tpm",  "farm_fecha",          "farm_01",            
                                                                       "farm_02_fecha",       "farm_02a",            "farm_02b",            "farm_02c",           
                                                                       "farm_02d",            "farm_02e",            "farm_02f",            "farm_calc_regla1",   
                                                                       "farm_calc_regla2",    "farm_calc_regla3",    "farm_calc_regla4",    "farm_calc_regla5",   
                                                                       "farm_03",             "farm_03_1",           "farm_03_2",           "farm_03_fecha",      
                                                                       "farm_03a",            "farm_03a_1",          "farm_03b",            "farm_03b_1",         
                                                                       "farm_03c",            "farm_03c_1",         "farm_03d",            "farm_03d_1",         
                                                                       "farm_03e",            "farm_03e_1",          "farm_03f",            "farm_03f_1",         
                                                                       "farm_calc_tpm_1",     "farm_calc_02",        "farm_calc_02_1",      "farm_calc_03",       
                                                                       "farm_calc_tpm",       "farm_calc_tpm_2",     "farm_calc_tpm3",      "farm_calc_tpm_final",
                                                                       "farm_04"
        ))}



base_farm<-as.data.frame(base_AD_repet %>% 
                                 group_by(farm_id) %>% 
                                 mutate(n = row_number()) %>% 
                                 {data.table::dcast(data = setDT(.), farm_id ~ n, value.var = c("farm_id",             "farm_instancia_tpm",  "farm_fecha",          "farm_01",            
                                                                                                "farm_02_fecha",       "farm_02a",            "farm_02b",            "farm_02c",           
                                                                                                "farm_02d",            "farm_02e",            "farm_02f",            "farm_calc_regla1",   
                                                                                                "farm_calc_regla2",    "farm_calc_regla3",    "farm_calc_regla4",    "farm_calc_regla5",   
                                                                                                "farm_03",             "farm_03_1",           "farm_03_2",           "farm_03_fecha",      
                                                                                                "farm_03a",            "farm_03a_1",          "farm_03b",            "farm_03b_1",         
                                                                                                "farm_03c",            "farm_03c_1",         "farm_03d",            "farm_03d_1",         
                                                                                                "farm_03e",            "farm_03e_1",          "farm_03f",            "farm_03f_1",         
                                                                                                "farm_calc_tpm_1",     "farm_calc_02",        "farm_calc_02_1",      "farm_calc_03",       
                                                                                                "farm_calc_tpm",       "farm_calc_tpm_2",     "farm_calc_tpm3",      "farm_calc_tpm_final",
                                                                                                "farm_04"
                                 ))})

base_farm <- base_farm %>% filter(farm_id !="2025f")

#Junta las bases
base_AD <- merge(base_AD, base_farm, by.x = "record_id", by.y = "farm_id", all.x = T)

#write.xlsx(base_AD, file = "base_completa_Adherencia_vbles.xlsx", sheetName = "Base",col.names = TRUE, row.names = TRUE, append = FALSE)



#######ARMADO DE VARIABLES#########

# Tiempo desde la ultima consulta medica hasta comienzo evaluacion PRE 01/11/2019

base_AD$tpm1_01a <- as.Date(base_AD$tpm1_01a)
base_AD$tpoconsultanov <- interval(base_AD$tpm1_01a,ymd('20191101'))#Fecha de corte: 01/11/2019
base_AD$tpoconsultanov <- base_AD$tpoconsultanov %/% days(1)
unique(base_AD$tpoconsultanov)

Descriptpoconsultanov <- data.frame(ptes = "Total",Mean = round(mean(base_AD$tpoconsultanov,na.rm=T),1),SD = round(sd(base_AD$tpoconsultanov,na.rm=T),1), Median = round(median(base_AD$tpoconsultanov,na.rm=T),1), Min = min(base_AD$tpoconsultanov,na.rm=T), Max = max(base_AD$tpoconsultanov,na.rm=T), Missing = sum(is.na(base_AD$tpoconsultanov)))

# Tiempo desde ultimo esquema terapeutico hasta comienzo evaluacion PRE 01/11/2019

base_AD$tpm1_02_fecha <- as.Date(base_AD$tpm1_02_fecha)
base_AD$tpoesquemanov <- interval(base_AD$tpm1_02_fecha,ymd('20191101'))#Fecha de corte: 01/11/2019
base_AD$tpoesquemanov <- base_AD$tpoesquemanov %/% days(1)
unique(base_AD$tpoesquemanov)

Descriptpoesquemanov <- data.frame(ptes = "Total",Mean = round(mean(base_AD$tpoesquemanov,na.rm=T),1),SD = round(sd(base_AD$tpoesquemanov,na.rm=T),1), Median = round(median(base_AD$tpoesquemanov,na.rm=T),1), Min = min(base_AD$tpoesquemanov,na.rm=T), Max = max(base_AD$tpoesquemanov,na.rm=T), Missing = sum(is.na(base_AD$tpoesquemanov)))

# Cantidad de drogas utilizadas

colsp1 <- c('tpm1_02a','tpm1_02b','tpm1_02c','tpm1_02d','tpm1_02e','tpm1_02f' ) 

base_AD$ndrogasesquemanov <- rowSums(base_AD[,colsp1]>0)

Descripndrogasesquemanov <- data.frame(ptes = "Total",Mean = round(mean(base_AD$ndrogasesquemanov,na.rm=T),1),SD = round(sd(base_AD$ndrogasesquemanov,na.rm=T),1), Median = round(median(base_AD$ndrogasesquemanov,na.rm=T),1), Min = min(base_AD$ndrogasesquemanov,na.rm=T), Max = max(base_AD$ndrogasesquemanov,na.rm=T), Missing = sum(is.na(base_AD$ndrogasesquemanov)))

Tabndrogas<-tblFun1(base_AD$ndrogasesquemanov)

# Distribucion de frecuencias por tipo de droga
base_AD$tpm1_02a_cat<-NA
base_AD$tpm1_02a_cat[base_AD$tpm1_02a > 0] <- "Yes"
base_AD$tpm1_02a_cat[base_AD$tpm1_02a == 0] <- "No"
base_AD$tpm1_02a_cat <- factor(base_AD$tpm1_02a_cat, levels = c("Yes", "No"))   
attr(base_AD$tpm1_02a_cat, "label") = "Enalapril"

base_AD$tpm1_02b_cat<-NA
base_AD$tpm1_02b_cat[base_AD$tpm1_02b > 0] <- "Yes"
base_AD$tpm1_02b_cat[base_AD$tpm1_02b == 0] <- "No"
base_AD$tpm1_02b_cat <- factor(base_AD$tpm1_02b_cat, levels = c("Yes", "No"))   
attr(base_AD$tpm1_02b_cat, "label") = "Losartan"

base_AD$tpm1_02c_cat<-NA
base_AD$tpm1_02c_cat[base_AD$tpm1_02c > 0] <- "Yes"
base_AD$tpm1_02c_cat[base_AD$tpm1_02c == 0] <- "No"
base_AD$tpm1_02c_cat <- factor(base_AD$tpm1_02c_cat, levels = c("Yes", "No"))   
attr(base_AD$tpm1_02c_cat, "label") = "Hidroclorot"

base_AD$tpm1_02d_cat<-NA
base_AD$tpm1_02d_cat[base_AD$tpm1_02d > 0] <- "Yes"
base_AD$tpm1_02d_cat[base_AD$tpm1_02d == 0] <- "No"
base_AD$tpm1_02d_cat <- factor(base_AD$tpm1_02d_cat, levels = c("Yes", "No"))   
attr(base_AD$tpm1_02d_cat, "label") = "Furosemida"

base_AD$tpm1_02e_cat<-NA
base_AD$tpm1_02e_cat[base_AD$tpm1_02e > 0] <- "Yes"
base_AD$tpm1_02e_cat[base_AD$tpm1_02e == 0] <- "No"
base_AD$tpm1_02e_cat <- factor(base_AD$tpm1_02e_cat, levels = c("Yes", "No"))   
attr(base_AD$tpm1_02e_cat, "label") = "Atenonol"

base_AD$tpm1_02f_cat<-NA
base_AD$tpm1_02f_cat[base_AD$tpm1_02f > 0] <- "Yes"
base_AD$tpm1_02f_cat[base_AD$tpm1_02f == 0] <- "No"
base_AD$tpm1_02f_cat <- factor(base_AD$tpm1_02f_cat, levels = c("Yes", "No"))   
attr(base_AD$tpm1_02f_cat, "label") = "Amlodipina"

Tabla1<-function(base_AD){
        
        Fila1<-tblFun1(base_AD$tpm1_02a_cat)['Yes',]
        Fila2<-tblFun1(base_AD$tpm1_02b_cat)['Yes',]
        Fila3<-tblFun1(base_AD$tpm1_02c_cat)['Yes',]
        Fila2<-tblFun1(base_AD$tpm1_02d_cat)['Yes',]
        Fila5<-tblFun1(base_AD$tpm1_02e_cat)['Yes',]
        Fila6<-tblFun1(base_AD$tpm1_02f_cat)['Yes',]
        Col<-rbind('Enalapril'=Fila1,'Losartan'=Fila2,
                   'Hidroclorot'=Fila3,'Furosemida'=Fila2,
                   'Atenolol'=Fila5,'Amlodipina'=Fila6)
Col
}
Tabla1_Result<-cbind(Tabla1(base_AD))
View(Tabla1_Result)


#####################TPM PRE INTERVENCION##########################

#TPM NOV 2019

base_AD$tpm1_02a_comp <- base_AD$tpm1_02a*30/10
base_AD$tpm1_02b_comp <- base_AD$tpm1_02b*30/50
base_AD$tpm1_02c_comp <- base_AD$tpm1_02c*30/25
base_AD$tpm1_02d_comp <- base_AD$tpm1_02d*30/20
base_AD$tpm1_02e_comp <- base_AD$tpm1_02e*30/50
base_AD$tpm1_02f_comp <- base_AD$tpm1_02f*30/5

colspresc1 <- c('tpm1_02a_comp','tpm1_02b_comp','tpm1_02c_comp','tpm1_02d_comp','tpm1_02e_comp','tpm1_02f_comp')
colsretir1 <- c('tpm1_03a','tpm1_03b','tpm1_03c','tpm1_03d','tpm1_03e','tpm1_03f')

base_AD$totalprescripto1 <- apply(base_AD[,colspresc1],1,sum, na.rm = TRUE)
base_AD$totalretirado1 <- apply(base_AD[,colsretir1],1,sum, na.rm = TRUE)
base_AD$tpm1 <- (base_AD$totalretirado1/base_AD$totalprescripto1)*100

#TPM DIC 2019
#sin cambios en prescripcion

colsretir2 <- c('tpm2_03a','tpm2_03b','tpm2_03c','tpm2_03d','tpm2_03e','tpm2_03f')
base_AD$totalretirado2 <- apply(base_AD[,colsretir2],1,sum, na.rm = TRUE)
base_AD$tpm2 <- (base_AD$totalretirado2/base_AD$totalprescripto1)*100

#TPM ENE 2020
#sin cambios en prescripcion

colsretir3 <- c('tpm3_03a','tpm3_03b','tpm3_03c','tpm3_03d','tpm3_03e','tpm3_03f')
base_AD$totalretirado3 <- apply(base_AD[,colsretir3],1,sum, na.rm = TRUE)
base_AD$tpm3 <- (base_AD$totalretirado3/base_AD$totalprescripto1)*100

#TPM FEB 2020
#sin cambios en prescripcion

colsretir4 <- c('tpm4_03a','tpm4_03b','tpm4_03c','tpm4_03d','tpm4_03e','tpm4_03f')
base_AD$totalretirado4 <- apply(base_AD[,colsretir4],1,sum, na.rm = TRUE)
base_AD$tpm4 <- (base_AD$totalretirado4/base_AD$totalprescripto1)*100

#TPM ABRIL 2020
#sin cambios en prescripcion

colsretir5 <- c('tpm5_03a','tpm5_03b','tpm5_03c','tpm5_03d','tpm5_03e','tpm5_03f')
base_AD$totalretirado5 <- apply(base_AD[,colsretir5],1,sum, na.rm = TRUE)
base_AD$tpm5 <- (base_AD$totalretirado5/base_AD$totalprescripto1)*100

#TPM MAYO 2020
#sin cambios en prescripcion

colsretir6 <- c('tpm6_03a','tpm6_03b','tpm6_03c','tpm6_03d','tpm6_03e','tpm6_03f')
base_AD$totalretirado6 <- apply(base_AD[,colsretir6],1,sum, na.rm = TRUE)
base_AD$tpm6 <- (base_AD$totalretirado6/base_AD$totalprescripto1)*100

#TPM JUNIO 2020
#sin cambios en prescripcion 

colsretir7 <- c('tpm7_03a','tpm7_03b','tpm7_03c','tpm7_03d','tpm7_03e','tpm7_03f')
base_AD$totalretirado7 <- apply(base_AD[,colsretir7],1,sum, na.rm = TRUE)
base_AD$tpm7 <- (base_AD$totalretirado7/base_AD$totalprescripto1)*100

#TPM JULIO 2020
#sin cambios en prescripcion 

colsretir8 <- c('tpm8_03a','tpm8_03b','tpm8_03c','tpm8_03d','tpm8_03e','tpm8_03f')
base_AD$totalretirado8 <- apply(base_AD[,colsretir8],1,sum, na.rm = TRUE)
base_AD$tpm8 <- (base_AD$totalretirado8/base_AD$totalprescripto1)*100

#####################TPM POST INTERVENCION##########################

#TPM POST 1 

#prescripcion 1
base_AD$tpm1_post_02a_comp <- base_AD$farm_02a_1*30/10
base_AD$tpm1_post_02b_comp <- base_AD$farm_02b_1*30/50
base_AD$tpm1_post_02c_comp <- base_AD$farm_02c_1*30/25
base_AD$tpm1_post_02d_comp <- base_AD$farm_02d_1*30/20
base_AD$tpm1_post_02e_comp <- base_AD$farm_02e_1*30/50
base_AD$tpm1_post_02f_comp <- base_AD$farm_02f_1*30/5
colspresc1_post <- c('tpm1_post_02a_comp','tpm1_post_02b_comp','tpm1_post_02c_comp','tpm1_post_02d_comp','tpm1_post_02e_comp','tpm1_post_02f_comp')
base_AD$totalprescripto1_post <- apply(base_AD[,colspresc1_post],1,sum, na.rm = TRUE)

#retiros 1
colsretir1_post <- c('farm_03a_1.y','farm_03b_1.y','farm_03c_1.y','farm_03d_1.y','farm_03e_1.y','farm_03f_1.y')
base_AD$totalretirado1_post <- apply(base_AD[,colsretir1_post],1,sum, na.rm = TRUE)

#TPM POST 2 

#prescripcion 2
base_AD$tpm2_post_02a_comp <- base_AD$farm_02a_2*30/10
base_AD$tpm2_post_02b_comp <- base_AD$farm_02b_2*30/50
base_AD$tpm2_post_02c_comp <- base_AD$farm_02c_2*30/25
base_AD$tpm2_post_02d_comp <- base_AD$farm_02d_2*30/20
base_AD$tpm2_post_02e_comp <- base_AD$farm_02e_2*30/50
base_AD$tpm2_post_02f_comp <- base_AD$farm_02f_2*30/5
colspresc2_post <- c('tpm2_post_02a_comp','tpm2_post_02b_comp','tpm2_post_02c_comp','tpm2_post_02d_comp','tpm2_post_02e_comp','tpm2_post_02f_comp')
base_AD$totalprescripto2_post <- apply(base_AD[,colspresc2_post],1,sum, na.rm = TRUE)

#retiros 2
colsretir2_post <- c('farm_03a_2','farm_03b_2','farm_03c_2','farm_03d_2','farm_03e_2','farm_03f_2')
base_AD$totalretirado2_post <- apply(base_AD[,colsretir2_post],1,sum, na.rm = TRUE)

#TPM POST 3 

#prescripcion 3
base_AD$tpm3_post_02a_comp <- base_AD$farm_02a_3*30/10
base_AD$tpm3_post_02b_comp <- base_AD$farm_02b_3*30/50
base_AD$tpm3_post_02c_comp <- base_AD$farm_02c_3*30/25
base_AD$tpm3_post_02d_comp <- base_AD$farm_02d_3*30/20
base_AD$tpm3_post_02e_comp <- base_AD$farm_02e_3*30/50
base_AD$tpm3_post_02f_comp <- base_AD$farm_02f_3*30/5
colspresc3_post <- c('tpm3_post_02a_comp','tpm3_post_02b_comp','tpm3_post_02c_comp','tpm3_post_02d_comp','tpm3_post_02e_comp','tpm3_post_02f_comp')
base_AD$totalprescripto3_post <- apply(base_AD[,colspresc3_post],1,sum, na.rm = TRUE)

#retiros 3
colsretir3_post <- c('farm_03a_3','farm_03b_3','farm_03c_3','farm_03d_3','farm_03e_3','farm_03f_3')
base_AD$totalretirado3_post <- apply(base_AD[,colsretir3_post],1,sum, na.rm = TRUE)

#TPM POST 4 

#prescripcion 4
base_AD$tpm4_post_02a_comp <- base_AD$farm_02a_4*30/10
base_AD$tpm4_post_02b_comp <- base_AD$farm_02b_4*30/50
base_AD$tpm4_post_02c_comp <- base_AD$farm_02c_4*30/25
base_AD$tpm4_post_02d_comp <- base_AD$farm_02d_4*30/20
base_AD$tpm4_post_02e_comp <- base_AD$farm_02e_4*30/50
base_AD$tpm4_post_02f_comp <- base_AD$farm_02f_4*30/5
colspresc4_post <- c('tpm4_post_02a_comp','tpm4_post_02b_comp','tpm4_post_02c_comp','tpm4_post_02d_comp','tpm4_post_02e_comp','tpm4_post_02f_comp')
base_AD$totalprescripto4_post <- apply(base_AD[,colspresc4_post],1,sum, na.rm = TRUE)

#retiros 4
colsretir4_post <- c('farm_03a_4','farm_03b_4','farm_03c_4','farm_03d_4','farm_03e_4','farm_03f_4')
base_AD$totalretirado4_post <- apply(base_AD[,colsretir4_post],1,sum, na.rm = TRUE)

#TPM POST 5 

#prescripcion 5
base_AD$tpm5_post_02a_comp <- base_AD$farm_02a_5*30/10
base_AD$tpm5_post_02b_comp <- base_AD$farm_02b_5*30/50
base_AD$tpm5_post_02c_comp <- base_AD$farm_02c_5*30/25
base_AD$tpm5_post_02d_comp <- base_AD$farm_02d_5*30/20
base_AD$tpm5_post_02e_comp <- base_AD$farm_02e_5*30/50
base_AD$tpm5_post_02f_comp <- base_AD$farm_02f_5*30/5
colspresc5_post <- c('tpm5_post_02a_comp','tpm5_post_02b_comp','tpm5_post_02c_comp','tpm5_post_02d_comp','tpm5_post_02e_comp','tpm5_post_02f_comp')
base_AD$totalprescripto5_post <- apply(base_AD[,colspresc5_post],1,sum, na.rm = TRUE)

#retiros 5
colsretir5_post <- c('farm_03a_5','farm_03b_5','farm_03c_5','farm_03d_5','farm_03e_5','farm_03f_5')
base_AD$totalretirado5_post <- apply(base_AD[,colsretir5_post],1,sum, na.rm = TRUE)

#TPM POST 6 

#prescripcion 6
base_AD$tpm6_post_02a_comp <- base_AD$farm_02a_6*30/10
base_AD$tpm6_post_02b_comp <- base_AD$farm_02b_6*30/50
base_AD$tpm6_post_02c_comp <- base_AD$farm_02c_6*30/25
base_AD$tpm6_post_02d_comp <- base_AD$farm_02d_6*30/20
base_AD$tpm6_post_02e_comp <- base_AD$farm_02e_6*30/50
base_AD$tpm6_post_02f_comp <- base_AD$farm_02f_6*30/5
colspresc6_post <- c('tpm6_post_02a_comp','tpm6_post_02b_comp','tpm6_post_02c_comp','tpm6_post_02d_comp','tpm6_post_02e_comp','tpm6_post_02f_comp')
base_AD$totalprescripto6_post <- apply(base_AD[,colspresc6_post],1,sum, na.rm = TRUE)

#retiros 6
colsretir6_post <- c('farm_03a_6','farm_03b_6','farm_03c_6','farm_03d_6','farm_03e_6','farm_03f_6')
base_AD$totalretirado6_post <- apply(base_AD[,colsretir6_post],1,sum, na.rm = TRUE)

#CORRECCIONES POR DROP-OUT
#1002w FALLECIDO EN JULIO 2020 (ENF HEPATOBILIAR, CA?), SE TOMA PRIMEROS 3 MESES, MISSING PARA ULT 3 MESES
base_AD$totalretirado4_post[base_AD$record_id == "1002w"] <- NA
base_AD$totalretirado5_post[base_AD$record_id == "1002w"] <- NA
base_AD$totalretirado6_post[base_AD$record_id == "1002w"] <- NA
base_AD$totalprescripto4_post[base_AD$record_id == "1002w"] <- NA
base_AD$totalprescripto5_post[base_AD$record_id == "1002w"] <- NA
base_AD$totalprescripto6_post[base_AD$record_id == "1002w"] <- NA

#5001v FALLECIDO EN MAYO 2020 (IAM), SE TOMA PRIMEROS 3 MESES, MISSING PARA ULT 3 MESES
base_AD$totalretirado4_post[base_AD$record_id == "5001v"] <- NA
base_AD$totalretirado5_post[base_AD$record_id == "5001v"] <- NA
base_AD$totalretirado6_post[base_AD$record_id == "5001v"] <- NA
base_AD$totalprescripto4_post[base_AD$record_id == "5001v"] <- NA
base_AD$totalprescripto5_post[base_AD$record_id == "5001v"] <- NA
base_AD$totalprescripto6_post[base_AD$record_id == "5001v"] <- NA

#2005j SE MUDA FUERA DEL AREA DE COBERTURA DEL CAPS, 
#TIENE LLAMADOS HASTA EL AGOSTO, ENTRAN TODOS LOS PERIODOS 

#4023b PERDIDA DE SEGUIMIENTO PARA VISITA FINAL,
#TIENE LLAMADO POR ADHERENCIA INSUFICIENTE CON CONTACTO HASTA AGOSTO, ENTRAN TODOS LOS PERIODOS

#TPM mensual 1
base_AD$tpm1_post <- (base_AD$totalretirado1_post/base_AD$totalprescripto1_post)*100

#TPM mensual 2
base_AD$tpm2_post <- (base_AD$totalretirado2_post/base_AD$totalprescripto2_post)*100

#TPM mensual 3
base_AD$tpm3_post <- (base_AD$totalretirado3_post/base_AD$totalprescripto3_post)*100

#TPM mensual 4
base_AD$tpm4_post <- (base_AD$totalretirado4_post/base_AD$totalprescripto4_post)*100

#TPM mensual 5
base_AD$tpm5_post <- (base_AD$totalretirado5_post/base_AD$totalprescripto5_post)*100

#TPM mensual 6
base_AD$tpm6_post <- (base_AD$totalretirado6_post/base_AD$totalprescripto6_post)*100



 #############
# Tabla descripcion participantes
                                                                              
TabSexo<-tblFun(base_AD$tam_02)
TabSexo
DescEdad <- c(
        Mean = round(mean(base_AD$tam_04,na.rm = T),1),
        SD = round(sd(base_AD$tam_04, na.rm = T),1),
        Med =  median(base_AD$tam_04, na.rm = T),
        Max = max(base_AD$tam_04, na.rm = T),
        Min = min(base_AD$tam_04, na.rm = T))
DescEdad
DescTAS <- c(
        Mean = round(mean(base_AD$tam_21a,na.rm = T),1),
        SD = round(sd(base_AD$tam_21a, na.rm = T),1),
        Med =  median(base_AD$tam_21a, na.rm = T),
        Max = max(base_AD$tam_21a, na.rm = T),
        Min = min(base_AD$tam_21a, na.rm = T))
DescTAS
DescTAD <- c(
        Mean = round(mean(base_AD$tam_21b,na.rm = T),1),
        SD = round(sd(base_AD$tam_21b, na.rm = T),1),
        Med =  median(base_AD$tam_21b, na.rm = T),
        Max = max(base_AD$tam_21b, na.rm = T),
        Min = min(base_AD$tam_21b, na.rm = T))
DescTAD

base_AD$TACateg <-NA
base_AD$TACateg[(base_AD$tam_21a >= 140) | (base_AD$tam_21b >= 90)] <- 1
base_AD$TACateg[(base_AD$tam_21a >= 160) | (base_AD$tam_21b >= 100)] <- 2
base_AD$TACateg[(base_AD$tam_21a >= 180) | (base_AD$tam_21b >= 110)] <- 3
base_AD$TACateg
TabTACateg<-tblFun(base_AD$TACateg)
TabTACateg

TabEstCiv<-tblFun(base_AD$inicial_01)
TabEstCiv

TabEduc<-tblFun(base_AD$inicial_02)
TabEduc

TabSitLab<-tblFun(base_AD$inicial_03)
TabSitLab

TabCobSalud<-tblFun(base_AD$final_01)
TabCobSalud

TabCovid<-tblFun(base_AD$inicial_04)
TabCovid
TabIntCovid<-tblFun(base_AD$inicial_05)
TabIntCovid

base_AD$Tabaco <-NA
base_AD$Tabaco[base_AD$inicial_06 == 1] <- 1
base_AD$Tabaco[base_AD$inicial_07 == 1 & base_AD$inicial_08 ==1] <- 1
base_AD$Tabaco[base_AD$inicial_07 == 1 & base_AD$inicial_08 ==2] <- 2
base_AD$Tabaco[base_AD$inicial_06 == 2 & base_AD$inicial_07 == 2 ] <- 3
TabTabaco<-tblFun(base_AD$Tabaco)
TabTabaco

TabHT<-tblFun(base_AD$inicial_09)
TabHT

TabDBT<-tblFun(base_AD$inicial_10)
TabDBT

TabHipolip<-tblFun(base_AD$inicial_11)
TabHipolip

TabInsul<-tblFun(base_AD$inicial_12)
TabInsul

TabHipercol<-tblFun(base_AD$inicial_13)
TabHipercol

TabEstat1<-tblFun(base_AD$inicial_13)
TabEstat1

TabEstat2<-tblFun(base_AD$final_12)
TabEstat2

TabAspir<-tblFun(base_AD$inicial_15)
TabAspir

TabIAM<-tblFun(base_AD$inicial_16___1)
TabIAM

TabACV<-tblFun(base_AD$inicial_16___2)
TabACV

TabCA<-tblFun(base_AD$inicial_16___3)
TabCA

TabAsma<-tblFun(base_AD$inicial_16___4)
TabAsma

TabEPOC<-tblFun(base_AD$inicial_16___5)
TabEPOC

DescConsA <- quantile (base_AD$final_14a,na.rm = T)
DescConsA
DescConsB <- quantile (base_AD$final_14b,na.rm = T)
DescConsB
DescConsC <- quantile (base_AD$final_14c,na.rm = T)
DescConsC


############CALCULOS DE TPM por PERIODOS y TABLAS##############

#TPM TOTAL PRE - PRE Pandemia e INTRA Pandemia

colsdretir <- c('totalretirado1','totalretirado2','totalretirado3','totalretirado4','totalretirado5','totalretirado6',
                'totalretirado7','totalretirado8')
base_AD$totalretirado_pre <- apply(base_AD[,colsdretir],1,sum, na.rm = TRUE)
base_AD$tpmtot_pre <- (base_AD$totalretirado_pre/(base_AD$totalprescripto1*8))*100
unique(base_AD$tpmtot_pre)

base_AD$tpmtot_pre_adec <-NA
base_AD$tpmtot_pre_adec[base_AD$tpmtot_pre >= 80] <- 1
base_AD$tpmtot_pre_adec[(base_AD$tpmtot_pre >= 0)& (base_AD$tpmtot_pre < 80)] <- 0
Tabtpmtot_pre_adec<-tblFun(base_AD$tpmtot_pre_adec)
Tabtpmtot_pre_adec
Meantpmtot_pre<-mean(base_AD$tpmtot_pre)
Meantpmtot_pre

colsdretirprepan <- c('totalretirado1','totalretirado2','totalretirado3','totalretirado4')
base_AD$totalretiradoprepan <- apply(base_AD[,colsdretirprepan],1,sum, na.rm = TRUE)
base_AD$tpmtotprepan <- (base_AD$totalretiradoprepan/(base_AD$totalprescripto1*4))*100
unique(base_AD$tpmtotprepan)

base_AD$tpmtot_prepan_adec <-NA
base_AD$tpmtot_prepan_adec[base_AD$tpmtotprepan >= 80] <- 1
base_AD$tpmtot_prepan_adec[(base_AD$tpmtotprepan >= 0)& (base_AD$tpmtotprepan < 80)] <- 0
Tabtpmtot_prepan_adec<-tblFun(base_AD$tpmtot_prepan_adec)
Tabtpmtot_prepan_adec
Meantpmtot_prepan<-mean(base_AD$tpmtotprepan)
Meantpmtot_prepan

colsdretirintrapan <- c('totalretirado5','totalretirado6','totalretirado7','totalretirado8')
base_AD$totalretiradointrapan <- apply(base_AD[,colsdretirintrapan],1,sum, na.rm = TRUE)
base_AD$tpmtotintrapan <- (base_AD$totalretiradointrapan/(base_AD$totalprescripto1*4))*100
unique(base_AD$tpmtotintrapan)

base_AD$tpmtot_intrapan_adec <-NA
base_AD$tpmtot_intrapan_adec[base_AD$tpmtotintrapan >= 80] <- 1
base_AD$tpmtot_intrapan_adec[(base_AD$tpmtotintrapan >= 0)& (base_AD$tpmtotintrapan < 80)] <- 0
Tabtpmtot_intrapan_adec<-tblFun(base_AD$tpmtot_intrapan_adec)
Tabtpmtot_intrapan_adec
Meantpmtot_intrapan<-mean(base_AD$tpmtotintrapan)
Meantpmtot_intrapan

#TPM TOTAL POST - Primeros 3 meses y Ultimos 3 meses

colsdretir_post <- c('totalretirado1_post','totalretirado2_post','totalretirado3_post',
                     'totalretirado4_post','totalretirado5_post','totalretirado6_post')
base_AD$totalretirado_post <- apply(base_AD[,colsdretir_post],1,sum, na.rm = TRUE)
colsdpresc_post <- c('totalprescripto1_post','totalprescripto2_post','totalprescripto3_post',
                     'totalprescripto4_post','totalprescripto5_post','totalprescripto6_post')
base_AD$totalprescripto_post <- apply(base_AD[,colsdpresc_post],1,sum, na.rm = TRUE)

base_AD$tpmtot_post <- (base_AD$totalretirado_post/base_AD$totalprescripto_post)*100
unique(base_AD$tpmtot_post)

base_AD$tpmtot_post_adec <-NA
base_AD$tpmtot_post_adec[base_AD$tpmtot_post >= 80] <- 1
base_AD$tpmtot_post_adec[(base_AD$tpmtot_post >= 0)& (base_AD$tpmtot_post < 80)] <- 0
Tabtpmtot_post_adec<-tblFun(base_AD$tpmtot_post_adec)
Tabtpmtot_post_adec
Meantpmtot_post<-mean(base_AD$tpmtot_post)
Meantpmtot_post

colsdretirprim3m <- c('totalretirado1_post','totalretirado2_post','totalretirado3_post')
base_AD$totalretiradoprim3m <- apply(base_AD[,colsdretirprim3m],1,sum, na.rm = TRUE)
colsdprescprim3m <- c('totalprescripto1_post','totalprescripto2_post','totalprescripto3_post')
base_AD$totalprescriptoprim3m <- apply(base_AD[,colsdprescprim3m],1,sum, na.rm = TRUE)

base_AD$tpmprim3m <- (base_AD$totalretiradoprim3m/base_AD$totalprescriptoprim3m)*100
unique(base_AD$tpmprim3m)

base_AD$tpmprim3m_adec <-NA
base_AD$tpmprim3m_adec[base_AD$tpmprim3m >= 80] <- 1
base_AD$tpmprim3m_adec[(base_AD$tpmprim3m >= 0)& (base_AD$tpmprim3m < 80)] <- 0
Tabtpmprim3m_adec<-tblFun(base_AD$tpmprim3m_adec)
Tabtpmprim3m_adec
Meantpmprim3m<-mean(base_AD$tpmprim3m)
Meantpmprim3m

colsdretirult3m <- c('totalretirado4_post','totalretirado5_post','totalretirado6_post')
base_AD$totalretiradoult3m <- apply(base_AD[,colsdretirult3m],1,sum, na.rm = TRUE)
colsdprescult3m <- c('totalprescripto4_post','totalprescripto4_post','totalprescripto6_post')
base_AD$totalprescriptoult3m <- apply(base_AD[,colsdprescult3m],1,sum, na.rm = TRUE)

base_AD$tpmult3m <- (base_AD$totalretiradoult3m/base_AD$totalprescriptoult3m)*100
unique(base_AD$tpmult3m)

base_AD$tpmult3m_adec <-NA
base_AD$tpmult3m_adec[base_AD$tpmult3m >= 80] <- 1
base_AD$tpmult3m_adec[(base_AD$tpmult3m >= 0)& (base_AD$tpmult3m < 80)] <- 0
Tabtpmult3m_adec<-tblFun(base_AD$tpmult3m_adec)
Tabtpmult3m_adec
Meantpmult3m<-mean(base_AD$tpmult3m, na.rm = TRUE)
Meantpmult3m


#########Tablas Presión arterial###########

DescTASpre <- c(
        Mean = round(mean(base_AD$tam_21a,na.rm = T),1),
        SD = round(sd(base_AD$tam_21a, na.rm = T),1))
DescTASpre

DescTADpre <- c(
        Mean = round(mean(base_AD$tam_21b,na.rm = T),1),
        SD = round(sd(base_AD$tam_21b, na.rm = T),1))
DescTADpre


DescTASpost <- c(
        Mean = round(mean(base_AD$final_20a,na.rm = T),1),
        SD = round(sd(base_AD$final_20a, na.rm = T),1))
DescTASpost

DescTADpost <- c(
        Mean = round(mean(base_AD$final_20b,na.rm = T),1),
        SD = round(sd(base_AD$final_20b, na.rm = T),1))
DescTADpost


base_AD$diferencia_TAS <- base_AD$final_20a-base_AD$tam_21a
base_AD$diferencia_TAD <- base_AD$final_20b-base_AD$tam_21b
DescDifTAS <- c(
        Mean = round(mean(base_AD$diferencia_TAS,na.rm = T),1),
        SD = round(sd(base_AD$diferencia_TAS, na.rm = T),1))
DescDifTAS
DescDifTAD <- c(
        Mean = round(mean(base_AD$diferencia_TAD,na.rm = T),1),
        SD = round(sd(base_AD$diferencia_TAD, na.rm = T),1))
DescDifTAD

base_AD$TACateg_post <-NA
base_AD$TACateg_post[(base_AD$final_20a < 140) | (base_AD$final_20b < 90)] <- 0
base_AD$TACateg_post[(base_AD$final_20a >= 140) | (base_AD$final_20b >= 90)] <- 1
base_AD$TACateg_post[(base_AD$final_20a >= 160) | (base_AD$final_20b >= 100)] <- 2
base_AD$TACateg_post[(base_AD$final_20a >= 180) | (base_AD$final_20b >= 110)] <- 3
base_AD$TACateg_post
TabTACateg_post<-tblFun(base_AD$TACateg_post)
TabTACateg_post


DescTASpost <- c(
        Mean = round(mean(base_AD$final_20a[base_AD$tpmtot_post_adec == 1],na.rm = T),1),
        SD = round(sd(base_AD$final_20a[base_AD$tpmtot_post_adec == 1], na.rm = T),1))
DescTASpost

DescTADpost <- c(
        Mean = round(mean(base_AD$final_20b[base_AD$tpmtot_post_adec == 1],na.rm = T),1),
        SD = round(sd(base_AD$final_20b[base_AD$tpmtot_post_adec == 1], na.rm = T),1))
DescTADpost

DescTASpost <- c(
        Mean = round(mean(base_AD$final_20a[base_AD$tpmtot_post_adec == 0],na.rm = T),1),
        SD = round(sd(base_AD$final_20a[base_AD$tpmtot_post_adec == 0], na.rm = T),1))
DescTASpost

DescTADpost <- c(
        Mean = round(mean(base_AD$final_20b[base_AD$tpmtot_post_adec == 0],na.rm = T),1),
        SD = round(sd(base_AD$final_20b[base_AD$tpmtot_post_adec == 0], na.rm = T),1))
DescTADpost