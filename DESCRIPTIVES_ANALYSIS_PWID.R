
# ==========================================================================#
# ANALISES PARA O ARTIGO DA DR. AURIA - PID MULEHRES
# CODIGO CRIADO POR : SHELSEA DAMIAO
#===========================================================================#

# RESPOSITORIO PRIVADO DO GITHUB


# PACOTES USADOS
 if(!require(dplyr)) install.packages("dplyr")
 if(!require(tidyverse)) install.packages("tidyverse")
 if(!require(janitor)) install.packages ("janitor")
 if(!require(openxlsx)) install.packages("openxlsx")
 
 # IMPORTACAO DA BASE DE DADOS
 
 BASE <- read.xlsx(choose.files())
 
 # FILTRAR A BASE POR SEXO
 #unique(BASE$SCREEN1)
 #table(BASE$SCREEN1)
 
 BASE_WWID <-  subset(BASE, BASE$SCREEN1 == "Feminino") #base de dados WWID
 
 # VARIAVEIS USADAS
 
 vars <- c("DEIDENT", "SCREEN1", "DESEXNOW", "DEBORN", "IDENT_DEBORN_OTHER", "DEMEDU1",
           "DEEDHIGH", "IDENT_DEEDHIGH_OTHER", "DERELIG", "IDENT_DERELIG_OTHER", "DEREG",
           "DELIVESX", "LIMF1_AGE_M", "LIMF1_AGE_MA", "LIMMSIX"
           )
 
 
 # CATEGORIZAR A BASE DE DADOS

 
 BASE_WWID <- BASE_WWID |> mutate(   PROVINCIA = case_when(DEREG %in% c("Maputo_Provincia", "Maputo_Cidade") ~ "1_Maputo",
                                                           DEREG == "Sofala" ~ "2_Beira",
                                                           DEREG == "Tete" ~ "3_Tete",
                                                           DEREG == "Zambezia" ~ "4_Quelimane",
                                                           DEREG == "Nampula" ~ "5_Nampula",
                                                           .default = NA), #provincia
                                     
                                  ORIENTACAO_SEXUAL = 
                                     case_when( DEIDENT == "Heterosexual" ~ "1_heterosexual"
                                       DEIDENT =="Homosexual" ~ "2_homosexual",
                                      DEIDENT == "Bisexual" ~ "3_bisexual"
                                       ), # orientacao sexual
                                  IDENTIDADE_GENERO = case_when( DESEXNOW == "Mulher" ~ "1_Mulher",
                                                               DESEXNOW %in% c("Homem", "Mulher") | IDENT_DEBORN_OTHER == "Nova Iorque" ~ "2_trans",
                                                               .default = NA
                                                                 ), # identidade de genero
                                  NACIONALIDADE = case_when( DEBORN == "Mocambicana" ~ "1_mocambicana", 
                                                             DEBORN %in% c("Malawiana", "Mocambicana",
                                                                           "Sul_Aricana", "Zimbabeana", "Outra")~
                                                                       "2_estrangeira",
                                                             .default = NA), # nacionalidade
                                  ESCOLARIDADE = case_when( DEMEDU1 == "Nao" | DEEDHIGH == "Sem_escolaridade" ~ "1_sem_escolaridade",
                                                            DEEDHIGH == "Primario_ou_Alfabetizacao" ~ "2_primario/alfabetizacao",
                                                            DEEDHIGH %in% c("Secundario", "Tecnico", "Superior", "Outra") | IDENT_DEEDHIGH_OTHER == "1 ANO INSTITUTO" ~ "3_secundario/tecnico/superior",
                                                            .default = NA), # escolaridade
                                  RELIGIAO = case_when(DERELIG %in% c("Catolica", "Protestante_Evangelica") | IDENT_DERELIG_OTHER =! NA
                                                          ~ "1_cristao",
                                                       DERELIG == "Muculmana" ~ "2_Muculmana",
                                                       DERELIG %in% c("Animista", "Sem_relegiao", "Siao_Zione", "Outra") ~ "3_outra/nenhuma"), #religiao
                                  ESTADO_CIVIL = case_when(DEMARSTA == "Solteiro" ~ "1_Solteira",
                                                           DEMRASTA %in% c("Uniao_de_factos", "Casado") | DELIVESX == "Sim" ~ "2_casada/vive_maritalmente",
                                                           DEMARSTA %in% c("Divorciado", "Separado", "Viuvo") ~ "3_Divorciada/separada/viuva",
                                                           .default = NA), # estado civil
                                  ACT_TECTO = case_when(DEACT == "Nao" ~ "2_Nao",
                                                        DEACT == "Sim" ~ "1_Sim",
                                                        .default = NA),
                                  IDADE_SEXO = case_when(LIMF1_AGE_M == "Anos"  & LIMF1_AGE_MA < 18 ~ "1_<18",
                                                         LIMF1_AGE_M == "Anos" & LIMF1_AGE_MA >= 18 ~ "2_>=18"),
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  )
                                  
                                  
                                  
                                  
   
 )
   
 
 