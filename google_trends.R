

library("gtrendsR")
library("dplyr")


###Variable: homicidios


homicidios_1_paper <- gtrends(keyword = c("denunciar","denuncia", "homidicio", "lesiones", "robo"), time = "all", geo="MX", Sys.sleep(20))
df_homicidios_1_paper <- homicidios$interest_over_time
df_homicidios_1_paper %>% distinct(keyword)
write.csv(df_homicidios, "df_homicidios_paper_1.csv")

#asalto, violacion, asesinato

###Variable: homicidios adulto

homicidios_adulto <- gtrends(keyword = c("homicidio", "muerte", "asesinato"), time = "all", geo = "MX", Sys.sleep(10))
df_hom_adulto <- homicidios_adulto$interest_over_time
df_hom_adulto %>% distinct(date)
write.csv(df_hom_adulto, "df_hom_adulto.csv")

###variable: homicidios no adulto

homicidios_no_adulto <- gtrends(keyword = c("homicidio menor de edad", "muerte menor de edad", "narcotrafico menor de edad"), 
                                time = "all", geo = "MX", Sys.sleep(10))
df_hom_no_adulto <- homicidios_no_adulto$interest_over_time
df_hom_no_adulto %>% distinct(keyword)
write.csv(df_hom_no_adulto, "df_hom_no_adulto.csv")

homicidios_hombre_adulto <- gtrends(keyword = c("homicidio hombre", "muerte hombre", "narcotrafico hombre"), time = "all",
                                    geo = "MX", Sys.sleep(10))
df_hom_hombre_adulto <- homicidios_hombre_adulto$interest_over_time
df_hom_hombre_adulto %>% distinct(keyword)
write.csv(df_hom_hombre_adulto, "df_hom_hombre_adulto.csv")

####Variable homicidio niño

homicidios_ninos <- gtrends(keyword = c("asesinato niño", "muerte niño", "violencia niño"), time = "all",
                            geo = "MX", Sys.sleep(10))
df_hom_ninos <- homicidios_ninos$interest_over_time
df_hom_ninos %>% distinct(keyword)
write.csv(df_hom_ninos, "df_hom_ninos.csv")

####Variable homocidios mujer adulto

homicidios_mujer_adulto <- gtrends(keyword = c("feminicidio", "muerte mujer", "narcotrafico mujer"), time = "all",
                                   geo = "MX", Sys.sleep(10))
df_hom_muj_adulto <- homicidios_mujer_adulto$interest_over_time
df_hom_muj_adulto %>% distinct(keyword)
write.csv(df_hom_muj_adulto, "df_hom_muj_adulto.csv")


###Variable homicidios niñas


homicidios_ninas <- gtrends(keyword = c("asesinato niña", "muerte niña", "violencia niña", "feminicidio niña"), time = "all",
                            geo = "MX", Sys.sleep(10))
df_hom_ninas <- homicidios_ninas$interest_over_time
df_hom_ninas %>% distinct(keyword)
write.csv(df_hom_ninas, "df_hom_ninas.csv")
