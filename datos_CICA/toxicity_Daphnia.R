library("drc")
datos <- read_csv("datos_CICA/datos_CICA_Daphnia.csv")
names(datos) <- c("conc", "total", "alive", "genera", "subj")
mod <- drm(alive / total ~ conc, weights = total , fct = LL.3() ,  
           type = "binomial", data = datos[datos$genera=="Macroteleia"&
                                             datos$subj==2,])
summary(mod)
