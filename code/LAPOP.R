# Artículo Corrupción y Acceso a la educación pública superior
# 2004-2019

if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
url <- "https://raw.githubusercontent.com/laboratoriolide/americas-barometer/main/output/csv/ab_04_09.csv"
download.file(url, here("data/ab_04_19.csv"))
df <- read.csv("data/ab_04_19.csv")

# Diseño Muestral
dm <- svydesign(ids = ~ upm,
          strata = ~ estratopri, 
          weights = ~ wt, 
          nest = TRUE,
          na.action = 'na.exclude',
          data = df)

# Limpieza de datos
exc16 <- factor(df$exc16)  
exc16 <- ifelse(df$exc16 == 'Yes' | df$exc16 == 'Si' | df$exc16 == 'Sí' , 1,
                  ifelse(df$exc16 == 'No', 0, NA))

# Tabulación con pesos de muestra
exc16_tab <- svyby(formula = ~exc16, 
                   by = ~year, 
                   design = dm,
                   FUN = svymean,
                   na.rm = T,
                   keep.names = F) %>%
  filter(exc16 != 0) 

# Graph
df %>%
    summarize(exc16 = mean(exc16, na.rm = T)) %>% 
    filter(!is.na(exc16)) %>% 
    ggplot(exc16_tab, aes(x = as.factor(year), y = exc16)) + geom_col()
  
comentario





