# Artículo Corrupción y Acceso a la educación pública superior
# 2004-2019

# Cargar librerías
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")

# Cargar datos

url <- "https://raw.githubusercontent.com/laboratoriolide/americas-barometer/main/output/csv/ab_04_09.csv"

download.file(url, here("data/ab_04_19.csv"))

df <- read.csv("data/ab_04_19.csv")

# Limpieza de datos de exc16

df$exc16 <- factor(df$exc16)

df$exc16 <- ifelse(df$exc16 == 'Yes' | df$exc16 == 'Si' | df$exc16 == 'Sí' | df$exc16 == 'S\xed', 1,
                ifelse(df$exc16 == 'No', 0, NA))

# Diseño Muestral de exc16
dm <- svydesign(ids = ~ upm,
          strata = ~ estratopri, 
          weights = ~ weight1500, 
          nest = TRUE,
          na.action = 'na.exclude',
          data = df)

# Tabulación con pesos de muestra exc16
exc16_tab <- svyby(formula = ~ exc16, 
                   by = ~ year, 
                   design = dm,
                   FUN = svymean,
                   na.rm = T,
                   keep.names = F)

# Agregar colores diferentes antes y después

exc16_tab <-
  exc16_tab %>% 
  mutate(colors = if_else(year >= 2012, '#2E5994', '#7bd9f2'))


# Tema para gráficos de ggplot2
theme_article_educacion <-
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "grey20"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey30", hjust = 0, face = 'italic'),
        legend.background = element_blank())

# Graph de exc16
caption_graph1<-
  'Las cifras representan el % de personas que pagaron coimas en planteles educativos en los últimos 12 meses, donde 0 significa que no hubo pago de sobornos y 1 que sí lo hubo. Las barras representan intervalos de confianza del 95% con errores ajustados por diseño muestral multietapa estratificado. 
  Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org.'

graph1 <- 
  ggplot(exc16_tab, aes(x = as.factor(year), y = exc16, fill = as.factor(colors))) + 
  geom_col(linewidth = 0.7,
           width = 0.5) + 
  scale_fill_manual(values = c('#2E5994', '#7bd9f2')) +
  geom_errorbar(aes(ymin = exc16 - 1.96*se,
                    ymax = exc16 + 1.96*se),
                width = 0.3)+
  geom_text(aes(label = scales::percent(exc16, accuracy = 0.1)),
            size = 4,
            vjust = -6.5) + 
  scale_y_continuous(limits = c(0, 0.3),
                     breaks = c(0, 0.1, 0.2, 0.3),
                     labels = c('0', '10', '20', '30')) +
  geom_vline(xintercept = 4.5, linetype = 'dotted', colour = '#FFAC8E') +
  annotate('label', x = 4.3, y = 0.245, label = '2011: se establece \n examen ENES', size = 3.5)+ 
  guides(fill = 'none') +
  labs(x = '',
       y = '',
       title = 'Pago de coimas en planteles educativos en Ecuador',
       subtitle = 'En los últimos 12 meses, ¿tuvo que pagar alguna coima/soborno en la escuela o colegio?',
       caption = str_wrap(caption_graph1, 175)) +
  theme_article_educacion +
  theme(plot.title = element_text(face = 'bold'),
        plot.caption = element_text(size = 8))

ggsave("../figures/grafico_pago_coimas.png",plot = graph1, 
       device = "png", 
       width = 10, 
       height = 6, 
       dpi = 1200)

# Tabulación con pesos de muestra ctol
ctol_tab <- svyby(formula = ~ctol, 
                   by = ~year, 
                   design = dm,
                   FUN = svymean,
                   na.rm = T,
                   keep.names = F)

# Graph de ctol
caption_graph2<-
  'Las cifras representan el % de personas que consideran que a veces el pago de sobornos es justificable, donde 0 significa que no justifica y 1 que sí lo hace. Las barras representan intervalos de confianza del 95% con errores ajustados por diseño muestral multietapa estratificado. 
  Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org.'

graph2 <- 
  ggplot(ctol_tab, aes(x = as.factor(year), y = ctol, fill = as.factor(year))) + 
  geom_col(fill = "#2E5994",
           linewidth = 0.7,
           width = 0.5) + 
  geom_errorbar(aes(ymin = ctol - 1.96*se,
                    ymax = ctol + 1.96*se),
                width = 0.3)+
  geom_text(aes(label = scales::percent(ctol, accuracy = 0.1)),
            size = 4,
            vjust = -4) +
  scale_y_continuous(limits = c(0, 0.3),
                     breaks = c(0, 0.1, 0.2, 0.3),
                     labels = c('0', '10', '20', '30')) +
  labs(x = '',
       y = '',
       title = 'Pagar coimas es justificable',
       subtitle = '¿Cree que como están las cosas a veces se justifica pagar una coima/soborno?',
       caption = str_wrap(caption_graph2, 175)) +
  guides(fill = F) +
  theme_article_educacion +
  theme(plot.title = element_text(face = 'bold'),
        plot.caption = element_text(size = 8)); graph2

ggsave("../figures/grafico_pago_coimas_justificable.png",plot = graph2, 
       device = "png", 
       width = 10, 
       height = 6, 
       dpi = 1200)


