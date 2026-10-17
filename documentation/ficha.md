# Ficha del artículo

Completa esta ficha al crear el repositorio y mantenla al día. Es el único
lugar con datos propios del artículo; el README es igual en todos los
repositorios.

## Identificación

- **Título:** Corrupción y acceso a la educación superior pública: ¿centralizar el ingreso a universidades?
- **Descripción del repositorio:** Artículo del Quantificador sobre corrupción en ámbitos educativos.
- **Autoría:** Romina Estévez
- **Fecha de publicación:** 2023-07-30
- **Artículo publicado:** https://elquantificador.org/post/corrupcion/2023-07-30-corrupcion-y-acceso-a-la-educacion-superior-publica-en-ecuador/
- **Categoría:** Política (el sitio también lo clasifica en Educación y en Valores y comportamiento)
- **Edición:** Febrero - Agosto 2023

## Estado

Publicado el 2023-07-30.

## Pregunta y alcance

El artículo pregunta cómo ha influido el sistema de ingreso a la universidad
pública (examen centralizado o ingreso descentralizado) en la corrupción en
ámbitos educativos en Ecuador. Usa las rondas 2004 a 2019 del Barómetro de las
Américas de LAPOP para estimar, con diseño muestral, el porcentaje de personas
que pagaron un soborno en escuelas o colegios en los últimos 12 meses y el
porcentaje que considera justificable pagar un soborno. La unidad de análisis
son las personas encuestadas, con cobertura nacional.

## Requisitos

- R: no documentada en el repositorio.
- Paquetes de R: dplyr, ggplot2, here, knitr (para renderizar el `.Rmd`),
  scales, survey, tidyverse.

## Fuentes de datos

| Fuente | Institución | Periodo | Acceso | Script |
|---|---|---|---|---|
| Barómetro de las Américas (AmericasBarometer), Ecuador | LAPOP (Latin American Public Opinion Project, Vanderbilt University), www.LapopSurveys.org | Rondas 2004, 2006, 2008, 2010, 2012, 2014, 2016 y 2019 | Copia local en `data/ab_04_19.csv`; si falta, el script la descarga desde el repositorio `laboratoriolide/americas-barometer` en GitHub | `code/LAPOP.R` |

El archivo remoto se llama `ab_04_09.csv`, pero contiene las rondas 2004 a 2019
(verificado). El CSV es una compilación de LIDE de las bases públicas de LAPOP. Los
términos de uso de los datos originales los fija LAPOP (ver `DATA-NOTICE.md`).

## Salidas

- `figures/grafico_pago_coimas.png`: porcentaje de personas que pagaron coimas
  en planteles educativos en los últimos 12 meses, por ronda, con intervalos de
  confianza del 95 %.
- `figures/grafico_pago_coimas_justificable.png`: porcentaje de personas que
  consideran justificable pagar un soborno, por ronda.
- `figures/grafico_postulantes.png`: figura presente en el repositorio; ningún
  script ni el `.Rmd` la genera o la cita.
- `report/report.docx` y `report/report.html`: versiones renderizadas de
  `report/report.Rmd`.

## Cómo replicar en este repositorio

Este repositorio no tiene `MASTER.R`. El README anterior solo indica clonar o
descargar el repositorio y ejecutar los scripts; el orden siguiente se deduce de
los archivos:

1. Clona o descarga el repositorio y ábrelo con `corrupcion-educacion-superior.Rproj`.
2. Ejecuta `code/LAPOP.R` con la raíz del repositorio como directorio de
   trabajo. Lee `data/ab_04_19.csv` (si falta, lo descarga), declara el diseño muestral (UPM
   `upm`, estrato `estratopri`, ponderador `weight1500`), estima por año las
   variables `exc16` y `ctol` y guarda dos figuras en `figures/` (crea la
   carpeta si no existe). Todas las rutas son relativas a la raíz.
3. Renderiza `report/report.Rmd` (Word o HTML) para regenerar el texto. Inserta
   las figuras de `figures/` y toma las referencias de `report/refs.bib` y
   `report/style.csl`.

## README anterior

Texto del README de este repositorio antes de adoptar el README estándar,
conservado sin cambios de contenido para no perder información.

### Corrupción y acceso a la educación superior pública: ¿centralizar el ingreso a universidades?

Este repositorio almacena todos los archivos necesarios para el análisis de la corrupción en el acceso a la educación superior en Ecuador utilizando la encuesta de LAPOP.

### Requisitos

- R (se recomienda usar RStudio).
- Paquetes de R que se instalarán automáticamente al ejecutar los scripts.
- Opcionalmente LaTeX para compilar el artículo en PDF.

### Como ejecutar el análisis

Se tienen dos opciones para replicar el análisis:

1. Clonar el repositorio utilizando Git y GitHub y ejecutar los scripts.

2. Descargar un archivo comprimido desde la página web del repositorio y ejecutar los scripts.

Para más información, consulta el [perfil de GitHub de la autora](https://github.com/RomiEstevez).
