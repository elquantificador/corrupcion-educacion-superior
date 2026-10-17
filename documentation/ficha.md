# Ficha del artículo

Completa esta ficha al crear el repositorio y mantenla al día. Es el único
lugar con datos propios del artículo; el README es igual en todos los
repositorios.

## Identificación

- **Título:** Corrupción y acceso a la educación superior pública: ¿centralizar el ingreso a universidades?
- **Descripción del repositorio:** Artículo del Quantificador sobre corrupción en ámbitos educativos.
- **Autoría:** [COMPLETAR: nombres, igual que en el sitio]
- **Fecha de publicación:** [COMPLETAR: AAAA-MM-DD]
- **Artículo publicado:** [COMPLETAR: enlace a elquantificador.org]
- **Categoría:** [COMPLETAR: Economía, Política, Salud, Educación o Valores y
  comportamiento]
- **Edición:** [COMPLETAR: por ejemplo, Julio-Diciembre 2026]

## Estado

[COMPLETAR: en preparación, en revisión editorial o publicado. Anota qué falta
para cerrar el artículo.]

## Pregunta y alcance

[COMPLETAR: dos o tres oraciones con la pregunta, el periodo, la cobertura
geográfica y la unidad de análisis.]

## Requisitos

- R [COMPLETAR: versión, por ejemplo 4.5.x].
- Paquetes de R: [COMPLETAR: lista en orden alfabético].

## Fuentes de datos

| Fuente | Institución | Periodo | Acceso | Script |
|---|---|---|---|---|
| [COMPLETAR] | [COMPLETAR] | [COMPLETAR] | [COMPLETAR: descarga automática o manual] | `code/cleaning/00_download_fuente.R` |

Si alguna fuente no permite descarga automática, explica aquí cómo obtenerla y
dónde colocarla en `data/raw/`.

## Salidas

- `outputs/tables/[COMPLETAR].csv`: [COMPLETAR: qué contiene].
- `outputs/graphs/[COMPLETAR].png`: [COMPLETAR: qué muestra].
- `article/images/graficoN.png`: copias numeradas según el orden del texto.

## Cómo replicar en este repositorio

Este repositorio conserva sus instrucciones originales en la sección
"README anterior". Si todavía no tiene `MASTER.R`, sigue esas instrucciones.
Cuando se migre a la estructura estándar, reemplaza esta sección por los pasos
de `MASTER.R`.

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
