
# MÓDULO 8 - RETO 2
# Preparación y depuración de datos
# Análisis de países de Latinoamérica

# 1. Cargar paquetes
library(readxl)
library(dplyr)
library(writexl)

# 2. CARGAR BASES DE DATOS ORIGINALES

archivo_mortalidad <- "../Base_Datos_Original/child_mortality_0_5_year_olds_dying_per_1000_born.xlsx"

archivo_fertilidad <- "../Base_Datos_Original/children_per_woman_total_fertility.xlsx"

archivo_poblacion <- "../Base_Datos_Original/Population.xlsx"

mortalidad <- read_excel(archivo_mortalidad)

fertilidad <- read_excel(archivo_fertilidad)

poblacion <- read_excel(archivo_poblacion)

# 3. COMPROBAR DIMENSIONES

dim(mortalidad)
dim(fertilidad)
dim(poblacion)

# 4. CONVERTIR LOS DATOS DE LOS AÑOS A NUMÉRICO


mortalidad[, 3:ncol(mortalidad)] <- lapply(
  mortalidad[, 3:ncol(mortalidad)],
  as.numeric
)

fertilidad[, 3:ncol(fertilidad)] <- lapply(
  fertilidad[, 3:ncol(fertilidad)],
  as.numeric
)

poblacion[, 3:ncol(poblacion)] <- lapply(
  poblacion[, 3:ncol(poblacion)],
  as.numeric
)

# 5. COMPROBAR TIPOS DE DATOS


str(mortalidad[, 1:5])
str(fertilidad[, 1:5])
str(poblacion[, 1:5])

# 6. COMPROBAR VALORES FALTANTES

sum(is.na(mortalidad))
sum(is.na(fertilidad))
sum(is.na(poblacion))

# 7. COMPROBAR PAÍSES DUPLICADOS

sum(duplicated(mortalidad$name))
sum(duplicated(fertilidad$name))
sum(duplicated(poblacion$name))

# 8. DEFINIR LOS 19 PAÍSES DE LATINOAMÉRICA

paises_latam <- c(
  "Argentina",
  "Bolivia",
  "Brazil",
  "Chile",
  "Colombia",
  "Costa Rica",
  "Cuba",
  "Dominican Republic",
  "Ecuador",
  "El Salvador",
  "Guatemala",
  "Haiti",
  "Honduras",
  "Mexico",
  "Nicaragua",
  "Panama",
  "Paraguay",
  "Peru",
  "Uruguay"
)


# 9. FILTRAR LAS BASES A LOS 19 PAÍSES

mortalidad <- mortalidad[mortalidad$name %in% paises_latam, ]

fertilidad <- fertilidad[fertilidad$name %in% paises_latam, ]

poblacion <- poblacion[poblacion$name %in% paises_latam, ]


# 10. COMPROBAR DIMENSIONES DESPUÉS DEL FILTRADO

dim(mortalidad)
dim(fertilidad)
dim(poblacion)

# 11. COMPROBAR VALORES FALTANTES DESPUÉS
#     DEL FILTRADO


sum(is.na(mortalidad))
sum(is.na(fertilidad))
sum(is.na(poblacion))

# 12. GUARDAR LAS BASES DEPURADAS

write_xlsx(
  mortalidad,
  "../Base_Datos_Depurada/mortalidad_LATAM.xlsx"
)

write_xlsx(
  fertilidad,
  "../Base_Datos_Depurada/fertilidad_LATAM.xlsx"
)

write_xlsx(
  poblacion,
  "../Base_Datos_Depurada/poblacion_LATAM.xlsx"
)