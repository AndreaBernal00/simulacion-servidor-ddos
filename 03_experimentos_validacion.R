# ============================================================
# 03_experimentos_validacion.R
# Experimentos, validación y resultados finales
# Proyecto: Simulación del desempeño de un servidor bajo tráfico DDoS
# Responsable: Andrea Fernanda Bernal Forero
# ============================================================

library(data.table)
library(ggplot2)

set.seed(123)

# ============================================================
# SECCIÓN 1: CONFIGURACIÓN GENERAL
# ============================================================

carpeta_salida <- "resultados_entrega_final"
carpeta_graficas <- file.path(carpeta_salida, "graficas_finales")

if (!dir.exists(carpeta_salida)) {
  dir.create(carpeta_salida)
}

if (!dir.exists(carpeta_graficas)) {
  dir.create(carpeta_graficas)
}

cat("\n============================================================\n")
cat("INICIO: EXPERIMENTOS, VALIDACIÓN Y RESULTADOS FINALES\n")
cat("============================================================\n\n")


# ============================================================
# SECCIÓN 2: CARGA DE ARCHIVOS GENERADOS POR CRISTIAN Y JENIFER
# ============================================================

archivo_real <- "flujos_por_segundo_por_condicion.csv"
archivo_replicas <- file.path(carpeta_salida, "resultados_replicas.csv")
archivo_detalle <- file.path(carpeta_salida, "detalle_simulacion_replica1.csv")

if (!file.exists(archivo_real)) {
  stop("No se encontró el archivo flujos_por_segundo_por_condicion.csv")
}

if (!file.exists(archivo_replicas)) {
  stop("No se encontró resultados_replicas.csv en resultados_entrega_final")
}

if (!file.exists(archivo_detalle)) {
  stop("No se encontró detalle_simulacion_replica1.csv en resultados_entrega_final")
}

datos_reales <- fread(archivo_real)
resultados_replicas <- fread(archivo_replicas)
detalle_simulacion <- fread(archivo_detalle)

# ============================================================
# ESTANDARIZAR NOMBRES EN detalle_simulacion
# ============================================================

cat("\nColumnas disponibles en detalle_simulacion:\n")
print(names(detalle_simulacion))

# Posibles nombres alternativos para estado de tráfico
if ("estado" %in% names(detalle_simulacion) && !"estado_trafico" %in% names(detalle_simulacion)) {
  setnames(detalle_simulacion, "estado", "estado_trafico")
}

if ("estado_actual" %in% names(detalle_simulacion) && !"estado_trafico" %in% names(detalle_simulacion)) {
  setnames(detalle_simulacion, "estado_actual", "estado_trafico")
}

if ("label" %in% names(detalle_simulacion) && !"estado_trafico" %in% names(detalle_simulacion)) {
  setnames(detalle_simulacion, "label", "estado_trafico")
}

# Posibles nombres alternativos para llegadas
if ("Xs" %in% names(detalle_simulacion) && !"llegadas" %in% names(detalle_simulacion)) {
  setnames(detalle_simulacion, "Xs", "llegadas")
}

if ("flujos" %in% names(detalle_simulacion) && !"llegadas" %in% names(detalle_simulacion)) {
  setnames(detalle_simulacion, "flujos", "llegadas")
}

# Validar columnas necesarias del detalle
columnas_detalle_necesarias <- c(
  "escenario",
  "mu",
  "replica",
  "segundo",
  "estado_trafico",
  "llegadas",
  "procesados",
  "exceso",
  "congestion"
)

faltantes_detalle <- setdiff(columnas_detalle_necesarias, names(detalle_simulacion))

if (length(faltantes_detalle) > 0) {
  stop(
    paste(
      "Faltan estas columnas en detalle_simulacion:",
      paste(faltantes_detalle, collapse = ", ")
    )
  )
}

cat("\n✔ Columnas de detalle_simulacion estandarizadas correctamente\n")
print(names(detalle_simulacion))

cat("✔ Archivos cargados correctamente\n")
cat("Datos reales:", nrow(datos_reales), "filas\n")
cat("Resultados réplicas:", nrow(resultados_replicas), "filas\n")
cat("Detalle simulación:", nrow(detalle_simulacion), "filas\n\n")


# ============================================================
# SECCIÓN 3: REVISIÓN DE ESTRUCTURA
# ============================================================

cat("Columnas datos reales:\n")
print(names(datos_reales))

cat("\nColumnas resultados réplicas:\n")
print(names(resultados_replicas))

cat("\nColumnas detalle simulación:\n")
print(names(detalle_simulacion))


# ============================================================
# SECCIÓN 4: TABLA FINAL DE RESULTADOS POR ESCENARIO
# ============================================================

# Revisar nombres reales de columnas
cat("\nColumnas disponibles en resultados_replicas:\n")
print(names(resultados_replicas))

# Estandarizar nombres por si vienen diferentes
if ("prop_congestion" %in% names(resultados_replicas)) {
  setnames(resultados_replicas, "prop_congestion", "proporcion_congestion")
}

if ("total_exceso" %in% names(resultados_replicas)) {
  setnames(resultados_replicas, "total_exceso", "exceso_total")
}

# Validar columnas necesarias
columnas_necesarias <- c(
  "escenario",
  "mu",
  "total_procesados",
  "exceso_total",
  "segundos_congestionados",
  "proporcion_congestion"
)

faltantes <- setdiff(columnas_necesarias, names(resultados_replicas))

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan estas columnas en resultados_replicas:",
      paste(faltantes, collapse = ", ")
    )
  )
}

tabla_resultados <- resultados_replicas[, .(
  replicas = .N,
  procesados_promedio = mean(total_procesados, na.rm = TRUE),
  procesados_sd = sd(total_procesados, na.rm = TRUE),
  exceso_promedio = mean(exceso_total, na.rm = TRUE),
  exceso_sd = sd(exceso_total, na.rm = TRUE),
  segundos_congestion_promedio = mean(segundos_congestionados, na.rm = TRUE),
  segundos_congestion_sd = sd(segundos_congestionados, na.rm = TRUE),
  proporcion_congestion_promedio = mean(proporcion_congestion, na.rm = TRUE),
  proporcion_congestion_sd = sd(proporcion_congestion, na.rm = TRUE)
), by = .(escenario, mu)]

tabla_resultados <- tabla_resultados[order(mu)]

fwrite(tabla_resultados, file.path(carpeta_salida, "tabla_resultados_finales.csv"))

cat("\n✔ Tabla final de resultados exportada\n")
print(tabla_resultados)


# ============================================================
# SECCIÓN 5: TABLA RESUMEN POR ESTADO DE TRÁFICO EN LA RÉPLICA 1
# ============================================================

tabla_estado <- detalle_simulacion[, .(
  segundos = .N,
  llegadas_promedio = mean(llegadas, na.rm = TRUE),
  llegadas_max = max(llegadas, na.rm = TRUE),
  procesados_promedio = mean(procesados, na.rm = TRUE),
  exceso_promedio = mean(exceso, na.rm = TRUE),
  exceso_total = sum(exceso, na.rm = TRUE),
  porcentaje_congestion = mean(congestion, na.rm = TRUE) * 100
), by = .(escenario, mu, estado_trafico)]

tabla_estado <- tabla_estado[order(mu, estado_trafico)]

fwrite(tabla_estado, file.path(carpeta_salida, "tabla_resultados_por_estado.csv"))

cat("\n✔ Tabla por estado exportada\n")
print(tabla_estado)


# ============================================================
# SECCIÓN 6: GRÁFICA DE CONGESTIÓN PROMEDIO POR CAPACIDAD
# ============================================================

png(file.path(carpeta_graficas, "01_congestion_promedio_por_mu.png"),
    width = 900, height = 600)

ggplot(tabla_resultados, aes(x = factor(mu), y = proporcion_congestion_promedio * 100)) +
  geom_col(fill = "gray70", color = "black") +
  labs(
    title = "Porcentaje promedio de tiempo con congestión por capacidad del servidor",
    x = "Capacidad del servidor μ (flujos/s)",
    y = "Tiempo con congestión (%)"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 7: GRÁFICA DE EXCESO PROMEDIO POR CAPACIDAD
# ============================================================

png(file.path(carpeta_graficas, "02_exceso_promedio_por_mu.png"),
    width = 900, height = 600)

ggplot(tabla_resultados, aes(x = factor(mu), y = exceso_promedio)) +
  geom_col(fill = "gray70", color = "black") +
  labs(
    title = "Exceso promedio de flujos por capacidad del servidor",
    x = "Capacidad del servidor μ (flujos/s)",
    y = "Exceso promedio total"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 8: GRÁFICA DE PROCESADOS PROMEDIO POR CAPACIDAD
# ============================================================

png(file.path(carpeta_graficas, "03_procesados_promedio_por_mu.png"),
    width = 900, height = 600)

ggplot(tabla_resultados, aes(x = factor(mu), y = procesados_promedio)) +
  geom_col(fill = "gray70", color = "black") +
  labs(
    title = "Flujos procesados promedio por capacidad del servidor",
    x = "Capacidad del servidor μ (flujos/s)",
    y = "Flujos procesados promedio"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 9: SERIE TEMPORAL DE UNA RÉPLICA SIMULADA
# ============================================================

png(file.path(carpeta_graficas, "04_serie_temporal_simulada_replica1.png"),
    width = 1000, height = 600)

ggplot(detalle_simulacion, aes(x = segundo, y = llegadas)) +
  geom_line(linewidth = 0.3) +
  facet_wrap(~ mu, scales = "free_y") +
  labs(
    title = "Serie temporal simulada de llegadas por segundo - réplica 1",
    x = "Segundo simulado",
    y = "Flujos simulados por segundo"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 10: HISTOGRAMA DE LLEGADAS SIMULADAS POR CAPACIDAD
# ============================================================

png(file.path(carpeta_graficas, "05_histograma_llegadas_simuladas.png"),
    width = 1000, height = 600)

ggplot(detalle_simulacion, aes(x = llegadas)) +
  geom_histogram(bins = 40, fill = "gray75", color = "black") +
  facet_wrap(~ mu, scales = "free_y") +
  labs(
    title = "Histograma de llegadas simuladas por capacidad",
    x = "Flujos por segundo simulados",
    y = "Frecuencia"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 11: VALIDACIÓN REAL VS SIMULADO POR ESTADO
# ============================================================

estados <- c("Benign", "Suspicious", "Attack")

validacion_ks <- data.table()

for (estado_actual in estados) {
  
  reales_estado <- datos_reales[label == estado_actual, flujos]
  simulados_estado <- detalle_simulacion[estado_trafico == estado_actual, llegadas]
  
  reales_estado <- reales_estado[!is.na(reales_estado)]
  simulados_estado <- simulados_estado[!is.na(simulados_estado)]
  
  if (length(reales_estado) > 1 && length(simulados_estado) > 1) {
    
    ks <- tryCatch(
      suppressWarnings(ks.test(reales_estado, simulados_estado)),
      error = function(e) NULL
    )
    
    if (is.null(ks)) next
    
    fila <- data.table(
      estado = estado_actual,
      n_real = length(reales_estado),
      n_simulado = length(simulados_estado),
      media_real = mean(reales_estado),
      media_simulada = mean(simulados_estado),
      varianza_real = var(reales_estado),
      varianza_simulada = var(simulados_estado),
      D_ks = as.numeric(ks$statistic),
      p_value = ks$p.value
    )
    
    validacion_ks <- rbind(validacion_ks, fila)
  }
}

fwrite(validacion_ks, file.path(carpeta_salida, "validacion_ks_real_vs_simulado.csv"))

cat("\n✔ Validación K-S exportada\n")
print(validacion_ks)


# ============================================================
# SECCIÓN 12: ECDF REAL VS SIMULADO POR ESTADO
# ============================================================

datos_ecdf <- data.table()

for (estado_actual in estados) {
  
  reales_tmp <- data.table(
    estado = estado_actual,
    flujos = datos_reales[label == estado_actual, flujos],
    tipo = "Real"
  )
  
  sim_tmp <- data.table(
    estado = estado_actual,
    flujos = detalle_simulacion[estado_trafico == estado_actual, llegadas],
    tipo = "Simulado"
  )
  
  datos_ecdf <- rbind(datos_ecdf, reales_tmp, sim_tmp, fill = TRUE)
}

png(file.path(carpeta_graficas, "06_ecdf_real_vs_simulado_por_estado.png"),
    width = 1000, height = 700)

ggplot(datos_ecdf, aes(x = flujos, color = tipo)) +
  stat_ecdf(linewidth = 1) +
  facet_wrap(~ estado, scales = "free_x") +
  labs(
    title = "ECDF real vs simulada por condición de tráfico",
    x = "Flujos por segundo",
    y = "F(x)",
    color = "Datos"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 13: HISTOGRAMA REAL VS SIMULADO POR ESTADO
# ============================================================

png(file.path(carpeta_graficas, "07_histograma_real_vs_simulado_por_estado.png"),
    width = 1000, height = 700)

ggplot(datos_ecdf, aes(x = flujos, fill = tipo)) +
  geom_histogram(
    aes(y = after_stat(density)),
    position = "identity",
    alpha = 0.45,
    bins = 40,
    color = "black"
  ) +
  facet_wrap(~ estado, scales = "free") +
  labs(
    title = "Histograma real vs simulado por condición de tráfico",
    x = "Flujos por segundo",
    y = "Frecuencia",
    fill = "Datos"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 14: BOXPLOT DE EXCESO POR CAPACIDAD
# ============================================================

png(file.path(carpeta_graficas, "08_boxplot_exceso_por_mu.png"),
    width = 900, height = 600)

ggplot(detalle_simulacion, aes(x = factor(mu), y = exceso)) +
  geom_boxplot(fill = "gray75", color = "black", outlier.size = 0.7) +
  labs(
    title = "Distribución del exceso instantáneo por capacidad",
    x = "Capacidad del servidor μ (flujos/s)",
    y = "Exceso instantáneo"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 15: BOXPLOT DE LLEGADAS, PROCESADOS Y EXCESO
# ============================================================

detalle_largo <- melt(
  detalle_simulacion,
  id.vars = c("escenario", "mu", "replica", "segundo", "estado_trafico"),
  measure.vars = c("llegadas", "procesados", "exceso"),
  variable.name = "variable",
  value.name = "valor"
)

png(file.path(carpeta_graficas, "09_boxplot_llegadas_procesados_exceso.png"),
    width = 1000, height = 700)

ggplot(detalle_largo, aes(x = factor(mu), y = valor)) +
  geom_boxplot(fill = "gray75", color = "black", outlier.size = 0.6) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "Comparación de llegadas, procesados y exceso por capacidad",
    x = "Capacidad del servidor μ (flujos/s)",
    y = "Valor"
  ) +
  theme_minimal()

dev.off()


# ============================================================
# SECCIÓN 16: TABLA DE INTERPRETACIÓN AUTOMÁTICA
# ============================================================

interpretacion <- tabla_resultados[, .(
  mu = mu,
  congestion_pct = round(proporcion_congestion_promedio * 100, 2),
  exceso_promedio = round(exceso_promedio, 2),
  procesados_promedio = round(procesados_promedio, 2),
  lectura = fifelse(
    proporcion_congestion_promedio > 0.30,
    "Alta congestión: la capacidad es insuficiente para la carga simulada.",
    fifelse(
      proporcion_congestion_promedio > 0.05,
      "Congestión moderada: el servidor presenta saturación en algunos periodos.",
      "Baja congestión: la capacidad responde adecuadamente ante la mayoría de llegadas."
    )
  )
)]

fwrite(interpretacion, file.path(carpeta_salida, "interpretacion_resultados.csv"))

cat("\n✔ Interpretación automática exportada\n")
print(interpretacion)

# ============================================================
# SECCIÓN 16A: RESUMEN VALIDACIÓN REAL VS SIMULADO
# ============================================================

validacion_resumen <- validacion_ks[, .(
  estado,
  media_real = round(media_real, 2),
  media_simulada = round(media_simulada, 2),
  var_real = round(varianza_real, 2),
  var_simulada = round(varianza_simulada, 2),
  D_ks = round(D_ks, 4),
  p_value = signif(p_value, 4)
)]

fwrite(
  validacion_resumen,
  file.path(carpeta_salida, "tabla_validacion_resumen.csv")
)

cat("\n✔ Tabla resumen de validación exportada\n")
print(validacion_resumen)

# ============================================================
# SECCIÓN 17: CONCLUSIONES NUMÉRICAS PARA EL INFORME
# ============================================================

mejor_escenario <- tabla_resultados[which.min(proporcion_congestion_promedio)]
peor_escenario <- tabla_resultados[which.max(proporcion_congestion_promedio)]

conclusiones <- c(
  "CONCLUSIONES NUMÉRICAS DEL MODELO",
  "----------------------------------",
  paste0(
    "El escenario con menor proporción de congestión fue μ = ",
    mejor_escenario$mu,
    ", con una congestión promedio de ",
    round(mejor_escenario$proporcion_congestion_promedio * 100, 2),
    "%."
  ),
  paste0(
    "El escenario con mayor proporción de congestión fue μ = ",
    peor_escenario$mu,
    ", con una congestión promedio de ",
    round(peor_escenario$proporcion_congestion_promedio * 100, 2),
    "%."
  ),
  paste0(
    "El exceso promedio más alto se presentó en μ = ",
    peor_escenario$mu,
    ", con ",
    round(peor_escenario$exceso_promedio, 2),
    " flujos excedentes promedio."
  ),
  "Los resultados permiten comparar el desempeño del servidor bajo diferentes capacidades experimentales.",
  "El modelo no detecta ataques automáticamente; únicamente evalúa el impacto del tráfico sobre el desempeño del servidor.",
  "La congestión se interpreta como los flujos que superan la capacidad definida del servidor en cada segundo."
)

writeLines(conclusiones, file.path(carpeta_salida, "conclusiones_numericas.txt"))

cat("\n✔ Conclusiones numéricas exportadas\n")
cat(paste(conclusiones, collapse = "\n"))


# ============================================================
# SECCIÓN 18: FIN
# ============================================================

cat("\n\n============================================================\n")
cat("FIN: EXPERIMENTOS, VALIDACIÓN Y RESULTADOS FINALES\n")
cat("Archivos guardados en:", carpeta_salida, "\n")
cat("Gráficas guardadas en:", carpeta_graficas, "\n")
cat("============================================================\n")
