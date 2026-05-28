# ============================================================
# PROYECTO SIMULACIÓN DIGITAL — AVANCE 2
# Simulación del desempeño de un servidor de red bajo tráfico
# benigno, sospechoso y ataques DDoS
#
# PERSONA 1 — Recolección de datos + Análisis Exploratorio (EDA)
#
# Universidad Industrial de Santander
# Escuela de Ingeniería de Sistemas e Informática
# Dataset: BCCC-cPacket-Cloud-DDoS-2024 | 18-dic-2023
# ============================================================


# ============================================================
# LIBRERÍAS
# ============================================================

library(data.table)
library(ggplot2)

# ============================================================
# SECCIÓN 1: CARGA DEL DATASET
# ============================================================
# Ajustar la ruta al archivo original si es necesario.
# Si ya se dispone del CSV limpio exportado anteriormente,
# se puede cargar directamente con fread("dataset_limpio_simulacion.csv")

data_raw <- fread("Monday_18_Dec_2023.csv")

cat("Dataset original cargado:", nrow(data_raw), "registros,",
    ncol(data_raw), "variables\n")


# ============================================================
# SECCIÓN 2: SELECCIÓN DE VARIABLES RELEVANTES
# ============================================================
# Se conservan únicamente las variables necesarias para el modelo SSED.
# Las variables de flags TCP y handshake_state se usan en el EDA
# para caracterizar el tipo de tráfico, no como entrada al simulador.

data <- data_raw[, .(
  flow_id,
  timestamp,
  label,
  activity,
  protocol,
  duration,
  packets_count,
  packets_rate,
  bytes_rate,
  total_payload_bytes,
  fwd_total_payload_bytes,
  bwd_total_payload_bytes,
  syn_flag_counts,
  ack_flag_counts,
  rst_flag_counts,
  fin_flag_counts,
  handshake_state
)]

cat("Variables seleccionadas:", ncol(data), "\n")


# ============================================================
# SECCIÓN 3: CONVERSIÓN DE TIMESTAMPS
# ============================================================
# La conversión a POSIXct con precisión de microsegundos es
# fundamental para construir las ventanas discretas de 1 segundo.

data[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")]

cat("Rango temporal del dataset:\n")
cat("  Inicio:", format(min(data$timestamp, na.rm = TRUE)), "\n")
cat("  Fin:   ", format(max(data$timestamp, na.rm = TRUE)), "\n")


# ============================================================
# SECCIÓN 4: LIMPIEZA Y VERIFICACIÓN DE CALIDAD
# ============================================================

cat("\n=== CALIDAD DE DATOS ===\n")

# 4.1 Dimensiones
cat("\nDimensiones del dataset:\n")
print(dim(data))

# 4.2 Distribución de etiquetas
cat("\nDistribución de etiquetas (conteo y %):\n")
print(table(data$label))
print(round(prop.table(table(data$label)) * 100, 2))

# 4.3 Valores NA
cat("\nValores NA por columna:\n")
print(colSums(is.na(data)))

# 4.4 Valores infinitos en variables numéricas
num_cols <- names(data)[sapply(data, is.numeric)]

cat("\nValores infinitos por columna numérica:\n")
inf_counts <- sapply(data[, ..num_cols], function(x) sum(is.infinite(x), na.rm = TRUE))
print(inf_counts)

# Reemplazar infinitos por NA (el flujo se conserva)
for (col in names(inf_counts)[inf_counts > 0]) {
  data[is.infinite(get(col)), (col) := NA]
}

# 4.5 Valores negativos
cat("\nValores negativos en variables donde no deberían existir:\n")
print(data[, .(
  duration_neg         = sum(duration < 0, na.rm = TRUE),
  packets_count_neg    = sum(packets_count < 0, na.rm = TRUE),
  packets_rate_neg     = sum(packets_rate < 0, na.rm = TRUE),
  bytes_rate_neg       = sum(bytes_rate < 0, na.rm = TRUE),
  payload_neg          = sum(total_payload_bytes < 0, na.rm = TRUE)
)])

# 4.6 Valores cero relevantes
cat("\nValores cero en variables de interés:\n")
print(data[, .(
  duration_cero      = sum(duration == 0, na.rm = TRUE),
  bytes_rate_cero    = sum(bytes_rate == 0, na.rm = TRUE),
  payload_cero       = sum(total_payload_bytes == 0, na.rm = TRUE),
  packets_count_cero = sum(packets_count == 0, na.rm = TRUE)
)])

# 4.7 Eliminación: solo se excluyen registros sin timestamp válido
data_clean <- data[!is.na(timestamp)]

cat("\nRegistros antes de limpiar:", nrow(data), "\n")
cat("Registros después de limpiar:", nrow(data_clean),
    "(excluidos:", nrow(data) - nrow(data_clean), ")\n")


# ============================================================
# SECCIÓN 5: ANÁLISIS DE FLUJOS CON Y SIN PAYLOAD
# ============================================================
# Los flujos con payload = 0 son paquetes de control TCP.
# Se conservan porque representan llegadas reales al servidor.

data_clean[, tipo_payload := fifelse(total_payload_bytes > 0,
                                     "Con payload",
                                     "Sin payload")]

cat("\n=== ANÁLISIS DE PAYLOAD ===\n")

cat("\nFlujos con/sin payload por etiqueta:\n")
print(data_clean[, .N, by = .(label, tipo_payload)][order(label, tipo_payload)])

cat("\nResumen de tasas por tipo de payload:\n")
print(data_clean[, .(
  n                = .N,
  media_pkt_count  = mean(packets_count,  na.rm = TRUE),
  media_pkt_rate   = mean(packets_rate,   na.rm = TRUE),
  media_bytes_rate = mean(bytes_rate,     na.rm = TRUE),
  media_duration   = mean(duration,       na.rm = TRUE),
  max_pkt_rate     = max(packets_rate,    na.rm = TRUE),
  max_bytes_rate   = max(bytes_rate,      na.rm = TRUE)
), by = tipo_payload])


# ============================================================
# SECCIÓN 6: CONSTRUCCIÓN DE VENTANAS DE 1 SEGUNDO — Xs(t)
# ============================================================
# Se crea la unidad de tiempo del modelo SSED:
# Xs(t) = número de flujos que llegan al servidor en el segundo t

data_clean[, time_sec := as.POSIXct(cut(timestamp, breaks = "1 sec"))]

# Serie global de flujos por segundo
flujos_sec <- data_clean[, .(flujos = .N), by = time_sec]

# Completar segundos sin flujos con cero
rango_tiempo <- data.table(
  time_sec = seq(
    from = min(data_clean$time_sec, na.rm = TRUE),
    to   = max(data_clean$time_sec, na.rm = TRUE),
    by   = "1 sec"
  )
)

flujos_sec <- merge(rango_tiempo, flujos_sec, by = "time_sec", all.x = TRUE)
flujos_sec[is.na(flujos), flujos := 0]

# Serie por condición
labels_modelo <- c("Benign", "Suspicious", "Attack")

plantilla_estado <- CJ(
  time_sec = rango_tiempo$time_sec,
  label = labels_modelo
)

flujos_sec_label <- data_clean[, .(flujos = .N), by = .(time_sec, label)]

flujos_sec_label <- merge(
  plantilla_estado,
  flujos_sec_label,
  by = c("time_sec", "label"),
  all.x = TRUE
)

flujos_sec_label[is.na(flujos), flujos := 0]

cat("\n=== SERIE TEMPORAL Xs(t) ===\n")
cat("Ventanas de 1 segundo generadas (global):", nrow(flujos_sec), "\n")
cat("Horizonte T ≈", round(nrow(flujos_sec) / 3600, 1), "horas\n")


# ============================================================
# SECCIÓN 7: ESTADÍSTICOS DESCRIPTIVOS DE Xs(t) POR CONDICIÓN
# ============================================================

cat("\n=== ESTADÍSTICOS DESCRIPTIVOS DE FLUJOS POR SEGUNDO ===\n")

# 7.1 Resumen global
resumen_flujos_total <- flujos_sec[, .(
  media           = mean(flujos),
  varianza        = var(flujos),
  VMR             = var(flujos) / mean(flujos),
  p50             = quantile(flujos, 0.50),
  p90             = quantile(flujos, 0.90),
  p95             = quantile(flujos, 0.95),
  p99             = quantile(flujos, 0.99),
  maximo          = max(flujos)
)]

cat("\nResumen global de flujos por segundo:\n")
print(resumen_flujos_total)

# 7.2 Resumen por condición — parámetros λs del modelo
resumen_flujos_estado <- flujos_sec_label[, .(
  lambda          = mean(flujos),
  varianza        = var(flujos),
  VMR             = var(flujos) / mean(flujos),
  p50             = quantile(flujos, 0.50),
  p90             = quantile(flujos, 0.90),
  p95             = quantile(flujos, 0.95),
  p99             = quantile(flujos, 0.99),
  maximo          = max(flujos)
), by = label]

cat("\nEstadísticos de flujos por segundo por condición:\n")
print(resumen_flujos_estado[order(label)])

cat("\nInterpretación del VMR:\n")
cat("  VMR ≈ 1 → Poisson adecuado\n")
cat("  VMR >> 1 → sobredispersión; usar distribución empírica Fs\n")


# ============================================================
# SECCIÓN 8: ANÁLISIS DE CARGA POR SEGUNDO
# ============================================================

carga_sec <- data_clean[, .(
  flujos           = .N,
  paquetes_total   = sum(packets_count,             na.rm = TRUE),
  bytes_total      = sum(as.numeric(total_payload_bytes), na.rm = TRUE),
  bytes_rate_total = sum(bytes_rate,                na.rm = TRUE)
), by = time_sec]

carga_sec_label <- data_clean[, .(
  flujos           = .N,
  paquetes_total   = sum(packets_count,             na.rm = TRUE),
  bytes_total      = sum(as.numeric(total_payload_bytes), na.rm = TRUE),
  bytes_rate_total = sum(bytes_rate,                na.rm = TRUE)
), by = .(time_sec, label)]

cat("\n=== CARGA POR SEGUNDO ===\n")

resumen_carga_estado <- carga_sec_label[, .(
  media_bytes = mean(bytes_total),
  var_bytes   = var(bytes_total),
  p50_bytes   = quantile(bytes_total, 0.50),
  p90_bytes   = quantile(bytes_total, 0.90),
  p95_bytes   = quantile(bytes_total, 0.95),
  p99_bytes   = quantile(bytes_total, 0.99),
  max_bytes   = max(bytes_total)
), by = label]

cat("\nResumen de carga total de bytes por segundo por condición:\n")
print(resumen_carga_estado[order(label)])

cat("\nNota: aunque Attack genera más flujos, Benign consume más bytes/s.\n")
cat("La congestión está determinada por el número de conexiones, no el ancho de banda.\n")


# ============================================================
# SECCIÓN 9: PARÁMETROS DEL MODELO — CAPACIDAD μ DEL SERVIDOR
# ============================================================
# El dataset NO incluye la capacidad real del servidor.
# μ se define experimentalmente con tres escenarios.

cat("\n=== PARÁMETROS DEL MODELO — CAPACIDAD μ ===\n")

flujos_benign_sec <- flujos_sec_label[label == "Benign"]

mu_baja  <- as.numeric(quantile(flujos_benign_sec$flujos, 0.95, na.rm = TRUE))
mu_media <- 20
mu_alta  <- 100

cat("mu_baja  = P95 flujos benignos/s =", mu_baja,
    " (servidor dimensionado para tráfico normal)\n")
cat("mu_media =", mu_media,
    " (permite absorber tráfico sospechoso moderado)\n")
cat("mu_alta  =", mu_alta,
    " (sin congestión normal; sobrepasado en ráfagas de ataque)\n")

# Referencia adicional en bytes (análisis complementario)
carga_benign_sec <- carga_sec_label[label == "Benign"]
cap_bytes_p95 <- as.numeric(quantile(carga_benign_sec$bytes_total, 0.95, na.rm = TRUE))
cap_bytes_p99 <- as.numeric(quantile(carga_benign_sec$bytes_total, 0.99, na.rm = TRUE))
cat("\nReferencia en bytes (análisis complementario):\n")
cat("  P95 bytes_total benigno/s =", round(cap_bytes_p95, 0), "\n")
cat("  P99 bytes_total benigno/s =", round(cap_bytes_p99, 0), "\n")


# ============================================================
# SECCIÓN 10: ANÁLISIS DE CONGESTIÓN
# ============================================================

carga_sec[, congestion_mu_baja  := flujos > mu_baja]
carga_sec[, congestion_mu_media := flujos > mu_media]
carga_sec[, congestion_mu_alta  := flujos > mu_alta]

carga_sec[, exceso_mu_baja  := pmax(0, flujos - mu_baja)]
carga_sec[, exceso_mu_media := pmax(0, flujos - mu_media)]
carga_sec[, exceso_mu_alta  := pmax(0, flujos - mu_alta)]

cat("\n=== ANÁLISIS DE CONGESTIÓN ===\n")

resumen_congestion <- data.table(
  escenario             = c("Capacidad baja", "Capacidad media", "Capacidad alta"),
  mu_flujos_seg         = c(mu_baja, mu_media, mu_alta),
  pct_tiempo_congestion = round(c(
    mean(carga_sec$congestion_mu_baja),
    mean(carga_sec$congestion_mu_media),
    mean(carga_sec$congestion_mu_alta)
  ) * 100, 2),
  segundos_congestion   = c(
    sum(carga_sec$congestion_mu_baja),
    sum(carga_sec$congestion_mu_media),
    sum(carga_sec$congestion_mu_alta)
  )
)

print(resumen_congestion)

resumen_exceso <- data.table(
  escenario             = c("Capacidad baja", "Capacidad media", "Capacidad alta"),
  total_exceso_flujos   = c(
    sum(carga_sec$exceso_mu_baja),
    sum(carga_sec$exceso_mu_media),
    sum(carga_sec$exceso_mu_alta)
  ),
  max_exceso_segundo    = c(
    max(carga_sec$exceso_mu_baja),
    max(carga_sec$exceso_mu_media),
    max(carga_sec$exceso_mu_alta)
  ),
  media_exceso_por_s    = round(c(
    mean(carga_sec$exceso_mu_baja),
    mean(carga_sec$exceso_mu_media),
    mean(carga_sec$exceso_mu_alta)
  ), 2)
)

cat("\nResumen de exceso de flujos por escenario:\n")
print(resumen_exceso)


# ============================================================
# SECCIÓN 11: DURACIÓN DE EPISODIOS POR CONDICIÓN
# ============================================================
# Determina t_cambio del pseudocódigo SSED: cuántos segundos
# permanece el sistema en cada estado antes de transicionar.

estado_sec <- flujos_sec_label[flujos > 0, .(N = flujos), by = .(time_sec, label)]

# Etiqueta dominante por segundo
estado_dom <- estado_sec[order(time_sec, -N), .SD[1], by = time_sec]
setorder(estado_dom, time_sec)

r <- rle(estado_dom$label)
duraciones_estado <- data.table(
  estado            = r$values,
  duracion_segundos = as.numeric(r$lengths)
)

resumen_duracion_estado <- duraciones_estado[, .(
  n_episodios      = .N,
  media_duracion   = as.numeric(mean(duracion_segundos)),
  mediana_duracion = as.numeric(median(duracion_segundos)),
  p90_duracion     = as.numeric(quantile(duracion_segundos, 0.90, na.rm = TRUE)),
  max_duracion     = as.numeric(max(duracion_segundos))
), by = estado]

cat("\n=== DURACIÓN DE EPISODIOS POR CONDICIÓN ===\n")
print(resumen_duracion_estado[order(estado)])

cat("\nNota: Los episodios de Attack tienen duración ~1,200 s (20 min),\n")
cat("coherente con el cronograma del paper base (Shafi et al., 2024).\n")


# ============================================================
# SECCIÓN 12: GRÁFICAS PARA EL INFORME
# ============================================================

if (!dir.exists("graficas_avance2")) dir.create("graficas_avance2")

# -- Figura 1: Serie temporal global --
png("graficas_avance2/01_serie_flujos_por_segundo.png",
    width = 1200, height = 600)
plot(
  carga_sec$time_sec, carga_sec$flujos,
  type = "l",
  main = "Flujos por segundo — 18 de diciembre de 2023",
  xlab = "Tiempo", ylab = "Número de flujos por segundo",
  col  = "black", lwd = 1
)
abline(h = mu_baja,  col = "red",    lty = 2)
abline(h = mu_media, col = "orange", lty = 2)
abline(h = mu_alta,  col = "blue",   lty = 2)
legend("topright",
       legend = c("Flujos por segundo",
                  paste("mu baja =",  mu_baja),
                  paste("mu media =", mu_media),
                  paste("mu alta =",  mu_alta)),
       lty = c(1, 2, 2, 2),
       col = c("black", "red", "orange", "blue"))
dev.off()

# -- Figura 2: Series temporales por condición --
png("graficas_avance2/02_serie_flujos_por_condicion.png",
    width = 1200, height = 800)
par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))
colores <- c("Benign" = "steelblue", "Suspicious" = "darkorange", "Attack" = "firebrick")
for (et in c("Benign", "Suspicious", "Attack")) {
  sub <- flujos_sec_label[label == et]
  lam <- resumen_flujos_estado[label == et, lambda]
  plot(sub$time_sec, sub$flujos, type = "l",
       main = paste("Flujos/s —", et),
       xlab = "", ylab = "flujos/s",
       col = colores[et], lwd = 1)
  abline(h = lam, col = "black", lty = 2)
  legend("topright",
         legend = paste("lambda =", round(lam, 2)),
         lty = 2, col = "black", bty = "n")
}
par(mfrow = c(1, 1))
dev.off()

# -- Figura 3: Histogramas por condición con VMR --
png("graficas_avance2/03_hist_flujos_por_condicion.png",
    width = 1200, height = 500)
par(mfrow = c(1, 3))
for (et in c("Benign", "Suspicious", "Attack")) {
  vals <- flujos_sec_label[label == et]$flujos
  vmr  <- round(var(vals) / mean(vals), 1)
  lam  <- round(mean(vals), 2)
  hist(vals, breaks = 40, col = "lightgrey",
       main = paste0("Flujos/s — ", et, "\nλ = ", lam, "  VMR = ", vmr),
       xlab = "Flujos por segundo", ylab = "Frecuencia")
  abline(v = mean(vals),   col = "black", lty = 1, lwd = 2)
  abline(v = median(vals), col = "black", lty = 2, lwd = 1)
  legend("topright", legend = c("Media", "Mediana"),
         lty = c(1, 2), col = "black", bty = "n")
}
par(mfrow = c(1, 1))
dev.off()

# -- Figura 4: ECDF por condición --
png("graficas_avance2/04_ecdf_flujos_por_condicion.png",
    width = 900, height = 600)
plot(ecdf(flujos_sec_label[label == "Benign"]$flujos),
     main = "ECDF de flujos por segundo por condición",
     xlab = "Flujos por segundo", ylab = "F(x)",
     col = "steelblue", lwd = 2, xlim = c(0, 100))
lines(ecdf(flujos_sec_label[label == "Suspicious"]$flujos),
      col = "darkorange", lwd = 2)
lines(ecdf(flujos_sec_label[label == "Attack"]$flujos),
      col = "firebrick", lwd = 2)
abline(h = c(0.50, 0.90, 0.95, 0.99),
       lty = 3, col = "grey60")
legend("bottomright",
       legend = c("Benign", "Suspicious", "Attack"),
       col = c("steelblue", "darkorange", "firebrick"),
       lwd = 2)
dev.off()

# -- Figura 5A: Violinplot de flujos por segundo por condición --
png("graficas_avance2/05A_violinplot_flujos_por_condicion.png",
    width = 900, height = 600)

ggplot(flujos_sec_label, aes(x = label, y = flujos)) +
  geom_violin(fill = "lightgrey", color = "black") +
  geom_boxplot(width = 0.12, outlier.size = 0.8) +
  labs(
    title = "Distribución de flujos por segundo por condición",
    x = "Condición del tráfico",
    y = "Flujos por segundo"
  ) +
  theme_minimal()

dev.off()

# -- Figura 5: Boxplot tasas de paquetes y bytes --
png("graficas_avance2/05_boxplot_tasas_paquetes_bytes.png",
    width = 1200, height = 600)
par(mfrow = c(1, 2))
boxplot(log1p(packets_rate) ~ label, data = data_clean,
        main = "Tasa de paquetes por etiqueta",
        xlab = "Etiqueta", ylab = "log(1 + packets_rate)",
        col  = c("firebrick", "steelblue", "darkorange"))
boxplot(log1p(bytes_rate) ~ label, data = data_clean,
        main = "Tasa de bytes por etiqueta",
        xlab = "Etiqueta", ylab = "log(1 + bytes_rate)",
        col  = c("firebrick", "steelblue", "darkorange"))
par(mfrow = c(1, 1))
dev.off()

# -- Figura 6: Carga total de bytes por segundo --
png("graficas_avance2/06_boxplot_carga_bytes_segundo.png",
    width = 900, height = 600)
boxplot(log1p(bytes_total) ~ label, data = carga_sec_label,
        main = "Carga total por segundo por etiqueta",
        xlab = "Etiqueta", ylab = "log(1 + bytes_total)",
        col  = c("firebrick", "steelblue", "darkorange"))
dev.off()

# -- Figura 7: Duración de flujos --
png("graficas_avance2/07_boxplot_duracion_flujos.png",
    width = 900, height = 600)
boxplot(log1p(duration) ~ label, data = data_clean,
        main = "Duración de flujos de red por condición",
        xlab = "Condición", ylab = "log(1 + duration) [ms]",
        col  = c("firebrick", "steelblue", "darkorange"))
dev.off()

# -- Figura 8: Test de sobredispersión (histograma vs Poisson teórico) --
png("graficas_avance2/08_test_sobredispersion_poisson.png",
    width = 1200, height = 500)
par(mfrow = c(1, 3))
for (et in c("Benign", "Suspicious", "Attack")) {
  vals <- flujos_sec_label[label == et]$flujos
  lam  <- mean(vals)
  vmr  <- round(var(vals) / mean(vals), 1)
  hist(vals, breaks = 40, col = "lightblue",
       freq = FALSE, xlim = c(0, quantile(vals, 0.99)),
       main = paste0(et, "\nVMR = ", vmr),
       xlab = "Flujos por segundo", ylab = "Densidad")
  x_seq <- 0:ceiling(quantile(vals, 0.99))
  lines(x_seq, dpois(x_seq, lambda = lam),
        col = "black", lty = 2, lwd = 2)
  legend("topright", legend = "Poisson teórico",
         lty = 2, col = "black", bty = "n")
}
par(mfrow = c(1, 1))
dev.off()

# -- Figura 9: Exceso de flujos por escenario a lo largo del tiempo --
png("graficas_avance2/09_exceso_flujos_por_escenario.png",
    width = 1200, height = 700)
par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))
for (escen in c("exceso_mu_baja", "exceso_mu_media", "exceso_mu_alta")) {
  mu_val <- switch(escen,
                   exceso_mu_baja  = mu_baja,
                   exceso_mu_media = mu_media,
                   exceso_mu_alta  = mu_alta)
  plot(carga_sec$time_sec, carga_sec[[escen]],
       type = "l", col = "firebrick", lwd = 1,
       main = paste("Exceso de flujos — mu =", mu_val, "f/s"),
       xlab = "", ylab = "Flujos en exceso")
}
par(mfrow = c(1, 1))
dev.off()

# -- Figura 10: Resumen visual de parámetros clave --
png("graficas_avance2/10_resumen_parametros_clave.png",
    width = 1200, height = 500)
par(mfrow = c(1, 3))

# (A) Lambda por condición
lambdas <- resumen_flujos_estado[order(label), lambda]
labels  <- resumen_flujos_estado[order(label), label]
barplot(lambdas, names.arg = labels,
        col = c("firebrick", "steelblue", "darkorange"),
        main = "(A) Tasa de llegada λ por condición",
        ylab = "flujos/seg", ylim = c(0, max(lambdas) * 1.2))

# (B) VMR por condición (escala log)
vmrs <- resumen_flujos_estado[order(label), VMR]
barplot(log10(vmrs + 1), names.arg = labels,
        col = c("firebrick", "steelblue", "darkorange"),
        main = "(B) Índice VMR por condición (log10)",
        ylab = "log10(VMR + 1)")
abline(h = log10(2), col = "black", lty = 2)
legend("topleft", legend = "VMR = 1 (Poisson)", lty = 2, bty = "n")

# (C) % tiempo con congestión por escenario
pct <- resumen_congestion$pct_tiempo_congestion
barplot(pct,
        names.arg = c(paste("μ =", mu_baja),
                      paste("μ =", mu_media),
                      paste("μ =", mu_alta)),
        col = c("firebrick", "darkorange", "steelblue"),
        main = "(C) % tiempo con congestión por escenario",
        ylab = "% tiempo congestionado",
        ylim = c(0, max(pct) * 1.2))
text(x = c(0.7, 1.9, 3.1),
     y = pct + max(pct) * 0.05,
     labels = paste0(pct, "%"), cex = 0.9)

par(mfrow = c(1, 1))
dev.off()

cat("\nGráficas guardadas en carpeta: graficas_avance2/\n")


# ============================================================
# SECCIÓN 13: EXPORTAR OBJETOS PARA PERSONA 2 (SIMULADOR)
# ============================================================
# Estos objetos son la interfaz entre el EDA (Persona 1) y
# el simulador SSED (Persona 2).

cat("\n=== EXPORTANDO DATOS PARA EL SIMULADOR ===\n")

# Dataset limpio
fwrite(data_clean, "dataset_limpio_simulacion.csv")
cat("→ dataset_limpio_simulacion.csv\n")

# Serie de flujos por segundo por condición (distribuciones Fs)
fwrite(flujos_sec_label, "flujos_por_segundo_por_condicion.csv")
cat("→ flujos_por_segundo_por_condicion.csv  (distribuciones Fs para el simulador)\n")

# Serie global de flujos por segundo
fwrite(flujos_sec, "flujos_por_segundo_global.csv")
cat("→ flujos_por_segundo_global.csv\n")

# Parámetros del modelo en formato tabla
parametros_modelo <- data.table(
  parametro   = c("lambda_Benign", "lambda_Suspicious", "lambda_Attack",
                  "mu_baja", "mu_media", "mu_alta",
                  "T_horizonte", "delta_t"),
  valor       = c(resumen_flujos_estado[label == "Benign",    lambda],
                  resumen_flujos_estado[label == "Suspicious", lambda],
                  resumen_flujos_estado[label == "Attack",    lambda],
                  mu_baja, mu_media, mu_alta,
                  nrow(flujos_sec), 1),
  unidad      = c("flujos/s", "flujos/s", "flujos/s",
                  "flujos/s", "flujos/s", "flujos/s",
                  "segundos", "segundo"),
  descripcion = c("Tasa media estado Benign",
                  "Tasa media estado Suspicious",
                  "Tasa media estado Attack",
                  "P95 tráfico benigno (capacidad baja)",
                  "Capacidad media experimental",
                  "Capacidad alta experimental",
                  "Horizonte total de simulación",
                  "Ventana de tiempo del modelo")
)

fwrite(parametros_modelo, "parametros_modelo_simulacion.csv")
cat("→ parametros_modelo_simulacion.csv  (parámetros λ y μ para el simulador)\n")

# Duraciones de episodios por condición (t_cambio del SSED)
fwrite(resumen_duracion_estado, "duracion_episodios_por_condicion.csv")
cat("→ duracion_episodios_por_condicion.csv  (t_cambio para el pseudocódigo SSED)\n")

# Tablas resumen para el informe
fwrite(resumen_flujos_estado,   "resumen_flujos_estado.csv")
fwrite(resumen_carga_estado,    "resumen_carga_estado.csv")
fwrite(resumen_congestion,      "resumen_congestion.csv")
fwrite(resumen_exceso,          "resumen_exceso.csv")
cat("→ tablas resumen del informe exportadas\n")


# ============================================================
# SECCIÓN 14: RESUMEN FINAL EN CONSOLA
# ============================================================

cat("\n")
cat("============================================================\n")
cat("  RESUMEN FINAL — PERSONA 1: EDA Y PREPARACIÓN DE DATOS\n")
cat("============================================================\n")

cat("\nDataset: BCCC-cPacket-Cloud-DDoS-2024 | 18-dic-2023\n")
cat("Registros válidos:", nrow(data_clean), "\n")

cat("\nDistribución por etiqueta:\n")
print(table(data_clean$label))
print(round(prop.table(table(data_clean$label)) * 100, 2))

cat("\nVentanas de 1 segundo (horizonte T):", nrow(flujos_sec), "segundos\n")

cat("\nTasas de llegada por condición (λ en flujos/s):\n")
print(resumen_flujos_estado[, .(label, lambda = round(lambda, 2),
                                VMR = round(VMR, 1), p95, maximo)])

cat("\nCapacidades experimentales del servidor (μ):\n")
cat("  mu_baja  =", mu_baja,  "f/s  (P95 tráfico benigno)\n")
cat("  mu_media =", mu_media, "f/s\n")
cat("  mu_alta  =", mu_alta,  "f/s\n")

cat("\nCongestión por escenario:\n")
print(resumen_congestion)

cat("\nDuración media de episodios por condición:\n")
print(resumen_duracion_estado[, .(estado,
                                  media_duracion = round(media_duracion),
                                  mediana_duracion)])

cat("\nConclusión distribucional:\n")
cat("  Ninguna condición sigue Poisson (VMR >> 1 en todas).\n")
cat("  Se adopta distribución empírica Xs(t) ~ Fs por condición.\n")
cat("  Esto captura fielmente ráfagas, sobredispersión y asimetría.\n")

cat("\nArchivos generados:\n")
cat("  dataset_limpio_simulacion.csv\n")
cat("  flujos_por_segundo_por_condicion.csv  ← entrada principal del simulador\n")
cat("  parametros_modelo_simulacion.csv       ← λ y μ para Persona 2\n")
cat("  duracion_episodios_por_condicion.csv   ← t_cambio para Persona 2\n")
cat("  graficas_avance2/  (10 figuras)\n")
cat("============================================================\n")
