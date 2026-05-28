
# ============================================================
# LIBRERÍAS
# ============================================================
library(data.table)

carpeta_salida <- "resultados_entrega_final"

if (!dir.exists(carpeta_salida)) {
  dir.create(carpeta_salida)
}

# ============================================================
# SECCIÓN 1: PARÁMETROS GLOBALES
# (calibrados con el EDA de Persona 1 — Avance 2, Tabla 3 y 4)
# ============================================================

# Tasas de llegada por condición [flujos/segundo]
LAMBDA_B  <- 2.84    # Benign
LAMBDA_S  <- 4.49    # Suspicious
LAMBDA_A  <- 10.57   # Attack (media; ráfagas capturadas por dist. empírica)

# Capacidades experimentales del servidor [flujos/segundo]
MU_BAJA   <- 6       # P95 tráfico benigno
MU_MEDIA  <- 20      # escenario intermedio
MU_ALTA   <- 100     # escenario alta capacidad

# Horizonte y granularidad temporal
T_TOTAL   <- 28800   # segundos  (≈ 8 horas: 08:56–17:00)
DELTA_T   <- 1       # segundo   (unidad de ventana)

# Réplicas de Monte Carlo
N_REP     <- 100

# Duración media de episodios por estado (análisis RLE, Tabla 6 Avance 2)
DUR_B     <- 1720    # Benign    — episodios variables, media 1720 s
DUR_S     <- 617     # Suspicious — coincide con intervalos entre ataques
DUR_A     <- 1200    # Attack     — exactamente 20 min (paper base, Shafi 2024)

cat("=== PARÁMETROS DEL MODELO ===\n")
cat(sprintf("  λ_Benign = %.2f f/s | λ_Suspicious = %.2f f/s | λ_Attack = %.2f f/s\n",
            LAMBDA_B, LAMBDA_S, LAMBDA_A))
cat(sprintf("  μ escenarios: %d / %d / %d f/s\n", MU_BAJA, MU_MEDIA, MU_ALTA))
cat(sprintf("  T = %d s | N_réplicas = %d\n\n", T_TOTAL, N_REP))


# ============================================================
# SECCIÓN 2: CARGAR DISTRIBUCIONES EMPÍRICAS Fs
# ============================================================
# Intenta leer el CSV generado por Persona 1.
# Si no existe, construye aproximaciones NegBin calibradas
# con los parámetros del EDA (VMR por condición).
# ─ VMR_Benign=2.3 → size = λ²/(Var−λ) = 2.84²/(6.53−2.84) ≈ 2.19
# ─ VMR_Susp=3.1   → size = 4.49²/(13.92−4.49)             ≈ 2.14
# ─ VMR_Attack=549 → cola pesada; se modela como mezcla bimodal

cargar_distribucion_empirica <- function() {
  archivo <- "flujos_por_segundo_por_condicion.csv"

  if (file.exists(archivo)) {
    cat("✔ Cargando distribución empírica real desde:", archivo, "\n\n")
    dt <- fread(archivo)

    # Retorna listas de valores observados por condición
    list(
      Benign     = dt[label == "Benign",     flujos],
      Suspicious = dt[label == "Suspicious", flujos],
      Attack     = dt[label == "Attack",     flujos]
    )
  } else {
    cat("⚠ CSV de Persona 1 no encontrado. Usando distribución NegBin calibrada.\n\n")
    NULL   # señal para usar generadores paramétricos
  }
}

DIST_EMPIRICA <- cargar_distribucion_empirica()


# ============================================================
# SECCIÓN 3: GENERADORES DE Xs(t)
# ============================================================
# Justificación distribucional (Avance 2, Sección 5.5–5.6):
#   Ninguna condición sigue Poisson (VMR >> 1 en las tres).
#   → Se adopta distribución empírica Fs por condición.
#   → Como aproximación paramétrica: NegBin para Benign/Suspicious,
#     mezcla bimodal para Attack (ráfagas extremas, VMR=549).

gen_Xs <- function(n, estado) {

  if (!is.null(DIST_EMPIRICA)) {
    # Muestreo directo desde datos reales (distribución empírica verdadera)
    sample(DIST_EMPIRICA[[estado]], size = n, replace = TRUE)

  } else {
    # Aproximación paramétrica cuando no hay CSV de Persona 1
    if (estado == "Benign") {
      pmax(0L, rnbinom(n, mu = LAMBDA_B, size = 2.19))

    } else if (estado == "Suspicious") {
      pmax(0L, rnbinom(n, mu = LAMBDA_S, size = 2.14))

    } else {  # Attack — mezcla: 70% fuera de pico, 30% ráfaga
      es_rafaga <- runif(n) < 0.30
      base      <- pmax(0L, rnbinom(n, mu = 3,  size = 1.0))
      pico      <- pmax(0L, rnbinom(n, mu = 30, size = 0.1))
      as.integer(ifelse(es_rafaga, pico, base))
    }
  }
}


# ============================================================
# SECCIÓN 4: GENERADOR DE DURACIÓN DE EPISODIOS
# ============================================================
# Attack: duración casi fija de 20 min (Shafi et al., 2024)
# Benign/Suspicious: exponencial con media del análisis RLE

gen_duracion_episodio <- function(estado) {
  switch(estado,
    Benign     = max(60L,  as.integer(round(rexp(1, rate = 1 / DUR_B)))),
    Suspicious = max(60L,  as.integer(round(rexp(1, rate = 1 / DUR_S)))),
    Attack     = max(600L, as.integer(round(rnorm(1, mean = DUR_A, sd = 60))))
  )
}


# ============================================================
# SECCIÓN 5: CADENA DE TRANSICIÓN ENTRE ESTADOS
# ============================================================
# Secuencia observada en el paper base (Shafi et al., 2024):
#   Benign → Attack → Suspicious → Attack → Suspicious → ...
# Los ataques de 20 min alternan con intervalos de 10 min sospechosos.

siguiente_estado <- function(estado_actual) {
  switch(estado_actual,
    Benign     = "Attack",
    Attack     = "Suspicious",
    Suspicious = "Attack"
  )
}


# ============================================================
# SECCIÓN 6: FUNCIÓN PRINCIPAL — simular_servidor
# ============================================================
# Argumentos:
#   mu           : capacidad del servidor [flujos/s]
#   estado_ini   : estado inicial del sistema
#   tiempo_total : duración total de la simulación [segundos]
#
# Retorna lista con:
#   flujos_procesados       — vector numérico, longitud tiempo_total
#   exceso                  — vector numérico, exceso observado por segundo
#   llegadas                — vector numérico, flujos generados por segundo
#   estados_trafico         — estado de tráfico usado en cada segundo
#   segundos_congestionados — entero, ventanas en estado congestionado
#   vector_simulacion       — character, estado en cada segundo

simular_servidor <- function(mu,
                             estado_ini   = "Benign",
                             tiempo_total = T_TOTAL) {
  
  # Inicialización
  tiempo <- 0L
  estado <- estado_ini
  
  llegadas_vec <- integer(tiempo_total)
  flujos_proc  <- integer(tiempo_total)
  exceso_vec   <- integer(tiempo_total)
  cong_vec     <- integer(tiempo_total)
  estado_vec   <- character(tiempo_total)
  
  seg_cong <- 0L
  
  # Bucle de eventos SSED
  while (tiempo < tiempo_total) {
    
    # EVENTO: cambio de estado — generar duración del episodio
    dur_ep <- gen_duracion_episodio(estado)
    t_fin  <- min(tiempo + dur_ep, tiempo_total)
    n_seg  <- t_fin - tiempo
    
    # Generar llegadas del episodio
    Xs <- gen_Xs(n_seg, estado)
    
    # EVENTO: actualización de ventana segundo a segundo
    for (i in seq_len(n_seg)) {
      
      t_idx <- tiempo + i
      
      llegada_total <- Xs[i]
      
      procesados <- min(llegada_total, mu)
      exceso     <- max(0, llegada_total - mu)
      congestion <- as.integer(exceso > 0)
      
      llegadas_vec[t_idx] <- llegada_total
      flujos_proc[t_idx]  <- procesados
      exceso_vec[t_idx]   <- exceso
      cong_vec[t_idx]     <- congestion
      estado_vec[t_idx]   <- estado
      
      if (congestion == 1) {
        seg_cong <- seg_cong + 1L
      }
    }
    
    tiempo <- t_fin
    
    # EVENTO: cambio de condición del tráfico
    if (tiempo < tiempo_total) {
      estado <- siguiente_estado(estado)
    }
  }
  
  # EVENTO: fin de simulación
  list(
    llegadas                = llegadas_vec,
    flujos_procesados       = flujos_proc,
    exceso                  = exceso_vec,
    congestion              = cong_vec,
    estados_trafico         = estado_vec,
    segundos_congestionados = seg_cong
  )
}


# ============================================================
# SECCIÓN 7: RÉPLICAS DE MONTE CARLO
# 3 escenarios de μ × 100 réplicas = 300 simulaciones
# ============================================================

set.seed(42)  # reproducibilidad

escenarios <- list(
  list(mu = MU_BAJA,  nombre = "mu_baja_6"),
  list(mu = MU_MEDIA, nombre = "mu_media_20"),
  list(mu = MU_ALTA,  nombre = "mu_alta_100")
)

todos_resultados <- data.table()
detalle_simulacion <- data.table()

for (esc in escenarios) {
  cat(sprintf("Simulando escenario μ = %d f/s ...", esc$mu))

  res_esc <- data.table(
    escenario               = character(N_REP),
    mu                      = integer(N_REP),
    replica                 = integer(N_REP),
    total_procesados        = numeric(N_REP),
    total_exceso            = numeric(N_REP),
    segundos_congestionados = integer(N_REP),
    prop_congestion         = numeric(N_REP)
  )


  for (i in seq_len(N_REP)) {
    res <- simular_servidor(mu = esc$mu)

    res_esc[i, `:=`(
      escenario               = esc$nombre,
      mu                      = esc$mu,
      replica                 = i,
      total_procesados        = sum(res$flujos_procesados),
      total_exceso            = sum(res$exceso),
      segundos_congestionados = res$segundos_congestionados,
      prop_congestion         = res$segundos_congestionados / T_TOTAL
    )]

    if (i == 1) {
      detalle_tmp <- data.table(
        escenario = esc$nombre,
        mu = esc$mu,
        replica = i,
        segundo = seq_len(T_TOTAL),
        estado_trafico = res$estados_trafico,
        llegadas = res$llegadas,
        procesados = res$flujos_procesados,
        exceso = res$exceso,
        congestion = res$congestion
      )
      
      detalle_simulacion <- rbindlist(
        list(detalle_simulacion, detalle_tmp),
        fill = TRUE
      )
    }
  }

  todos_resultados <- rbindlist(list(todos_resultados, res_esc))
  cat(" ✔\n")
}

cat("\n")


# ============================================================
# SECCIÓN 8: EXPORTAR RESULTADOS
# ============================================================

fwrite(todos_resultados, file.path(carpeta_salida, "resultados_replicas.csv"))
cat("✔ resultados_replicas.csv exportado (", nrow(todos_resultados), "filas )\n")

fwrite(detalle_simulacion, file.path(carpeta_salida, "detalle_simulacion_replica1.csv"))
cat("✔ detalle_simulacion_replica1.csv exportado (", nrow(detalle_simulacion), "filas )\n")


# ============================================================
# SECCIÓN 9: RESUMEN ESTADÍSTICO EN CONSOLA
# ============================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  RESUMEN — PERSONA 2: SIMULADOR SSED\n")
cat("  Dataset: BCCC-cPacket-Cloud-DDoS-2024 | 18-dic-2023\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat(sprintf("  T = %d s | %d réplicas | Estados: Benign / Suspicious / Attack\n\n",
            T_TOTAL, N_REP))

resumen_final <- todos_resultados[, .(
  media_procesados  = round(mean(total_procesados)),
  sd_procesados     = round(sd(total_procesados)),
  media_exceso      = round(mean(total_exceso)),
  sd_exceso         = round(sd(total_exceso)),
  pct_cong_media    = round(mean(prop_congestion) * 100, 1),
  pct_cong_ic_low   = round(quantile(prop_congestion, 0.025) * 100, 1),
  pct_cong_ic_high  = round(quantile(prop_congestion, 0.975) * 100, 1)
), by = .(escenario, mu)][order(mu)]

for (i in seq_len(nrow(resumen_final))) {
  r <- resumen_final[i]
  cat(sprintf("── μ = %3d f/s (%s)\n", r$mu, r$escenario))
  cat(sprintf("   Flujos procesados  : %s ± %s\n",
              format(r$media_procesados, big.mark = ","),
              format(r$sd_procesados,   big.mark = ",")))
  cat(sprintf("   Exceso acumulado   : %s ± %s\n",
              format(r$media_exceso, big.mark = ","),
              format(r$sd_exceso,    big.mark = ",")))
  cat(sprintf("   %% congestión       : %.1f%%  (IC 95%%: [%.1f%%, %.1f%%])\n\n",
              r$pct_cong_media, r$pct_cong_ic_low, r$pct_cong_ic_high))
}
cat("═══════════════════════════════════════════════════════════════\n\n")


# ============================================================
# ============================================================
# SECCIÓN 10: GRÁFICAS DE RESULTADOS
# ============================================================

dir_graficas <- file.path(carpeta_salida, "graficas_simulador")

if (!dir.exists(dir_graficas)) {
  dir.create(dir_graficas)
}

graphics.off()

colores_esc <- c(
  "mu_baja_6"   = "firebrick",
  "mu_media_20" = "darkorange",
  "mu_alta_100" = "steelblue"
)


png(
  file.path(dir_graficas, "10_boxplot_congestion.png"),
  width = 1000,
  height = 700
)
# ── Figura 1: Boxplot de proporción de congestión ───────────
boxplot(
  prop_congestion * 100 ~ escenario,
  data     = todos_resultados,
  col      = colores_esc[unique(todos_resultados[order(mu)]$escenario)],
  main     = "Proporción de congestión por escenario (100 réplicas)",
  xlab     = "Escenario de capacidad",
  ylab     = "% tiempo congestionado",
  names    = c("μ = 6 f/s", "μ = 20 f/s", "μ = 100 f/s"),
  outline  = TRUE
)

abline(h = mean(todos_resultados[mu == MU_BAJA, prop_congestion]) * 100,
       col = "firebrick", lty = 2)

abline(h = mean(todos_resultados[mu == MU_MEDIA, prop_congestion]) * 100,
       col = "darkorange", lty = 2)

abline(h = mean(todos_resultados[mu == MU_ALTA, prop_congestion]) * 100,
       col = "steelblue", lty = 2)

dev.off()

png(
  file.path(dir_graficas, "11_histogramas_congestion.png"),
  width = 1400,
  height = 500
)

# ── Figura 2: Histogramas ───────────────────────────────────
par(mfrow = c(1,3))

for (esc in escenarios) {
  
  sub   <- todos_resultados[mu == esc$mu]
  media <- mean(sub$prop_congestion) * 100
  ic_lo <- quantile(sub$prop_congestion, 0.025) * 100
  ic_hi <- quantile(sub$prop_congestion, 0.975) * 100
  
  hist(
    sub$prop_congestion * 100,
    breaks = 20,
    col    = colores_esc[esc$nombre],
    main   = sprintf("μ = %d f/s\nMedia = %.1f%%",
                     esc$mu, media),
    xlab   = "% tiempo congestionado",
    ylab   = "Frecuencia"
  )
  
  abline(v = media, col = "black", lty = 2, lwd = 2)
}

par(mfrow = c(1,1))

dev.off()

# ── Figura 3: Boxplot flujos procesados ─────────────────────

png(
  file.path(dir_graficas, "12_boxplot_procesados.png"),
  width = 1000,
  height = 700
)

boxplot(
  total_procesados ~ escenario,
  data    = todos_resultados,
  col     = colores_esc[unique(todos_resultados[order(mu)]$escenario)],
  main    = "Total de flujos procesados",
  xlab    = "Escenario",
  ylab    = "Flujos procesados",
  names   = c("μ = 6", "μ = 20", "μ = 100")
)

dev.off()


# ── Figura 4: Boxplot exceso acumulado ──────────────────────

png(
  file.path(dir_graficas, "13_boxplot_exceso.png"),
  width = 1000,
  height = 700
)


boxplot(
  total_exceso ~ escenario,
  data    = todos_resultados,
  col     = colores_esc[unique(todos_resultados[order(mu)]$escenario)],
  main    = "Exceso acumulado",
  xlab    = "Escenario",
  ylab    = "Flujos en exceso",
  names   = c("μ = 6", "μ = 20", "μ = 100")
)

dev.off()

# ── Figura 5: Trayectorias de simulación ────────────────────

png(
  file.path(dir_graficas, "14_trayectorias_simulacion.png"),
  width = 1200,
  height = 900
)

par(mfrow = c(3,1), mar = c(3,4,2,1))

set.seed(1)

for (esc in escenarios) {
  
  res   <- simular_servidor(mu = esc$mu)
  t_seq <- seq_len(T_TOTAL)
  
  plot(
    t_seq,
    res$flujos_procesados,
    type = "l",
    col  = colores_esc[esc$nombre],
    lwd  = 0.8,
    main = sprintf("Trayectoria — μ = %d", esc$mu),
    xlab = "Tiempo",
    ylab = "Flujos"
  )
  
  abline(h = esc$mu, col = "black", lty = 2)
}

par(mfrow = c(1,1))

dev.off()

# ── Figura 6: Trayectoria exceso ────────────────────────────

png(
  file.path(dir_graficas, "15_trayectorias_exceso.png"),
  width = 1200,
  height = 900
)

par(mfrow = c(3,1), mar = c(3,4,2,1))

set.seed(1)

for (esc in escenarios) {
  
  res <- simular_servidor(mu = esc$mu)
  
  plot(
    seq_len(T_TOTAL),
    res$exceso,
    type = "l",
    col  = colores_esc[esc$nombre],
    lwd  = 0.8,
    main = sprintf("Exceso instantáneo — μ = %d", esc$mu),
    xlab = "Tiempo",
    ylab = "Exceso"
  )
}

par(mfrow = c(1,1))

dev.off()

# ── Figura 7: Resumen comparativo ───────────────────────────

png(
  file.path(dir_graficas, "16_resumen_comparativo.png"),
  width = 1400,
  height = 500
)

par(mfrow = c(1,3))

# (A) Congestión
pcts <- resumen_final$pct_cong_media

barplot(
  pcts,
  names.arg = c("μ=6", "μ=20", "μ=100"),
  col       = c("firebrick", "darkorange", "steelblue"),
  main      = "% Congestión",
  ylab      = "%"
)

# (B) Flujos procesados
mproc <- resumen_final$media_procesados / 1000

barplot(
  mproc,
  names.arg = c("μ=6", "μ=20", "μ=100"),
  col       = c("firebrick", "darkorange", "steelblue"),
  main      = "Flujos procesados",
  ylab      = "Miles"
)

# (C) Exceso
mexc <- resumen_final$media_exceso / 1000

barplot(
  mexc,
  names.arg = c("μ=6", "μ=20", "μ=100"),
  col       = c("firebrick", "darkorange", "steelblue"),
  main      = "Exceso acumulado",
  ylab      = "Miles"
)

par(mfrow = c(1,1))

dev.off()

cat("\n✔ Gráficas mostradas en el panel Plots de RStudio\n")

