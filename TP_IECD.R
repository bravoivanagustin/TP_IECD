# --------------------- Trabajo practico IECD, simulaciones --------------------
# Sección de comentarios:
#  - LU mas chico entre los tres integrantes: 64
#  - La idea es modularizar lo mas posible el codigo
#  - Vamos a intentar generalizar los procedimientos, para que sea simple cambiar los parametros que estan involucrados

library(ggplot2)
library(dplyr)

# ------------------------------- Sección 1 ------------------------------------
# Vamos a crear el metodo de la simulación requerida para la primera sección del trabajo.
#
# Los inputs del metodo son:
#  - theta: es el valor real para el cual vamos a hacer la simulacion
#  - n: tamaño de la muestra
#  - N: cantidad de intervalos que vamos a generar
# Los outputs son:
#  - intervalos: conjunto de los intervalos obtenidos, con estos mismos podemos graficar el cubrimiento
set.seed(64)

# Funcion de simulacion
simulacion_1 <- function(theta, n, alpha, N) {
  inf <- numeric(N)
  sup <- numeric(N)
  cubre <- logical(N)
  centro <- numeric(N)

  for (i in 1:N) {
    muestras <- rbinom(n, 1, theta)
    t_p <- mean(muestras)
    z_a <- qnorm(1 - alpha / 2)
    err <- sqrt(t_p * (1 - t_p) / n) * z_a

    inf[i] <- t_p - err
    sup[i] <- t_p + err
    centro[i] <- t_p
    cubre[i] <- theta >= inf[i] && theta <= sup[i]
  }

  return(data.frame(simulacion = 1:N, inf = inf, sup = sup, cubre = cubre, centro = centro))
}

# Definicion de los intervalos
datos_totales <- data.frame()

for (i in c(3,5,10,25,50,100,500,1000,5000)) {
  intervalos = simulacion_1(0.25, i, 0.05, 1000) # Theta = 0.25, alpha = 0.05
  
  cob = mean(intervalos$cubre) * 100
  intervalos$n = i
  intervalos$n_label = paste0("n = ", i, "\n(Cob: ", cob, "%)")
    
  datos_totales = rbind(datos_totales, intervalos)
}

# Ordeno para que los graficos aparezcan de menor a mayor n
datos_totales$n_label <- reorder(datos_totales$n_label, datos_totales$n)

medias_resumen <- datos_totales %>%
  group_by(n_label) %>%
  summarise(media_del_centro = mean(centro))

# Grafico en conjunto todos los datos
ggplot(datos_totales, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +

  # Dibujamos los intervalos obtenidos en la simulación
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +

  # La linea punteada negra es el valor real de theta
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "black") +

  # La linea solida azul es la media del punto medio de los intervalos para cada n
  geom_vline(
    data = medias_resumen, aes(xintercept = media_del_centro),
    color = "blue", linetype = "solid", linewidth = 0.7, alpha = 0.6
  ) +

  # Color para identificar si cubre o no al valor real de theta
  scale_color_manual(values = c("#C51B7D", "#222222"), labels = c("No cubre", "Cubre")) +

  # Se separan los graficos en función de n
  facet_wrap(~n_label, scales = "free_x") +

  # Labels del grafico
  labs(
    title = "Intervalos de confianza asintoticos para theta = 0.25",
    x = "Valor del parámetro",
    y = "Simulación n°",
    color = "¿Cubre a 0.25?"
  ) +

  # Temas utilizados, fondo blanco.
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"), # Fondo blanco en los títulos de los paneles
    strip.text = element_text(face = "bold") # Negrita en los títulos de los paneles
  )



# ------------------------------- Sección 2 ------------------------------------

# Vamos a crear el metodo de la simulación requerida para la segunda sección del trabajo.
#
# Los inputs del metodo son:
#  - theta: es el valor real para el cual vamos a hacer la simulacion
#  - n: tamaño de la muestra
#  - N: cantidad de intervalos que vamos a generar
# Los outputs son:
#  - intervalos: conjunto de los intervalos obtenidos, con estos mismos podemos graficar el cubrimiento


# Parámetros fijos para los ejercicios
Se_default <- 0.9
Sp_default <- 0.95
theta_default <- 0.25

# ----------------------------- Ejercicio 3 ------------------------------------

# Gráficos de p en función de theta, Se, Sp

# Función auxiliar para calcular p
calc_p <- function(theta, Se, Sp) {
  return(Se * theta + (1 - Sp) * (1 - theta))
}

# (a) p vs theta (Se, Sp fijos)
theta_seq <- seq(0, 1, length.out = 100)
df_theta <- data.frame(
  theta = theta_seq,
  p = calc_p(theta_seq, Se_default, Sp_default),
  variable = "Theta"
)

g1 <- ggplot(df_theta, aes(x = theta, y = p)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "p vs Prevalencia (theta)",
    subtitle = paste("Se =", Se_default, ", Sp =", Sp_default),
    x = "Theta",
    y = "Probabilidad p"
  ) +
  theme_bw()

# (b) p vs Se (theta, Sp fijos)
Se_seq <- seq(0.5, 1, length.out = 100)
df_Se <- data.frame(
  Se = Se_seq,
  p = calc_p(theta_default, Se_seq, Sp_default),
  variable = "Sensibilidad"
)

g2 <- ggplot(df_Se, aes(x = Se, y = p)) +
  geom_line(color = "red", linewidth = 1) +
  labs(
    title = "p vs Sensibilidad (Se)",
    subtitle = paste("Theta =", theta_default, ", Sp =", Sp_default),
    x = "Sensibilidad",
    y = "Probabilidad p"
  ) +
  theme_bw()

# (c) p vs Sp (theta, Se fijos)
Sp_seq <- seq(0.5, 1, length.out = 100)
df_Sp <- data.frame(
  Sp = Sp_seq,
  p = calc_p(theta_default, Se_default, Sp_seq),
  variable = "Especificidad"
)

g3 <- ggplot(df_Sp, aes(x = Sp, y = p)) +
  geom_line(color = "green", linewidth = 1) +
  labs(
    title = "p vs Especificidad (Sp)",
    subtitle = paste("Theta =", theta_default, ", Se =", Se_default),
    x = "Especificidad",
    y = "Probabilidad p"
  ) +
  theme_bw()

# Guardar gráficos Punto 3
ggsave("p_vs_theta.png", plot = g1, width = 6, height = 4)
ggsave("p_vs_Se.png", plot = g2, width = 6, height = 4)
ggsave("p_vs_Sp.png", plot = g3, width = 6, height = 4)

# ----------------------------- Ejercicio 6 ------------------------------------

# Función de simulación para calcular métricas empíricas y teóricas
run_mom_simulation <- function(n_values, theta, Se, Sp, R = 1000) {
  denom <- Se + Sp - 1
  p_true <- calc_p(theta, Se, Sp)

  results <- data.frame()

  for (n in n_values) {
    # 1. Simulación (Empírico)
    # Genera R conteos totales de positivos directamente (suma de n Bernoullis)
    X <- rbinom(R, n, p_true)
    # Convierte conteos a proporciones (equivalente a mean de ceros y unos)
    T_bar <- X / n

    # Estimador MoM
    theta_mom <- (T_bar + Sp - 1) / denom

    # Métricas Empíricas (Para Punto 7)
    bias_emp <- mean(theta_mom) - theta
    var_emp <- var(theta_mom)
    ecm_emp <- mean((theta_mom - theta)^2)

    # 2. Teoría (Para Punto 6 y validación del 7)
    # ECM Teórico del MoM
    ecm_theo <- (p_true * (1 - p_true)) / (n * denom^2)

    # ECM Test Perfecto (Referencia)
    # ECM Perf = theta(1-theta) / n
    ecm_perf <- (theta * (1 - theta)) / n

    results <- rbind(results, data.frame(
      n = n,
      bias_emp = bias_emp,
      var_emp = var_emp,
      ecm_emp = ecm_emp,
      ecm_theo = ecm_theo,
      ecm_perf = ecm_perf
    ))
  }
  return(results)
}

# Ejecución de simulación
n_sims <- c(3, 5, 10, 25, 50, 100, 500, 1000, 5000)
mom_results <- run_mom_simulation(n_sims, theta_default, Se_default, Sp_default)

# Punto 6: Gráfico ECM Teórico vs ECM Test Perfecto
# Objetivo: Ver cuánto perdemos por usar un test imperfecto (comparación teórica)

# Armamos dataframe solo con las curvas teóricas
df_ecm_punto6 <- data.frame(
  n = rep(mom_results$n, 2),
  ecm = c(mom_results$ecm_theo, mom_results$ecm_perf),
  tipo = rep(c("MoM Teórico (Imperfecto)", "Test Perfecto (Ideal)"), each = nrow(mom_results))
)

g4 <- ggplot(df_ecm_punto6, aes(x = n, y = ecm, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  labs(
    title = "Comparación de ECM: Test Imperfecto vs Perfecto",
    y = "ECM (Escala Log)",
    x = "Tamaño de Muestra n (Escala Log)"
  ) +
  theme_bw()

# Guardar gráfico Punto 6
ggsave("ecm_comparison.png", plot = g4, width = 6, height = 4)

# Versión sin escala logarítmica
g4_nolog <- ggplot(df_ecm_punto6, aes(x = n, y = ecm, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Comparación de ECM: Test Imperfecto vs Perfecto (Escala Lineal)",
    y = "ECM",
    x = "Tamaño de Muestra n"
  ) +
  theme_bw()

ggsave("ecm_comparison_linear.png", plot = g4_nolog, width = 6, height = 4)

# ----------------------------- Ejercicio 7 ------------------------------------

# Gráficos de Validación (Bias y Ajuste ECM)

# 1. Gráfico de Sesgo (Bias) -> Demuestra que el estimador es insesgado (oscila en 0)
g_bias <- ggplot(mom_results, aes(x = n, y = bias_emp)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "darkgrey") +
  geom_point(size = 2, color = "blue") +
  scale_x_log10(breaks = n_sims) +
  labs(
    title = "Validación: Sesgo del Estimador (Bias) vs n",
    subtitle = "Se espera que oscile alrededor de 0",
    y = "Sesgo Empírico",
    x = "Tamaño de Muestra n (Escala Log)"
  ) +
  theme_bw()

ggsave("validacion_bias.png", plot = g_bias, width = 6, height = 4)

# 2. Gráfico Ajuste ECM -> Demuestra que la teoría coincide con la simulación
df_val <- data.frame(
  n = rep(mom_results$n, 2),
  val = c(mom_results$ecm_emp, mom_results$ecm_theo),
  metric = rep(c("Empírico (Simulado)", "Teórico (Fórmula)"), each = nrow(mom_results))
)

g_val <- ggplot(df_val, aes(x = n, y = val, color = metric, shape = metric)) +
  geom_line(aes(linetype = metric), linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_x_log10(breaks = n_sims) +
  scale_y_log10() +
  labs(
    title = "Validación: Ajuste Empírico vs Teórico",
    subtitle = "Los puntos deben caer sobre la línea",
    y = "ECM (Escala Log)",
    x = "Tamaño de Muestra n (Escala Log)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("validacion_ecm_ajuste.png", plot = g_val, width = 6, height = 4)


# ----------------------------- Ejercicio 8 ------------------------------------

# Bootstrap
# Fijamos semilla para reproducibilidad de la muestra fija "observada"
n_boot <- 10
p_true <- calc_p(theta_default, Se_default, Sp_default)
muestra_fija <- rbinom(n_boot, 1, p_true)
t_bar_obs <- mean(muestra_fija)
denom_mom <- Se_default + Sp_default - 1
theta_mom_obs <- (t_bar_obs + Sp_default - 1) / denom_mom

# Bootstrap Loop
B <- 2000
boot_ests <- numeric(B)

for (i in 1:B) {
  muestra_boot <- sample(muestra_fija, size = n_boot, replace = TRUE)
  t_bar_boot <- mean(muestra_boot)
  theta_boot <- (t_bar_boot + Sp_default - 1) / denom_mom
  boot_ests[i] <- theta_boot
}

# Histograma Bootstrap
df_boot <- data.frame(theta_est = boot_ests)
g5 <- ggplot(df_boot, aes(x = theta_est)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = theta_mom_obs, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = paste("Distribución Bootstrap (n =", n_boot, ")"),
    subtitle = paste("Centrada en Theta_obs =", round(theta_mom_obs, 3)),
    x = "Estimador MoM Bootstrap",
    y = "Densidad"
  ) +
  theme_bw()

# Guardar gráfico Punto 8 (Histograma)
ggsave("bootstrap_dist.png", plot = g5, width = 6, height = 4)

# Boxplot Bootstrap
g6 <- ggplot(df_boot, aes(x = "", y = theta_est)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  geom_hline(yintercept = theta_mom_obs, color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 0.25, color = "blue", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Boxplot Bootstrap",
    subtitle = "Rojo: Theta Obs | Azul: Theta Real (0.25)",
    y = "Estimador MoM Bootstrap",
    x = ""
  ) +
  theme_bw()

ggsave("bootstrap_boxplot.png", plot = g6, width = 4, height = 4)


# ----------------------------- Ejercicio 9 ------------------------------------

# Cantidad de muestras bootstrap a generar por cada n
N = 1000

# Definimos los valores fijos dados en el ejercicio
theta = 0.25; Se = 0.9; Sp = 0.95

# Definimos los valores de n para realizar las simulaciones
ns = c(5, 10, 25, 50, 100, 500) 

# Defimos p
p = Se*theta + (1-Sp)*(1-theta)
# Defimos p
p <- Se * theta + (1 - Sp) * (1 - theta)

# Definimos el estimador de momentos de theta
estimador_de_momentos <- function(est_p, Se, Sp) {
  return((est_p + Sp - 1) / (Se + Sp - 1))
#Definimos el estimador de momentos de theta
estimador_de_momentos = function(est_p, Se, Sp){
  return((est_p+Sp-1)/(Se+Sp-1))
}
# Tomando como inputs:
# - n: tamaño de la muestr
# - Se, Sp, y p dados en el ejercicio
# - el nivel del intervalo
# - N la cantidad de muestras a genenar
# Creamos intervalos de confianza percentil bootstrap
ic_bootstrap_percentil_2_9 <- function(n, Se = 0.9, Sp = 0.95, p = p, nivel = 0.05, N = 5000) {
  estimadores_de_momentos <- numeric(N)
  muestra_2_9 <- rbinom(n, 1, p)

# Tomando como inputs:
#  - n: tamaño de la muestr
#  - Se, Sp, y p dados en el ejercicio
#  - el nivel del intervalo
#  - N la cantidad de muestras a genenar
# Creamos intervalos de confianza percentil bootstrap

ic_bootstrap_percentil_2_9 = function(n, Se=0.9, Sp=0.95, p=p, nivel=0.05, N=5000){
  estimadores_de_momentos = numeric(N)
  muestra_2_9 = rbinom(n, 1, p)
  for (i in 1:N) {
    # muestra_2_9=rbinom(n, 1, p)
    tboot <- sample(muestra_2_9, n, replace = TRUE)
    est_p <- mean(tboot)
    estimadores_de_momentos[i] <- estimador_de_momentos(est_p, Se, Sp)
    #muestra_2_9=rbinom(n, 1, p)
    tboot = sample(muestra_2_9, n, replace = TRUE)
    est_p=mean(tboot)
    estimadores_de_momentos[i]=estimador_de_momentos(est_p, Se, Sp)
  }
  ic_bootstrap_perc_inf <- quantile(estimadores_de_momentos, nivel / 2)
  ic_bootstrap_perc_sup <- quantile(estimadores_de_momentos, 1 - nivel / 2)
  ic_bootstrap_perc <- c(ic_bootstrap_perc_inf, ic_bootstrap_perc_sup)
  return(ic_bootstrap_perc)
}
# Creamos intervalos para cada n
for (n in ns) {

# Creamos intervalos para cada n
for (n in ns){
  print(ic_bootstrap_percentil_2_9(n, Se, Sp, p, 0.05, N))
}

# ----------------------------- Ejercicio 10 ------------------------------------

# Construimos los intervalos de confianza de nivel asintotico 0.95 para θ basados en θMoM.
  ic_asintoticos_2_10=function(n, Se, Sp, p, nivel=0.05){
    muestra_2_10=rbinom(n, 1, p)
    est_p=mean(muestra_2_10)
    estimadores_de_momentos=estimador_de_momentos(est_p, Se, Sp)
    s=sqrt(est_p*(1-est_p))/(Se+Sp-1)
    z=qnorm(1 - nivel/2)
    ic_asintoticos=c(inf=estimadores_de_momentos-z*sqrt(est_p*(1-est_p))/((Se+Sp-1)*sqrt(n)) , sup=estimadores_de_momentos+z*sqrt(est_p*(1-est_p))/((Se+Sp-1)*sqrt(n)))
    return(ic_asintoticos)
  }

# Creamos intervalos para cada n 
for (n in ns){
  ic=ic_asintoticos_2_10(n,Se, Sp, p, 0.95)
}

# ----------------------------- Ejercicio 11 ------------------------------------

# Para comparar los intervalos definimos un data frame con información sobre ellos
simulacion_2_11=function(theta, n, Se, Sp, p, nivel=0.05, B=100){
  intervalos_ip=data.frame(
    simulacion = 1:B,
    inf = numeric(B),
    sup = numeric(B),
    cubre = logical(B),
    long = numeric(B),
    tipo = "Bootstrap percentil"
  )
  intervalos_ic <- data.frame(
    simulacion = 1:B,
    inf = numeric(B),
    sup = numeric(B),
    cubre = logical(B),
    long = numeric(B),
    tipo = "Asintotico"
  )
  for (i in 1:B) {
    ip <- ic_bootstrap_percentil_2_9(n, Se, Sp, p, nivel, N = 5000)
    intervalos_ip$inf[i] <- ip[1]
    intervalos_ip$sup[i] <- ip[2]
    intervalos_ip$cubre[i] <- theta >= ip[1] && theta <= ip[2]
    intervalos_ip$long[i] <- ip[2] - ip[1]
    ic <- ic_asintoticos_2_10(n, Se, Sp, p, nivel)
    intervalos_ic$inf[i] <- ic[1]
    intervalos_ic$sup[i] <- ic[2]
    intervalos_ic$cubre[i] <- theta >= ic[1] && theta <= ic[2]
    intervalos_ic$long[i] <- ic[2] - ic[1]
  }
  intervalos_ip_ic <- rbind(intervalos_ip, intervalos_ic)
  return(intervalos_ip_ic)
}
datos_2_11 data.frame()
for (n in ns) {
  intervalos_n <- simulacion_2_11(theta, n, Se, Sp, p, 0.05, 100)
  intervalos_n$n <- n
  intervalos_n$n_label <- paste0("n = ", n)

  datos_2_11 <- rbind(datos_2_11, intervalos_n)
}
# Ordeno para que los graficos aparezcan de menor a mayor n
datos_2_11$n_label <- reorder(datos_2_11$n_label, datos_2_11$n)
datos_2_11$centro <- (datos_2_11$inf + datos_2_11$sup) / 2

# Grafico en conjunto todos los datos
ggplot(datos_2_11, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +

  # Dibujamos los intervalos obtenidos en la simulación
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +

  # La linea punteada negra es el valor real de theta
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "black") +


  # Color para identificar si cubre o no al valor real de theta
  scale_color_manual(values = c("#C51B7D", "#222222"), labels = c("No cubre", "Cubre")) +

  # Se separan los graficos en función de n y en que tipo de intervalos son
  facet_grid(tipo ~ n_label, scales = "free_x") +

  # Labels del grafico
  labs(
    title = "Intervalos de confianza bootstrap percentil vs asintóticos",
    x = "Valor del parámetro",
    y = "Simulación n°",
    color = "¿Cubre a 0.25?"
  ) +

  # Temas utilizados, fondo blanco.
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"), # Fondo blanco en los títulos de los paneles
    strip.text = element_text(face = "bold") # Negrita en los títulos de los paneles
  )
# Comparamos las longitudes promedios y porcentajes de cobertura para cada n
comparaciones <- datos_2_11 %>%
  group_by(n_label, tipo) %>%
  summarise(
    longitud_promedio = mean(long),
    cobertura = mean(cubre) * 100
  )
ggplot(comparaciones, aes(x = n_label, y = longitud_promedio, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Longitud promedio de los intervalos",
    x = "Tamaño muestral (n)",
    y = "Longitud promedio"
  ) +
  theme_bw()
ggplot(comparaciones, aes(x = n_label, y = cobertura, fill = tipo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Cobertura (%) por método",
    x = "Tamaño muestral (n)",
    y = "Porcentaje de cobertura (%)"
  ) +
  theme_bw()
# Ahora hacemos una tabla comparativa
comparaciones <- datos_2_11 %>%
  group_by(n, tipo) %>%
  summarise(
    longitud_promedio = mean(long),
    cobertura = mean(cubre) * 100,
    .groups = "drop"
  ) %>%
  group_by(n) %>%
  summarise(
    cobertura_asintotico    = cobertura[tipo == "Asintotico"],
    cobertura_percentil     = cobertura[tipo == "Bootstrap percentil"],
    long_prom_asintotico    = longitud_promedio[tipo == "Asintotico"],
    long_prom_percentil     = longitud_promedio[tipo == "Bootstrap percentil"]
  )
tabla_comparaciones <- data.frame(
  n = comparaciones$n,
  variable = rep(c(
    "Cobertura asintótico",
    "Cobertura percentil",
    "Long. prom. asintótico",
    "Long. prom. percentil"
  ), each = length(comparaciones$n)),
  valor = c(
    comparaciones$cobertura_asintotico,
    comparaciones$cobertura_percentil,
    comparaciones$long_prom_asintotico,
    comparaciones$long_prom_percentil
  )
)
ggplot(tabla_comparaciones, aes(x = variable, y = factor(n))) +
  geom_tile(fill = "white", color = "black") +
  geom_text(aes(label = sprintf("%.2f", valor)), size = 4) +
  scale_x_discrete(position = "top") +
  labs(
    x = "",
    y = "n",
    title = "Coberturas y longitudes promedio"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold")
  )

# ----------------------------- Ejercicio 13 ------------------------------------


# Definimos el estimador truncado
estimador_truncado = function(estimador_momentos){
  if(estimador_momentos<0){
    return(0)
  } else {
    if (estimador_momentos > 1) {
      return(1)
    } else {
      return(estimador_momentos)
    }
  }
}

caracteristicas_truncado <- function(theta = 0.25, n, Se = 0.9, Sp = 0.95, p, N = 5000) {
  est_truncado <- numeric(N)
  for (i in 1:N) {
    muestra_2_13 <- rbinom(n, 1, p)
    est_p <- mean(muestra_2_13)
    estimador_momentos <- estimador_de_momentos(est_p, Se, Sp)
    estimador__mom_truncado <- estimador_truncado(estimador_momentos)
    est_truncado[i] <- estimador__mom_truncado
caracteristicas_truncado=function(theta=0.25, n, Se=0.9, Sp=0.95, p, N=5000){
  est_truncado= numeric(N)
  for (i in 1:N){
    muestra_2_13=rbinom(n, 1, p)
    est_p=mean(muestra_2_13)
    estimador_momentos=estimador_de_momentos(est_p, Se, Sp)
    estimador__mom_truncado=estimador_truncado(estimador_momentos)
    est_truncado[i]=estimador__mom_truncado
  }
  # Guardamos los datos que queremos analizar
  varianza <- var(est_truncado)
  sesgo <- mean(est_truncado) - theta
  ECM <- varianza - (sesgo)**2 # Compromiso sesgo - Varianza
  dist_asint <- sqrt(n) * (est_truncado - theta)
  return(list(
    varianza = varianza,
    sesgo = sesgo,
    ECM = ECM,
    dist_asint = dist_asint,
    est_truncado = est_truncado
  ))
}

#Creamos un data frame que guarde para cada n los atributos de interes
est_truncado_10=caracteristicas_truncado(0.25, 10, 0.9, 0.95, p, 5000)
est_truncado_100=caracteristicas_truncado(0.25, 100, 0.9, 0.95, p, 5000)
est_truncado_1000=caracteristicas_truncado(0.25, 1000, 0.9, 0.95, p, 5000)
#Visualizamos los valores obtenidos de la varianza, sesgo y ECM para n =10, 100, 1000
Var_ECM_Sesgo_truncado=data.frame(
  n = rep(c(10, 100, 1000), times = 3),
  variable = rep(c("Varianza", "Sesgo", "ECM"), each = 3),
  value = c(
    est_truncado_10$varianza, est_truncado_100$varianza, est_truncado_1000$varianza,
    est_truncado_10$sesgo, est_truncado_100$sesgo, est_truncado_1000$sesgo,
    est_truncado_10$ECM, est_truncado_100$ECM, est_truncado_1000$ECM
  )
)


ggplot(Var_ECM_Sesgo_truncado, aes(x = n, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(10, 100, 1000)) +
  labs(
    title = "Sesgo, Varianza y ECM del estimador truncado",
    x = "n ", y = "Valor", color = "Medida"
  ) +
  theme_minimal(base_size = 14)
# Visualizamos la distribucion asintotica con qqplot
dist_asint_truncado <- data.frame(
  dist = c(
    est_truncado_10$dist_asint,
    est_truncado_100$dist_asint,
    est_truncado_1000$dist_asint
  ),
  n = factor(rep(c(10, 100, 1000), each = 5000))
)
ggplot(dist_asint_truncado, aes(sample = dist)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1) +
  facet_wrap(~n, scales = "free") +
  labs(title = "Distribución asintótica del estimador de momentos truncado") +
  theme_minimal(base_size = 14)


# ------------------------------- Sección 3 ------------------------------------
# Vamos a crear un metodo de simulación requerida para la tercera sección del trabajo.
#
# Los inputs del metodo son:
#  - theta: es el valor real para el cual vamos a hacer la simulacion
#  - n: tamaño de la muestra
#  - N: cantidad de intervalos que vamos a generar
# Los outputs son:
#  - intervalos: conjunto de los intervalos obtenidos, con estos mismos podemos graficar el cubrimiento
#
# Utilizando los resultados del metodo se realizan graficos mostrando el cubrimiento
#

simulacion_3 <- function(theta_pre, theta_post, n_pre, n_post, Se, Sp, alpha, N) {
  inf <- numeric(N)
  sup <- numeric(N)
  cubre <- logical(N)
  centro <- numeric(N)

simulacion_3 = function(theta1, theta2, n1, n2, Se, Sp, alpha, N) {
  inf = numeric(N)
  sup = numeric(N)
  cubre = logical(N)
  centro = numeric(N)
  
  for (i in 1:N) {
    delta <- theta_post - theta_pre

    p_pre <- (Se + Sp - 1) * theta_pre + (1 - Sp)
    p_post <- (Se + Sp - 1) * theta_post + (1 - Sp)

    muestras_pre <- rbinom(n_pre, 1, p_pre)
    muestras_post <- rbinom(n_post, 1, p_post)

    est_theta_pre <- (mean(muestras_pre) + Sp - 1) / (Se + Sp - 1)
    est_theta_post <- (mean(muestras_post) + Sp - 1) / (Se + Sp - 1)

    z_a <- qnorm(1 - alpha / 2)

    est_sigma_pre <- mean(muestras_pre) * (1 - mean(muestras_pre)) / ((Se + Sp - 1)**2)
    est_sigma_post <- mean(muestras_post) * (1 - mean(muestras_post)) / ((Se + Sp - 1)**2)

    inf[i] <- est_theta_post - est_theta_pre - z_a * sqrt(est_sigma_post / n_post + est_sigma_pre / n_pre)
    sup[i] <- est_theta_post - est_theta_pre + z_a * sqrt(est_sigma_post / n_post + est_sigma_pre / n_pre)
    centro[i] <- est_theta_post - est_theta_pre
    cubre[i] <- delta >= inf[i] && delta <= sup[i]
    delta = theta2 - theta1
    
    p1 = (Se+Sp-1)*theta1 + (1-Sp)
    p2 = (Se+Sp-1)*theta2 + (1-Sp)
    
    muestras1 = rbinom(n1, 1, p1)
    muestras2 = rbinom(n2, 1, p2)
    
    est_theta1 = (mean(muestras1) + Sp - 1)/(Se + Sp - 1) 
    est_theta2 = (mean(muestras2) + Sp - 1)/(Se + Sp - 1)
    
    z_a = qnorm(1 - alpha/2)
    
    est_sigma1 = mean(muestras1) * (1-mean(muestras1)) / ((Se+Sp-1)**2)
    est_sigma2 = mean(muestras2) * (1-mean(muestras2)) / ((Se+Sp-1)**2)
    
    inf[i] = est_theta2 - est_theta1 - z_a * sqrt(est_sigma2/n2 + est_sigma1/n1)
    sup[i] = est_theta2 - est_theta1 + z_a * sqrt(est_sigma2/n2 + est_sigma1/n1)
    centro[i] = est_theta2 - est_theta1
    cubre[i] = delta >= inf[i] && delta <= sup[i]
  }

  return(data.frame(simulacion = 1:N, inf = inf, sup = sup, cubre = cubre, centro = centro))
}

datos_totales_3 <- data.frame()

for (i in c(5, 10, 25, 50, 100, 500, 1000, 5000, 10000)) {
  intervalos <- simulacion_3(0.2, 0.15, i, i, 0.9, 0.95, 0.05, 1000)

  cob <- mean(intervalos$cubre) * 100
  intervalos$n <- i
  intervalos$n_label <- paste0("n_pre = n_post = ", i, "\n(Cob: ", cob, "%)")

  datos_totales_3 <- rbind(datos_totales_3, intervalos)
for (i in c(3,5,10,25,50,100,500,1000,5000)) {
  intervalos = simulacion_3(0.2, 0.15, i, i, 0.9, 0.95, 0.05, 1000)
  
  cob = mean(intervalos$cubre) * 100
  intervalos$n = i
  intervalos$n_label = paste0("n_pre = n_post = ", i, "\n(Cob: ", cob, "%)")
  
  datos_totales_3 = rbind(datos_totales_3, intervalos)
}

# Ordeno para que los graficos aparezcan de menor a mayor n
datos_totales_3$n_label <- reorder(datos_totales_3$n_label, datos_totales_3$n)

medias_resumen <- datos_totales_3 %>%
  group_by(n_label) %>%
  summarise(media_del_centro = mean(centro))

# Grafico en conjunto todos los datos
ggplot(datos_totales_3, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +

  # Dibujamos los intervalos obtenidos en la simulación
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +

  # La linea punteada negra es el valor real de theta
  geom_vline(xintercept = -0.05, linetype = "dashed", color = "black") +

  # La linea solida azul es la media del punto medio de los intervalos para cada n
  geom_vline(
    data = medias_resumen, aes(xintercept = media_del_centro),
    color = "blue", linetype = "solid", linewidth = 0.7, alpha = 0.6
  ) +

  # Color para identificar si cubre o no al valor real de theta
  scale_color_manual(values = c("#C51B7D", "#222222"), labels = c("No cubre", "Cubre")) +

  # Se separan los graficos en función de n
  facet_wrap(~n_label, scales = "free_x") +

  # Labels del grafico
  labs(
    title = "Intervalos de confianza asintoticos para delta = -0.05",
    x = "Valor del parámetro",
    y = "Simulación n°",
    color = "¿Cubre a -0.05?"
  ) +

  # Temas utilizados, fondo blanco.
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"), # Fondo blanco en los títulos de los paneles
    strip.text = element_text(face = "bold") # Negrita en los títulos de los paneles
  )
