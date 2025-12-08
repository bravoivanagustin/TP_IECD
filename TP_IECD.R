# --------------------- Trabajo practico IECD, simulaciones --------------------
# Sección de comentarios:
#  - LU mas chico entre los tres integrantes: 64
#  - La idea es modularizar lo mas posible el codigo
#  - Vamos a intentar generalizar los procedimientos, para que sea simple cambiar los parametros que estan involucrados

library(ggplot2)
library(dplyr)

Se_default <- 0.9
Sp_default <- 0.95
theta_default <- 0.25

# ------------------------------- Sección 1 ------------------------------------
# Vamos a crear el metodo de la simulación requerida para la primera sección del trabajo.
#

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

# ------------------ Grafico para los intervalos -------------------------------

medias_resumen <- datos_totales %>%
  group_by(n_label) %>%
  summarise(media_del_centro = mean(centro))

# Grafico en conjunto todos los datos
ggplot(datos_totales, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # 1. Intervalos
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
  
  # 2. Línea Verde (Theta Real)
  # Mapeamos linetype dentro de aes() para que cree leyenda
  geom_vline(aes(xintercept = 0.25, linetype = "Theta = 0.25"), 
             color = "green", linewidth = 0.9) +
  
  # 3. Línea Azul (Media calculada)
  # Mapeamos linetype dentro de aes() para que cree leyenda
  geom_vline(data = medias_resumen, 
             aes(xintercept = media_del_centro, linetype = "Media de los centros"),
             color = "blue", linewidth = 0.7, alpha = 0.6) +
  
  # 4. Configuración de la leyenda de las líneas (Referencias)
  scale_linetype_manual(
    name = NULL, # Título de la leyenda
    values = c("Theta = 0.25" = "dashed", "Media de los centros" = "solid")
  ) +
  
  # 5. Colores de intervalos
  scale_color_manual(
    name = NULL,
    values = c("#C51B7D", "#222222"), 
    labels = c("No cubre a theta = 0.25", "Cubre a theta =  0.25")
  ) +
  
  # 6. Truco para que las líneas en la leyenda tengan color verde/azul
  # (Por defecto saldrían negras porque el color está fijo fuera de aes)
  guides(
    linetype = guide_legend(
      override.aes = list(color = c("blue", "green")) # Orden alfabético: Media (azul), Theta (verde)
    )
  ) +
  
  facet_wrap(~n_label, scales = "free_x") +
  
  labs(
    x = "Theta",
    y = "Simulación n°"
  ) +
  
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  )

# ------------------- Grafico de longitud media --------------------------------

resumen_longitud <- datos_totales %>%
  mutate(longitud = sup - inf) %>%   # Calculamos largo de cada intervalo
  group_by(n) %>%                    # Agrupamos por tamaño de muestra
  summarise(
    longitud_media = mean(longitud)  # Promedio
  )

# 2. Graficamos
ggplot(resumen_longitud, aes(x = n, y = longitud_media, group = 1)) +
  
  # Línea que conecta los puntos
  geom_line(color = "steelblue", linewidth = 1) +
  
  # Puntos destacados
  geom_point(color = "#C51B7D", size = 3) +
  
  # Etiquetas con el valor exacto sobre cada punto (opcional, ayuda a leer)
  geom_text(aes(label = round(longitud_media, 3)), 
            vjust = -0.8, color = "black", fontface = "bold", size = 3.5) +
  
  scale_x_log10() +
  
  # Escalas y etiquetas
  labs(
    x = "n",
    y = "Longitud media de los intervalos"
  ) +
  
  # Ajuste del eje Y para que no corte las etiquetas de arriba
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  
  # Tema estético (manteniendo el estilo que usabas antes)
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank() # Limpiamos lineas menores para que sea más claro
  )

# ------------------------------- Sección 2 ------------------------------------

# Vamos a crear el metodo de la simulación requerida para la segunda sección del trabajo.

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

ggplot(df_theta, aes(x = theta, y = p)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
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

ggplot(df_Se, aes(x = Se, y = p)) +
  geom_line(color = "red", linewidth = 1) +
  labs(
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

ggplot(df_Sp, aes(x = Sp, y = p)) +
  geom_line(color = "green", linewidth = 1) +
  labs(
    subtitle = paste("Theta =", theta_default, ", Se =", Se_default),
    x = "Especificidad",
    y = "Probabilidad p"
  ) +
  theme_bw()


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
n_sims <- c(3, 5, 10, 25, 50, 75, 100, 500, 1000, 5000)
mom_results <- run_mom_simulation(n_sims, theta_default, Se_default, Sp_default)

# Punto 6: Gráfico ECM Teórico vs ECM Test Perfecto
# Objetivo: Ver cuánto perdemos por usar un test imperfecto (comparación teórica)

# Armamos dataframe solo con las curvas teóricas
df_ecm_punto6 <- data.frame(
  n = rep(mom_results$n, 2),
  ecm = c(mom_results$ecm_theo, mom_results$ecm_perf),
  tipo = rep(c("Test imperfecto", "Test perfecto"), each = nrow(mom_results))
)

ggplot(df_ecm_punto6, aes(x = n, y = ecm, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  labs(
    y = "log(ECM)",
    x = "log(n)",
    # Eliminar el título de la leyenda para color y linetype
    color = NULL,
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    # Mover la leyenda dentro del gráfico (coordenadas 0 a 1)
    legend.position = c(0.8, 0.8),
    # Opcional: añade un fondo para que sea más legible
    legend.background = element_rect(fill = "white", color = "black")
  )

# Versión sin escala logarítmica
ggplot(df_ecm_punto6, aes(x = n, y = ecm, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    y = "ECM",
    x = "Tamaño de Muestra n"
  ) +
  theme_bw()

# ----------------------------- Ejercicio 7 ------------------------------------

# Gráficos de Validación (Bias y Ajuste ECM)

# 1. Gráfico de Sesgo (Bias) -> Demuestra que el estimador es insesgado (oscila en 0)
ggplot(mom_results, aes(x = n, y = bias_emp)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line(color = "darkgrey") +
  geom_point(size = 2, color = "blue") +
  scale_x_log10(breaks = n_sims) +
  labs(
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

ggplot(df_val, aes(x = n, y = val, color = metric, shape = metric)) +
  geom_line(aes(linetype = metric), linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_x_log10(breaks = n_sims) +
  scale_y_log10() +
  labs(
    y = "ECM (Escala Log)",
    x = "Tamaño de Muestra n (Escala Log)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# ----------------------------- Ejercicio 8 ------------------------------------

# Bootstrap
n_boot <- 10
p_true <- calc_p(theta_default, Se_default, Sp_default)
muestra_fija <- rbinom(n_boot, 1, p_true)
t_bar_obs <- mean(muestra_fija)
denom_mom <- Se_default + Sp_default - 1
theta_mom_obs <- (t_bar_obs + Sp_default - 1) / denom_mom

# Bootstrap Loop
B <- 1000
boot_ests <- numeric(B)

for (i in 1:B) {
  muestra_boot <- sample(muestra_fija, size = n_boot, replace = TRUE)
  t_bar_boot <- mean(muestra_boot)
  theta_boot <- (t_bar_boot + Sp_default - 1) / denom_mom
  boot_ests[i] <- theta_boot
}

# Histograma Bootstrap
df_boot <- data.frame(theta_est = boot_ests)

ggplot(df_boot, aes(x = theta_est)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = theta_mom_obs, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = paste("Distribución Bootstrap (n =", n_boot, ")"),
    subtitle = paste("Centrada en Theta_obs =", round(theta_mom_obs, 3)),
    x = "Estimador MoM Bootstrap",
    y = "Densidad"
  ) +
  theme_bw()


# Boxplot Bootstrap
ggplot(df_boot, aes(x = "", y = theta_est)) +
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

# ----------------------------- Ejercicio 9 ------------------------------------

# Cantidad de muestras bootstrap a generar por cada n
N = 1000

# Definimos los valores de n para realizar las simulaciones
ns = c(3,5,10,25,50,100,500,1000,5000) 

# Defimos p
p_default <- Se_default * theta_default + (1 - Sp_default) * (1 - theta_default)

#Definimos el estimador de momentos de theta
estimador_de_momentos = function(est_p, Se, Sp){
  return((est_p+Sp-1)/(Se+Sp-1))
}

# Tomando como inputs:
#  - n: tamaño de la muestra
#  - Se, Sp, y p dados en el ejercicio
#  - el nivel del intervalo
#  - N la cantidad de muestras a genenar
# Creamos intervalos de confianza percentil bootstrap

ic_bootstrap_percentil_2_9=function(n, Se=0.9, Sp=0.95, nivel=0.95, p_estimado,  N=1000){
  estimadores_de_momentos=numeric(N)
  for (i in 1:N) {
    muestra_2_9=rbinom(n, 1, p_estimado)
    est_p=mean(muestra_2_9)
    estimadores_de_momentos[i]=estimador_de_momentos(est_p, Se, Sp)
  }
  ic_bootstrap_perc_inf=quantile(estimadores_de_momentos, (1-nivel)/2)
  ic_bootstrap_perc_sup=quantile(estimadores_de_momentos,1-(1-nivel)/2)
  ic_bootstrap_perc=c(ic_bootstrap_perc_inf, ic_bootstrap_perc_sup)
  return(ic_bootstrap_perc)
}

# ----------------------------- Grafico para los intervalos ------------------------------------

# Creamos intervalos para cada n 
ic_percentil<-data.frame(n = ns, inf = NA, sup = NA)
for (i in 1:length(ns)){
  muestra_2_9=rbinom(ns[i], 1, p_default)
  est=mean(muestra_2_9)
  tita_estimado=estimador_de_momentos(est, Se_default, Sp_default)
  p_estimado=Se_default*tita_estimado+(1-Sp_default)*(1-tita_estimado)
  n_intervalo_b_perc=data.frame(
    n=numeric(100),
    sup=numeric(100),
    inf=numeric(100)
  )
  for (j in 1:100){
    ic_perc <- ic_bootstrap_percentil_2_9(ns[i], Se_default, Sp_default, 0.95, p_estimado, N)
    n_intervalo_b_perc$n[j]=ns[i]
    n_intervalo_b_perc$inf[j] <- ic_perc[1]
    n_intervalo_b_perc$sup[j] <- ic_perc[2]
    }
  ic_percentil=rbind(ic_percentil,n_intervalo_b_perc)
  
}

# Graficamos los intervalos bootstrap percentil promedio de nive 0.95 obtenidos para cada n
ic_promedio <- ic_percentil %>%
  filter(!is.na(inf)) %>% 
  group_by(n) %>%
  summarise(
    inf_prom = mean(inf, na.rm = TRUE),
    sup_prom = mean(sup, na.rm = TRUE)
  )

ggplot(ic_promedio, aes(x = n)) +
  geom_segment(aes(x = n, xend = n,
                   y = inf_prom, yend = sup_prom),
               color = "#1f77b4", linewidth = 1) +
  geom_point(aes(y = inf_prom), color = "#1f77b4", size = 2) +
  geom_point(aes(y = sup_prom), color = "#1f77b4", size = 2) +
  # Linea Negra (Theta Real)
  geom_hline(
    aes(yintercept = 0.25, linetype = "Theta = 0.25"),
    linewidth = 0.8,
    color = "black"
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c("Theta = 0.25" = "dashed")
  ) +
  scale_x_log10(breaks = ns, labels = ns) +
  labs(
    x = "n",
    y = "IC bootstrap percentil promedio de nivel 0.95"
  ) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "black")) 



# ----------------------------- Ejercicio 10 ------------------------------------

# Construimos los intervalos de confianza de nivel asintotico 0.95 para θ basados en θMoM.

ic_asintoticos_2_10=function(muestra, n, Se, Sp, nivel=0.95){
  est_p <- mean(muestra)
  estimador_mom=estimador_de_momentos(est_p, Se, Sp)
  s<-sqrt(est_p*(1-est_p))/(Se+Sp-1)
  z<-qnorm(1 - (1-nivel)/2)
  ic_asintoticos<-c(inf=estimador_mom-z*s/sqrt(n) , sup=estimador_mom+z*s/sqrt(n))
  return(ic_asintoticos)
}

# Creamos intervalos para cada n 
ics_asintoticos<-data.frame(n = ns, inf = NA, sup = NA)
for (i in 1:length(ns)){
  muestra_2_10=rbinom(ns[i], 1, p_default)
  n_intervalo_asint=data.frame(
    n=numeric(100),
    sup=numeric(100),
    inf=numeric(100)
  )
  for (j in 1:100){
    ic_asint <- ic_asintoticos_2_10(muestra_2_10, ns[i], Se_default, Sp_default, 0.95)
    n_intervalo_asint$n[j]=ns[i]
    n_intervalo_asint$inf[j] <- ic_asint[1]
    n_intervalo_asint$sup[j] <- ic_asint[2]
  }
  ics_asintoticos=rbind(ics_asintoticos,n_intervalo_asint)
  
}

# ----------------------------- Grafico para los intervalos ------------------------------------

# Graficamos los intervalos bootstrap percentil promedio de nive 0.95 obtenidos para cada n
ic_promedio_asint <- ics_asintoticos %>%
  filter(!is.na(inf)) %>% 
  group_by(n) %>%
  summarise(
    inf_prom = mean(inf, na.rm = TRUE),
    sup_prom = mean(sup, na.rm = TRUE)
  )

ggplot(ic_promedio_asint, aes(x = n)) +
  geom_segment(aes(x = n, xend = n,
                   y = inf_prom, yend = sup_prom),
               color = "#9467bd", linewidth = 1) +
  geom_point(aes(y = inf_prom), color = "#9467bd", size = 2) +
  geom_point(aes(y = sup_prom), color = "#9467bd", size = 2) +
  # Linea Negra (Theta Real)
  geom_hline(
    aes(yintercept = 0.25, linetype = "Theta = 0.25"),
    linewidth = 0.8,
    color = "black"
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c("Theta = 0.25" = "dashed")
  ) +
  scale_x_log10(breaks = ns, labels = ns) +
  labs(
    x = "n",
    y = "IC bootstrap percentil promedio de nivel 0.95"
  ) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.8),
        legend.background = element_rect(fill = "white", color = "black")) 


# ----------------------------- Ejercicio 11 ------------------------------------

# Para comparar los intervalos definimos un data frame con información sobre ellos
simulacion_2_11=function(theta, n, Se, Sp, p, nivel=0.95, B=1000){
  
  intervalos_ip=data.frame(
    simulacion = 1:B,
    inf = numeric(B),
    sup = numeric(B),
    cubre = logical(B),
    long = numeric(B),
    tipo = "Bootstrap percentil"
  )
  
  intervalos_ic=data.frame(
    simulacion = 1:B,
    inf = numeric(B),
    sup = numeric(B),
    cubre = logical(B),
    long = numeric(B),
    tipo = "Asintotico"
  )
  
  for (i in 1:B){
    
    muestra_2_11=rbinom(n, 1, p_default)
    est=mean(muestra_2_11)
    tita_estimado=estimador_de_momentos(est, Se_default, Sp_default)
    p_estimado=Se_default*tita_estimado+(1-Sp_default)*(1-tita_estimado)
    
    ip = ic_bootstrap_percentil_2_9(n, Se_default, Sp_default, nivel,p_estimado, N=1000)
    intervalos_ip$inf[i] = ip[1]
    intervalos_ip$sup[i] = ip[2]
    intervalos_ip$cubre[i] = theta >= ip[1] && theta <= ip[2]
    intervalos_ip$long[i] =ip[2]-ip[1]
    
    ic = ic_asintoticos_2_10(muestra_2_11, n, Se_default, Sp_default,  nivel)
    intervalos_ic$inf[i] = ic[1]
    intervalos_ic$sup[i] = ic[2]
    intervalos_ic$cubre[i] = theta >= ic[1] && theta <= ic[2]
    intervalos_ic$long[i] = ic[2]-ic[1]
  }
  return(list(intervalos_ip, intervalos_ic))
}

# ----------------------------- Grafico para los intervalos ------------------------------------


#Creamos dataframes para guardar los intervalos de cada tipo para los diferentes n
datos_2_11_ip=data.frame()
datos_2_11_asint=data.frame()

datos_2_11_ip=data.frame()
datos_2_11_asint=data.frame()
for (n in ns){
  simulaciones=simulacion_2_11(theta_default, n, Se_default, Sp_default, p_default, 0.95, 1000)
  intervalos_ip=simulaciones[[1]]
  cob = mean(intervalos_ip$cubre) * 100
  intervalos_ip$n = n
  intervalos_ip$n_label = paste0("n = ",n, "\n(Cob: ", cob, "%)")
  datos_2_11_ip = rbind(datos_2_11_ip, intervalos_ip)
  
  intervalos_asint=simulaciones[[2]]
  cob = mean(intervalos_asint$cubre) * 100
  intervalos_asint$n = n
  intervalos_asint$n_label = paste0("n = ",n, "\n(Cob: ", cob, "%)")
  datos_2_11_asint = rbind(datos_2_11_asint, intervalos_asint)
}
#Graficamos intervalos de confianza bootstrap percentil para analizar el porcentaje de cobertura de cada n
# Ordeno para que los graficos aparezcan de menor a mayor n
datos_2_11_ip$n_label = reorder(datos_2_11_ip$n_label, datos_2_11_ip$n)

medias_resumen = datos_2_11_ip %>%
  group_by(n_label) 

# Grafico en conjunto todos los datos
ggplot(datos_2_11_ip, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # 1. Intervalos
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
  
  # 2. Línea Verde (Theta Real)
  geom_vline(aes(xintercept = 0.25, linetype = "Theta = 0.25"),
             color = "green", linewidth = 0.9) +
  
  #3.  Configuración de la leyenda de las líneas (Referencias)
  scale_linetype_manual(
    name = NULL, #Título de la leyenda
    values=c("Theta = 0.25" = "dashed")
  ) +
  #4. Colores de intervalos 
  scale_color_manual(
    name=NULL, 
    values = c("#C51B7D", "#222222"), 
    labels = c("No cubre a theta = 0.25", "Cubre a theta = 0.25")) +
  
  facet_wrap(~ n_label, scales = "free_x") +
  
  labs(
    x = "Theta",
    y = "Simulación n°",
  ) +
  
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"), 
    strip.text = element_text(face = "bold") 
  )

  
#Graficamos intervalos de confianza asintoticos para analizar el porcentaje de cobertura de cada n


# Ordenamos para que los graficos aparezcan de menor a mayor n
datos_2_11_asint$n_label = reorder(datos_2_11_asint$n_label, datos_2_11_asint$n)

medias_resumen = datos_2_11_asint %>%
  group_by(n_label) 

# Graficamos en conjunto todos los datos
ggplot(datos_2_11_asint, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # 1. Intervalos
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
  
  # 2. Línea Verde (Theta Real)
  geom_vline(aes(xintercept = 0.25, linetype = "Theta = 0.25"),
             color = "green", linewidth = 0.9) +

  
  #3.  Configuración de la leyenda de las líneas (Referencias)
  scale_linetype_manual(
    name = NULL, #Título de la leyenda
    values=c("Theta = 0.25" = "dashed")
  ) +
  #4. Colores de intervalos 
  scale_color_manual(
   name=NULL, 
    values = c("#C51B7D", "#222222"), 
    labels = c("No cubre a theta = 0.25", "Cubre a theta = 0.25")) +
  
  facet_wrap(~ n_label, scales = "free_x") +
  
  labs(
    x = "Theta",
    y = "Simulación n°",
  ) +
  
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"), 
    strip.text = element_text(face = "bold") 
  )

  # ----------------------------- Grafico para comparaciones ------------------------------------  
# Comparamos las longitudes promedios y porcentajes de cobertura para cada n
  # Creamos un data frame para guardar el porcentaje de cobertura y longitud promedio de cada n
datos_2_11=rbind(datos_2_11_ip, datos_2_11_asint)
comparaciones <- datos_2_11 %>%
  group_by(n, tipo) %>%
  summarise(
    longitud_promedio = mean(long),
    cobertura = mean(cubre) * 100
  ) %>%
  mutate(n_label = paste0("n = ", n))
comparaciones$n_label = reorder(comparaciones$n_label, comparaciones$n)

 # Graficamos las longitudes promedio de ambos intervalos para cada n
ggplot(comparaciones, aes(x = n_label, y = longitud_promedio, fill = tipo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      #Azul (Bootstrap percentil)
      "Bootstrap percentil" = "#1f77b4",
      #Violeta (Asintotico)
      "Asintotico" = "#9467bd"
    )
  ) +
  labs(
    x = "n",
    y = "Longitud promedio",
    fill = "Intervalo de confianza"
  ) +
  theme_bw() +
  theme(    legend.position = c(0.8, 0.7),
            legend.background = element_rect(fill = "white", color = "black"))

# Graficamos el porcentaje de ambos intervalos para cada n

ggplot(comparaciones, aes(x = n_label, y = cobertura, fill = tipo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      #Azul (Bootstrap percentil)
      "Bootstrap percentil" = "#1f77b4",
      #Violeta (Asintotico)
      "Asintotico" = "#9467bd"
    )
  ) +
  labs(
    x = "n",
    y = "Porcentaje de cobertura", 
    fill = "Intervalo de confianza"
  ) +
  ylim(0, 130) +
  theme_bw() +
  theme(    legend.position = c(0.8, 0.85),
            legend.background = element_rect(fill = "white", color = "black")) 



# ----------------------------- Ejercicio 13 ------------------------------------



# Definimos el estimador truncado
estimador_truncado = function(estimador_mom){
  if(estimador_mom<0){
    return(0)
  } else {
    if (estimador_mom > 1) {
      return(1)
    } else {
      return(estimador_mom)
    }
  }
}

#Definimos las simulaciones para obtener las características del estimador truncado

caracteristicas_truncado <- function(theta = 0.25, n, Se = 0.9, Sp = 0.95, p_est, N = 1000) {
  est_truncado= numeric(N)
  for (i in 1:N){
    muestra = rbinom(n, 1, p_est)
    est_p = mean(muestra)
    estimador_mom = estimador_de_momentos(est_p, Se, Sp)
    est_truncado[i] = estimador_truncado(estimador_mom)
  }
  # Guardamos los datos que queremos analizar
  
  varianza = var(est_truncado)
  sesgo = mean(est_truncado) - theta
  ECM = varianza + sesgo^2
  dist_asint = sqrt(n) * (est_truncado - theta)
  
  return(list(
    varianza = varianza,
    sesgo = sesgo,
    ECM = ECM,
    dist_asint = dist_asint
  ))
}

# ----------------------------- Graficos de varianza, sesgo y ECM para n de interes ------------------------------------

# Creamos un data frame que guarde para cada n los atributos de interes


Var_ECM_Sesgo_truncado = data.frame(
  n = numeric(0),
  variable = character(0),
  value = numeric(0),
  stringsAsFactors = FALSE
)

#n=10
muestra_2_13_10 <- rbinom(10, 1, p_default)
est=mean(muestra_2_13_10)
tita_estimado=estimador_de_momentos(est, Se_default, Sp_default)
tita_estimado_trunc=estimador_truncado(tita_estimado)
p_estimado = Se_default * tita_estimado_trunc + (1 - Sp_default) * (1 - tita_estimado_trunc)
Var_ECM_Sesgo_truncado_10 <- caracteristicas_truncado(theta_default, 10, Se_default, Sp_default, p_estimado, 1000)
Var_ECM_Sesgo_truncado <- rbind(
  Var_ECM_Sesgo_truncado,
  data.frame(n = 10, variable = "varianza", value = Var_ECM_Sesgo_truncado_10$varianza),
  data.frame(n = 10, variable = "sesgo", value = Var_ECM_Sesgo_truncado_10$sesgo),
  data.frame(n = 10, variable = "ECM", value = Var_ECM_Sesgo_truncado_10$ECM)
)

#n=100
muestra_2_13_100 <- rbinom(100, 1, p_default)
est=mean(muestra_2_13_100)
tita_estimado=estimador_de_momentos(est, Se_default, Sp_default)
tita_estimado_trunc=estimador_truncado(tita_estimado)
p_est=Se_default*tita_estimado_trunc+(1-Sp_default)*(1-tita_estimado_trunc)
Var_ECM_Sesgo_truncado_100 <- caracteristicas_truncado(theta_default, 100, Se_default,Sp_default, p_est, 1000)
Var_ECM_Sesgo_truncado <- rbind(
  Var_ECM_Sesgo_truncado,
  data.frame(n = 100, variable = "varianza", value = Var_ECM_Sesgo_truncado_100$varianza),
  data.frame(n = 100, variable = "sesgo", value = Var_ECM_Sesgo_truncado_100$sesgo),
  data.frame(n = 100, variable = "ECM", value = Var_ECM_Sesgo_truncado_100$ECM))

#n=1000
muestra_2_13_1000 <- rbinom(1000, 1, p_default)
est=mean(muestra_2_13_1000)
tita_estimado=estimador_de_momentos(est, Se_default, Sp_default)
tita_estimado_trunc=estimador_truncado(tita_estimado)
p_est=Se_default*tita_estimado_trunc+(1-Sp_default)*(1-tita_estimado_trunc)
Var_ECM_Sesgo_truncado_1000 <- caracteristicas_truncado(theta_default, 1000, Se_default, Sp_default, p_est, 1000)
Var_ECM_Sesgo_truncado <- rbind(
  Var_ECM_Sesgo_truncado,
  data.frame(n = 1000, variable = "varianza", value = Var_ECM_Sesgo_truncado_1000$varianza),
  data.frame(n = 1000, variable = "sesgo", value = Var_ECM_Sesgo_truncado_1000$sesgo),
  data.frame(n = 1000, variable = "ECM", value = Var_ECM_Sesgo_truncado_1000$ECM))

#Visualizamos mediante un gráfico los valores de la varianza, sesgo y el ECM

ggplot(Var_ECM_Sesgo_truncado, aes(x = n, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(10, 100, 1000)) +
  labs(
    title = "Sesgo, Varianza y ECM del estimador truncado",
    x = "n", y = "Valor", color = "Medida"
  ) +
  theme_minimal(base_size = 14)

# ----------------------------- Grafico para la distribución asintótica  ------------------------------------

# Visualizamos la distribucion asintotica con qqplot

dist_asint_truncado = data.frame(
  dist = c(Var_ECM_Sesgo_truncado_10$dist_asint,
           Var_ECM_Sesgo_truncado_100$dist_asint,
           Var_ECM_Sesgo_truncado_1000$dist_asint),
  n = factor(rep(c(10, 100, 1000), each = length(Var_ECM_Sesgo_truncado_10$dist_asint)))
)

ggplot(dist_asint_truncado, aes(sample = dist)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1) +
  facet_wrap(~n, scales = "free") +
  labs(title = "Distribución asintótica del estimador truncado") +
  theme_minimal(base_size = 14)

#Visualizamos como se ve la distribución asintótica con un histograma
ggplot(dist_asint_truncado, aes(x = dist)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  facet_wrap(~n, scales = "free") +
  labs(
    x = "Distribución asintótica",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14)


# ------------------------------- Sección 3 ------------------------------------
# Vamos a crear un metodo de simulación requerida para la tercera sección del trabajo.
#
# Los inputs del metodo son:
#  - theta1: prevalencia previa
#  - theta2: prevalencia posterior
#  - n1: tamaño de la muestra previa
#  - n2: tamaño de la muestra posterior
#  - Se: sensibilidad del test
#  - SP: especificidad del test
#  - alpha: uno menos el nivel que queremos alcanzar
#  - N: cantidad de intervalos que vamos a generar
# Los outputs son:
#  - intervalos: conjunto de los intervalos obtenidos, con estos mismos podemos graficar el cubrimiento
#

simulacion_3 = function(theta1, theta2, n1, n2, Se, Sp, alpha, N) {
  inf = numeric(N)
  sup = numeric(N)
  cubre = logical(N)
  centro = numeric(N)
  
  for (i in 1:N) {
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

for (i in c(3,5,10,25,50,100,500,1000,5000)) {
  # Limites del conjunto de enteros
  min_val = ceiling(3/4 * i)
  max_val = floor(5/4 * i)
  
  # Definir el conjunto de enteros a muestrear
  range_to_sample = min_val:max_val
  
  # Muestrear k y j independientemente (1 valor cada uno)
  k = sample(range_to_sample, 1)
  j = sample(range_to_sample, 1)
  
  intervalos = simulacion_3(0.2, 0.15, k, j, Se_default, Sp_default, 0.05, 1000)
  
  # Se eligieron n_pre y n_post distintos, pero de manera que se cumplan los supuestos propuestos
  # cuando tienden a infinito
  
  cob = mean(intervalos$cubre) * 100
  intervalos$n = i
  intervalos$n_label = paste0("n_pre = ", k, "\n n_post = ", j, "\n(Cob: ", cob, "%)")
  
  datos_totales_3 = rbind(datos_totales_3, intervalos)
}

# Ordeno para que los graficos aparezcan de menor a mayor n
datos_totales_3$n_label <- reorder(datos_totales_3$n_label, datos_totales_3$n)

medias_resumen_3 <- datos_totales_3 %>%
  group_by(n_label) %>%
  summarise(media_del_centro = mean(centro))

# Grafico en conjunto todos los datos
ggplot(datos_totales_3, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # 1. Intervalos
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
  
  # 2. Línea Verde (Delta Real)
  # CORREGIDO: El label coincide con el valor (-0.05) y con el scale_linetype
  geom_vline(aes(xintercept = -0.05, linetype = "Delta = -0.05"), 
             color = "green", linewidth = 0.9) +
  
  # 3. Línea Azul (Media calculada)
  geom_vline(data = medias_resumen_3, 
             aes(xintercept = media_del_centro, linetype = "Media de los centros"),
             color = "blue", linewidth = 0.7, alpha = 0.6) +
  
  # 4. Configuración de la leyenda de las líneas (Referencias)
  scale_linetype_manual(
    name = NULL,
    values = c("Delta = -0.05" = "dashed", "Media de los centros" = "solid")
  ) +
  
  # 5. Colores de intervalos (Esto se mantiene igual)
  scale_color_manual(
    name = NULL,
    values = c("#C51B7D", "#222222"), 
    labels = c("No cubre a delta = -0.05", "Cubre a delta = -0.05")
  ) +
  
  # 6. Colores en orden para las etiquetas
  guides(
    linetype = guide_legend(
      override.aes = list(color = c("green", "blue")) 
    )
  ) +
  
  facet_wrap(~n_label, scales = "free_x") +
  
  labs(
    x = "Delta",
    y = "Simulación n°"
  ) +
  
  theme_bw() +
  
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 0.8)
  )

