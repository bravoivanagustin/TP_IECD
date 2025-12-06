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
simulacion_1 = function(theta, n, alpha, N) {
  inf = numeric(N)
  sup = numeric(N)
  cubre = logical(N)
  centro = numeric(N)
  
  for (i in 1:N) {
    muestras = rbinom(n, 1, theta)
    t_p = mean(muestras)
    z_a = qnorm(1 - alpha/2)
    err = sqrt(t_p*(1-t_p)/n) * z_a
    
    inf[i] = t_p - err
    sup[i] = t_p + err
    centro[i] = t_p
    cubre[i] = theta >= inf[i] && theta <= sup[i]
  }
  
  return(data.frame(simulacion = 1:N, inf = inf, sup = sup, cubre = cubre, centro = centro))
}

# Definicion de los intervalos
datos_totales = data.frame()

for (i in c(5,10,25,50,100,500,1000,5000,10000)) {
  intervalos = simulacion_1(0.25, i, 0.05, 1000)
  
  cob = mean(intervalos$cubre) * 100
  intervalos$n = i
  intervalos$n_label = paste0("n = ", i, "\n(Cob: ", cob, "%)")
    
  datos_totales = rbind(datos_totales, intervalos)
}

# Ordeno para que los graficos aparezcan de menor a mayor n
datos_totales$n_label = reorder(datos_totales$n_label, datos_totales$n)

medias_resumen = datos_totales %>%
  group_by(n_label) %>%
  summarise(media_del_centro = mean(centro))

# Grafico en conjunto todos los datos
ggplot(datos_totales, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # Dibujamos los intervalos obtenidos en la simulación
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
    
  # La linea punteada negra es el valor real de theta
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "black") +
  
  # La linea solida azul es la media del punto medio de los intervalos para cada n
  geom_vline(data = medias_resumen, aes(xintercept = media_del_centro), 
             color = "blue", linetype = "solid", linewidth = 0.7, alpha = 0.6) +
  
  # Color para identificar si cubre o no al valor real de theta
  scale_color_manual(values = c("#C51B7D", "#222222"), labels = c("No cubre", "Cubre")) +
    
  # Se separan los graficos en función de n
  facet_wrap(~ n_label, scales = "free_x") +
  
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
# Vamos a crear el metodo de la simulación requerida para la tercera sección del trabajo.
#
# Los inputs del metodo son:
#  - theta: es el valor real para el cual vamos a hacer la simulacion
#  - n: tamaño de la muestra
#  - N: cantidad de intervalos que vamos a generar
# Los outputs son:
#  - intervalos: conjunto de los intervalos obtenidos, con estos mismos podemos graficar el cubrimiento

  
###########################################################################################################
#2.9
 

N=5000 #Cantidad de muestras bootstrap a generar por cada n
#Definimos los valores fijos dados en el ejercicio y distintos valores de n para realizar las simulaciones
theta=0.25
Se=0.9
Sp=0.95
#Definimos los valores de n
ns=c(5, 10, 25, 50, 100, 500) 

#Defimos p
p=Se*theta + (1-Sp)*(1-theta)

#Definimos el estimador de momentos de theta
estimador_de_momentos=function(est_p, Se, Sp){
  return((est_p+Sp-1)/(Se+Sp-1))
}
#Tomando como inputs:
# - n: tamaño de la muestr
# - Se, Sp, y p dados en el ejercicio
# - el nivel del intervalo
# - N la cantidad de muestras a genenar
#Creamos intervalos de confianza percentil bootstrap
ic_bootstrap_percentil_2_9=function(n, Se=0.9, Sp=0.95, p=p, nivel=0.05, N=5000){
  estimadores_de_momentos=numeric(N)
  muestra_2_9=rbinom(n, 1, p)
  for (i in 1:N) {
    #muestra_2_9=rbinom(n, 1, p)
    tboot=sample(muestra_2_9, n, replace = TRUE)
    est_p=mean(tboot)
    estimadores_de_momentos[i]=estimador_de_momentos(est_p, Se, Sp)
  }
  ic_bootstrap_perc_inf=quantile(estimadores_de_momentos, nivel/2)
  ic_bootstrap_perc_sup=quantile(estimadores_de_momentos,1-nivel/2)
  ic_bootstrap_perc=c(ic_bootstrap_perc_inf, ic_bootstrap_perc_sup)
  return(ic_bootstrap_perc)
}
#Creamos intervalos para cada n
for (n in ns){
  print(ic_bootstrap_percentil_2_9(n, Se, Sp, p, 0.05, N))
}
    
##########################################################################################################################################
#2.10
#Primero construimos los intervalos de confianza de nivel asintotico 0.95 para θ basados en ˆθMoM.
  ic_asintoticos_2_10=function(n, Se, Sp, p, nivel=0.05){
    muestra_2_10=rbinom(n, 1, p)
    est_p=mean(muestra_2_10)
    estimadores_de_momentos=estimador_de_momentos(est_p, Se, Sp)
    s=sqrt(est_p*(1-est_p))/(Se+Sp-1)
    z=qnorm(1 - nivel/2)
    ic_asintoticos=c(inf =estimadores_de_momentos-z*sqrt(est_p*(1-est_p))/((Se+Sp-1)*sqrt(n)) , sup=estimadores_de_momentos+z*sqrt(est_p*(1-est_p))/((Se+Sp-1)*sqrt(n)))
    return(ic_asintoticos)
  }
#Creamos intervalos para cada n 
for (n in ns){
  ic=ic_asintoticos_2_10(n,Se, Sp, p, 0.95)
}


########################################################################################################################################## 
#2.11
#Para comparar los intervalos definimos un data frame con información sobre ellos
simulacion_2_11=function(theta, n, Se, Sp, p, nivel=0.05, B=100){
  intervalos_ip=data.frame(
    simulacion = 1:B,
    inf = numeric(B),
    sup = numeric(B),
    cubre = logical(B),
    long=numeric(B),
    tipo="Bootstrap percentil"
  )
  intervalos_ic=data.frame(
    simulacion = 1:B,
    inf = numeric(B),
    sup = numeric(B),
    cubre = logical(B),
    long=numeric(B),
    tipo="Asintotico"
  )
  for (i in 1:B){
    ip = ic_bootstrap_percentil_2_9(n, Se, Sp,p,  nivel, N=5000)
    intervalos_ip$inf[i] = ip[1]
    intervalos_ip$sup[i] = ip[2]
    intervalos_ip$cubre[i] = theta >= ip[1] && theta <= ip[2]
    intervalos_ip$long[i] =ip[2]-ip[1]
    ic=ic_asintoticos_2_10(n, Se, Sp, p, nivel)
    intervalos_ic$inf[i] = ic[1]
    intervalos_ic$sup[i] = ic[2]
    intervalos_ic$cubre[i] = theta >= ic[1] && theta <= ic[2]
    intervalos_ic$long[i] =ic[2]-ic[1]
  }
  intervalos_ip_ic=rbind(intervalos_ip, intervalos_ic)
  return(intervalos_ip_ic)   
}
datos_2_11=data.frame()
for (n in ns){
  intervalos_n=simulacion_2_11(theta, n, Se, Sp, p, 0.05, 100)
  intervalos_n$n = n
  intervalos_n$n_label = paste0("n = ", n)
  
  datos_2_11 = rbind(datos_2_11, intervalos_n)
}
# Ordeno para que los graficos aparezcan de menor a mayor n
datos_2_11$n_label = reorder(datos_2_11$n_label, datos_2_11$n)
datos_2_11$centro = (datos_2_11$inf + datos_2_11$sup) / 2

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
#Comparamos las longitudes promedios y porcentajes de cobertura para cada n
comparaciones=datos_2_11 %>%
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
#Ahora hacemos una tabla comparativa
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
    axis.text.x = element_text(angle = 0, hjust = 0.5, size=8, face = "bold")
  )

##################################################################################################
#2.13
#Definimos el estimador truncado
estimador_truncado=function(estimador_momentos){
  if(estimador_momentos<0){
    return(0)
  }
  else{
    if(estimador_momentos>1){
      return(1)
    }
    else{
      return(estimador_momentos)
    }
  }
}

caracteristicas_truncado=function(theta=0.25, n, Se=0.9, Sp=0.95,p,N=5000){
  est_truncado= numeric(N)
  for (i in 1:N){
    muestra_2_13=rbinom(n, 1, p)
    est_p=mean(muestra_2_13)
    estimador_momentos=estimador_de_momentos(est_p, Se, Sp)
    estimador__mom_truncado=estimador_truncado(estimador_momentos)
    est_truncado[i]=estimador__mom_truncado
  }
  #Guardamos los datos que queremos analizar
  varianza=var(est_truncado)
  sesgo=mean(est_truncado)-theta
  ECM=varianza-(sesgo)**2 #Compromiso sesgo - Varianza
  dist_asint=sqrt(n)*(est_truncado-theta)
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
  value = c(est_truncado_10$varianza, est_truncado_100$varianza, est_truncado_1000$varianza,
            est_truncado_10$sesgo,    est_truncado_100$sesgo,    est_truncado_1000$sesgo,
            est_truncado_10$ECM,      est_truncado_100$ECM,      est_truncado_1000$ECM)
)


ggplot(Var_ECM_Sesgo_truncado, aes(x = n, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_log10(breaks = c(10, 100, 1000)) +
  labs(title = "Sesgo, Varianza y ECM del estimador truncado",
       x = "n ", y = "Valor", color = "Medida") +
  theme_minimal(base_size = 14)
#Visualizamos la distribucion asintotica con qqplot
dist_asint_truncado=data.frame(
  dist = c(est_truncado_10$dist_asint,
           est_truncado_100$dist_asint,
           est_truncado_1000$dist_asint),
  n = factor(rep(c(10,100,1000), each = 5000))
)
ggplot(dist_asint_truncado, aes(sample = dist)) +
  stat_qq() +
  stat_qq_line(color = "red", linewidth = 1) +
  facet_wrap(~ n, scales = "free") +
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

simulacion_3 = function(theta_pre, theta_post, n_pre, n_post, Se, Sp, alpha, N) {
  inf = numeric(N)
  sup = numeric(N)
  cubre = logical(N)
  centro = numeric(N)
  
  for (i in 1:N) {
    delta = theta_post - theta_pre
    
    p_pre = (Se+Sp-1)*theta_pre + (1-Sp)
    p_post = (Se+Sp-1)*theta_post + (1-Sp)
    
    muestras_pre = rbinom(n_pre, 1, p_pre)
    muestras_post = rbinom(n_post, 1, p_post)
    
    est_theta_pre = (mean(muestras_pre) + Sp - 1)/(Se + Sp - 1) 
    est_theta_post = (mean(muestras_post) + Sp - 1)/(Se + Sp - 1)
    
    z_a = qnorm(1 - alpha/2)
    
    est_sigma_pre = mean(muestras_pre) * (1-mean(muestras_pre)) / ((Se+Sp-1)**2)
    est_sigma_post = mean(muestras_post) * (1-mean(muestras_post)) / ((Se+Sp-1)**2)
    
    inf[i] = est_theta_post - est_theta_pre - z_a * sqrt(est_sigma_post/n_post + est_sigma_pre/n_pre)
    sup[i] = est_theta_post - est_theta_pre + z_a * sqrt(est_sigma_post/n_post + est_sigma_pre/n_pre)
    centro[i] = est_theta_post - est_theta_pre
    cubre[i] = delta >= inf[i] && delta <= sup[i]
  }
  
  return(data.frame(simulacion = 1:N, inf = inf, sup = sup, cubre = cubre, centro = centro))
}

datos_totales_3 = data.frame()

for (i in c(5,10,25,50,100,500,1000,5000,10000)) {
  intervalos = simulacion_3(0.2, 0.15, i, i, 0.9, 0.95, 0.05, 1000)
  
  cob = mean(intervalos$cubre) * 100
  intervalos$n = i
  intervalos$n_label = paste0("n_pre = n_post = ", i, "\n(Cob: ", cob, "%)")
  
  datos_totales_3 = rbind(datos_totales_3, intervalos)
}

# Ordeno para que los graficos aparezcan de menor a mayor n
datos_totales_3$n_label = reorder(datos_totales_3$n_label, datos_totales_3$n)

medias_resumen = datos_totales_3 %>%
  group_by(n_label) %>%
  summarise(media_del_centro = mean(centro))

# Grafico en conjunto todos los datos
ggplot(datos_totales_3, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # Dibujamos los intervalos obtenidos en la simulación
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
  
  # La linea punteada negra es el valor real de theta
  geom_vline(xintercept = -0.05, linetype = "dashed", color = "black") +
  
  # La linea solida azul es la media del punto medio de los intervalos para cada n
  geom_vline(data = medias_resumen, aes(xintercept = media_del_centro), 
             color = "blue", linetype = "solid", linewidth = 0.7, alpha = 0.6) +
  
  # Color para identificar si cubre o no al valor real de theta
  scale_color_manual(values = c("#C51B7D", "#222222"), labels = c("No cubre", "Cubre")) +
  
  # Se separan los graficos en función de n
  facet_wrap(~ n_label, scales = "free_x") +
  
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
