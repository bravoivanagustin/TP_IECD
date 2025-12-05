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