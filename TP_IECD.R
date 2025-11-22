# --------------------- Trabajo practico IECD, simulaciones --------------------
# Sección de comentarios:
#  - LU mas chico entre los tres integrantes: 786
#  - La idea es modularizar lo mas posible el codigo
#  - Vamos a intentar generalizar los procedimientos, para que sea simple cambiar los parametros que estan involucrados


# ------------------------------- Sección 1 ------------------------------------
# Vamos a crear el metodo de la simulación requerida para la primera sección del trabajo.
# 
# Primero construimos una función auxiliar para calcular el intervalo de confianza.
#
# Los inputs del metodo son:
#  - theta: es el valor real para el cual vamos a hacer la simulacion
#  - n: tamaño de la muestra
#  - N: cantidad de intervalos que vamos a generar
# Los outputs son:
#  - intervalos: conjunto de los intervalos obtenidos, con estos mismos podemos graficar el cubrimiento
library(ggplot2)
library(dplyr)

set.seed(786)

intervalo = function(datos, alpha) {
  n = length(datos)
  t_p = mean(datos)
  z_a = -qnorm(alpha/2)
  return(c(inf = t_p - sqrt(t_p*(1-t_p)/n)*z_a, sup = t_p + sqrt(t_p*(1-t_p)/n)*z_a))
}

simulacion_1 = function(theta, n, alpha, N) {
  inf = numeric(N)
  sup = numeric(N)
  cubre = logical(N)
  
  for (i in 1:N) {
    muestras = rbinom(n, 1, theta)
    int = intervalo(muestras, alpha)
    inf[i] = int["inf"]
    sup[i] = int["sup"]
    cubre[i] = theta >= int["inf"] && theta <= int["sup"]
  }
  
  return(data.frame(simulacion = 1:N, inf = inf, sup = sup, cubre = cubre))
}

datos_totales <- data.frame()

for (i in c(5,10,25,50,100,500)) {
  intervalos = simulacion_1(0.25, i, 0.05, 500)
  
  cob = mean(intervalos$cubre) * 100
  intervalos$n = i
  intervalos$n_label = paste0("n = ", i, "\n(Cob: ", cob, "%)")
    
  datos_totales = rbind(datos_totales, intervalos)
}

# Ordeno para que los graficos aparezcan de menor a mayor n
datos_totales$n_label <- reorder(datos_totales$n_label, datos_totales$n)

# Grafico en conjunto todos los datos
ggplot(datos_totales, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # Dibujamos los intervalos obtenidos en la simulación
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
    
  # La linea punteada es el valor real de theta
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "black") +
  
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

