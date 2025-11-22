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
set.seed(786)

intervalo = function(datos, n, alpha) {
  t_p = mean(datos)
  z_a = -qnorm(alpha)
  return(c(inf = t_p - sqrt(t_p*(1-t_p)/n)*z_a, sup = t_p + sqrt(t_p*(1-t_p)/n)*z_a))
}

simulacion_1 = function(theta, n, alpha, N) {
  intervalos = data.frame(
    simulacion = 1:N, 
    inf = numeric(N),
    sup = numeric(N),
    cubre = logical(N)
  )
  
  for (i in 1:N) {
    muestras = rbinom(n, 1, theta)
    int = intervalo(muestras, n, alpha)
    intervalos$inf[i] = int[1]
    intervalos$sup[i] = int[2]
    intervalos$cubre[i] = theta >= int[1] && theta <= int[2]
  }
  
  return(intervalos)
}

intervalos_10 = simulacion_1(0.25, 10, 0.05, 1000)

ggplot(intervalos_10, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  
  # Dibujamos los intervalos obtenidos en la simulación
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
  
  # La linea punteada es el valor real de theta
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "black") +
  
  # Color para identificar si cubre o no
  scale_color_manual(values = c("#C51B7D", "#222222"), labels = c("No cubre", "Cubre")) +
  
  # Labels del grafico
  labs(
    title = "Intervalos de confianza asintoticos para theta = 0.25",
    x = "Valor del parámetro",
    y = "Simulación n°",
    color = "¿Cubre a 0.25?"
  ) +
  
  theme_bw()

sum(intervalos_10$cubre)/1000
