
# Lee el archivo .hea

lineas <- readLines("ECGPCG0003.hea")
# Lee el archivo .dat (asumiendo formato binario de 16 bits)
archivo_dat <- "ECGPCG0003.dat"
con <- file(archivo_dat, "rb")
señales <- readBin(con, integer(), n = 960000 / 2, size = 2, endian = "little")
#close(con)
head(señales)
# Convierte las señales en una matriz (2 columnas: ECG y PCG)
señales <- matrix(señales, ncol = 2, byrow = TRUE)
colnames(señales) <- c("ECG", "PCG")
señales_def<-as.data.frame(señales)
# Muestra las primeras filas
head(señales_def)

# Graficar la señal de ECG
plot(señales[, "ECG"], type = "l", col = "blue", main = "Señal ECG", xlab = "Muestras", ylab = "Amplitud")

# Graficar la señal de PCG
plot(señales[, "PCG"], type = "l", col = "red", main = "Señal PCG", xlab = "Muestras", ylab = "Amplitud")

##-----------------------------------
#Detectamos los picos
library(pracma)


# Detectar picos R en la señal de ECG
picos_R <- findpeaks(señales_def$ECG, minpeakheight = max(señales_def$ECG) * 0.5)  # Ajusta el umbral según sea necesario

# Mostrar las posiciones de los picos R
print(picos_R)


# Extraer las posiciones de los picos R
posiciones_R <- picos_R[, 2]  # La segunda columna contiene las posiciones de los picos

# Calcular los intervalos RR (en muestras)
intervalos_RR_muestras <- diff(posiciones_R)

# Mostrar los intervalos RR
print(intervalos_RR_muestras)

## Convertimos los intervalos RR a tiempo

# Frecuencia de muestreo (en Hz)
frecuencia_muestreo <- 1000  # Ajusta según el valor extraído del archivo .hea

# Convertir los intervalos RR a segundos
intervalos_RR_segundos <- intervalos_RR_muestras / frecuencia_muestreo

# Mostrar los intervalos RR en segundos
print(intervalos_RR_segundos)



# Graficar los intervalos RR
plot(intervalos_RR_segundos, type = "o", col = "blue", main = "Intervalos RR", xlab = "Número de intervalo", ylab = "Intervalo RR (segundos)")
