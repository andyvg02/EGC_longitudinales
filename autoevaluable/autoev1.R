library(dplyr)
library(gcmrec)
data(MMC, package="survrec")
str(MMC)
head(MMC)
# Ver número de eventos y mediana del tiempo por grupo (sexo)

aggregate(time ~ group, data = MMC, FUN = function(x) c(mediana = median(x), media = mean(x), n = length(x)))


library(dplyr)

# Calcular time1 y time2 como tiempos acumulados
MMC_preparado <- MMC %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(
    time1 = lag(cumsum(time), default = 0),
    time2 = cumsum(time)
  ) %>%
  ungroup()

# Convertir 'group' en variable binaria o factor (si es necesario)
MMC_preparado$group <- factor(MMC_preparado$group)


# Estimar modelo con fragilidad gamma
modelo_fragilidad <- survfitr(Survr(time1, time2, event) ~ group, id = id, data = MMC_preparado, type="MLEfrailty")

# Resumen del modelo
summary(modelo_fragilidad)

# Ver valor de alpha (fragilidad)
alpha <- modelo_fragilidad$theta
print(paste("Valor estimado de alpha:", round(alpha, 3)))