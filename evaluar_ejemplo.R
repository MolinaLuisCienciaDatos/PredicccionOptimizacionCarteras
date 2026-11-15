# Ejecutar desde la carpeta entrega_estudiantes.
source("eval_funcs.R")
source("funciones_dummy.R")

datos <- read.csv("datos/stock_returns_train.csv")
activos <- paste0("X", 1:5)
ajuste <- datos[1:258, activos, drop = FALSE]
validacion <- datos[259:378, activos, drop = FALSE]

pred <- getPred_ts(ajuste, validacion, getPred)
optimizadores <- list(MV = getAlphaMV, LOG = getAlphaLog, RENT = getAlphaU)
for (inciso in names(optimizadores)) {
  pesos <- getAlpha_ts(pred$mu_hat, pred$Sigma_hat, optimizadores[[inciso]])
  cat("\nInciso:", inciso, "\n")
  print(resumenEstrategia(rowSums(pesos * as.matrix(validacion))))
}
print(getPredMetrics(ajuste, validacion, pred))
