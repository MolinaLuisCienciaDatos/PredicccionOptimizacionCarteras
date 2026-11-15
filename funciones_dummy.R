# Plantilla mínima: media y covarianza históricas, con pesos iguales.

#' Predice con la media y la covarianza históricas del entrenamiento.
#' @param X_train Tabla o matriz con las cinco columnas nombradas X1 a X5.
#' @param X_test_past Observaciones anteriores del bloque de prueba; esta plantilla no las usa.
#' @return Lista con mu_hat (cinco medias) y Sigma_hat (matriz 5 × 5).
getPred <- function(X_train, X_test_past = NULL) {
  activos <- paste0("X", 1:5)
  train <- as.matrix(X_train[, activos, drop = FALSE])
  list(mu_hat = colMeans(train), Sigma_hat = stats::cov(train))
}


#' Plantilla del optimizador de MV. Sustituid los pesos iguales por vuestra solución. El objetivo del inciso usa gamma = 30.
#' @param mu Vector de cinco medias previstas.
#' @param Sigma Matriz de covarianzas prevista de 5 × 5.
#' @return Vector de cinco pesos iguales, en el orden de mu.
getAlphaMV <- function(mu, Sigma) {
  rep(1 / length(mu), length(mu))
}


#' Plantilla del optimizador de Log. Sustituid los pesos iguales por vuestra solución. El objetivo del inciso usa gamma = 30.
#' @param mu Vector de cinco medias previstas.
#' @param Sigma Matriz de covarianzas prevista de 5 × 5.
#' @return Vector de cinco pesos iguales, en el orden de mu.
getAlphaLog <- function(mu, Sigma) {
  rep(1 / length(mu), length(mu))
}

# Inciso 3: ejemplo de utilidad elegida por el equipo.

#' Ejemplo de objetivo para el inciso 3: media-varianza con gamma = 10,
#' fijado dentro de la función.
#' @param w Vector de cinco pesos.
#' @param mu Vector de cinco medias previstas.
#' @param Sigma Matriz de covarianzas prevista de 5 × 5.
#' @return Valor escalar de utilidad, mayor para una cartera preferida según el criterio.
utilidadU <- function(w, mu, Sigma) {
  gamma <- 10
  sum(w * mu) - gamma * drop(crossprod(w, Sigma %*% w)) / 2
}


#' Plantilla del optimizador de rentabilidad. Sustituid los pesos iguales por vuestra solución. Debe usar utilidadU.
#' @param mu Vector de cinco medias previstas.
#' @param Sigma Matriz de covarianzas prevista de 5 × 5.
#' @return Vector de cinco pesos iguales, en el orden de mu.
getAlphaU <- function(mu, Sigma) {
  # Plantilla: el equipo debe reemplazar los pesos iguales por su optimizador.
  rep(1 / length(mu), length(mu))
}
