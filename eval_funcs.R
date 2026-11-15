# Evaluacion comun 2026-2027. Sin dependencias de la solucion benchmark.
# Contrato: predictor(train, test_past) -> list(mu_hat[5], Sigma_hat[5,5]).
# El predictor puede ser un cierre que conserve modelos ajustados en train.
# Optimizadores: funcion(mu, Sigma) -> pesos[5]. Gamma=30 para MV y Log penalizada.

#' Normaliza los datos de rendimientos.
#' @param x Tabla con columnas X1 a X5, o matriz numérica de cinco columnas.
#' @param vacio Permite una matriz con cero filas si es TRUE.
#' @return Matriz numérica finita, con los activos en orden X1 a X5 si se seleccionaron por nombre.
.eval_matrix <- function(x, vacio=FALSE) {
  if(is.data.frame(x) && all(paste0('X',1:5)%in%names(x))) x<-x[paste0('X',1:5)]
  x<-as.matrix(x)
  if(!is.numeric(x)||ncol(x)!=5L||(!vacio&&nrow(x)==0L)||any(!is.finite(x)))
    stop('Se requiere una matriz numerica finita con cinco activos.')
  x
}

#' Valida los momentos de una predicción.
#' @param mu Vector de cinco medias finitas.
#' @param Sigma Matriz numérica finita, simétrica y semidefinida positiva de 5 × 5.
#' @return TRUE invisiblemente; produce un error si los momentos no son válidos.
.eval_moments <- function(mu,Sigma) {
  if(!is.numeric(mu)||length(mu)!=5L||any(!is.finite(mu))) stop('Media invalida.')
  if(!is.matrix(Sigma)||!is.numeric(Sigma)||!identical(dim(Sigma),c(5L,5L))||
     any(!is.finite(Sigma))) stop('Covarianza invalida.')
  tol<-1e-8*max(abs(Sigma),.Machine$double.eps)
  if(max(abs(Sigma-t(Sigma)))>tol ||
     min(eigen((Sigma+t(Sigma))/2,symmetric=TRUE,only.values=TRUE)$values)< -tol)
    stop('La covarianza debe ser simetrica y semidefinida positiva.')
  invisible(TRUE)
}

#' Comprueba las restricciones de cartera.
#' @param alpha_hat Vector de cinco pesos o matriz con una cartera por fila.
#' @param tol Tolerancia para los límites y la suma de pesos.
#' @return TRUE si todas las filas son factibles; FALSE en caso contrario.
getChecks <- function(alpha_hat,tol=1e-6) {
  if(is.null(dim(alpha_hat))) alpha_hat<-matrix(alpha_hat,nrow=1)
  is.matrix(alpha_hat)&&is.numeric(alpha_hat)&&ncol(alpha_hat)==5L&&
    nrow(alpha_hat)>0L&&all(is.finite(alpha_hat))&&
    all(abs(rowSums(alpha_hat)-1)<=tol)&&all(alpha_hat>=-tol)&&all(alpha_hat<=1+tol)
}

#' Genera predicciones de un paso para cada fila del bloque de evaluación.
#' @param Xtrain Rendimientos de entrenamiento.
#' @param Xtest Rendimientos del bloque de evaluación, en orden cronológico.
#' @param getPredFunc Función que recibe entrenamiento y solo las filas anteriores a la fecha prevista.
#' @return Lista con mu_hat (H × 5) y Sigma_hat (5 × 5 × H), donde H es el número de fechas.
getPred_ts <- function(Xtrain,Xtest,getPredFunc) {
  Xtrain<-.eval_matrix(Xtrain);Xtest<-.eval_matrix(Xtest)
  H<-nrow(Xtest)
  mu<-matrix(NA_real_,H,5,dimnames=list(NULL,colnames(Xtrain)))
  Sigma<-array(NA_real_,c(5,5,H))
  for(h in seq_len(H)) {
    pr<-getPredFunc(Xtrain,Xtest[seq_len(h-1L),,drop=FALSE])
    .eval_moments(pr$mu_hat,pr$Sigma_hat)
    mu[h,]<-pr$mu_hat;Sigma[,,h]<-pr$Sigma_hat
  }
  list(mu_hat=mu,Sigma_hat=Sigma)
}

#' Ejecuta un optimizador y valida los pesos devueltos.
#' @param mu Vector de cinco medias.
#' @param Sigma Matriz de covarianzas de 5 × 5.
#' @param getAlphaFunc Función que recibe mu y Sigma y calcula los pesos.
#' @return Vector numérico de cinco pesos factibles.
getAlpha <- function(mu,Sigma,getAlphaFunc) {
  .eval_moments(mu,Sigma)
  alpha<-getAlphaFunc(mu,Sigma)
  if(!is.numeric(alpha)||!is.null(dim(alpha))||!getChecks(alpha)) stop('Pesos no factibles.')
  alpha
}

#' Calcula una cartera por cada fecha.
#' @param mus Matriz de medias H × 5.
#' @param Sigmas Array de covarianzas 5 × 5 × H.
#' @param getAlphaFunc Función que convierte los momentos de una fecha en pesos.
#' @return Matriz H × 5 de pesos, en el orden de las predicciones.
getAlpha_ts <- function(mus,Sigmas,getAlphaFunc) {
  mus<-.eval_matrix(mus)
  if(!identical(dim(Sigmas),c(5L,5L,nrow(mus)))) stop('Dimensiones de covarianza invalidas.')
  t(vapply(seq_len(nrow(mus)),function(h)getAlpha(mus[h,],Sigmas[,,h],getAlphaFunc),numeric(5)))
}

#' Reparte el capital por igual entre los cinco activos.
#' @param mu Vector de medias; no se usa.
#' @param Sigma Matriz de covarianzas; no se usa.
#' @return Vector con cinco pesos de 0.2.
getAlphaEqual <- function(mu,Sigma) rep(.2,5)

#' Calcula el rendimiento acumulado realizado.
#' @param alpha_hat Matriz H × 5 de pesos decididos antes de observar cada fila.
#' @param Xtest Matriz H × 5 de rendimientos observados en fracción.
#' @param passChecks Indicador de validación previa; se vuelven a comprobar los pesos.
#' @return Rendimiento en fracción: producto de (1 + rendimiento diario) menos uno.
getRet <- function(alpha_hat,Xtest,passChecks=getChecks(alpha_hat)) {
  Xtest<-.eval_matrix(Xtest)
  if(!isTRUE(passChecks)||!getChecks(alpha_hat)) stop('Pesos no factibles.')
  if(!identical(dim(alpha_hat),dim(Xtest))) stop('Dimensiones incompatibles.')
  r<-rowSums(alpha_hat*Xtest)
  if(any(r<=-1)) stop('Rendimiento incompatible con riqueza positiva y utilidad log.')
  prod(1+r)-1
}

#' Calcula la utilidad media-varianza con gamma = 30.
#' @param alpha Vector de cinco pesos.
#' @param mu_hat Vector de cinco medias.
#' @param Sigma_hat Matriz de covarianzas de 5 × 5.
#' @return Media de cartera menos 30 veces su varianza dividida por dos.
Umv <- function(alpha,mu_hat,Sigma_hat) {
  sum(alpha*mu_hat)-30*drop(crossprod(alpha,Sigma_hat%*%alpha))/2
}

#' Calcula la utilidad logarítmica penalizada con gamma = 30.
#' @param alpha Vector de cinco pesos.
#' @param mu_hat Vector de cinco medias.
#' @param Sigma_hat Matriz de covarianzas de 5 × 5.
#' @return Utilidad Log; -Inf si uno más la media de cartera no es positivo.
Ulog <- function(alpha,mu_hat,Sigma_hat) {
  a<-1+sum(alpha*mu_hat)
  if(a<=0) return(-Inf)
  log(a)-30*drop(crossprod(alpha,Sigma_hat%*%alpha))/(2*a^2)
}
# Para comparar optimizadores, mus y Sigmas deben ser casos COMUNES a todos.
# La ganancia es absoluta: no divide por una utilidad de referencia cercana a cero.

#' Puntúa cada cartera con momentos de evaluación comunes y la compara con pesos iguales.
#' @param alpha_hat Matriz H × 5 de pesos.
#' @param mus Matriz H × 5 de medias de evaluación.
#' @param Sigmas Array 5 × 5 × H de covarianzas de evaluación.
#' @param Ufunc Función escalar de pesos, media y covarianza, por ejemplo Umv o Ulog.
#' @return Tabla de utilidad, utilidad con pesos iguales y diferencia absoluta, por fecha.
getUEval <- function(alpha_hat,mus,Sigmas,Ufunc) {
  mus<-.eval_matrix(mus)
  if(!getChecks(alpha_hat)||!identical(dim(alpha_hat),dim(mus))||
     !identical(dim(Sigmas),c(5L,5L,nrow(mus)))) stop('Entradas incompatibles.')
  valores<-vapply(seq_len(nrow(mus)),function(h) {
    .eval_moments(mus[h,],Sigmas[,,h])
    Ufunc(alpha_hat[h,],mus[h,],Sigmas[,,h])
  },numeric(1))
  referencia<-vapply(seq_len(nrow(mus)),function(h)
    Ufunc(rep(.2,5),mus[h,],Sigmas[,,h]),numeric(1))
  data.frame(utilidad=valores,utilidad_pesos_iguales=referencia,ganancia=valores-referencia)
}

#' Resume la precisión de las predicciones por activo.
#' @param Xtrain Entrenamiento para calcular la media histórica de referencia.
#' @param Xtest Rendimientos observados del bloque de evaluación.
#' @param pred Lista devuelta por getPred_ts, con varianzas marginales positivas.
#' @return Tabla por activo con RMSE, MAE, sesgo observado menos previsto, RMSE de referencia, cobertura normal al 95 % y log-verosimilitud negativa normal media.
getPredMetrics <- function(Xtrain,Xtest,pred) {
  Xtrain<-.eval_matrix(Xtrain);Xtest<-.eval_matrix(Xtest)
  if(!identical(dim(pred$mu_hat),dim(Xtest))||
     !identical(dim(pred$Sigma_hat),c(5L,5L,nrow(Xtest)))) stop('Predicciones incompatibles.')
  for(h in seq_len(nrow(Xtest))) .eval_moments(pred$mu_hat[h,],pred$Sigma_hat[,,h])
  v<-t(vapply(seq_len(nrow(Xtest)),function(h)diag(pred$Sigma_hat[,,h]),numeric(5)))
  if(any(v<=0)) stop('Las metricas normales requieren varianzas positivas.')
  e<-Xtest-pred$mu_hat
  ref<-matrix(colMeans(Xtrain),nrow(Xtest),5,byrow=TRUE)
  data.frame(activo=if(is.null(colnames(Xtrain)))paste0('X',1:5) else colnames(Xtrain),
    RMSE=sqrt(colMeans(e^2)),MAE=colMeans(abs(e)),sesgo=colMeans(e),
    RMSE_media_train=sqrt(colMeans((Xtest-ref)^2)),
    cobertura_95=colMeans(abs(e)<=qnorm(.975)*sqrt(v)),
    NLL_normal=colMeans(.5*(log(2*pi*v)+e^2/v)))
}

#' Resume una trayectoria de rendimientos realizados.
#' @param r Vector cronológico de rendimientos diarios de cartera en fracción, mayores que -1.
#' @return Vector con rendimiento acumulado, media, desviación típica, crecimiento logarítmico y caída máxima desde un máximo previo.
resumenEstrategia <- function(r) {
  if(!is.numeric(r)||!length(r)||any(!is.finite(r))||any(r<=-1)) stop('Retornos invalidos.')
  riqueza<-c(1,cumprod(1+r))
  c(rendimiento_acumulado=tail(riqueza,1)-1,media=mean(r),desviacion=sd(r),
    log_crecimiento=sum(log1p(r)),max_drawdown=max(1-riqueza/cummax(riqueza)))
}

#' Evalúa las carteras MV y Log y una referencia de pesos iguales. Para el inciso 3, usad getAlpha_ts con getAlphaU, como en evaluar_ejemplo.R.
#' @param Xtrain Rendimientos del bloque de ajuste.
#' @param Xtest Rendimientos del bloque de evaluación.
#' @param getPredFunc Predictor compartido.
#' @param getAlphaMVFunc Optimizador del inciso MV.
#' @param getAlphaLogFunc Optimizador del inciso Log.
#' @return Lista con predicciones, métricas por activo, pesos, rendimientos diarios y resumen por estrategia.
evaluarSolucion <- function(Xtrain,Xtest,getPredFunc,getAlphaMVFunc,getAlphaLogFunc) {
  Xtrain<-.eval_matrix(Xtrain);Xtest<-.eval_matrix(Xtest)
  pred<-getPred_ts(Xtrain,Xtest,getPredFunc)
  pesos<-list(MV=getAlpha_ts(pred$mu_hat,pred$Sigma_hat,getAlphaMVFunc),
              LOG=getAlpha_ts(pred$mu_hat,pred$Sigma_hat,getAlphaLogFunc),
              IGUALES=getAlpha_ts(pred$mu_hat,pred$Sigma_hat,getAlphaEqual))
  retornos<-vapply(pesos,function(w) {
    getRet(w,Xtest) # valida pesos, dimensiones y dominio
    rowSums(w*Xtest)
  },numeric(nrow(Xtest)))
  resumen<-data.frame(estrategia=names(pesos),
    t(vapply(pesos,function(w)resumenEstrategia(rowSums(w*Xtest)),numeric(5))),row.names=NULL)
  list(predicciones=pred,metricas=getPredMetrics(Xtrain,Xtest,pred),
       pesos=pesos,retornos=retornos,resumen_estrategias=resumen)
}
