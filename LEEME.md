# Material del proyecto

- `enunciado.pdf`: objetivo, reglas y entregas.
- `datos/stock_returns_train.csv`: 378 observaciones de entrenamiento para los cinco activos.
- `funciones_dummy.R`: plantilla con una predicción compartida, los tres optimizadores y la utilidad del tercer inciso con sus parámetros definidos dentro de la función. Los pesos iguales son un ejemplo de formato.
- `eval_funcs.R`: funciones para evaluar predicciones y carteras en un bloque de validación. Incluye los criterios MV y Log penalizada con gamma = 30.
- `evaluar_ejemplo.R`: ejemplo que ajusta con las primeras 258 observaciones y valida con las 120 siguientes.

Desde esta carpeta, ejecutad `Rscript evaluar_ejemplo.R` o `source("evaluar_ejemplo.R")` en R. El ejemplo calcula métricas de predicción y rentabilidad realizada. La comparación con los benchmarks se proporcionará en el informe de cada entrega.
