####################
# Segundo apartado #
####################

#' Realiza el cálculo de la entropía.
#'
#' @param x (vector): Vector de variables discretas.
#' @return (numeric): Valor de la entropía.
calc_entropia<-function(x){
  # Calcular frecuencias absolutas
  frecuencias <- table(x)
  
  # Calcular probabilidades
  probabilidades <- prop.table(frecuencias)
  probabilidades <- probabilidades[probabilidades>0] # Evitar problemas de NA
  
  # Calcular entropía
  return(-sum(probabilidades * log(probabilidades)))
}

#' Realiza el cálculo del AUC, TPR y FPR.
#'
#' @param dx (vector): Vector de variables a analizar.
#' @param dy (vector): Vector de etiquetas.
#' @param solo_auc (bool): Booleano para devolver solo el AUC o una lista, útil para mejorar la eficiencia de calcular_metricas.
#' @return (list): $AUC, área bajo la curva ROC; $TPR, sensibilidad o true positive rate; $FPR, false positive rate.
#' @export
calc_auc<-function(dx,dy,solo_auc=F){
  # Valores de corte, que dan lugar a cambios en el TPR y FPR
  ptos_corte<-sort(unique(dx))
  
  # Función para generar las predicciones según un punto de corte
  calc_predict<-function(p_corte,x1){x1>=p_corte}
  calc_tp<-function(predict,y){sum(predict & y)}   # Función para el TP
  calc_fp<-function(predict,y){sum(predict & !y)}  # Función para el FP
  calc_tn<-function(predict,y){sum(!predict & !y)} # Función para el TN
  calc_fn<-function(predict,y){sum(!predict & y)}  # Función para el TN
  
  
  predicts<-lapply(ptos_corte,calc_predict,x1=dx) # Todas las predicciones posibles
  tp<-as.numeric(lapply(predicts,calc_tp,y=dy)) 
  fp<-as.numeric(lapply(predicts,calc_fp,y=dy))
  tn<-as.numeric(lapply(predicts,calc_tn,y=dy))
  fn<-as.numeric(lapply(predicts,calc_fn,y=dy))
  
  # Cálculo del TPR y FPR
  tpr<-tp/(tp+fn)
  fpr<-fp/(fp+tn)
  tpr<-append(1,tpr) 
  fpr<-append(1,fpr)
  tpr<-append(tpr,0)
  fpr<-append(fpr,0)
  
  # Cálculo del área bajo la curva ROC
  calc_area<-function(x){min(x[1],x[2])*abs(x[3]-x[4])+abs(x[1]-x[2])*abs(x[3]-x[4])/2} 
  n<-length(tpr)
  auc<-sum(apply(cbind(tpr[1:(n-1)],tpr[2:n],fpr[1:(n-1)],fpr[2:n]),1,calc_area))
  if (solo_auc){
    return(auc)
  }else{
    return(list(AUC=auc,TPR=tpr,FPR=fpr))
  }
}

#' Realiza el cálculo de la varianza.
#'
#' @param x (vector): Vector de variables continuas.
#' @return (numeric): Valor de la varianza.
calc_var<-function(x){
  return((1/(length(x)-1))*sum((x-mean(x))^2))
}

#' Realiza el cálculo de métricas para los atributos de un dataset.
#'  + Varianza y AUC para las variables contínuas y entropía para las discretas
#'
#' @param datos (data.frame): Dataset con diferentes variables.
#' @param clase (int | str): Columna de la variable de la clase binaria, default=NaN.
#' @return (matrix): Matriz con la varianza, AUC, y Entropia correspondiente a cada columna de datos.
#' @export
#' @examples
#' calcular_metricas(df,"Etiqueta")
calcular_metricas <- function(datos, clase=NaN) {
  # Preparación de la matriz resultante
  columnas <- names(datos)
  metricas <- matrix(NA, nrow=3, ncol=ncol(datos))
  rownames(metricas) <- c("Varianza", "AUC", "Entropia")
  colnames(metricas) <- columnas
  
  # Seleccionar las columnas numéricas
  columnas_numericas <- columnas[sapply(datos, is.numeric)]
  if (length(columnas_numericas)==1){ # Solo hay una variable numérica
    metricas["Varianza",columnas_numericas]<-calc_var(datos[,columnas_numericas])
  } else if (length(columnas_numericas)>1){
    metricas["Varianza",columnas_numericas]<-apply(datos[,columnas_numericas],2,calc_var)
  }
  if (!is.na(clase)){ # Si se ha especificado una variable binaria
    if (length(columnas_numericas)==1){
      metricas["AUC",columnas_numericas]<-calc_auc(datos[,columnas_numericas],dy=datos[clase],solo_auc=T)
    } else if (length(columnas_numericas)>1){
      metricas["AUC",columnas_numericas]<-apply(datos[,columnas_numericas],2,calc_auc,dy=datos[clase],solo_auc=T)
    }
  }
  # Seleccionar las columnas discretas
  columnas_discretas <- columnas[sapply(datos, is.factor)]
  if (length(columnas_discretas)==1){ # Solo hay una variable discreta
    metricas["Entropia",columnas_discretas]<-calc_entropia(datos[,columnas_discretas])
  } else if (length(columnas_discretas)>1){
    metricas["Entropia",columnas_discretas]<-apply(datos[,columnas_discretas],2,calc_entropia)
  }
  return(metricas)
}