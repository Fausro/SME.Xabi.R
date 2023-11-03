###################
# Tercer apartado #
###################

#' Normaliza el vector x, con valores entre 0 y 1.
#'
#' @param x (data.frame): Vector númerico.
#' @return (vector): Vector normalizado.
normalizacion<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

#' Estandariza el vector x, con media 0 y desviación 1.
#'
#' @param x (data.frame): Vector númerico.
#' @return (vector): Vector estandarizado.
estandarizacion<-function(x){
  return ((x-mean(x))/sd(x))
}

#' Normalización y estandarización de variables tanto para un dataset como de manera individual.
#'
#' @param x (data.frame | vector): DataFrame o vector númerico para escalar.
#' @param norm (logical): Booleano que determina el tipo de transformación a realizar, default=False (normaliza). 
#' @return (data.frame | vector): Entrada transformada. 
#' @export
#' @examples
#' scale_x(c(-0.29, 2.11, -3.84, 10.69, 5.30),norm=T)
scale_x<-function(x,norm=F){
  if (is.data.frame(x)){
    # Seleccionar las columnas numéricas
    columnas_numericas <- names(x)[sapply(x, is.numeric)]
    transformacion<-ifelse(norm,normalizacion,estandarizacion)
    if (length(columnas_numericas)==1){
      x[,columnas_numericas]<-transformacion(x[,columnas_numericas])
    } else if (length(columnas_numericas)>1){
      x[,columnas_numericas] <- apply(x[,columnas_numericas],2,transformacion)
    }
    return (x)
  }else if (is.numeric(x)){
    if (norm){
      return (normalizacion(x))
    }else{
      return (estandarizacion(x))
    }
  }
}