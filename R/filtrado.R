###################
# Cuarto apartado #
###################

#' Filtrado de variables en base a las métricas implementadas.
#'
#' @param datos (data.frame): Dataset de diferentes variables.
#' @param clase (int | str): Columna de la variable de la clase binaria, default=None.
#' @param ... (int): Diferentes valores de umbrales para filtrar las variables.
#' @return (data.frame): Dataset filtrado.
#' @export
#' @examples
#' filtrado(df,'Etiquetas',min_Var = 15, max_Var = 90)
filtrado<-function(datos,clase=NA,min_Var=-Inf,max_Var=Inf,min_AUC=0,max_AUC=1,min_Entropia=-Inf,max_Entropia=Inf){
  metricas<-calcular_metricas(datos,clase)
  metricas['Varianza',sapply(metricas['Varianza',], is.na)]<-min_Var
  metricas['AUC',sapply(metricas['AUC',], is.na)]<-min_AUC
  metricas['Entropia',sapply(metricas['Entropia',], is.na)]<-min_Entropia
  filtro<-metricas['Varianza',]>=min_Var & metricas['Varianza',]<=max_Var
  filtro<-filtro & metricas['AUC',]>=min_AUC & metricas['AUC',]<=max_AUC
  filtro<-filtro & metricas['Entropia',]>=min_Entropia & metricas['Entropia',]<=max_Entropia
  return (datos[,filtro])
}