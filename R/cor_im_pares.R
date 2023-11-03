###################
# Quinto apartado #
###################

#' Cálculo de la correlación/información mutua por pares entre variables de un dataset.
#'
#' @param datos (data.frame): Dataset con diferentes variables.
#' @return (data.frame): Dataset con las correlaciones/información mutua.
#' @export
#' @examples
#' cor_im(df)
cor_im<-function(datos){
  n <- nrow(datos) 
  res <- diag(ncol(datos))
  columnas<-names(datos)
  rownames(res) <- columnas
  colnames(res) <- columnas
  # Cálculo de las correlaciones para las variables númericas
  columnas_numericas<-columnas[sapply(datos, is.numeric)]
  if (length(columnas_numericas)>1){
    for (i in 1:(length(columnas_numericas)-1)){
      col_i<-columnas_numericas[i]
      X <- datos[, col_i]
      for (j in (i+1):length(columnas_numericas)){
        col_j<-columnas_numericas[j]
        Y <- datos[, col_j]
        r <- sum((X - mean(X)) * (Y - mean(Y))) / sqrt(sum((X - mean(X))^2) * sum((Y - mean(Y))^2))
        res[col_i, col_j] <- r
        res[col_j, col_i] <- r
      }
    }
  }
  # Cálculo de la información mutua para las variables categóricas
  columnas_categoricas<-columnas[sapply(datos, is.factor)]
  if (length(columnas_categoricas)>=1){
    for (i in 1:length(columnas_categoricas)){
      col_i<-columnas_categoricas[i]
      # Calcular la probabilidad de p_i
      p_i <- table(datos[, col_i]) / n
      for (j in i:length(columnas_categoricas)){
        col_j<-columnas_categoricas[j]
        if (i==j){
          res[col_i, col_j]<-0
        }
        # Calcular la probabilidad de p_ij
        p_ij <- table(datos[, col_i], datos[, col_j]) / n
        # Calcular la probabilidad de p_j
        p_j <- table(datos[, col_j]) / n
        # Calcular la información mutua entre las variables i y j
        for (x in names(p_i)) {
          for (y in names(p_j)) {
            if (p_ij[x, y] > 0) {
              res[col_i, col_j] <- res[col_i, col_j] + p_ij[x, y] * log(p_ij[x, y] / (p_i[x] * p_j[y]))
            }
          }
        }
        res[col_j, col_i]<-res[col_i, col_j]
      }
    }
  }
  
  return(res)
}