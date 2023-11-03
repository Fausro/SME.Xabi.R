##################
# Sexto apartado #
##################

#' Función para trazar la curva ROC.
#'
#' @param TPR (vector): Vector de tasas de verdaderos positivos.
#' @param FPR (vector): Vector de tasas de falsos positivos.
#' @export
#' @examples
#' plot_auc(auc$TPR,auc$FPR)
plot_auc<-function(TPR,FPR){
  plot(FPR,TPR,xlim=c(0,1),ylim=c(0,1),asp=1,type='l')
  abline(a=0,b=1,lty=2)
}

#' Función para la representación de correlación/información mutua.
#'
#' @param matriz (matrix): Matriz con las correlaciones e información mutua.
#' @details Esta función requiere de los paquetes, ggplot2 y reshape2
#' @export
#' @examples
#' plot_cor(cor_im(df))
plot_cor<-function(matriz){
  ggplot2::ggplot(data = reshape2::melt(matriz), ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2() +
    ggplot2::labs(title = "Matriz de Correlación/Información mutua",
                  x = "Variables",
                  y = "Variables") +
    ggplot2::theme_minimal()
}