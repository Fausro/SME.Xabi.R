###################
# Primer apartado #
###################

#' Realiza la discretización tanto para un solo atributo como para un dataset completo.
#'
#' @param x (data.frame | list): Un DataFrame o lista que será discretizado.
#' @param num.bins (int): Un número natural mayor de 1, que determina el número de intervalos.
#' @param algoritmo (str): Algoritmo de discretización, default='anchura'.
#' @return (data.frame | factor): DataFrame o vector de valores categóricos resultado de aplicar el algoritmo.
#' @export
#' @examples
#' discretize(c(1,2,3,4),2)
discretize<-function(x, num.bins, algoritmo='anchura'){
  if(is.data.frame(x)){ # Si x es un data.frame
    return (apply(x,MARGIN = 2,discretize,num.bins=num.bins,algoritmo=algoritmo))
  }else{ # x es un atributo
    if (algoritmo=='anchura'){
      minimo <- min(x)
      w <- (max(x)-minimo)/num.bins # Tamaño de los intervalos
      
      ptos_corte<-(1:(num.bins-1)*w)+minimo # Hay num.bins-1 puntos de corte, y cada uno será el anterior más w
    }else{
      orden <- order(x) # Es necesario el orden para determinar los puntos de corte 
      n <- length(x)
      indices<-1:(num.bins-1)
      # Cada intervalo tendrá como un mínimo elementos
      indices<-(indices*n)/num.bins 
      # Algunos intervalos tendrán un elemento más
      indices<-ifelse(indices%%1==0,as.integer(indices),as.integer(indices)+1)
      if (algoritmo=='frecuencia'){
        # El punto de corte de dos intervalos consecutivos será el punto medio entre los dos elementos más cercanos de dichos intervalos
        ptos_corte=(x[orden[indices]]+x[orden[indices+1]])/2
      }else{ # algoritmo de cuantil
        ptos_corte=x[orden[indices]]
      } 
    }
    etiquetas <- rep(paste('(',ptos_corte[num.bins-1],', Inf )'), length(x))
    aux<- -Inf
    for (p_corte in ptos_corte){ # Asignación de los elementos al intervalo correspondiente
      etiquetas[aux<x & x <= p_corte]<-paste('(',aux,',',p_corte,' ] ')
      aux<-p_corte
    }
    return (factor(etiquetas))
  }
}
