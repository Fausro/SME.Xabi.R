# SME.Xabi.R

Paquete de funciones de SME de R. Este paquete implementa las funciones que se solicitaban y algunas funcionalidades adicionales.

## Instalación

Puedes instalar el paquete utilizando devtools:

```r
devtools::install_github("Fausro/SME.Xabi.R")
```

#### Dependencias

- ggplot2
- reshape2

## Uso

A continuación, se muestra un ejemplo de cómo utilizar el paquete:

```r
library(SME.Xabi)

discretize(c(1,2,3,4),2)
#  [1] ( -Inf , 2.5  ]  ( -Inf , 2.5  ]  ( 2.5 , Inf )    ( 2.5 , Inf )   
#  Levels: ( -Inf , 2.5  ]  ( 2.5 , Inf )
```

## Contacto

Xabier Larrayoz (<xabier.larrayoz@ehu.eus>)
