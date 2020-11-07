# Simulaciones

## Simule 100 de los datos que perdió el experto. Para esta pregunta desde una semilla igual a 0, genere los datos normales y utilice el teorema mostrado.

```{r}
set.seed(0)
#tamaño de n
n <- 100
# 2 dimensiones
d=2
#creamos una matrix llamada x, que contenga 2 dimensión donde n y r son normales
x <- matrix( rnorm(d * n), n, d )
#Calculamos la norma de = raíz (suma los valores de x^2)
x.norma <- sqrt( rowSums( x^2 ) )
# le decimos que n se distribuye uniforme
u <- runif( n )
# graficamos la norma de x
plot( x / x.norma, col = "black", xlab = "x", ylab = "y" )
...

## Obtenga el estimador máximo verosímil con 1000 datos generados.
