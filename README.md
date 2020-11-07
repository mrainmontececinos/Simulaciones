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
```

## Obtenga el estimador máximo verosímil con 1000 datos generados.

```{r}
set.seed(5)
#distribución exponencial con rate 0.9
x<-rexp(1000,rate=0.9)
# creamos una función, que sume y calcule el logaritmo
f<-function(rate,x){
  -sum(dexp(x,rate=rate,log = TRUE))
}
s<-nlm(f,rate<-c(runif(1)),x=x,hessian = TRUE) #Minimiza la función f, y se distribuye uniforme. Y utilizamos la matriz hessiana
s$estimate
```

## Intervalo de confianza y test de hipótesis

Suponga que los puntajes de una prueba internacional siguen una ley normal de parámetros desconocidos. Genera 30 datos normales de parámetro de media igual a 5 y de varianza igual a 2. Desde una semilla igual a 1,

```{r}
set.seed(1)
ds=sqrt(2);ds
x=rnorm(30,5,1.414214)
```

### Encuentre los estimadores de media y varianza con los 30 datos.

```{r}
#media
mean(x)
#varianza
var(x)
#des.estan
sd(x)
```


### Encuentre un intervalo de confianza para la media y para la varianza con una confianza igual a 0,01

Estimación del intervalo de la media poblacional con varianza desconocida

```{r}
# prueba t, x=muestra, intervalo de confianza
t.test (x,conf.level = 0.99)

```

### Realice el test de hipótesis

```{r}
t.test(x, alternative='two.sided',conf.level=0.95, mu=5.1)
```

## Regression lineal


```{r}
set.seed(0)
x1=rnorm(100)
x2=rnorm(100)
y1=3+5*x1+rnorm(100,0,2)
y2=33+53*x1+0.1*x2*rnorm(100,0,2)
#Creamos un data.frame llamado data
data=data.frame(y1,y2,x1,x2)

```

Grafico


```{r}
library(ggplot2)
ggplot(data=data,aes(x = x1, y = y1)) + 
  geom_point()+ theme_bw()+stat_smooth(method = lm)
# dibuja puntos, lineas de fondo, metodo de lm =regresion lineal simple
```



Hipótesis nula (H0): los coeficientes son iguales a cero (es decir, sin relación entre x e y)
Hipótesis alternativa (Ha): los coeficientes no son iguales a cero (es decir, hay alguna relación entre x e y)


```{r}
modelo1<-lm(y1~x1,data=data)
summary(modelo1)
```

Interpretación

El primer paso para interpretar el análisis de regresión múltiple es examinar el estadístico F y el valor p asociado, en la parte inferior del resumen del modelo.

En nuestro ejemplo, se puede ver que el valor p del estadístico F es < 2.2e-16 que es altamente significativo. Esto significa que, al menos, una de las variables predictoras está significativamente relacionada con la variable de resultado.

Para ver qué variables predictoras son significativas, puede examinar la tabla de coeficientes, que muestra la estimación de los coeficientes beta de regresión y los valores p estadísticos t asociados:

```{r}
summary(modelo1)$coef
```
Para un predictor dado, el estadístico t evalúa si existe o no una asociación significativa entre el predictor y la variable de resultado, es decir, si el coeficiente beta del predictor es significativamente diferente de cero

$y_1=2,72+5,05*x_1$


```{r}
ggplot(data=data,aes(x = x1+x2, y = y1)) + 
  geom_point()  + theme_bw()+stat_smooth(method = lm)
```


```{r}


modelo2<-lm(y2~x1+x2)
summary(modelo2)
```

$y_2=32,99+52,99x_1-0,005x_2$
Interpretación
En nuestro ejemplo, se puede ver que el valor p del estadístico F es < 2.2e-16 que es altamente significativo. Esto significa que, al menos, una de las variables predictoras está significativamente relacionada con la variable de resultado.



```{r}
summary(modelo2)$coef
```

Se puede ver que  x1 están significativamente asociados a y1,mientras que x2 no están significativamente asociados a y2.





