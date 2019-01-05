---
title: 'Tipología y ciclo de vida de los datos: Práctica2'
author: "Antonio Guzmán Martín & Joaquín Fernández León"
date: "Diciembre 2018"
output:
  pdf_document:
    toc: yes
    toc_depth: '2'
  html_document:
    highlight: default
    number_sections: yes
    theme: cosmo
    toc: yes
    toc_depth: 2
---


*****
# 1. Descripción del dataset
*****

Comenzamos con la lectura del dataset.

```{r}
library(readr)
redwine <- read.csv(file="winequality-red.csv", header = TRUE)
#numero de filas por dataset
nrow(redwine)
ncol<-ncol(redwine)
#sacamos 5 primeras filas
head(redwine[,1:ncol])
sapply(redwine,class)
```

**Variables del dataset**

* **fixed acidity:** Conjunto de los ácidos naturales procedentes de la uva (tartárico, málico, cítrico y succínico) o formados en la fermentación maloláctica (láctico). En general, los ácidos (acidez fija) son preservante naturales del vino y ayuda a mantener el color y cualidades aromáticas. 

* **volatile acidity:** Conjunto de ácidos formados durante la fermentación o como consecuencia de alteraciones microbianas. Estos ácidos son, principalmente: ácido Acético, ácido Propionico, ácido Butírico y ácido Sulfúrico. Si la acidez volátil, presente en todos los vinos, es muy elevada el vino se picará y avigranará con el paso del tiempo. Es conveniente que la acidez volatil de un vino sea lo más baja posible.

El contenido en acidez volátil no puede ser superior a:
a) 18 miliequivalentes por litro para los mostos de uva parcialmente fermentados,
b) 18 miliequivalentes por litro para los vinos blancos y rosados,
c) 20 miliequivalentes por litro para los vinos tintos.


* **citric acid:** En pequeñas cantidades este hacido puede añadir frescor y sabor a los vinos (dentro de acido fijo).

* **residual sugar:** Azúcar que queda en el vino después de la fermentación. Es raro encontrar vinos con menos de  1 g/l y vinos con más de  45 g/l son considerados dulces.

* **chlorides:** cantidad de sal en el vino.

* **free sulfur dioxide :** Previene del crecimiento microbial y de la oxidación del vino.La oxidación enturbia sus colores característicos (tornándolos en amarillos intensos e, incluso, marrones).Por lo que respecta al gusto, al beberlo notaremos sabores más secos y ásperos, incluso amargos en algunos casos.

* **total sulfur dioxide:** suma de concentraciones libres y amarradas de S02; concentraciones de dioxodo de sulfuro libres superiores a  50 ppm se vuelven evidentes en el sabor y olor.

* **density:** densidad del vino, suele ser similar al del agua dependiendo de la concentración de azucar y alcohol.

* **pH:** Describe como de ácido o básico es el vino 0 (very acidic) to 14 (very basic); mayoria vinos en escala 3-4.(principalmente 3,55 a 4).

* **sulphates:** Actua como un antimicrobial and antioxidante. Los sulfatos de sodio y calcio aparecen en el agua y por lo tanto la uva y el vino pueden contenerlos. Un agua con una cantidad de sulfatos inferior a 250mg/l se considera en este aspecto un agua de calidad y con valores superiores a 400mg/l insalubre.

* **alcohol:** cantidad de alcohol del vino. No es muy útil para hallar la calidad.

* **quality:** calidad del vino entre 0 y 10.

## 1.1 ¿Por qué es importante y qué pregunta/problema pretende responder?

Pregunta: ¿Qué componentes fisico-químicos influyen en que un vino sea bueno?. Obtener un modelo cuya combinación de variables permita determinar si es un buen vino.

*****
# 2. Integración y selección de los datos de interés a analizar.
*****

La mayoría de los atributos corresponden con características necesarias para determinar la calidad del vino. 

Sin embargo, a priori, podemos prescindir de la variable `total sulfur dioxide` (indica elsuma de concentraciones libres y amarradas, solo nos interesan las libres) y `density` (indica proporción de alcohol y esta no es interesante para determinar la calidad) (Según el estudio de: https://www.vinopack.es/criterios-que-determinan-la-calidad-en-el-vino).

No obstante, en los siguientes apartados comprobaremos si esto es cierto, o por el contrario si que afecta en la calidad.

*****
# 3 Limpieza de datos
*****

## 3.1 Elementos vacíos
```{r}
# Números de valores desconocidos por campo
sapply(redwine, function(x) sum(is.na(x)))
#No se han encontrado valores vacíos o NAs. 
# Resumen de las variables
summary(redwine)
#ph: correcto (entre 2 y 4)
```

## 3.2 Valores extremos

Para cada una de las variables observemos si existen valores atípicos:

```{r}
boxplot(redwine$fixed.acidity,main = "fixed.acidity",col="gray")
boxplot.stats(redwine$fixed.acidity)$out
boxplot(redwine$volatile.acidity,main = "volatile",col="gray")
boxplot.stats(redwine$volatile.acidity)$out
boxplot(redwine$citric.acid,main = "citric.acid",col="gray")
boxplot.stats(redwine$citric.acid)$out
boxplot(redwine$residual.sugar,main = "residual.sugar",col="gray")
boxplot.stats(redwine$residual.sugar)$out
boxplot(redwine$chlorides,main = "chlorides",col="gray")
boxplot.stats(redwine$residual.sugar)$out
boxplot(redwine$chlorides,main = "residual.sugar",col="gray")
boxplot.stats(redwine$residual.sugar)$out
boxplot(redwine$free.sulfur.dioxide,main = "free.sulfur.dioxide",col="gray")
boxplot.stats(redwine$free.sulfur.dioxide)$out
boxplot(redwine$pH,main = "pH",col="gray")
boxplot.stats(redwine$pH)$out
#datos correctos porque los valores de pH estan entre 2 y 7
boxplot(redwine$sulphates,main = "sulphates",col="gray")
boxplot.stats(redwine$sulphates)$out
boxplot(redwine$ alcohol,main = " alcohol",col="gray")
boxplot.stats(redwine$ alcohol)$out
#alcohol está dentro de unos rangos adecuados
```

Todas las gráficas presentan outliers pero dado a que, la creación de un vino es meramente una reacción química, se puede decir que los valores que se presentan son posibles y simplemente podemos decir que dichos outliers pertenecen a vinos que tienen características muy diferentes al resto.

*****
# 4. Análisis de datos.
*****

## 4.1 Selección de los grupos de datos que se quieren analizar/comparar (planificación de los análisis a aplicar).

 Vamos a analizar si son significativas cada variable del dataset para ello haremos un test t con un nivel de significación=0.05.


 Diferentes hipótesis se pueden establecer cambiando el valor de parámetro alternative en la función t.test

 1. **Alternative='two.sided':** Hipótesis nula H0: $\mu{1}$ =  $\mu{2}$ Hipótesis alternativa H1:  $\mu{1}$ != $\mu{2}$
 2. **Alternative='greater':** Hipótesis nula H0: $\mu{1}$ =  $\mu{2}$ Hipótesis alternativa H1:  $\mu{1}$ > $\mu{2}$
 3. **Alternative='less':** Hipótesis nula H0: $\mu{1}$ =  $\mu{2}$ Hipótesis alternativa H1:  $\mu{1}$ < $\mu{2}$
 
```{r}
 # A más alcohol, hay más calidad, significativo
high_alcohol<-quantile(redwine$alcohol, probs =0.75)
redwine.altoAlcohol<-redwine[redwine$alcohol>=high_alcohol,]$quality
redwine.bajoAlcohol<-redwine[redwine$alcohol<high_alcohol,]$quality
t.test(redwine.altoAlcohol, redwine$quality, alternative = "greater")
t.test(redwine.bajoAlcohol, redwine$quality, alternative = "less")
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que a más cantidad de alcohol hay más calidad y por tanto que la variable `alcohol` es significativa.

```{r}
# A más azucar no hay más calidad y a menos azúcar tampoco
high_sugar<-quantile(redwine$residual.sugar, probs =0.75)
redwine.altoGradoAzucar<-redwine[redwine$residual.sugar>=high_sugar,]$quality
redwine.bajoGradoAzucar<-redwine[redwine$residual.sugar<high_sugar,]$quality
t.test(redwine.altoGradoAzucar, redwine$quality, alternative = "greater")
t.test(redwine.bajoGradoAzucar, redwine$quality, alternative = "less")
```

El `p-valor` no es menor que el nivel de significación por lo que no podemos rechazar la hipótesis nula y por tanto que la variable `residual.sugar` no es significativa.

```{r}
#  Volatile.acidity, significativo
high_volatile.acidity<-quantile(redwine$volatile.acidity, probs =0.75)
redwine.altoVolatile.acidity<-redwine[redwine$volatile.acidity>=high_volatile.acidity,]$quality
redwine.bajoVolatile.acidity<-redwine[redwine$volatile.acidity<high_volatile.acidity,]$quality

t.test(redwine.altoVolatile.acidity, redwine$quality, alternative = "less")
t.test(redwine.bajoVolatile.acidity, redwine$quality, alternative = "greater")
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que la variable `volatile.acidity` es significativa.

```{r}
#sulphates, significativo
high_sulphates<-quantile(redwine$sulphates, probs =0.75)
redwine.altoSulphates<-redwine[redwine$sulphates>=high_sulphates,]$quality
redwine.bajoSulphates<-redwine[redwine$sulphates<high_sulphates,]$quality
t.test(redwine.altoSulphates, redwine$quality, alternative = "greater") # significativo sulfato alto
t.test(redwine.bajoSulphates, redwine$quality, alternative = "less") #
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que la variable `sulphates` es significativa.

```{r}
#PH, no significativo
high_pH<-quantile(redwine$pH, probs =0.75)
redwine.altopH<-redwine[redwine$pH>=high_pH,]$quality
redwine.bajopH<-redwine[redwine$pH<high_pH,]$quality
t.test(redwine.altopH, redwine$quality, alternative = "greater") # NO significativo  PH
t.test(redwine.bajopH, redwine$quality, alternative = "less") # NO significativo  PH
```

El `p-valor` no es menor que el nivel de significación por lo que no podemos rechazar la hipótesis nula y por tanto que la variable `pH` no es significativa.

```{r}
# Citric acid es significativo
high_citric_acid<-quantile(redwine$citric.acid, probs =0.75)
redwine.altoCitricAcid<-redwine[redwine$citric.acid>=high_citric_acid,]$quality
redwine.bajoCitricAcid<-redwine[redwine$citric.acid<high_citric_acid,]$quality
t.test(redwine.altoCitricAcid, redwine$quality, alternative = "greater")
t.test(redwine.bajoCitricAcid, redwine$quality, alternative = "less")
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que la variable `citric.acid` es significativa.

```{r}
# fixed acidity es significativo
high_fixed_acidity<-quantile(redwine$fixed.acidity, probs =0.75)
redwine.altoFixedAcidity<-redwine[redwine$fixed.acidity>=high_fixed_acidity,]$quality
redwine.bajoFixedAcidity<-redwine[redwine$fixed.acidity<high_fixed_acidity,]$quality
t.test(redwine.altoFixedAcidity, redwine$quality, alternative = "greater")
t.test(redwine.bajoFixedAcidity, redwine$quality, alternative = "less")
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que la variable `fixed.acidity` es significativa.

```{r}
#Chlorides, significativo, a más chlorides peor calidad
high_chlorides<-quantile(redwine$chlorides, probs =0.75)
redwine.altoChlorides<-redwine[redwine$chlorides>=high_chlorides,]$quality
redwine.bajoChlorides<-redwine[redwine$chlorides<high_chlorides,]$quality
t.test(redwine.altoChlorides, redwine$quality, alternative = "less")
t.test(redwine.bajoChlorides, redwine$quality, alternative = "greater")
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que la variable `chlorides` es significativa.

```{r}
#free sulfur dioxide, no significativo
high_free_sulfur<-quantile(redwine$free.sulfur.dioxide, probs =0.75)
redwine.altoFreeSulfur<-redwine[redwine$free.sulfur.dioxide>=high_free_sulfur,]$quality
redwine.bajoFreeSulfur<-redwine[redwine$free.sulfur.dioxide<high_free_sulfur,]$quality
t.test(redwine.altoFreeSulfur, redwine$quality, alternative = "greater")
t.test(redwine.bajoFreeSulfur, redwine$quality, alternative = "less")
```

El `p-valor` no es menor que el nivel de significación por lo que no podemos rechazar la hipótesis nula y por tanto que la variable `free.sulfur.dioxide` no es significativa.

```{r}
# total sulfur dioxide,  significativo, a más cantidad de  total sulfur peor calidad
high_total_sulfur<-quantile(redwine$total.sulfur.dioxide, probs =0.75)
redwine.altoTotalSulfur<-redwine[redwine$total.sulfur.dioxide>=high_total_sulfur,]$quality
redwine.bajoTotalSulfur<-redwine[redwine$total.sulfur.dioxide<high_total_sulfur,]$quality
t.test(redwine.altoTotalSulfur, redwine$quality, alternative = "less")
t.test(redwine.bajoTotalSulfur, redwine$quality, alternative = "greater")
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que la variable `total.sulfur.dioxide` es significativa.

```{r}
# Density, significativo a mayor densidad peor calidad
high_density<-quantile(redwine$density, probs =0.75)
redwine.altoDensity<-redwine[redwine$density>=high_density,]$quality
redwine.bajoDensity<-redwine[redwine$density<high_density,]$quality
t.test(redwine.altoDensity, redwine$quality, alternative = "less")
t.test(redwine.bajoDensity, redwine$quality, alternative = "greater")
```

El `p-valor` es menor que el nivel de significación por lo que rechazamos la hipótesis nula y podemos decir que la variable `density` es significativa. Sólo se acepta una de las colas, la cola derecha.

**Finalmente realizamos una recopilación de las variables significativas que serán: alcohol , volatile.acidity , sulphates , citric.acid , fixed.acidity , chlorides , total.sulfur.dioxide , density**

## 4.2 Comprobación de la normalidad y homogeneidad de la varianza

**Comprobar  que variables siguen distribución normal**

Aplicando la siguiente función podemos comprobar si siguen una distribución normal o si por el contrario no lo hacen.

```{r}
#install.packages("nortest")
library(nortest)
alpha = 0.05
col.names = colnames(redwine)
for (i in 1:ncol(redwine)) {
  if (i == 1) cat("Variables que no siguen una distribución normal:\n")
  if (is.integer(redwine[,i]) | is.numeric(redwine[,i])) {
      p_val = ad.test(redwine[,i])$p.value
        if (p_val < alpha) {
            cat(col.names[i])
                  # Format output
            if (i < ncol(redwine) - 1) cat(", ")
            if (i %% 3 == 0) cat("\n") 
        }
      } 
  }
```

De esta manera podemos afirmar que las variables no siguen una distrubución normal.

## 4.3 Aplicación de pruebas estadísticas para comparar los grupos de datos. En función de los datos y el objetivo del estudio, aplicar pruebas de contraste de hipótesis, correlaciones, regresiones, etc.

En primer lugar, procedemos a realizar un análisis de correlación entre las distintas variables para determinar cuáles de ellas ejercen una mayor influencia sobre la calidad del vino:


```{r}
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlación para cada variable cuantitativa # con respecto al campo "precio"
for (i in 1:(ncol(redwine) - 1)) {
if (is.integer(redwine[,i]) | is.numeric(redwine[,i]))
    { 
    spearman_test = cor.test(redwine[,i],redwine[,length(redwine)],method = "spearman")
      corr_coef = spearman_test$estimate
      p_val = spearman_test$p.value
      # Add row to matrix
    pair = matrix(ncol = 2, nrow = 1)
    pair[1][1] = corr_coef
    pair[2][1] = p_val
    corr_matrix <- rbind(corr_matrix, pair)
    rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(redwine)[i]
  } 
}
a <- corr_matrix[,'p-value']
corr_matrix[order(a),]
```

Así, identificamos cuáles son las variables más correlacionadas con la calidad según su proximidad con los valores -1 y +1.

Teniendo esto en cuenta, queda patente cómo la variable más relevante para **la calidad** es la variable `alcohol`. Pero en términos generales podemos decir que los valores que obtenemos son bastante modestos y no sería adecuado obtener ninguna conclusión por ahora. Lo único que podemos hacer es utilizar estos valores como tendencias.

```{r}
#install.packages("corrplot")
library(corrplot)
corrplot(cor(redwine))
```

*Nota: Para cada coeficiente de correlación se muestra también su p-valor asociado, puesto que éste puede dar información acerca del peso estadístico de la correlación obtenida.*

### Modelo de regresión lineal logística

Para realizar predicciones en la calidad del vino vamos a plantear un modelo de regresión logística en dónde solamente tendremos regresores cuantitativos. La variable a predecir será una variable dicotómica transformada previamente (`good_wine`esta tomará valores de calidad *0* si es menor que 7 ó *1* en caso contrario).

```{r}
good_wine <-ifelse(test=redwine$quality>=7,yes=1,no=0)
redwine$good_wine=good_wine
quality<- redwine$quality
```

**Se usa regresión logística ya que nuestro modelo predicirá si el vino es de calidad o no (variable dicotómica).**

Para obtener un modelo de regresión logística considerablemente eficiente, lo que haremos será obtener varios modelos de regresión.

En primer lugar partiremos del modelo que utiliza las variables que hemos detectado como significativas según el test t para un valor significación=0.05.

```{r}
GLM.1 <- glm( redwine$good_wine ~ alcohol + volatile.acidity + sulphates + citric.acid + fixed.acidity + chlorides + total.sulfur.dioxide + density, family=binomial(logit),data=redwine)

summary(GLM.1)$aic
```

En segundo lugar realizamos una comparación con la selección de todas las variables de nuestro dataset.

```{r}
GLM.2 <- glm( redwine$good_wine ~ . -quality , family=binomial(logit),data=redwine)
summary(GLM.2)$aic
```

Finalmente, vamos a realizar una última prueba con la función `step` que os proporcionará la mejor combinación de atributos para la obtención de un mejor modelo.

```{r}
info = step(object = GLM.2, direction = "both", trace = 1)

GLM.Mejor = glm(formula = redwine$good_wine ~ fixed.acidity + volatile.acidity + 
    residual.sugar + chlorides + total.sulfur.dioxide + density + 
    sulphates + alcohol, family = binomial(logit), data = redwine)

summary(GLM.Mejor)$aic
```

Como se puede observar, el mejor modelo es el último modelo ya que proporciona un mayor valor de `AIC` y la función `step` en sí te lo proporciona. Sin embargo, creemos que para nuevas entradas de datos, este modelo proporcionado por la función `step`  está sobreajustado al conjunto de datos y no será muy bueno para nuevos conjuntos de datos. Por este motivo, vamos a seleccionar el modelo *GLM.1* que parte de las variables que hemos estudiado como significativas.

```{r}
redwine$prob_qualityM=predict(GLM.1, redwine, type="response")
newdatarisk=subset(redwine, prob_qualityM>0.7)
Q3 <-quantile(redwine$alcohol)[4]
alcohol <- which(newdatarisk$alcohol>Q3)

```


# 5. Representación de los resultados a partir de tablas y gráficas.

### Calculo curva ROC

El cálculo de la curva ROC será interesante para poder evaluar cómo de bueno es nuestro modelo. Esta medida está comprendida en tanto por 1 de manera que cuanto mayor sea mejor será la predicción del modelo.

Es interesante utilizar este tipo de métrica ya que podemos detectar cuando, para clases desbalanceadas, el modelo arrastra los datos a la clase mayoritaria y obtiene una tasa de acierto bastante alta como consecuencia del desbalanceo pero una curva ROC muy mala ya que para la otra clase no tiene ningún acierto.

```{r}
library(pROC)

g=roc(redwine$good_wine,redwine$prob_qualityM, data=redwine)
plot(g)
auc(g)
```


Como vemos, obtenemos un área bajo la curva de 0.8779 lo cual supone un modelo bastante bueno.

# 6. Resolución del problema. A partir de los resultados obtenidos, ¿cuáles son las conclusiones? ¿Los resultados permiten responder al problema?

Ahora podemos responder a nuestra pregunta del primer apartado *¿Qué componentes fisico-químicos influyen en que un vino sea bueno?* diciendo que los componentes más influyentes son: *alcohol + volatile.acidity + sulphates + citric.acid + fixed.acidity + chlorides + total.sulfur.dioxide + density*.

Todos ellos en conjunto permiten montar un modelo capaz de predecir la calidad de un vino.
