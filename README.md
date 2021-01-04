# Crear-particiones-de-datos-para-modelos-de-Machine-Learning-
Los analistas necesitan una evaluación imparcial de la calidad de sus modelos de aprendizaje automático. Para conseguirlo, dividen los datos disponibles en dos partes. Usan una parte para construir el modelo de aprendizaje automático y retienen los datos restantes para la  evaluación del desempeño del modelo en los datos de reserva. Veremos 4 varios casos para hacer las particiones:
Caso 1: variable objetivo numérica y dos particiones 
Caso 2: variable objetivo numérica y tres particiones 
Caso 3: variable objetivo categórica y dos particiones 
Caso 4: variable objetivo categórica y tres particiones

library(caret)
bh <- read.csv("BostonHousing.csv")

trg.idx <- createDataPartition(bh$MEDV, p = 0.8, list = FALSE)
trg.part <- bh[trg.idx, ]
val.part <- bh[-trg.idx, ]

trg.idx <- createDataPartition(bh$MEDV, p = 0.7, list = FALSE)
trg.part <- bh[trg.idx, ]
temp <- bh[-trg.idx, ]
val.idx <- createDataPartition(temp$MEDV, p = 0.5, list = FALSE)
val.part <- temp[val.idx, ]
test.part <- temp[-val.idx, ]

bh2 <- read.csv("boston-housing-classification.csv")
trg.idx <- createDataPartition(bh2$MEDV_CAT, p = 0.7, list = FALSE)
trg.part <- bh2[trg.idx, ]
val.part <- bh2[-trg.idx, ]

trg.idx <- createDataPartition(bh3$MEDV_CAT, p = 0.7, list = FALSE)
trg.part <- bh2[trg.idx, ]
temp <- bh2[-trg.idx, ]
val.idx <- createDataPartition(temp$MEDV_CAT, p = 0.5, list = FALSE)
val.part <- temp[val.idx, ]
test.part <- temp[-val.idx, ]

rda.cb.partition2 <- function(ds, target.index, prob) {
  library(caret)
  train.idx <- createDataPartition(y=ds[,target.index], 
      p = prob, list = FALSE)
  list(train =  ds[train.idx, ], val = ds[-train.idx, ])
}

rda.cb.partition3 <- function(ds, 
             target.index, prob.train, prob.val) {
  library(caret)
  train.idx <- createDataPartition(y=ds[,target.index], 
          p = prob.train, list = FALSE)
  train <- ds[train.idx, ]
  temp <- ds[-train.idx, ]
  val.idx <- createDataPartition(y=temp[,target.index], 
          p = prob.val/(1-prob.train), list = FALSE)
  list(train =  ds[train.idx, ], 
          val = temp[val.idx, ], test = temp[-val.idx, ])
}

dat1 <- rda.cb.partition2(bh, 14, 0.8)
dat2 <- rda.cb.partition2(bh, 14, 0.7, 0.15)

sam.idx <- sample(1:nrow(df), 50, replace = FALSE)
