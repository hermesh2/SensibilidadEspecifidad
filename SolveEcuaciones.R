# install.packages("matlib")

library(matlib) # use the package
library(dplyr)
## 1*x1 - 1*x2  =  2 
## 2*x1 + 2*x2  =  1
A <- matrix(c(1, 2, -1, 2), 2, 2)
b <- c(2,1)
showEqn(A, b)
Solve(A, b, fractions = TRUE)



# problema covid ----------------------------------------------------------
# x1 Verdaderos Positivos
# x2 Falsos positivos
# x3 Verdaderos negativos
# x4 Falsos negativos
# x5 Positivos en test
# x6 Negativos en test
# RMas numero de casos reales
# RMenos numero de sanos

# Ecuaciones
# Verdaderos Positivos +  Falsos positivos = Positivos en test
# Verdaderos negativos +  Falsos negativos = Negativos en test
# Verdaderos positivos +  Falsos negativos = numero de casos reales
# Verdaderos negativos +  Falsos positivos = numero de sanos
# Verdaderos Positvos  / Positivos en test = Sensibilidad
# Verdaderos negativos / Negativos en test = Especifidad

n <- 1
RMas <- 0.01 * n
RMas
RMenos <- n - RMas
S <- 0.933 # Sensisibilidad
E <- 0.994 # Especificidad
A <- matrix(c(
  1  , 1, 0,   0, -1,  0,
  0  , 0, 1,   1,  0, -1,
  1  , 0, 0,   1,  0,  0,
  0  , 1, 1,   0,  0,  0,
  1  , 0, 0,   0, -S,  0,
  0  , 0, 1,   0,  0, -E,
  1,   1, 1,   1, -1, -1
  ), byrow = TRUE, nrow = 7)
A
b <- c(0,  0, RMas, RMenos, 0, 0, 0)
showEqn(A, b)
Solve(A, b, fractions = TRUE)
Solve(A, b, fractions = FALSE)


Sol <- Solve(A, b, fractions = FALSE)
Sol %>% str
SolNum <- Sol %>% strsplit(split = "=") %>% lapply( ., function(x){x[2]}) %>% 
  gsub(pattern = "[^0-9|^.|^-]", replacement = "", x = .) %>% as.numeric %>% .[1:6]
SolNum <- SolNum %>% c(., SolNum[1] + SolNum[4], SolNum[2] + SolNum[3], 
                       SolNum[1] / (SolNum[1] + SolNum[4]), SolNum[4] / (SolNum[1] + SolNum[4]) )

names( SolNum ) <- c("Ver Pos", "Fal Pos", "Ver Neg", "Fal Neg", "Pos test", "Neg test", "Real Pos", "Real Neg",
                     "Pos Detect", "Pos pasan")
SolNum
