# Base functions ----------------------------------------------------------
## "Not in" function
"%nin%" <- Negate("%in%")

## Function to compute water content
wet.f <- function(permi, b0, b1){
  top <- sqrt(permi) - b0
  bot <- b1
  theta <- top/bot
}

##Function to compute permittivity
permi.f <- function(theta, b0, b1){
  permi <- b1^2 * theta^2 + 2*b1*theta*b0 + b0^2
}
## Function to compute NDVI
NDVI.fun <- function(NIRr, NIRi, Redr, Redi){
  top <- (NIRr/NIRi) - (Redr/Redi)
  bottom <- (NIRr/NIRi) + (Redr/Redi)
  NDVI <- top/bottom
}
## Function to compute PRI
PRI.fun <- function(`570r`, `570i`, `531r`, `531i`){
  top <- (`570r`/`570i`) - (`531r`/`531i`)
  bottom <- (`570r`/`570i`) + (`531r`/`531i`)
  NDVI <- top/bottom
}

## Function to compute albedo
albedo.fun <- function(incident, reflected){
  top <- rowSums(reflected)
  bottom <- rowSums(incident)
  NDVI <- top/bottom
}
## Function to normalize data between a and b
normalize <- function(x, a, b){
  top <- (b-a) * (x - min(x, na.rm = T))
  bot <- max(x, na.rm = T) - min(x, na.rm = T)
  y <- top/bot + a
}

## Function to normalize data between a and b
scale_perso <- function(x){
  top <- x - mean(x, na.rm = T)
  bot <- sd(x, na.rm = T)
  y <- top/bot
}
