library("readxl")
data <- read_excel("C:/Users/AA/Desktop/mydata.xlsx")
View(data)
str(data)

h=4  ; 
y = data$x; 

## Série d'ajustement.
s=4
th=nrow(data)-h
I=vector("numeric",th)
S = vector("numeric",th)
T = vector("numeric",th)
comp_sais =as.numeric(data$Composante);
## Fonction à Minimiser (MAPE)

DAM<-function(x){
  S[2]<-x[1];
  T[2]<- x[2];
  I[1:4]<-x[3:6];
  alpha <-x[7];
  gamma <-  x[8];
  δ<-x[9]
  phi  <- x[10];
  mape_ajus=0 ;
  for (i in 3:4){
    S[i]=alpha*(y[i]/I[i])+(1-alpha)*(S[i-1]+phi*T[i-1]);
    T[i]=gamma*(S[i]-S[i-1] )+(1-gamma)*phi*T[i-1] ;
  }
  mape_ajus = 0 ; 
  for (i in 5:th){
    S[i]=alpha*(y[i]/I[i-s] )+(1-alpha)*(S[i-1]+phi*T[i-1]); 
    T[i]=gamma*(S[i]-S[i-1] )+(1-gamma)*phi*T[i-1] ; 
    I[i]=δ*(y[i]-S[i] )+(1-δ)*I[i-s];  
    mape_ajus = mape_ajus+abs(y[i]-S[i-1]-T[i-1]-I[i-s] )*100/y[i];
  }
  mape_ajus =  mape_ajus/(th-h) ;
  return(mape_ajus)
}

## Solution initiale.
niveau_init  = ((y[1]/comp_sais[3])+(y[2]/comp_sais[4])+(y[3]/comp_sais[5]))/3;
tend_init = ((y[3]/comp_sais[5])-(y[1]/comp_sais[3]))/2;


x0<- c(niveau_init,tend_init,comp_sais[3],comp_sais[4],comp_sais[5],comp_sais[6],0.3,0.4,0.2,0.2);
## Mape de la solution initiale (pour apprécier l'optimation)

DAM(x0)  

## Optim

ob<-optim(x0,DAM, lower=c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,0,0,0,0), 
          upper=c(Inf,Inf,Inf,Inf,Inf,Inf,1,1,1,1)) 
x<-ob$par;
x
DAM(x)

## Calcul le modèle optimale pour la série d'ajustement :

S[2]<-x[1];
T[2]<- x[2];
I[1:4]<-x[3:6];
alpha <-x[7];
gamma <-  x[8];
δ<-x[9]
phi  <- x[10];
for (i in 3:4){
  S[i]=alpha*(y[i]/I[i])+(1-alpha)*(S[i-1]+phi*T[i-1]);
  T[i]=gamma*(S[i]-S[i-1] )+(1-gamma)*phi*T[i-1] ;
}
for (i in 5:th){
  S[i]=alpha*(y[i]/I[i-s] )+(1-alpha)*(S[i-1]+phi*T[i-1]); 
  T[i]=gamma*(S[i]-S[i-1] )+(1-gamma)*phi*T[i-1] ; 
  I[i]=δ*(y[i]-S[i] )+(1-δ)*I[i-s];  
}

## Calcul du Mape sur l'horizon des prévisions.

y_pred =  vector("numeric",h)
for(i in 1:h){
  somme = 0
  for (j in 1:i){
    somme = somme + (phi^(j) * T[th])
  }
  y_pred[i] =(S[th]+somme)*I[th+h-s]

}
test_mape = 0
for (i in 1:h){
  test_mape = test_mape + abs(y[th+i]-y_pred[i])*100/y[th+i]; 
}

test_mape/h

