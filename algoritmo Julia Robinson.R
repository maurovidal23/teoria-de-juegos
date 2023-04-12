#ALGORITMO GENERAL


#lectura de los datos
datos=read.csv2("datos.csv",header=FALSE)
datos
A=as.matrix(datos)
A
dim(A)
n=nrow(A);n
m=ncol(A);m

x=vector(mode = "numeric",length =n)
y=vector(mode= "numeric", length=m)

#inicialización con frecuencias acumuladas arbitrarias, en este caso, supongamos 0,0

x=rep(0,n)
y=rep(0,m)

iter=100


Xk=matrix(0,nrow=iter,ncol=n) #matriz que acumula las frecuencias relativas en cada paso
Yk=matrix(0,nrow=iter,ncol=m) #matriz que acumula las frecuencias relativas en cada paso

#el bucle en cada iteración hace el calculo de los pagos al jugadores 1 y 2 suponiendo
#que ambos jugadores van a jugar la estrategia mixta determinada por sus frecuencias 
#relativas (esta implementado con frecuencias absolutas ya que no influye a la hora de tomar
#el máximo o el mínimo de un conjunto que las cantidades esten multiplciadas por un mismo factor) 

for (k in 1:50) {
  
  # tomamos el mínimo de los pagos del jugador y
  
  H2=x%*%A #pagos al jugador 2
  H1=A%*%y #pagos al jugador 1
  
  
  s=which.min(H2) #jugador 2 intenta minimizar sus pagos
  t=which.max(H1) #jugador 1 intenta maximixar sus pagos
  
  
  #actualizamos las elecciones correspondientes al paso dado por esta iteración
  y[s]=y[s]+1 
  x[t]=x[t]+1
  
  #escribimos en una matriz las frecuencias relativas jugadas por ambos jugadores hasta la etapa jugada k
  Yk[k,]=y/k
  Xk[k,]=x/k
}