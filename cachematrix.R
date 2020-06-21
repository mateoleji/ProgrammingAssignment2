

makeCacheMatrix <- function(x = matrix()) 
{
j <- NULL
   set <- function(y){
       x <<- y
       j <<- NULL
       }
     get <- function()x
     setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse) 
}

cacheSolve <- function(x, ...)
{
      j <- x$getInverse()
   if(!is.null(j)){
       message("getting cached data")
       return(j)
       }
     mat <- x$get()
     j <- solve(mat,...)
     x$setInverse(j)
     j
 }
 
 makeCacheMatrix <- function(x = matrix()) {
   
   inv <- NULL
   set <- function(y){
     x <<- y
     inv <<- NULL
   }
   get <- function() {x}
   setInverse <- function(inverse) {inv <<- inverse}
   getInverse <- function() {inv}
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }
 
 cacheSolve <- function(x, ...) {
   ## Devuelve una matriz que es la inversa de 'x'
   inv <- x$getInverse()
   if(!is.null(inv)){
     message("obtener datos en caché")
     return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
 }
 
 acheMatrix <- function( m = matrix() ) {    
   ## Inicializa la propiedad inversa 
   i <- NULL    
   ## Método para configurar la matriz
   set <- function( matrix )  {   
     m <<- matrix    
     i <<- NULL  }    ## Metodo para obtener la matriz
   get <- function()
   {    ## Devuelve la matriz
     m  
   }  
   ## Metodo para configurar la inversa de la matriz 
   setInverse <- function(inverse)
   {   
     i <<- inverse  
   }    
   ## Metodo para obtener la inversa de la matriz 
   getInverse <- function()
   {   
     ## Regresa la propiedad inversa
     i  }    
   ## Devuelve una lista de los metodos
   list(set = set, get = get,setInverse = setInverse, getInverse = getInverse )
 }
 
 
 cache.cacheSolve <- function(x, ...)
 { 
   ## Devuelve una matriz que es la inversa de 'x' 
   m <- x$getInverse()   
   ## Solo regresa la inversa si esa esta configurada
   if( !is.null(m) )
   {  
     message("getting cached data")  
     return(m) 
   }    
   ## Obtiene la matriz de nuestro objeto 
   data <- x$get()   
   ## Calcula la inversa usando la multiplicacion de la matriz  
   m <- solve(data) %*% data   
   ## Configura la inversa para el objeto 
   x$setInverse(m)   
   ## Devuelve la matriz
   m  
}
