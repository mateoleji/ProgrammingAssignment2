

## la funcion makeCacheMatrix realiza un objeto especial de matriz, que almacena en el cache de manera inversa.
makeCacheMatrix <- function(x = matrix())
{
 i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
}
get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## la  función calcula el inverso de la "matriz" especial devuelta por makeCacheMatrix

cacheSolve <- function(x, ...) 
{
        ## Devuelve una matriz que es la inversa.
        i <- x$getinverse()
  if (!is.null(i)) 
  {
    message("obtener datos en el cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
