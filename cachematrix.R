makeCacheMatrix <- function(x = matrix()) {
  
  gama <- NULL
  set <- function(y) {
    x <<- y
    gama <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) gama <<- inverse
  getinverse <- function() gama
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}




cacheSolve <- function(x, ...) {
  gama <- x$getinverse()
  if (!is.null(gama)) {
    message("getting cached data")
    return(gama)
  }
  data <- x$get()
  gama <- solve(data, ...)
  x$setinverse(gama)
  gama
}



#Test
A <- matrix(c(1,8,314),2,2)
Alfa <- makeCacheMatrix(A)
cacheSolve(Alfa)


Tita <- matrix(c(224,333,555,666),2,2)
Beta <- makeCacheMatrix(Tita)
cacheSolve(Beta)
