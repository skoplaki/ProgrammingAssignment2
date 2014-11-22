## Set the enviroment and calculate the inverser matrix

## setting the enviroment for lexical scoping

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function(){
    return (x)
  }
  
  setinverse <- function(yy){
    m <<- yy
  }
  
  getinverse <- function(){
    return (m)
  }
  
  list(set= set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## If matrix_inversed is not NULL it will be computed, 
## if not the old matrix_inversed will be returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix_inversed <- x$getinverse()
  
  if(!is.null(matrix_inversed)) {
    message("Getting cached data")
    return(matrix_inversed)
  }
  
  data <- x$get()
  matrix_inversed <- solve(data, ...)
  x$setinverse(matrix_inversed)
  return(matrix_inversed)
}
