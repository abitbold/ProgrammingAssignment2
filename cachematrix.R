## Put comments here that give an overall description of what your
## functions do

## This function will create a matrix able to stro its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  #Function used to create the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  # Function used to get the matrix
  get <- function() x
  
  # Function used to stro the inverse matrix
  setInverse <- function(i) inv <<- i
  
  #Function used to get the inverse
  getInverse <- function() inv
  
  #return the listx
  list(get = get, set = set,
       getInverse = getInverse, setInverse = setInverse)
}


## This function will check if the inverse already exists and if not will compute it

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          print("taking inverse from cache")
          return(inv)
        } else{
          mtx <- x$get()
          inv <- solve(mtx)
          x$setInverse(inv)
          inv
        }
}



