## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){ ##set the new matrix to your variablex which is initialised as a matrix vector.
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##get the matrix that you have set previously.
  setinverse <- function(inverse) inv <<- inverse ##set your matrix passed as an argument to the function to the inv variable. 
  getinverse <- function() inv ##get the inverse of mmatrix.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ##get the inverse of matrix that you have set in the makeCacheMatrix function call.
  if (!is.null(inv)){ ##check if it is null or not.
    message('getting cached data')
    return(inv)
  }
  data <- x$get() ##get the value of the matrix passed oriiginally to the first makeCacheMatrix function call.
  inv <- solve(data, ...) ##use solve function to find inverse of matrix.
  x$setinverse(inv) ##reset the inv variable according to the inverse of matrix. If it is same as inv in first line, there is no change else, the value is changed to the inverse of the matrix passed originally.
  inv
        ## Return a matrix that is the inverse of 'x'
}
