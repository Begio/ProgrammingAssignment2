## In this example we introduce the <<- operator which can be used to assign a value to 
## an object in an environment that is different from the current environment. 

## makeCacheMatrix: 
## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function( mtx = matrix() ) {
  
  ## initialize inverse as NULL 
  imtx <- NULL
  
  ## methods  
  mset <- function(x) {
    mtx <<- x
    imtx <<- NULL #reset imtx for new matrix
  }
  
  mget <- function() {
    mtx
  }
  
  msetInverse <- function(x) {
    imtx <<- x
  }
  
  mgetInverse <- function() {
    imtx
  }
  
  ##list of the methods
  list(set = mset, 
       get = mget,
       setInverse = msetInverse,
       getInverse = mgetInverse
       )
}

## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## first, return the inverse if its already set
  imtx <- x$getInverse()
  
  if( !is.null(imtx) ) {
    message("Already set inverse; getting cached data")
    return(imtx)
  }

  ## if not, return the inverse   
  ## get the matrix from our object
  mtx <- x$get()
  
  ## calculate inverse
  ## for this assignment, assume that the matrix supplied is always invertible.
  ## then solve(X) returns its inverse.
  imtx <- solve(mtx)
  
  ## set inverse
  x$setInverse(imtx)
  
  ## return the matrix
  imtx
}

## TEST
## A <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
## mtx <- makeCacheMatrix(A)
## cacheSolve(mtx)

## [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500

## cacheSolve(mtx)
## Already set inverse; getting cached data
## [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500
