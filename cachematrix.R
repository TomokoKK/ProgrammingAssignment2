## This function creates a special "matrix" object that can cache its inverse
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix


makeCacheMatrix <- function(X = matrix()) {
  TT <- NULL
  TSET <- function(Y) {
    X  <<- Y
    TT <<- NULL
  }
  TGET <- function() X
  TSETINV <- function(tinv) TT <<- tinv
  TGETINV <- function() TT
  list(TSET=TSET, TGET=TGET, TSETINV=TSETINV, TGETINV=TGETINV)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.)

cacheSolve <- function(X, ...) {
  TT <- X$TGETINV()
  if(!is.null(TT)){
    message("getting cached data")
    return(TT)
  }
  indt <- X$TGET()
  TT <- solve(indt, ...)
  X$TSETINV(TT)
  TT
}
