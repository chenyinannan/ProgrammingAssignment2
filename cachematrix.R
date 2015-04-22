## Put comments here that give an overall description of what your
## functions do

## makeCahcheMatrix function: creates a special "Matrix" object that can cache its inverse
## It will result a list containing a list of function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  setMat <- function (y) {
    x<<-y
    invMat <<- NULL   
  }
  getMat <- function () x 
  setInvMat<- function (inv) invMat <<- inv
  getInvMat <- function () invMat
  list (setMat = setMat, getMat = getMat,
        setInvMat = setInvMat, getInvMat = getInvMat)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse
## matrix from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInvMat()
  if (!is.null(invMat)) {
    message ("getting the cached data !")
    return (invMat)
  }
  invMat <- solve(x$getMat())
  x$setInvMat(invMat)
  invMat
}