# This code cache the inverse of a matrix.
# Function makeCacheMatrix creates a special matrix object that can cache its inverse.
# Function cacheSolve computes the inverse of a given matrix; if the inverse has already been calculated (and the given matrix has not changed),
# then this function retrieves the inverse from the cache.
# These functions assume that the matrix supplied is always invertible (square matrix with non-zero determinant).

# Besides, there is a commented alternative code block at the end that calculates a generalization of the inverse matrix, meaning the pseudoinverse of a matrix.
# This is done using ginv from the MASS library and allows for the given matrix to not be invertible. However, if the given matrix is 
# invertible, then its pseudoinverse is its inverse.
# ----------------------------------------------------------------------------------------------

# makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL       # initializes invmat as an empty object
  # defines 4 functions to access (getters) and change (setters) data within makeCacheMatrix 
  set <- function(y) { 
    x <<- y      # the <<- operator assigns the input y (from the current function environment) to x (in the parent environment of makeCacheMatrix)
    invmat <<- NULL # assigns NULL to invmat in the parent environment, so that every time x is reset, invmat is cleared
  }
  get <- function() x # since x is not defined within get(), it will be retrieved from the parent environment of makeCacheMatrix
  setinv <- function(z) invmat <<- z # the <<- operator assigns the input argument z (from the current function environment) to invmat (in the parent environment of makeCacheMatrix)
  getinv <- function() invmat # since invmat is not defined within getinv(), it will be retrieved from the parent environment of makeCacheMatrix
  # returns a list containing the 4 functions created within the makeCacheMatrix object environment to the parent environment, so the parent environment will also have access to the entire environment defined by makeCacheMatrix
  # naming the list elements (as set, get, setinv and getinv) allows the use of the $ form of the extract operator to access the functions by name 
  list(set = set, 
       get = get, 
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve computes the inverse of a given (invertible) matrix; if the inverse has already been calculated (and the given matrix has not changed),
# then this function retrieves the inverse from the cache 

cacheSolve <- function(x, ...) { 
  invmat <- x$getinv()     # retrieves the value stored in the object (type makeCacheMatrix) passed in as the argument
  if(!is.null(invmat)) {   # checks if the inverse matrix was previously computed to decide whether to keep the data retrieved from cache or to calculate a new inverse matrix
    message("getting cached data")
    return(invmat)   # returns the inverse matrix previously calculated retrieved from cache
  }
  data <- x$get()            # gets the matrix to be inverted from the input object (of the type makeCacheMatrix)
  invmat <- solve(data, ...) # computes the inverse matrix
  x$setinv(invmat)           # uses the setinv() function on the input object to pass the recently calculated inverse matrix into the input object 
  ## Returns a matrix that is the inverse of 'x' ('x' passed in as the argument to makeCacheMatrix)
  invmat
}


# ----------------------------------------------------------------------------------------------
## Examples:
## ex. 1
# a <- makeCacheMatrix(matrix(1:4, 2, 2)) # call makeCacheMatrix for a square invertible matrix
# a$get() # shows your given matrix (in this example, a 2x2 matrix)
# a$getinv() # when running this before running cacheSolve, you will get NULL; after, it will return the inverse matrix
# cacheSolve(a) # call cacheSolve for a input argument type makeCacheMatrix, that will return the inverse matrix; when you run it for the second time without changing your given matrix, it will show the cached inverse.
## ex. 2
# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# a <- makeCacheMatrix(m1)
# cacheSolve(a)
## ex. 3
# n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), 2, 2)
# a$set(n2)    # taking advantage of the set(), i.e. feeding another matrix as input without calling makeCacheMatrix again
# cacheSolve(a)

## Alternative code (using ginv)
# library(MASS)
# makeCacheMatrix <- function(x = matrix()) {
#         invmat <- NULL
#         set <- function(y) {
#                 x <<- y
#                 invmat <<- NULL
#         }
#         get <- function() x
#         setinv <- function(z) invmat <<- z
#         getinv <- function() invmat
#         list(set = set, get = get,
#              setinv = setinv,
#              getinv = getinv)
# }
# 
# cacheSolve <- function(x, ...) {
#         invmat <- x$getinv()
#         if(!is.null(invmat)) {   
#                 message("getting cached data")
#                 return(invmat)   
#         }
#         data <- x$get()
#         invmat <- ginv(data, ...) 
#         x$setinv(invmat)
#         invmat
# }
# 
## Examples:
## ex. 1
#  a <- makeCacheMatrix(matrix(1:8, 2, 4)) # call makeCacheMatrix for any matrix composed by real or complex numbers
#  a$get() # shows your given matrix (in this example, a 2x4 matrix)
#  a$getinv() # when running this before running cacheSolve, you will get NULL; after, it will return the pseudoinverse matrix
#  cacheSolve(a) # call cacheSolve, that will return the pseudoinverse matrix; when you run it for the second time without changing your given matrix, it will show the cached pseudoinverse.
## ex. 2
#  n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), 2, 2)
#  a$set(n2)
#  cacheSolve(a)
