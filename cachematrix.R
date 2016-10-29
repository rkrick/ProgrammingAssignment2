# Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
# Coursera - R Programming -  by Johns Hopkins University
# 
# makeCacheMatrix get a matrix as argument and prepares its own environment with the following
# functions: 
# set(x) -> will be called internally when we instantiate makeCacheMatrix
# get()  -> returns the value of x, set when makeCacheMatrix was instantiated
# setSolved() -> a function saves the inverted value of the matrix. As stated in the assignment,
#                 we should expect all matrixes to be inversable for this exercise
# getSolved() -> a function that returns the inversed matrix, already calculated with solve (setSolved)
makeCacheMatrix <- function(x = matrix()) {
  solvedMatrix <- NULL
  set <- function(y) {
    x <<- y
    solvedMatrix <<- NULL
  }
  get <- function() x
  setSolved <- function(invertedMatrix) solvedMatrix <<- invertedMatrix
  getSolved <- function() solvedMatrix
  list(set=set, get=get, setSolved=setSolved, getSolved=getSolved)
}


# The cacheSolve function implements a kind of cache for the calculated reversed matrixes. 
# If the function is called with a matrix for which we already have calculated the inverted matrix
# it will return if from cache. If not, the inverted matrix will be calculated using the solve function,
# the value will be cached using setSolved() and the calculated value will be returned to the caller
# 
cacheSolve <- function(x, ...) {
  solvedMatrix <- x$getSolved()
  if(!is.null(solvedMatrix)) {
    message("Already computed. Getting from cache")
    # return the value to caller
    return(solvedMatrix)
  }
  tempMatrix <- x$get()
  solvedMatrix <- solve(tempMatrix)
  x$setSolved(solvedMatrix)
  # return the value to caller
  solvedMatrix
}
# Do you want to test it? Here some examples, extracted from 
# https://de.wikipedia.org/wiki/Inverse_Matrix
# x <- matrix(c(2,1,5,3),nrow=2,ncol=2)
# matrix <- makeCacheMatrix(x)
# The first time the value will be computed
#      [,1] [,2]
#[1,]    3   -5
#[2,]   -1    2
#
# The second time it will get it from cache
# cacheSolve(matrix)
# Already computed. Getting from cache
# [,1] [,2]
# [1,]    3   -5
# [2,]   -1    2

# For the second example use
# x <- matrix(c(1,2,2,2,4,1,0,1,0),nrow=3,ncol=3)