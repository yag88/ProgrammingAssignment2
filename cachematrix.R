# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there 
# are also alternatives to matrix inversion that we will not discuss here). Your
# assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# 
# For this assignment, assume that the matrix supplied is always invertible.

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.  Computing the inverse of a square matrix can be done with
# the solve function in R. For example, if X is a square invertible matrix, then
# solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(invertedm) inv <<- invertedm
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      loc_mat <- x$get()
      inv <- solve(loc_mat, ...)
      x$setinv(inv)
      inv
}

## try on : 
## my_matrix <- matrix(c(4,7,2,6),2,2)
# > solve(my_matrix)m
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# OR
# > my_matrix2 <- matrix(c(1,0,3,2,2,4,3,2,1),ncol=3)
# > solve(my_matrix2)
# [,1]       [,2]       [,3]
# [1,]  0.5 -0.8333333  0.1666667
# [2,] -0.5  0.6666667  0.1666667
# [3,]  0.5 -0.1666667 -0.1666667
# 
##############################
# Result of the trial: 
# 
# > Smy_matrix <- makeCacheMatrix(matrix(c(4,7,2,6),2,2))  ## create special matrix instanciation
# 
# > Smy_matrix$get()    ## we check the content of the matrix is correct
# [,1] [,2]
# [1,]    4    2
# [2,]    7    6
# > Smy_matrix$getinv()  ## we cannot get the mean directly, it's not yet been calculated
# NULL
# > cacheSolve(Smy_matrix) ## we calculate and cache the inverted matrix in Smy_matrix
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > Smy_matrix$getinv()  ## now the inverted matrix is there
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > cacheSolve(Smy_matrix) ## if we calculate again, the function tells us it's using the cache value.
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
