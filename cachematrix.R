## This program caches the result of inverse of  square invertible matrix, 
## which is a costly opertaion. 
## The code advantage of the scoping rules of the R language and how they can be manipulated 
## to preserve state inside of an R object.

## For example consider the below example and steps to test the code
## > source("inversematrix.R")
## > c=rbind(c(1, -1/4), c(-1/4, 1))  
## > c
##      [,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00
## mat <-makeCacheMatrix(c)
## For the first time
##> cacheSolve(mat)
##          [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
## getting cache value
##> cacheSolve(mat)
##getting cached data
##          [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
		# save the value of matrix in scope variable
                x <<- y
                inv <<- NULL
        }
        get <- function() x
		# save the inverse in scope variable
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		# if the inverse is present then return it from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        # compute the inverse using solve function
		inv <- solve(data, ...)
		#store the result for future use
        x$setinverse(inv)
        inv	

}

