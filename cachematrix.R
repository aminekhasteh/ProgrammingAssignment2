## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # First, we create the object m, corresponding to the inverse of x
        m <- NULL
        # When the set() function is executed, two thing happen:
        # 1) Assign the input argument to the x object in the parent environment
        # 2) Assign the value of NULL to the m object in the parent environment. 
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        # Getting the matrix function
        get <- function() x
        # Setting the inverse matrix withing the function
        setinverse <- function(inverse) m <<- inverse
        # Getting the inverese of the matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        # Inititally, this function tries to get the inverese 
        # of the given matrix, if exists in the makeCacheMatrix() function
        # If it doesn't exist then it'll assign NULL to it
        m <- x$getinverse()
        # If m is not NULL (The inverese exists in makeCacheMatrix() function)
        # Then it'll look up the inverese and returns it
        if(!is.null(m)){
                message("getting cashed data")
                return(m)
        }
        # If the inverese is NULL, then this function calulates the inverese
        # By using the solve function and set it to m
        matrix <- x$get()
        m <- solve(matrix, ...)
        # Then it sets the inverse of the matrix to makeCacheMatrix() function
        x$setinverse(m)
        # Finally returns the inverese
        m
}


