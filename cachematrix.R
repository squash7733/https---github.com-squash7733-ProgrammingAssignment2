
## This function sets up and returns a list of four functions
## set: Input matrix 
## get: the matrix
## setinverse: obtains the inverse of the matrix 
## getinverse: gets the inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
   setinverse <- function(x) m <<- solve(x)
   getinverse <- function() m
   
    lx <- list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Gets the Inverse of a matrix if not already in cache

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
# Matrix Inverse not in cache - so have to compute it fresh
    
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    return(m)
}

######  end of code ########################################################

# Tests the above code #

a <- matrix(runif(36), 2, 2)
a1 <- makeCacheMatrix(a)
a1$get()               # retrieve the value of x
a1$getinverse() 
                       # retrieve the value of m, which should be NULL

ab <- matrix(rnorm(16), 4, 4)   # new matrix

a1 <- makeCacheMatrix(ab)
a1$set(ab)          # reset value with a new matrix

a1$setinverse(ab)           # inverse calculated is for new matrix ab

# m2 <- a1$getinverse() 
# print(m2)

print(ab)

m1 <- cacheSolve(a1)


print(ab %*% m2)  # to check that we get Identity matrix when multiplying by the inverse
#  Note: Any non-zero off-diagonal entries are close to zero

