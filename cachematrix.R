# Matrix inversion is usually a costly computation and there
# may be some benefit to caching the inverse of a matrix rather
# than compute it repeatedly (as in loops). The following two 
# functions are used to cache the inverse of a matrix.

# makeCacheMatrix is a list containing four functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

# The input to makeCacheMatrix is a variable x of type matrix
# Example: x <- matrix(1:4, nrow=2, ncol=2)

makeCacheMatrix <- function(x = matrix()) {
        # inv changes when the user sets the x value
        inv <- NULL
        
        # <<- operator assigns the matrix in a different environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get function gets the matrix but not the inverse
        get <- function() x
        
        # Sets the inverse in a different environment
        setinverse <- function(inverse) inv <<- inverse
        
        # Gets the inverse
        getinverse <- function() inv
        
        # Encapsulates into a list
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)	
}

# cacheSolve: This function computes the inverse of the special
# matrix" returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), 
# then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # If getinverse has already been computed then inv will
        # not be NULL anymore
        inv <- x$getinverse()
        
        # If inv is not NULL then...
        if(!is.null(inv)) {
                # ...the inv value is returned from cache		
                message("Getting cached matrix")
                return(inv)
        }
        # If inv was NULL then get the matrix
        data <- x$get()
        
        # R solve function returns the inverse of x matrix
        inv <- solve(data, ...)
        
        # Caches this result in the object
        x$setinverse(inv)
        
        # Returns this new result
        inv    
}

# Testing:
# x <- matrix(1:4, nrow=2, ncol=2)
# We check that the matrix is correct by printing x:
# x
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

# We have to convert the matrix x to a list (I named it makeCM) 
# in order for the $ operator to work:
# makeCM <- makeCacheMatrix(x)

# Now we can compute the inverse of the x matrix:
# cacheSolve(makeCM)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# We compute the inverse again. Since it has already been
# calculated, the cachesolve retrieves the inverse from the cache:
# cacheSolve(makeCM)
# 'Getting cached matrix' message is displayed before the result
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
