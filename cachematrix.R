## This Function creates a special matrix the inverse of which can be cached. 
## It also creates a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { #Function that changes the matrix stored in main function
                x <<- y #Substitutes the main matrix x with y in main function
                m <<- NULL #Restores the mean value of m to null
        }
        get <- function() x #Returns the matrix x stored in main function
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m #Returns the inverse of matrix
        list(set = set, get = get, #Stores all 4 functions in makeCacheMatrix
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function calculates the inverse of the special matrix in the above function. 
##First it checks t see if inverse has already been calculated if so it retrieves it from the cache, 
##if not then it calculates the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) { #Checks if m value stored by getinverse is not null
                message("getting cached data")
                return(m) #If m value exits in memory return m value
        }
        data <- x$get() #If m value stored by getinverse is null then get matrix stored in makeCacheMatrix
        m <- solve(data, ...) #Caclulates the inverse of the matrix now assigned to data
        x$setinverse(m) #Stores inverse of matrix in object assigned in MakeCache Matrix
        m
        ## Return a matrix that is the inverse of 'x'
}