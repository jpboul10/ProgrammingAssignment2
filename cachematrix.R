makeCacheMatrix <- function(x = matrix()) 
  
        {
        ## Initializing x and j.
        x <<- y
        j <- NULL
        ## Setting the function.
        set <- function(y) 
          
                {
                x <<- y
                j <<- NULL
                }
        ##Getting the function.
        get <- function() x
        
        ##Set and get inverse functions.
        setinverse <- function(inverse) j <<- inverse
        getinverse <- function() j
        list(set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
        }

##Solver.
cacheSolve <- function(x) 
        {
        j <- x$getinverse()
        ##Return the matrix if it is not null.
        if (!is.null(j)) 
                {
                return(j)
                }
        data <- x$get()
        ##Using matlab package to get "solve()" function.
        j <- solve(data)
        x$setinverse(j)
        j
        }