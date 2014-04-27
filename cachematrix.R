## makeCacheMaxtrix is a function that creates special "matrx" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {  ## you can define matrix by using set function
        x <<- y          
        m <<- NULL       ## whenever new matrix is set, make its inverse NULL
        
    }
    
    ## get function will return the original matrix            
    get <- function() x  
    ## set_matrix function will get the inverse of original matrix
    set_matrix <- function() m <<- solve(x)
    ## get_matrix function will leverage from m but not solve
    get_matrix <- function() m
        
    list(set=set, get=get, get_matrix=get_matrix, set_matrix=set_matrix)
    
    
}

## cacheSolve is a function that computes the inverse of special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x' conditional on cache
        
    get_inv <- x$get_matrix() ## storing value of get_matrix() in get_inv
    
    if(!is.null(get_inv)) {   ## if value has been calculated
        message("getting cached inverted matrix")  ## then message
        return(get_inv)       ## and use it from cache
    }
    
    else 
        get_inv <- solve(x$get())  ## else solve it
        message("solving inverted matrix")
        get_inv  ## and print it
    
}
