## These functions crteates a special matrix object to cache the inverse of a matrix so that
## during large computation the inverse result from cache is used if available otherwise inverse
## is computed

## This function creates a special matrix object which defines functions to set and get the
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL                 # setting i to reserve a place in memory for future value        
        set<- function(y){      # define a fun to set matrix x to a new matrix y
                x<<-y
                i<<-NULL        # reset i to null
        }
        get<-function()x        # return the matrix x 
        setinverse<- function(solve) i<<- solve # sets i to inverse of matrix
        getinverse<- function() m       # returns inverse of matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        # returns the special matrix object containing all functions just defined above   

}


## This function calculates the inverse of special matrix object created above.If the inverse
## is available in cache it uses that and skips calculation.otherwise it calculates the inverse
## of data and sets the value of inverse in cache by setinverse function

cacheSolve <- function(x, ...) {
        i<-x$getinverse() # i is assigned result of getinverse function on x
        if(!is.null(i)){                # if i is not null 
                message("getting cached data")
                return(i)                       #returns i
        }
        
        data<- x$get()                  # data is assigned value of function get on x
        i<- solve(data, ...)            # i is assigned inverse of data, ... denotes other args
        x$setinverse(i)                 # x get assigned inverse i
        i                               
                                        
}
