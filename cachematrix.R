#Caching inverse of matrix

#Creating a matrix that can cache its inverse.
#Below function get/set value and get/set inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        #Setter for matrix
        set<-function(t){
                x<<-t
                m<<-NULL
        }
        #Getter for matrix
        get<-function()x
        setinver<-function(inverse) m <<-inverse
        getinver <-function() m
        #list of function of Matrix
        list(set=set, get=get, setinver=setinver, getinver=getinver)
}


## Computes the inverse of Matrix.

cacheSolve <- function(x, ...) {
        ## Return already cached matrix 
        m<-x$getInverse()
        if(!is.null(m)){
                message("retrieveing cache")
                return(m)
        }
        #Inverse of matrix is computed
        me<-x$get()
        m<-solve(me,...)
        #Cache inverse
        x$setInverse(m)
        #Return inverse of matrix
        return(m)
}