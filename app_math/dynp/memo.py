from functools import wraps

def memo(func):
    cache = {}                                  
    @wraps(func)                                
    def wrap(*args):                            
        if args not in cache:
            print 'onbellekte yok -', args[0]
            cache[args] = func(*args)
        else: print 'onbellekte var -', args[0]
        return cache[args]                      
    return wrap 
