import bdm

bdm.run("uk_emu_cons.csv",iter=10)
print 
bdm.run("uk_emu_labor.csv",iter=10)
print 
bdm.run("emission.csv",iter=10,xmin=4)
print 
bdm.run("iran.csv",iter=4)
