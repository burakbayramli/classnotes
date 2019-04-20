import os, sys, glob

d = os.environ['HOME']
if len(sys.argv) == 1 :
    os.system("pdfunite \
    phy_lagrange/phy_lagrange.pdf \
    phy_varcalc/phy_varcalc.pdf \
    " + d + "/Downloads/phy.pdf" \
    )
    exit()
    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("pny*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape %s" % glob.glob("phy_*.tex")[0] )
        os.chdir("..")
        
    
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
            
elif sys.argv[1] == 'tex':
    file = glob.glob('phy_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
