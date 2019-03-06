import os, sys, glob
d = os.environ['HOME']
if len(sys.argv) == 1 :
    os.system("pdfunite \
    pde_00/pde_00.pdf \
    pde_01/pde_01.pdf \
    pde_02/pde_02.pdf \
    pde_03/pde_03.pdf \
    pde_04/pde_04.pdf \
    pde_heat/pde_heat.pdf \
    pde_heat_deriv/pde_heat_deriv.pdf \
    pde_level/pde_level.pdf \
    pde_lk/pde_lk.pdf \
    pde_curvature/pde_curvature.pdf \
    pde_wave_deriv/pde_wave_deriv.pdf \
    pde_varcalc/pde_varcalc.pdf \
    " + d + "/Downloads/pde.pdf" \
    )
    exit()
    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("pde*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape %s" % glob.glob("pde_*.tex")[0] )
        os.chdir("..")
               
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
    
elif sys.argv[1] == 'tex':
    file = glob.glob('pde_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
