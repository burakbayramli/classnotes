import os, sys, glob

if len(sys.argv) == 1:
    cmd = """
    pdftk \
    ./tser_00/tser_00.pdf \
    ./tser_intro/tser_intro.pdf \
    ./tser_back/tser_back.pdf \
    ./tser_stoc/tser_stoc.pdf \
    ./tser_ar/tser_ar.pdf \
    ./tser_mean/tser_mean.pdf \
    ./tser_coint/tser_coint.pdf \
    ./tser_mrimp/tser_mrimp.pdf \
    ./tser_rwtst/tser_rwtst.pdf \
    ./tser_futures/tser_futures.pdf \
    ./tser_curr/tser_curr.pdf \
    ./tser_mom/tser_mom.pdf \
    ./tser_port/tser_port.pdf \
    ./tser_kelly/tser_kelly.pdf \
    ./tser_voltar/tser_voltar.pdf \
    ./tser_hmm/tser_hmm.pdf \
    ./tser_kf/tser_kf.pdf \
    ./tser_pf/tser_pf.pdf \
    ./tser_ukf/tser_ukf.pdf \
    ./tser_int/tser_int.pdf \
    ./tser_bubble/tser_bubble.pdf \
    ./tser_peak/tser_peak.pdf \
    ./tser_conv/tser_conv.pdf \
    ./tser_freq1/tser_freq1.pdf \
    ./tser_freq2/tser_freq2.pdf \
    ./tser_sinreg/tser_sinreg.pdf \
    ./tser_sound/tser_sound.pdf \
    ./tser_z001/tser_z001.pdf \
    output ../../Dropbox/Public/skfiles/tser.pdf
    """
    os.system(cmd)
    exit()
    
elif sys.argv[1] == 'all':
    dlist = glob.glob("tser*")
    for a in sorted(dlist):
        os.chdir(a)
        os.system("pdflatex -shell-escape %s" % a)
        os.chdir("..")
    
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
    os.system("find . -name '*.aux' | xargs rm  -rf")
    os.system("find . -name '*.pyc' | xargs rm  -rf")
    os.system("find . -name '*.log' | xargs rm  -rf")
    os.system("find . -name '*.out' | xargs rm  -rf")
    
elif sys.argv[1] == 'tex':
    file = glob.glob('tser_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
