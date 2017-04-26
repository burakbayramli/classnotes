import os, sys, glob

if len(sys.argv) == 1 :
    cmd = "pdftk " + \
    "./stat_00/stat_00.pdf " + \
    "./stat_intro/stat_intro.pdf " + \
    "./stat_cov_corr/stat_cov_corr.pdf " + \
    "./stat_cond/stat_cond.pdf " + \
    "./stat_summary/stat_summary.pdf " + \
    "./stat_sampling/stat_sampling.pdf " + \
    "./stat_ci/stat_ci.pdf " + \
    "./stat_tests/stat_tests.pdf " + \
    "./stat_tests2/stat_tests2.pdf " + \
    "./stat_multigauss/stat_multigauss.pdf " + \
    "./stat_kl/stat_kl.pdf " + \
    "./stat_linreg/stat_linreg.pdf " + \
    "./stat_fxu/stat_fxu.pdf " + \
    "./stat_powerlaw/stat_powerlaw.pdf " + \
    "./stat_predint/stat_predint.pdf " + \
    "./stat_logit/stat_logit.pdf " + \
    "./stat_count/stat_count.pdf " + \
    "./stat_multlev/stat_multlev.pdf " + \
    "./stat_gauss_fusion/stat_gauss_fusion.pdf " + \
    "./stat_mcmc/stat_mcmc.pdf " + \
    "./stat_coal/stat_coal.pdf " + \
    "./stat_mixbern/stat_mixbern.pdf " + \
    "./stat_gmm/stat_gmm.pdf " + \
    "./stat_lmm/stat_lmm.pdf " + \
    "./stat_regular/stat_regular.pdf " + \
    "./stat_preproc/stat_preproc.pdf " + \
    "./stat_ratings/stat_ratings.pdf " + \
    "./stat_assoc/stat_assoc.pdf " + \
    "./stat_worldcup/stat_worldcup.pdf " + \
    "./stat_hadoop_patent/stat_hadoop_patent.pdf " + \
    "./stat_hadoop_logreg/stat_hadoop_logreg.pdf " + \
    "./stat_multiout/stat_multout_reg.pdf " + \
    "./stat_naive/stat_naive.pdf " + \
    "./stat_pcd/stat_pcd.pdf " + \
    "./stat_boltzmann/stat_boltzmann.pdf " + \
    "./stat_rbm/stat_rbm.pdf " + \
    "./stat_appendix/stat_appendix.pdf " + \
    "output ../../Dropbox/Public/skfiles/stat.pdf"
    
    print cmd
    os.system(cmd)
    exit()
    
elif sys.argv[1] == 'all':
    dlist = glob.glob("stat_*")
    dlist = [x for x in dlist if x != 'stat_mr_rnd_svd']
    for a in sorted(dlist):
        os.chdir(a)
        os.system("pdflatex -shell-escape %s" % a)
        os.chdir("..")
        
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")

elif sys.argv[1] == 'tex':
    file = glob.glob('stat_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
    
