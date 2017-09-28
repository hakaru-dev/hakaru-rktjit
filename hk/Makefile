benchmarks: clinicalTrial_simp.hk linearRegression_simp.hk gmm_gibbs_simp.hk naive_bayes_gibbs_simp.hk lda_gibbs_simp.hk

%_simp.hk: %.hk
	hk-maple --timelimit=600 $< | ./unsample > $@
