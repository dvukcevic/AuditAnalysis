from utility.utility import CachedMethod, beta_binomial_cdf, beta_pdf, betafn, beta_cdffrom utility.hashable_array import Hashablefrom math import ceil, log, isinf, expfrom rpy2 import robjectsimport pandas as pdimport numpy as npfrom scipy.stats import norm, hypergeomclass Bayesian:    name = "bayesian"    def __init__(self, a=1, b=1, thresh=0.95,  min_stop=False):        self.a = a        self.b = b        self.thresh = thresh        self.min_stop = min_stop            @CachedMethod    def __call__(self, n, t, y_t):        if self.min_stop is not False and t < self.min_stop:            return False        k = ceil(n/2 - y_t)        p_reject = 1 - beta_binomial_cdf(k, y_t+self.a, t-y_t+self.b, n-t)        return p_reject > self.thresh    def __str__(self):        return f"{self.name}_{self.thresh*100:05.2f}_{self.a:02}_{self.b:02}"class HyperGeomBayesian:    name = "bayesian_without_replacement"    def __init__(self, a=1, b=1, thresh=0.95,  min_stop=False):        self.a = a        self.b = b        self.thresh = thresh        self.min_stop = min_stop    @CachedMethod    def __call__(self, n, t, y_t):        if self.min_stop is not False and t < self.min_stop:            return False        k = ceil(n/2 - y_t)        prob_p1 = 0        for i in range(ceil(n / 2), n+1):            prob_p1 += hypergeom.pmf(y_t, n, i, t) * beta_pdf(i/n, self.a, self.b)        prob_p0 = 0        for i in range(0, ceil(n / 2)):            prob_p0 += hypergeom.pmf(y_t, n, i, t) * beta_pdf(i/n, self.a, self.b)        return prob_p1/prob_p0 > self.thresh / (1 - self.thresh)    def __str__(self):        return f"{self.name}_{self.thresh*100:05.2f}_{self.a:02}_{self.b:02}"# BRAVOclass BRAVO:    name = "bravo"    quantiles = pd.DataFrame(        [            [12, 22, 38, 60, 131, 30],            [23, 38, 66, 108, 236, 53],            [49, 84, 149, 244, 538, 119],            [77, 131, 231, 381, 840, 184],            [93, 332, 587, 974, 2157, 469],            [301, 518, 916, 1520, 3366, 730],            [531, 914, 1619, 2700, 5980, 1294],            [1188, 2051, 3637, 6053, 13455, 2900],            [4725, 8157, 14486, 24149, 53640, 11556],            [18839, 32547, 57838, 96411, 214491, 46126]        ],        index=[0.7, 0.65, 0.60, 0.58, 0.55, 0.54, 0.53, 0.52, 0.51, 0.505],        columns=[0.25, 0.5, 0.75, 0.9, 0.99, "mean"]    )    def __init__(self, p, alpha, p_0=0.5):        """        :param p: The assumed true share        :param alpha: The risk limit        :param p_0: p_0 used for null hypothesis (default to 0.5)        """        self.alpha = alpha        self.p = p                self.y_val = log(p/p_0)        self.not_y_val = log((1-p)/p_0)        self.thresh = log(1/alpha)    @CachedMethod    def __call__(self, n, t, y_t):        y = y_t        not_y = t - y                # log(p/0.5)^y        sum_y_val = self.y_val * y        # log((1-p)/0.5)^(t-y)        sum_not_y_val = self.not_y_val * not_y                # total log(p/0.5)^y + log((1-p)/0.5)^(t-y) = log()        sum_val = sum_y_val + sum_not_y_val        return sum_val >= self.thresh    def __str__(self):        return f"{self.name}_{self.alpha*100:04.1f}_{self.p*100:02.0f}"# BRAVOclass HyperGeomBRAVO:    name = "bravo_without_replacement"    def __init__(self, p, alpha, p_0=0.5):        """        :param p: The assumed true share        :param alpha: The risk limit        :param p_0: p_0 used for null hypothesis (default to 0.5)        """        self.alpha = alpha        self.p = p        self.thresh = log(1/alpha)        self.p_0 = p_0    @CachedMethod    def __call__(self, n, t, y_t):        y = y_t        not_y = t - y        p1N = int(n * self.p)        p0N = int(n * self.p_0)        # log(top)        log_prob_p1 = hypergeom.logpmf(y, n, p1N, t)        # log(bot)        log_prob_p0 = hypergeom.logpmf(y, n, p0N, t)        # TODO handle the exception properly        if isinf(log_prob_p0) or isinf(log_prob_p1):            return False        # total log(top) - log(bottom)        ratio = log_prob_p1 - log_prob_p0        return ratio >= self.thresh    def __str__(self):        return f"{self.name}_{self.alpha*100:04.1f}_{self.p*100:02.0f}"class Clip:    name = "clip"    betas = pd.DataFrame(        [            [2.683, 2.500, 2.236, 2.000, 1.732, 1.155],            [2.887, 2.694, 2.425, 2.145, 1.877, 1.343],            [3.054, 2.864, 2.546, 2.294, 2.000, 1.414],            [3.184, 3.000, 2.670, 2.401, 2.095, 1.511],            [3.290, 3.077, 2.770, 2.496, 2.183, 1.633],            [3.357, 3.144, 2.828, 2.556, 2.240, 1.715],            [3.411, 3.206, 2.889, 2.638, 2.324, 1.747],            [3.487, 3.273, 2.958, 2.684, 2.375, 1.817],            [3.530, 3.309, 3.000, 2.734, 2.438, 1.890],            [3.560, 3.352, 3.040, 2.782, 2.474, 1.937]        ],        columns=[0.01, 0.02, 0.05, 0.1, 0.2, 0.5],        index=[100, 300, 1000, 3000, 10000, 30000, 100000, 300000,               1000000, 3000000])    @staticmethod    def _compute_beta(n, alpha, conservative=True):        """        Approximate compute beta based on values        """        if conservative:            const = 1        else:            const = 0.86        return 0.075 * np.log(n) + 0.7 * norm.isf(alpha) + const    def __init__(self, n, alpha, conservative=True):        self.n = n        self.alpha = alpha        if n in self.betas.index and alpha in self.betas.columns:            self.beta = self.betas.loc[n, alpha]        else:            self.beta = self._compute_beta(n, alpha, conservative)            @CachedMethod    def __call__(self, n, t, y_t):        a = y_t        b = t - y_t        return (a - b) > self.beta * np.sqrt(t)    def __str__(self):        return f"{self.name}_{self.n:06}_{self.alpha*100:04.1f}"class KMartRecursive:    name = "kmart"    def __init__(self, alpha, p_0=0.5):        self.alpha = alpha        self.p_0 = p_0    @CachedMethod    def integral_from_roots(self, c, maximal=True):        """        Integrate the polynomial \prod_{k=1}^n (x-c_j) from 0 to 1, i.e.,           \int_0^1 \prod_{k=1}^n (x-c_j) dx        using a recursive algorithm devised by Steve Evans.        If maximal == True, finds the maximum of the integrals over lower degrees:           \max_{1 \le k \le n} \int_0^1 \prod_{j=1}^k (x-c_j) dx        Input        ------        c : array of roots        Returns        ------        the integral or maximum integral and the vector of nested integrals        """        if isinstance(c, Hashable):            c = c.unwrap()        n = len(c)        a = np.zeros((n+1,n+1))        a[0,0]=1        for k in np.arange(n):            for j in np.arange(n+1):                a[k+1,j] = -c[k]*((k+1-j)/(k+1))*a[k,j]                a[k+1,j] += 0 if j==0 else (1-c[k])*(j/(k+1))*a[k,j-1]        integrals = np.zeros(n)        for k in np.arange(1,n+1):            integrals[k-1] = np.sum(a[k,:])/(k+1)        if maximal:            integral = np.max(integrals[1:])        else:            integral = np.sum(a[n,:])/(n+1)        return integral, integrals    @CachedMethod    def HK_ps_se_p(self, x, N, t, random_order=True):        """        p-value for the hypothesis that the mean of a nonnegative population with        N elements is t, computed using recursive algorithm devised by Steve Evans.        The alternative is that the mean is larger than t.        If the random sample x is in the order in which the sample was drawn, it is        legitimate to set random_order = True.        If not, set random_order = False.        If N = np.inf, treats the sampling as if it is with replacement.        If N is finite, assumes the sample is drawn without replacement.        Input:   x, array-like, the sample        ------   N, int, population size. Use np.inf for sampling with replacement                 t, double, the hypothesized population mean                 random_order, boolean, is the sample in random order?        Returns: p, double, p-value of the null        -------  mart_vec, array, martingale as elements are added to the sample        """        if isinstance(x, Hashable):            x = x.unwrap()        x = np.array(x)        assert all(x >= 0),  'Negative value in a nonnegative population!'        assert len(x) <= N, 'Sample size is larger than the population!'        assert N > 0,       'Population size not positive!'        if np.isfinite(N):            assert N == int(N), 'Non-integer population size!'        Stilde = (np.insert(np.cumsum(x),0,0)/N)[0:len(x)] # \tilde{S}_{j-1}        t_minus_Stilde = t - Stilde        mart_vec = np.ones_like(x, dtype=np.float)        if any(t_minus_Stilde < 0):            mart_max = np.inf        else:            jtilde = 1 - np.array(list(range(len(x))))/N            c = np.multiply(x, np.divide(jtilde, t_minus_Stilde))-1            r = -np.array([1/cc for cc in c[0:len(x)+1] if cc != 0])            Y_norm = np.cumprod(np.array([cc for cc in c[0:len(x)+1] if cc != 0]))            r = Hashable(r)            integral, integrals = self.integral_from_roots(r, maximal=False)            mart_vec = np.multiply(Y_norm,integrals)            mart_max = max(mart_vec) if random_order else mart_vec[-1]        p = min(1/mart_max,1)        return p, mart_vec    @classmethod    def count_to_vote(cls, t, y_t):        return np.concatenate([np.ones([y_t]), np.zeros([t-y_t])])    @CachedMethod    def __call__(self, n, t, y_t):        trials = self.count_to_vote(t, y_t)        trials = Hashable(trials)        p_mart, mart_vec = self.HK_ps_se_p(trials, n, self.p_0, random_order=False)        return p_mart < self.alpha    def __str__(self):        return f"{self.name}_{self.alpha*100:04.1f}"class TruncatedBayesian:    name = "truncated_bayesian"    # Should this be changed to thresh instead    def __init__(self, thresh=0.05, p_0=0.5, a=1, b=1):        self.thresh = thresh        self.p_0 = p_0        self.a = a        self.b = b        self.max_point_lookup = dict()    @CachedMethod    def __call__(self, n, t, y_t):        rejection = self.compute_upset_prob(t, y_t) < self.thresh        # Either rejected or it's too small to be rejected        return rejection    @CachedMethod    def compute_upset_prob(self, t, y_t):        # TODO refactor to more general class and files        betalnfn_ratio = betafn(y_t + self.a, t - y_t + self.b, log=True) - betafn(self.a, self.b, log=True)        betafn_ratio = exp(betalnfn_ratio)        betadist_ratio = (beta_cdf(1/2, y_t + self.a, t - y_t + self.b, lower_tail=False)                          / beta_cdf(1/2, self.a, self.b, lower_tail=False))        k_prime = 1/(2**(t+1)) + 1/2 * betafn_ratio * betadist_ratio        upset_prob = (1/(2**(t+1))) / k_prime        # TODO more verification here        # No log transformation        upset_prob - exp(-(t+1) * log(2) - log(k_prime))        return upset_probif __name__ == "__main__":    # verify clip compute    # for n in Clip.betas.index:    #     for alpha in Clip.betas.columns:    #         beta = Clip.betas.loc[n, alpha]    #         compute_beta = Clip._compute_beta(n, alpha)    #         if abs(beta - compute_beta) > 1e-3:    #             print(f"Warning: beta = {beta}, "    #                   f"compute_beta = {compute_beta}")    n = 500    t = 200    truncated_bayesian = TruncatedBayesian()    probs = [truncated_bayesian.compute_upset_prob(t, y_t) for y_t in range(t)]    rejections = [truncated_bayesian(n, t, y_t) for y_t in range(t)]    import matplotlib.pyplot as plt    plt.plot(range(t), probs)    plt.show()    pass