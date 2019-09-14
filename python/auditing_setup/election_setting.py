class Election:
    """
    Stores the current election information and is used to feed to the audit_method for initialisation of auditing.
    """
    def __init__(self, n, m, p=1/2, step=1, replacement=False):
        self.n = n
        self.m = m
        self.p = p
        self.step = step
        self.replacement = replacement

    def __str__(self):
        return "n={:06d}_m={:05d}_p={:.3f}_replacement={}_step={}".format(self.n, self.m, self.p, bool(self.replacement), self.step)

    def clone(self):
        return Election(self.n, self.m, self.p, self.step, self.replacement)
