<h5>Development Level: <span style="text-color:green">Beta</span></h5>

# IntegrativeHTEcf
Integrative analysis of heterogeneous treatment effect with confounding function that combines randomize trial and real-world evidence studies.

❗❗❗This is a late stage beta release. If you encounter any issues or have suggested changes, please reach out to shannon.holloway at duke.edu.

<h4>Usage</h4>
<pre>
IntHTEcf(data.rct, data.rwe, cfName = NULL, ...,
         ps.rct = NULL,
         outcome.type = c("cont", "bin"),
         outcome.method = c("gam", "glm", "SL"),
         outcome.controls = list(family = "gaussian"),
         ps.method = c("glm", "gam", "SL"),
         ps.controls = list(family = "binomial"),
         n.boot = 100L,
         optim.controls = list(method = "L-BFGS-B")
)
</pre>

<h5>Abbreviated Formal Argument Descriptions</h5>

- data.rct: The value object returned by *dataInput()* for the
data from a randomized clinical trial (RCT). 
- data.rwe: The value object returned by *dataInput()* for the
data from a real-world evidence (RWE) study.
- cfName: The names of the covariates of the confounding function.
- ...: Ignored
- ps.rct: Optional input providing a vector of known propensity
  scores P(A=1) for the RCT dataset.
- outcome.type: The type of outcome. 
- outcome.method: The regression method for outcomes.
- outcome.controls: Additional inputs provided to the specified outcome regression method.
- ps.method: The regression method for propensity score analysis.
- ps.controls: Additional inputs provided to the specified propensity score regression method.
- n.boot: The number of bootstrap samples to generate
when estimating the confidence intervals.
- optim.controls: A list. For binary outcomes, the meta-analysis
uses stats::optim() to estimate the parameters of the HTE. This input can 
be used to modify the default settings
of that analysis.

<h5>Abbreviate Returned Object Description</h5>

A list with components:

- est.meta: The HTE estimator based on a meta analysis.
- est.rct: The HTE estimator using the semiparametric 
    efficient estimation based only on RCT data.
- est.int: The HTE estimator using the semiparametric 
    efficient estimation based on the combined RCT and RWE.
- att.meta: The ATT estimator based on a meta analysis.
- att.rct: The ATT estimator based only on RCT data.
- att.int: The ATT estimator based on the combined RCT and RWE.
- ve.meta: The variance estimates of est.meta.
- ve.rct: The variance estimates of est.rct.
- ve.int: The variance estimates of est.int.
- ve.att.meta: The variance estimates of att.meta.
- ve.att.rct: The variance estimates of att.rct.
- ve.att.int: The variance estimates of att.int.

<h3>Examples</h3>

<pre>
  
# load provided illustrative toy dataset with continuous outcome
data("IntHTEToy.cont")

# conduct the elastic integrative analysis with defaults
result.cont <- IntHTEcf(dataInput(IntHTEToy.cont.rct, 
                                  outcome.model = Y ~ X1*A,
                                  ps.model = A ~ X1 + X2),
                        dataInput(IntHTEToy.cont.rwe, 
                                  outcome.model = Y ~ X2 + X1*A,
                                  ps.model = A ~ X1 + X2),
                        cfName = "X1")
</pre>

<h3>Install</h3>

In your R console

<pre>
library(remotes)
install_github("IntegrativeStats/IntegrativeHTEcf")
</pre>

