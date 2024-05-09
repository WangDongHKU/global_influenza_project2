<h1> Persistence Regression Analysis </h1>

<p>
Analysis below examines the relationship between antigenic drift, air traffic (defined as the converse of the rescaled flight passenger arrivals), and influenza dwell time (also referred as to persistence time) in the context of pre-pandemic, acute and transition phase of the pandemic, and post-pandemic period across twelve world regions. Previous literature (Bedford et al., 2015) has observed effects of antigenic drift on persistence given seasonality on influenza. Present analyses, part of a broader study (Chen et al., 2023), attempt to explore whether the dwell time was affected by the COVID-19 pandemic's restrictions (i.e. lower amongst-region human movement), alongside antigenic drift, at a global scale via a causal Bayesian model.
</p>

<h1> Model </h1>
<p> The directed acyclic graph (DAG) below indicates the pre-established relationships between relevant variables.

<p align="center">
	<img src="dag_graphviz.png" width="500" height="500" />
</p>

Where Antigenic is antigenic drift, and is the main exposure variable affecting persistence. Other variables represent conditioned variables. To estimate these effects, we built the following Bayesian model:  
</p>

<p align="center"> a<sub>s</sub>, b<sub>s</sub>, c<sub>s</sub> ~ HalfNormal(1)  </p>
<p align="center"> a<sub>l</sub>, b<sub>l</sub>, c<sub>l</sub> ~ Normal(0, 1)  </p>
<p align="center"> a<sub>z</sub>, b<sub>z</sub>, c<sub>z</sub> ~ Normal(0, 1)  </p>
<p align="center"> a = a<sub>l</sub> + a<sub>s</sub>a<sub>z</sub>  </p>
<p align="center"> b = b<sub>l</sub> + b<sub>s</sub>b<sub>z</sub> [region,month] </p>
<p align="center"> c = c<sub>l</sub> + c<sub>s</sub>c<sub>z</sub> [region,month] </p>
<p align="center"> &gamma;  = a + cx </p>
<p align="center"> &epsilon; ~ HalfNormal(1) </p>
<p align="center"> &#373; ~ Normal(&gamma;, &epsilon; ) [observed = w]
<p align="center"> &mu;  = a + bw + cx </p>
<p align="center"> &sigma; ~ HalfNormal(1) </p>
<p align="center"> &#375; ~ Gaussian(&mu;, &sigma;) [observed = y] </p>

Where <i>a</i> is an intercept, <i>b</i> and <i>c</i> are varying parameters of region by month size, and w, x and y are the standarised values of air traffic, antigenic-drift and persistence respectively.


<h1> Results </h1>

<p> The model was sampled using HMC NUTS with 2000 tuning steps, 2000 samples and 0.99 target-accept. The model sampled well with ESS > 1000, R-hats ~ 1 and BFMIs >= 0.75. However posterior predictive checks indicate predictive inefficiency. </p>

<p align="center">
	<img src="rank_plots.png" width="700" height="" />
</p>

The model shows a reasonable fit based on posterior predictive checks.

<p align="center">
	<img src="posterior_predictives.png" width="700" height="500" />
</p>



The direct effects of air-traffic and antigenic-drift are summarised in the forest_plots directory. Example plots for antigenic-drift in mostly-tropical regions below:

<p align="center">
	<img src="forest_plots/Africa_anti_drift_forest.png" width="330" height="300" />
	<img src="forest_plots/South America_anti_drift_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/South-eastern Asia_anti_drift_forest.png" width="330" height="300" />
	<img src="forest_plots/Oceania_anti_drift_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/Southern Asia_anti_drift_forest.png" width="330" height="300" />

</p>

Note that during pandemic effects are strong for Africa, while other regions remain around zero, maybe with exception of China.

<p align="center">
	<img src="forest_plots/Europe_anti_drift_forest.png" width="330" height="300" />
	<img src="forest_plots/Western Asia_anti_drift_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/North China_anti_drift_forest.png" width="330" height="300" />
	<img src="forest_plots/South China_anti_drift_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/North America_anti_drift_forest.png" width="330" height="300" />
</p>

Effects of air-traffic are generally positive, with strong effects on Africa for 2020 and 2021 and on South Asia during 2021.
<p align="center">
	<img src="forest_plots/Africa_air_traffic_forest.png" width="330" height="300" />
	<img src="forest_plots/South America_air_traffic_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/South-eastern Asia_air_traffic_forest.png" width="330" height="300" />
	<img src="forest_plots/Oceania_air_traffic_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/Southern Asia_air_traffic_forest.png" width="330" height="300" />
</p>

Effects remain moderate in other regions, maybe with exception of China, which shows slightly higher effects between 2020 and 2022.

<p align="center">
	<img src="forest_plots/Europe_air_traffic_forest.png" width="330" height="300" />
	<img src="forest_plots/Western Asia_air_traffic_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/North China_air_traffic_forest.png" width="330" height="300" />
	<img src="forest_plots/South China_air_traffic_forest.png" width="330" height="300" />
</p>

<p align="center">
	<img src="forest_plots/North America_air_traffic_forest.png" width="330" height="300" />
</p>

<h1> Conclusion </h1>

<p> Present results indicate that Africa shows the strongest pandemic-related effects of antigenic-drift, while the effects of inter-regional air traffic remain relatively constant in most regions, with slight decreases in the pandemic period, Africa and South Asia show the opposite pattern.  </p>

<H1> References </H1>

Bedford, T., Riley, S., Barr, I. G., Broor, S., Chadha, M., Cox, N. J., Daniels, R. S., Gunasekaran, C. P., Hurt, A. C., Kelso, A., Klimov, A., Lewis, N. S., Li, X., McCauley, J. W., Odagiri, T., Potdar, V., Rambaut, A., Shu, Y., Skepner, E., & Smith, D. J. (2015). Global circulation patterns of seasonal influenza viruses vary with antigenic drift. Nature, 523(7559), 217–220. https://doi.org/10.1038/nature14460

Chen, Z., Joseph L.-H. Tsui, Gutierrez, B., Simon Busch Moreno, Louis du Plessis, Deng, X., Cai, J., Bajaj, S., Suchard, M. A., Pybus, O. G., Philippe Lemey, Moritz, & Yu, H. (2023). COVID-19 pandemic re-shaped the global dispersal of seasonal influenza viruses. MedRxiv (Cold Spring Harbor Laboratory). https://doi.org/10.1101/2023.12.20.23300299
