# -*- coding: utf-8 -*-
import jax
import pymc as pm
import numpy as np
import arviz as az
import pandas as pd
import pytensor.tensor as pt
import matplotlib.pyplot as plt
import itertools
prod = itertools.product
from tqdm import tqdm
import graphviz
import seaborn as sns

np.random.seed(27)

#####plotting parameters
plt.rcParams.update({'font.size': 14})
plt.rcParams.update({'figure.titlesize': 16})
plt.rcParams['font.family'] = "DeJavu Serif"
plt.rcParams['font.serif'] = "Cambria Math"

## Load data
df = pd.read_csv("data.csv")
 
df['year'] = [y[:4] for y in df.year_month]
df['month'] = [y[5:] for y in df.year_month]
df = df.sort_values("year_month")
df.reset_index(inplace=True, drop=True)


#  a) pre pandemic: 2017-01 to 2020-03; b) pandemic period 1: 2020-04 to 2021-03; 
#  c) pandemic period 2: 2021-04 to 2023-04; d) post-pandemic period: 2023-05 to 2023-12.
pre = np.repeat("pre", len(df[df.year_month.between('2017-01','2020-03')]))
pan1 = np.repeat("pan1", len(df[df.year_month.between('2020-04','2021-03')]))
pan2 = np.repeat("pan2", len(df[df.year_month.between('2021-04','2023-04')]))
pos = np.repeat("pos", len(df[df.year_month.between('2023-05','2023-12')]))

df['epoch'] = list(pre) + list(pan1) + list(pan2) + list(pos)

years = df.year.unique().shape[0]
months = df.month.unique().shape[0]
year_months = df.year_month.unique().shape[0]
regions = df.region.unique().shape[0]
climates = df.climate.unique().shape[0]
epochs = df.epoch.unique().shape[0]

pers = df.persistence.values
anti = df.antigen_drift.values
air = 1 - df.inter_air_traffic.values #converse of total arrivals proportion


reg_idx = pd.factorize(df.region)[0]
cli_idx = pd.factorize(df.climate)[0]
epo_idx = pd.factorize(df.epoch)[0]
yea_idx  = pd.factorize(df.year_month)[0]

y = (pers - pers.mean()) / pers.std()
w = (anti - anti.mean()) / anti.std()
x = (air - air.mean()) / air.std()


coords = {'region':df.region.unique(), 'climate':df.climate.unique(), 
          'month':df.year_month.unique(),'loc':df.index.values}


with pm.Model(coords=coords) as mod:
    reg_idx = pm.ConstantData("reg_idx", reg_idx, dims="loc")
    cli_idx = pm.ConstantData("cli_idx", cli_idx, dims="loc")
    epo_idx = pm.ConstantData("epo_idx", epo_idx, dims="loc")
    yea_idx = pm.ConstantData("year_idx", yea_idx, dims="loc")
    
    a_s = pm.HalfNormal("a_s", 1)
    a_z = pm.Normal("a_z", mu=0, sigma=1)
    a = pm.Deterministic("a", a_s*a_z)
    
    b_s = pm.HalfNormal("b_s", 1)
    b_l = pm.Normal("b_l", 0, 1)
    b_z = pm.Normal("b_z", mu=0, sigma=1, dims=("region", "month"))
    b = pm.Deterministic("b", b_l + b_s*b_z, dims=("region", "month"))
    
    c_s = pm.HalfNormal("c_s", 1)
    c_l = pm.Normal("c_l", 0, 1)
    c_z = pm.Normal("c_z", mu=0, sigma=1, dims=("region", "month"))
    c = pm.Deterministic("c", c_l + c_s*c_z, dims=("region", "month"))
    
    epsilon = pm.HalfNormal("epsilon", 1)
    gamma = a + c[reg_idx,yea_idx]*x
    w_hat = pm.Normal("w", mu=gamma, sigma=epsilon, observed=w)
    
    mu = pm.Deterministic("mu", a + b[reg_idx,yea_idx]*w + c[reg_idx,yea_idx]*x )
    
    sigma = pm.HalfNormal("sigma", 1)
    
    y_hat = pm.Normal("y", mu=mu, sigma=sigma, observed=y)
    
    
with mod:
    ppc = pm.sample_prior_predictive()

    
#az.plot_ppc(ppc, kind="cumulative", group="prior")    

az.plot_ppc(ppc, group="prior", var_names=['y', 'w'])    
plt.suptitle("Prior Predictive")
plt.tight_layout()
plt.savefig("prior_predictive.png", dpi=300)
plt.show()
plt.close()

# idata = az.from_netcdf("idata_causal.nc")

with mod:
    #idata = pm.sampling.jax.sample_numpyro_nuts(1000, tune=1000, chains=4, target_accept=0.95)
    idata = pm.sample(2000, tune=2000, chains=4, cores=12, target_accept=0.99, 
                      nuts_sampler="numpyro", random_seed=27)
    pred = pm.sample_posterior_predictive(idata)



# az.plot_ppc(pred, kind="cumulative", group="posterior") 
pred_y_z = az.extract(pred.posterior_predictive)['y'].values
pred_y = pred_y_z*pers.std() + pers.mean()

df['y_m'] = pred_y.mean(axis=1)
df['y_s'] = pred_y.std(axis=1)
df['y_u'] = pred_y.mean(axis=1) + pred_y.std(axis=1)
df['y_l'] = pred_y.mean(axis=1) - pred_y.std(axis=1)
df['y_l'][df['y_l'] < 0] = 0 

df_term = df[['year_month', 'persistence', 'y_m', 'y_l', 'y_u']].groupby("year_month", as_index=False).mean()
df_term['persistence'] = df_term['persistence'].replace({0:np.nan})
df_country = df[['region', 'persistence', 'y_m', 'y_l', 'y_u']].groupby(["region"], as_index=False).mean()

fig, ax = plt.subplots(2,1, figsize=(20,12))
ax[0].plot(np.arange(len(df_term)), df_term.persistence, "-o", color='k', lw=3, label="Observed")
ax[0].plot(np.arange(len(df_term)), df_term.y_m, "--o", color='dodgerblue', linestyle="--", lw=3, label="Predicted mean")
ax[0].fill_between(np.arange(len(df_term)), df_term.y_l, df_term.y_u, color='orangered', alpha=0.2, label="SD")
ax[0].legend()
ax[0].grid(alpha=0.3)
ax[0].set_ylabel("Average Persistence", size=16)
ax[0].set_xticks(np.arange(len(df_term)), df_term.year_month, rotation=90)
ax[0].set_xlabel("Year-Month", size=16, labelpad=10)
ax[0].set_title("Persistence Posterior Predictives (month)", size=20)
ax[0].spines[['right', 'top']].set_visible(False)
ax[0].set_ylim(ymin=0, ymax=1)
ax[1].plot(np.arange(len(df_country)), df_country.persistence, "o", color='k', ms=12, label="Observed")
ax[1].plot(np.arange(len(df_country)), df_country.y_m, "o", color='crimson', ms=12, label="Predicted mean")
ax[1].vlines(np.arange(len(df_country)), df_country.y_l, df_country.y_u, color='crimson', linewidth=4, alpha=0.7, label="SD")
ax[1].legend()
ax[1].grid(alpha=0.3)
ax[1].set_xticks(np.arange(len(df_country)), df_country.region, rotation=40)
ax[1].set_xlabel("Average Persistence", size=16)
ax[1].set_title("Persistence Posterior Predictives (region)", size=20)
ax[1].spines[['right', 'top']].set_visible(False)
ax[1].set_ylim(ymin=0, ymax=1)
plt.tight_layout()
plt.subplots_adjust(hspace=0.7)
plt.savefig("posterior_predictives.png", dpi=300)
plt.show()
plt.close()

#mu_m = az.extract(idata)['mu'].values.mean(axis=1)

# a_z = az.extract(idata.posterior)['a'].values
# bz = az.extract(idata.posterior)['b'].values
# cz = az.extract(idata.posterior)['c'].values

# am = pers.std()*(a_z - bz*(anti.mean()/anti.std()) - cz*(air.mean()/air.std())) + pers.mean()
# bm = bz*(pers.std()/anti.std())
# cm = cz*(pers.std()/air.std())

# mu_m = am + bm + cm
# mu_5, mu_95 = az.hdi(az.extract(idata)['mu'].values.T).T
# mu_m = mu_m.mean(axis=2).flatten()


# df['mu_mean'] = mu_m
# df['mu_hdi5'] = mu_5
# df['mu_hdi95'] = mu_95

# inter = az.extract(idata)['a'].values
# slope1 = az.extract(idata)['b'].values
# slope2 = az.extract(idata)['c'].values

# df['pers_z'] = y
# df['anti_z'] = w
# df['air_z'] = x

# for r in tqdm(range(regions)):
#     reg = df.region.unique()[r]
#     pr = df[df.region==reg].pers_z.values
#     wr = df[df.region==reg].anti_z.values
#     wr_pos = inter + (slope1[r].T*wr).T 
#     wr_m = wr_pos.mean(axis=1)
#     wr_h5, wr_h95 = az.hdi(wr_pos.T, hdi_prob=0.9).T
#     xr = df[df.region==reg].air_z.values
#     xr_pos = inter + (slope2[r].T*xr).T 
#     xr_m = xr_pos.mean(axis=1)
#     xr_h5, xr_h95 = az.hdi(xr_pos.T, hdi_prob=0.9).T
#     if "/" in reg:
#         reg = reg.replace("/","-")
#     #plot pre pandemic
#     plt.plot(np.sort(pr[:37]), np.sort(wr_m[:37]), color="crimson")
#     plt.fill_between(np.sort(pr[:37]), np.sort(wr_h5[:37]), np.sort(wr_h95[:37]), color="crimson", alpha=0.2)
#     plt.scatter(np.sort(pr[:37]), np.sort(wr[:37]), color="crimson", marker="o", alpha=0.5)
#     plt.plot(np.sort(pr[:37]), np.sort(xr_m[:37]), color="steelblue")
#     plt.fill_between(np.sort(pr[:37]), np.sort(xr_h5[:37]), np.sort(xr_h95[:37]), color="steelblue", alpha=0.2)
#     plt.scatter(np.sort(pr[:37]), np.sort(xr[:37]), color="steelblue", marker="d", alpha=0.5)
#     plt.grid(alpha=0.3)
#     plt.xlabel("Persistence (z-score)")
#     plt.ylabel("Antigenic Drift / Air Traffic (z-score)", size=10)
#     plt.title(reg+" Pre Pandemic")
#     plt.tight_layout()
#     plt.savefig("./regression_plots/"+reg+"_pre.png", dpi=300)
#     plt.close()
#     #plot pandemic
#     plt.plot(np.sort(pr[37:55]), np.sort(wr_m[37:55]), color="crimson")
#     plt.fill_between(np.sort(pr[37:55]), np.sort(wr_h5[37:55]), np.sort(wr_h95[37:55]), color="crimson", alpha=0.2)
#     plt.scatter(np.sort(pr[37:55]), np.sort(wr[37:55]), color="crimson", marker="o", alpha=0.5)
#     plt.plot(np.sort(pr[37:55]), np.sort(xr_m[37:55]), color="steelblue")
#     plt.fill_between(np.sort(pr[37:55]), np.sort(xr_h5[37:55]), np.sort(xr_h95[37:55]), color="steelblue", alpha=0.2)
#     plt.scatter(np.sort(pr[37:55]), np.sort(xr[37:55]), color="steelblue", marker="d", alpha=0.5)
#     plt.grid(alpha=0.3)
#     plt.xlabel("Persistence (z-score)")
#     plt.ylabel("Antigenic Drift / Air Traffic (z-score)", size=10)
#     plt.title(reg+" Pandemic")
#     plt.tight_layout()
#     plt.savefig("./regression_plots/"+reg+"_pan.png", dpi=300)
#     plt.close()
#     #plot post pandemic
#     plt.plot(np.sort(pr[55:]), np.sort(wr_m[55:]), color="crimson")
#     plt.fill_between(np.sort(pr[55:]), np.sort(wr_h5[55:]), np.sort(wr_h95[55:]), color="crimson", alpha=0.2)
#     plt.scatter(np.sort(pr[55:]), np.sort(wr[55:]), color="crimson", marker="o", alpha=0.5)
#     plt.plot(np.sort(pr[55:]), np.sort(xr_m[55:]), color="steelblue")
#     plt.fill_between(np.sort(pr[55:]), np.sort(xr_h5[55:]), np.sort(xr_h95[55:]), color="steelblue", alpha=0.2)
#     plt.scatter(np.sort(pr[55:]), np.sort(xr[55:]), color="steelblue", marker="d", alpha=0.5)
#     plt.grid(alpha=0.3)
#     plt.xlabel("Persistence (z-score)")
#     plt.ylabel("Antigenic Drift / Air Traffic (z-score)", size=10)
#     plt.title(reg+" Post Pandemic")
#     plt.tight_layout()
#     plt.savefig("./regression_plots/"+reg+"_post.png", dpi=300)
#     plt.close()
        
    

#colorblind palette
colors = ['#377eb8','#ff7f00','#4daf4a','#f781bf','#a65628',
          '#984ea3','#999999','#e41a1c','#dede00','#9EDAE5','#C5B0D5']

pos_b = az.extract(idata)['b'].values
#pos_b = pos_b*(pers.std()/anti.std())
posb = []
for i in range(years):
    l1 = 12*(1+i) - 12
    l2 = 12*(1+i) 
    if i == 6:
        l2 = 77
    pb = pos_b[:,l1:l2-1,:].mean(axis=1)
    posb.append(pb)
pos_b = np.array(posb)

for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(years)):
        yea = df.year.unique()[j]
        m = pos_b[j,i].mean()
        h5, h95 = az.hdi(pos_b[j,i], hdi_prob=0.9) 
        y = j+1
        plt.scatter([m],[y], s=100, facecolor="w", edgecolor=colors[j], zorder=3)
        plt.hlines(y=y, xmin=h5,xmax=h95, color=colors[j], linewidth=3)
    plt.yticks(ticks=np.arange(years)+1, labels=df.year.unique())
    plt.gca().invert_yaxis()
    plt.xlabel("z-score")
    plt.title(reg+"\nAntigenic Drift Effects")
    plt.grid(color="gainsboro")
    plt.tight_layout()
    plt.savefig("./forest_plots/"+reg+"_anti_drift_forest.png", dpi=300)
    plt.show()
    plt.close()

pos_c = az.extract(idata)['c'].values
#pos_c = pos_c*(pers.std() / air.std())
posc = []
for i in range(years):
    l1 = 12*(1+i) - 12
    l2 = 12*(1+i) 
    if i == 6:
        l2 = 77
    pc = pos_c[:,l1:l2-1,:].mean(axis=1)
    posc.append(pc)
pos_c = np.array(posc)

for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(years)):
        yea = df.year.unique()[j]
        m = pos_c[j,i].mean()
        h5, h95 = az.hdi(pos_c[j,i], hdi_prob=0.9) 
        y = j+1
        plt.scatter([m],[y], s=100, facecolor="w", edgecolor=colors[j], zorder=3)
        plt.hlines(y=y, xmin=h5,xmax=h95, color=colors[j], linewidth=3)
    plt.yticks(ticks=np.arange(years)+1, labels=df.year.unique())
    plt.gca().invert_yaxis()
    plt.xlabel("z-score")
    plt.title(reg+"\nAir Traffic Effects")
    plt.grid(color="gainsboro")
    plt.tight_layout()
    plt.savefig("./forest_plots/"+reg+"_air_traffic_forest.png", dpi=300)
    plt.show()
    plt.close()


############ Save forest plots epoch #############

#  a) pre pandemic: 2017-01 to 2020-03; b) pandemic period 1: 2020-04 to 2021-03; 
#  c) pandemic period 2: 2021-04 to 2023-03; d) post-pandemic period: 2023-04 to 2023-12.

pos_b = az.extract(idata)['b'].values
#pos_b = pos_b*(pers.std()/anti.std())
pb1 = pos_b[:,:39,:].mean(axis=1)
pb2 = pos_b[:,39:51,:].mean(axis=1)
pb3 = pos_b[:,51:76,:].mean(axis=1)
pb4 = pos_b[:,76:,:].mean(axis=1)
pos_b = np.array([pb1,pb2,pb3,pb4])

for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(epochs)):
        yea = df.year.unique()[j]
        m = pos_b[j,i].mean()
        h5, h95 = az.hdi(pos_b[j,i], hdi_prob=0.9) 
        y = j+1
        plt.scatter([m],[y], s=100, facecolor="w", edgecolor=colors[j], zorder=3)
        plt.hlines(y=y, xmin=h5,xmax=h95, color=colors[j], linewidth=3)
    plt.yticks(ticks=np.arange(epochs)+1, labels=["Pre Pandemic", "Pandemic Period 1", "Pandemic Period 2", "Post Pandemic"])
    plt.gca().invert_yaxis()
    plt.xlabel("z-score")
    plt.title(reg+"\nAntigenic Drift Effects")
    plt.grid(color="gainsboro")
    plt.tight_layout()
    plt.savefig("./forest_plots_epoch/"+reg+"_anti_drift_forest.png", dpi=300)
    plt.show()
    plt.close()

pos_c = az.extract(idata)['c'].values
#pos_c = pos_c*(pers.std()/air.std())
pc1 = pos_c[:,:39,:].mean(axis=1)
pc2 = pos_c[:,39:51,:].mean(axis=1)
pc3 = pos_c[:,51:76,:].mean(axis=1)
pc4 = pos_c[:,76:,:].mean(axis=1)
pos_c = np.array([pc1,pc2,pc3,pc4])


for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(epochs)):
        yea = df.year.unique()[j]
        m = pos_c[j,i].mean()
        h5, h95 = az.hdi(pos_c[j,i], hdi_prob=0.9) 
        y = j+1
        plt.scatter([m],[y], s=100, facecolor="w", edgecolor=colors[j], zorder=3)
        plt.hlines(y=y, xmin=h5,xmax=h95, color=colors[j], linewidth=3)
    plt.yticks(ticks=np.arange(epochs)+1, labels=["Pre Pandemic", "Pandemic Period 1", "Pandemic Period 2", "Post Pandemic"])
    plt.gca().invert_yaxis()
    plt.xlabel("z-score")
    plt.title(reg+"\nAir Traffic Effects")
    plt.grid(color="gainsboro")
    plt.tight_layout()
    plt.savefig("./forest_plots_epoch/"+reg+"_air_traffic_forest.png", dpi=300)
    plt.show()
    plt.close()




######## save forest_plot data ########

#antigenic drift effects
pos_b = az.extract(idata)['b'].values
#pos_b = pos_b*(pers.std()/anti.std())
posb = []
for i in range(years):
    l1 = 12*(1+i) - 12
    l2 = 12*(1+i) 
    if i == 6:
        l2 = 77
    pb = pos_b[:,l1:l2-1,:].mean(axis=1)
    posb.append(pb)
pos_b = np.array(posb)

summ_anti = {"var":[], "year":[], "region":[],
              "mean":[], "sd":[], 
              "hdi_5%":[], "hdi_95%":[], 
              "hdi_2.5%":[], "hdi_97.5%":[]}

for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(years)):
        yea = df.year.unique()[j]
        m = pos_b[j,i].mean()
        s = pos_b[j,i].std()
        h5, h95 = az.hdi(pos_b[j,i], hdi_prob=0.9) 
        h25, h975 = az.hdi(pos_b[j,i], hdi_prob=0.95)
        summ_anti['var'].append("antigenic-drift") 
        summ_anti['year'].append(yea) 
        summ_anti['region'].append(reg)
        summ_anti['mean'].append(m)
        summ_anti['sd'].append(s)
        summ_anti['hdi_5%'].append(h5)
        summ_anti['hdi_95%'].append(h95)
        summ_anti['hdi_2.5%'].append(h25)
        summ_anti['hdi_97.5%'].append(h975)     
summ_anti = pd.DataFrame(summ_anti)
summ_anti.to_csv("antigenic_effect_sumary.csv", index=False)


#air traffic effects
pos_c = az.extract(idata)['c'].values
#pos_c = pos_c*(pers.std()/air.std())
posc = []
for i in range(years):
    l1 = 12*(1+i) - 12
    l2 = 12*(1+i) 
    if i == 6:
        l2 = 77
    pc = pos_c[:,l1:l2-1,:].mean(axis=1)
    posc.append(pc)
pos_c = np.array(posc)

summ_at = {"var":[], "year":[], "region":[],
              "mean":[], "sd":[], 
              "hdi_5%":[], "hdi_95%":[], 
              "hdi_2.5%":[], "hdi_97.5%":[]}

for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(years)):
        yea = df.year.unique()[j]
        m = pos_c[j,i].mean()
        s = pos_c[j,i].std()
        h5, h95 = az.hdi(pos_c[j,i], hdi_prob=0.9) 
        h25, h975 = az.hdi(pos_c[j,i], hdi_prob=0.95)
        summ_at ['var'].append("air-traffic") 
        summ_at ['year'].append(yea) 
        summ_at['region'].append(reg)
        summ_at['mean'].append(m)
        summ_at['sd'].append(s)
        summ_at['hdi_5%'].append(h5)
        summ_at['hdi_95%'].append(h95)
        summ_at['hdi_2.5%'].append(h25)
        summ_at['hdi_97.5%'].append(h975)
summ_at = pd.DataFrame(summ_at)
summ_at.to_csv("air-traffic_effect_sumary.csv", index=False)




######## save Epoch forest_plot data ########

#antigenic drift effects
pos_b = az.extract(idata)['b'].values
#pos_b = pos_b*(pers.std()/anti.std())
pb1 = pos_b[:,:39,:].mean(axis=1)
pb2 = pos_b[:,39:51,:].mean(axis=1)
pb3 = pos_b[:,51:76,:].mean(axis=1)
pb4 = pos_b[:,76:,:].mean(axis=1)
pos_b = np.array([pb1,pb2,pb3,pb4])

summ_anti = {"var":[], "epoch":[], "region":[],
              "mean":[], "sd":[], 
              "hdi_5%":[], "hdi_95%":[], 
              "hdi_2.5%":[], "hdi_97.5%":[]}

for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(epochs)):
        epo = df.epoch.unique()[j]
        m = pos_b[j,i].mean()
        s = pos_b[j,i].std()
        h5, h95 = az.hdi(pos_b[j,i], hdi_prob=0.9) 
        h25, h975 = az.hdi(pos_b[j,i], hdi_prob=0.95)
        summ_anti['var'].append("antigenic-drift") 
        summ_anti['epoch'].append(epo) 
        summ_anti['region'].append(reg)
        summ_anti['mean'].append(m)
        summ_anti['sd'].append(s)
        summ_anti['hdi_5%'].append(h5)
        summ_anti['hdi_95%'].append(h95)
        summ_anti['hdi_2.5%'].append(h25)
        summ_anti['hdi_97.5%'].append(h975)     
summ_anti = pd.DataFrame(summ_anti)
summ_anti.to_csv("epoch_antigenic_effect_sumary.csv", index=False)


#air traffic effects
pos_c = az.extract(idata)['c'].values
#pos_c = pos_c*(pers.std() / air.std())
pc1 = pos_c[:,:39,:].mean(axis=1)
pc2 = pos_c[:,39:51,:].mean(axis=1)
pc3 = pos_c[:,51:76,:].mean(axis=1)
pc4 = pos_c[:,76:,:].mean(axis=1)
pos_c = np.array([pc1,pc2,pc3,pc4])


summ_at = {"var":[], "epoch":[], "region":[],
              "mean":[], "sd":[], 
              "hdi_5%":[], "hdi_95%":[], 
              "hdi_2.5%":[], "hdi_97.5%":[]}

for i in tqdm(range(regions)):
    reg = df.region.unique()[i]
    if "/" in reg:
        reg = reg.replace("/", "-")
    for j in np.flip(range(epochs)):
        epo = df.epoch.unique()[j]
        m = pos_c[j,i].mean()
        s = pos_c[j,i].std()
        h5, h95 = az.hdi(pos_c[j,i], hdi_prob=0.9) 
        h25, h975 = az.hdi(pos_c[j,i], hdi_prob=0.95)
        summ_at ['var'].append("air-traffic") 
        summ_at ['epoch'].append(epo) 
        summ_at['region'].append(reg)
        summ_at['mean'].append(m)
        summ_at['sd'].append(s)
        summ_at['hdi_5%'].append(h5)
        summ_at['hdi_95%'].append(h95)
        summ_at['hdi_2.5%'].append(h25)
        summ_at['hdi_97.5%'].append(h975)
summ_at = pd.DataFrame(summ_at)
summ_at.to_csv("epoch_air-traffic_effect_sumary.csv", index=False)


## Plot DAG

g = graphviz.Digraph('G', filename='dag_graphviz.gv')

g.edge('Antigenic Drift', 'Persistence')
g.edge('Air Traffic', 'Persistence')
g.edge('Month', 'Persistence')
g.edge('Region', 'Persistence')

g.edge('Air Traffic', 'Antigenic Drift')
g.edge('Month', 'Antigenic Drift')
g.edge('Region', 'Antigenic Drift')

g.edge('Month', 'Air Traffic')
g.edge('Region', 'Air Traffic')

g.render("dag_graphviz")


#save as png
g = graphviz.Digraph('G', filename='dag_graphviz.gv', format="png")
g.graph_attr['dpi'] = '300'

g.edge('Antigenic Drift', 'Persistence')
g.edge('Air Traffic', 'Persistence')
g.edge('Month', 'Persistence')
g.edge('Region', 'Persistence')

g.edge('Air Traffic', 'Antigenic Drift')
g.edge('Month', 'Antigenic Drift')
g.edge('Region', 'Antigenic Drift')

g.edge('Month', 'Air Traffic')
g.edge('Region', 'Air Traffic')

g.render("dag_graphviz")


az.plot_trace(idata, kind="rank_vlines")
plt.tight_layout()
plt.title("Estimates")
plt.savefig("rank_plots.png", dpi=300)
plt.show()
plt.close()

summ = az.summary(idata, hdi_prob=0.9)
summ.to_csv("inference_summary.csv")