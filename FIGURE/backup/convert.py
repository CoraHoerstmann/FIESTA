import numpy as np
from scipy.io import loadmat
import xarray as xr


# file 1
data = loadmat('/Users/nielsfuchs/Downloads/20220726_nrt_cmems_d0_lambda_only.mat')
ds = xr.Dataset(data_vars=dict(lambd=(['lat','lon'], data['lambda'])), coords=dict(lat=data['latvv'][0,:], lon=data['lonvv'][0,:]))
ds.to_netcdf('/Users/nielsfuchs/Downloads/20220726_nrt_cmems_d0_lambda_only.nc')

# file 2

data = loadmat('/Users/nielsfuchs/Downloads/nrt_global_allsat_phy_l4_20220726_20220726.mat')

ds = xr.Dataset(data_vars=dict(adt=(['lat','lon'], data['adt'][:,:]), u=(['lat','lon'], data['u'][:,:]), v=(['lat','lon'], data['v'][:,:])), 
coords=dict(lat=data['lat'][0,:], lon=data['lon'][0,:]))

ds.to_netcdf('/Users/nielsfuchs/Downloads/nrt_global_allsat_phy_l4_20220726_20220726.nc')

# file 3

data = loadmat('')

ds = xr.Dataset(data_vars=dict(adt=(['lat','lon'], data['adt'][:,:]), u=(['lat','lon'], data['u'][:,:]), v=(['lat','lon'], data['v'][:,:])),
coords=dict(lat=data['lat'][0,:], lon=data['lon'][0,:]))

ds.to_netcdf('/Users/nielsfuchs/Downloads/nrt_global_allsat_phy_l4_20220726_20220726.nc')
