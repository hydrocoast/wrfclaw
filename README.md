# GeoClaw with WRF
## Overview
This repository is developed for storm surge simulation of geoclaw with user-defined wind and slp fields.

<p align="center">
<img src="https://github.com/hydrocoast/wrfclaw/blob/master/fig/sample_surface.gif", width="450">
<img src="https://github.com/hydrocoast/wrfclaw/blob/master/fig/sample_gauges.svg", width="400">
</p>  

## Requirements
- [clawpack](https://github.com/clawpack/clawpack) v5.4.1  
v5.5.0 is not supported yet
- NetCDF4 libraries ([netcdf-c](https://github.com/Unidata/netcdf-c) and [netcdf-fortran](https://github.com/Unidata/netcdf-fortran))

## Usage
- install the required libraries and packages  
- set the environmental variables  
You can check whether some environmental variables are set appropriately by the following command.
```shell
echo $CLAW # top directory of your clawpack
echo $PYTHONPATH # top directory of your clawpack
echo $FC # Fortran compiler
echo $NETCDF_F_ROOT # directory of your netcdf-fortran
```  
- modify `data.py`  
The variable `storm_type` is updated and the new type 4 is defined in this repository.  
In order to avoid an error, run  
```shell
./replace_datapy.sh
```
Then `$CLAW/geoclaw/src/python/geoclaw/data.py` was updated and original `data.py` was saved as `data.py_org` at the same directory.

- run a test simulation  
A test simulations can be implemented by
```shell
make clobber
make .exe
make .data
make .output
```

## License
MIT

## Authors
- [Takuya Miyashita](https://github.com/hydrocoast)  miyashita(at)hydrocoast.jp
- [Marc Kjerland](https://github.com/MarcKjerland)
