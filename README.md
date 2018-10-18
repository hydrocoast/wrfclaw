# GeoClaw with WRF
## Overview
This repository is developed for storm surge simulation of geoclaw with user-defined wind and slp fields.

<p align="center">
<img src="https://github.com/hydrocoast/wrfclaw/blob/master/fig/sample_surface.gif", width="450">
<img src="https://github.com/hydrocoast/wrfclaw/blob/master/fig/sample_gauges.svg", width="400">
</p>  

## Requirements
- [clawpack](https://github.com/clawpack/clawpack) v5.4.1  
This repo doesn't support clawpack v5.5.0 yet.
- NetCDF4 libraries ([netcdf-c](https://github.com/Unidata/netcdf-c) and [netcdf-fortran](https://github.com/Unidata/netcdf-fortran))

## Usage
- install the required libraries and packages  
- set the environmental variables  
You can check whether the variables are set appropriately by running the following commands in your shell.
```shell
echo $CLAW # top directory of your clawpack
echo $PYTHONPATH # top directory of your clawpack
echo $FC # Fortran compiler
echo $NETCDF_F_ROOT # directory of your netcdf-fortran
```  
- modify `data.py`  
The variable `storm_type` is updated and the new type 4 is defined.  
In order to avoid errors caused by this update, run  
```shell
./replace_datapy.sh
```
Then `$CLAW/geoclaw/src/python/geoclaw/data.py` was updated and original `data.py` was saved as `data.py_org`.

- run a test simulation  
Test simulations can be implemented by
```shell
make clobber
make .exe
make .data
make storm
make .output
```

## License
BSD 3-Clause


## Acknowledgements
The author would like to thank [Marc Kjerland](https://github.com/MarcKjerland) Ph.D. for collaboration on the early stages of this work.


## Author
- [Takuya Miyashita](https://github.com/hydrocoast)  miyashita (at) hydrocoast.jp
