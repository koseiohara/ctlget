# CTLGET

Control file reader for Fortran.

## Install and Build
Install the source code by cloning this repository:
```sh
git clone https://github.com/koseiohara/ctlget.git
cd ctlget
```
This library can be built by Makefile.

### Makefile
Rewrite the `Makefile` for your environment:
```sh
cd src
vim Makefile
```
you can change the definitions of `DIR`, `FC`, and `FLAGS`.
`${DIR}/lib` and `${DIR}/include` are needed.  
After making these settings, execute the makefile
```sh
make
make install
```
`libctlget.a` and `ctlget.mod` will be copied to `${DIR}/lib` and `${DIR}/include`, respectively.


## Tools
- [ctlget](#init)
- [get_dset](#dset)
- [get_title](#title)
- [get_undef](#undef)
- [get_options](#options)
- [isYrev](#yrev)
- [isZrev](#zrev)
- [includeLeap](#leap)
- [get_endian](#endian)
- [get_gridnum](#gridnum)
- [get_nt](#nt)
- [get_x](#x)
- [get_y](#y)
- [get_z](#z)
- [get_xinfo](#xinfo)
- [get_yinfo](#yinfo)
- [get_zinfo](#zinfo)
- [get_tini](#tini)
- [get_dt](#dt)
- [get_nvars](#nvars)
- [get_var_idx](#var-idx)
- [get_var_name](#var-name)
- [get_var_description](#var-description)


### ctlget<a id="init"></a>
```fortran
function init(ctlname, linemax) result(output)
    character(*), intent(in) :: ctlname
    integer     , intent(in), optional :: linemax     ! DEFAULT : 100
```
Constructor of this class.
`ctlname` is the name of the target control file.
`linemax` is the maximum allowable number of lines in the file (default : 100).
For example, if `linemax=100` is substituted, lines after line 100 will not be read.  
Although the name of function is `init`, you can call this routine by `ctlget` like
```fortran
example = ctlget(ctlname, 100)
```

### get_dset<a id="dset"></a>
```fortran
subroutine get_dset(self, output)
    class(ctl)  , intent(in)  :: self
    character(*), intent(out) :: output
```
`output` is the name of the target binary file.

### get_title<a id="title"></a>
```fortran
subroutine get_title(self, output)
    class(ctl), intent(in)    :: self
    character(*), intent(out) :: output
```
`output` is the title written in the target control file.

### get_undef<a id="undef"></a>
```fortran
subroutine get_undef(self, undef, undef_char)
    class(ctl), intent(in) :: self
    real(real32), intent(out), optional :: undef
    character(*), intent(out), optional :: undef_char
```
`undef` is the value for undefined grids, its type is `real32`.
`undef_char` is same as `undef`, but its type is `character`.

### get_options<a id="options"></a>
```fortran
subroutine get_options(self, output)
    class(ctl)  , intent(inout) :: self
    character(*), intent(out)   :: output
```
`output` is the options written in the target control file.

### isYrev<a id="yrev"></a>
```fortran
function isYrev(self) result(output)
    class(ctl), intent(inout) :: self
    logical :: output
```
`output` is a logical value, `.TRUE.` if the order of y is reversed.

### isZrev<a id="zrev"></a>
```fortran
function isZrev(self) result(output)
    class(ctl), intent(inout) :: self
    logical :: output
```
`output` is a logical value, `.TRUE.` if the order of z is reversed.

### includeLeap<a id="leap"></a>
```fortran
function includeLeap(self) result(output)
    class(ctl), intent(inout) :: self
    logical :: output
```
`output` is a logical value, `.TRUE.` if data include leap days.

### get_endian<a id="endian"></a>
```fortran
subroutine get_endian(self, output)
    class(ctl)  , intent(inout) :: self
    character(*), intent(out)   :: output
```
`output` is the endian of data, `"little"`, `"big"`, or `"native"`.

### get_gridnum<a id="gridnum"></a>
```fortran
subroutine get_gridnum(self, nx, ny, nz)
    class(ctl), intent(in) :: self
    integer   , intent(out), optional :: nx
    integer   , intent(out), optional :: ny
    integer   , intent(out), optional :: nz
```
`nx`, `ny`, and `nz` are number of grids.

### get_nt<a id="nt"></a>
```fortran
subroutine get_nt(self, output)
    class(ctl), intent(in)  :: self
    integer   , intent(out) :: output
```
`output` is the number of timesteps.

### get_x<a id="x"></a>
```fortran
subroutine get_x(self, output)
    class(ctl)  , intent(in)  :: self
    real(real32), intent(out) :: output(:)
```
`output` is the x-coordinate, its size is `nx` obtained by `get_gridnum()`

### get_y<a id="y"></a>
```fortran
subroutine get_y(self, output)
    class(ctl)  , intent(in)  :: self
    real(real32), intent(out) :: output(:)
```
`output` is the y-coordinate, its size is `ny` obtained by `get_gridnum()`

### get_z<a id="z"></a>
```fortran
subroutine get_z(self, output)
    class(ctl)  , intent(in)  :: self
    real(real32), intent(out) :: output(:)
```
`output` is the z-coordinate, its size is `nz` obtained by `get_gridnum()`

### get_xinfo<a id="xinfo"></a>
```fortran
subroutine get_xinfo(self, xmin, dx, islinear)
    class(ctl)  , intent(in)  :: self
    real(real32), intent(out) :: xmin
    real(real32), intent(out) :: dx
    logical     , intent(out), optional :: islinear
```
`xmin` is the minimum value of xaxis.
`dx` is the differences between adjacent coordinates.
`islinear` is `.TRUE.` if x-axis is linear. If `islinear=.FALSE.`, `xmin` and `dx` are 0.

### get_yinfo<a id="yinfo"></a>
```fortran
subroutine get_yinfo(self, ymin, dy, islinear)
    class(ctl)  , intent(in)  :: self
    real(real32), intent(out) :: ymin
    real(real32), intent(out) :: dy
    logical     , intent(out), optional :: islinear
```
`ymin` is the minimum value of yaxis.
`dy` is the differences between adjacent coordinates.
`islinear` is `.TRUE.` if y-axis is linear. If `islinear=.FALSE.`, `ymin` and `dy` are 0.

### get_zinfo<a id="zinfo"></a>
```fortran
subroutine get_zinfo(self, zmin, dz, islinear)
    class(ctl)  , intent(in)  :: self
    real(real32), intent(out) :: zmin
    real(real32), intent(out) :: dz
    logical     , intent(out), optional :: islinear
```
`zmin` is the minimum value of zaxis.
`dz` is the differences between adjacent coordinates.
`islinear` is `.TRUE.` if z-axis is linear. If `islinear=.FALSE.`, `zmin` and `dz` are 0.

### get_tini<a id="tini"></a>
```fortran
subroutine get_tini(self, calendar)
    class(ctl), intent(in)  :: self
    integer   , intent(out) :: calendar(5)
```
Each element of `calendar` contains the initial date and time information (year, month, day, hour, and minute).

### get_dt<a id="dt"></a>
```fortran
subroutine get_dt(self, dt, unit)
    class(ctl)  , intent(in)  :: self
    integer     , intent(out) :: dt
    character(*), intent(out), optional :: unit
```
`dt` is the increment to calendar and `unit` is its unit.
One of `"mn"`, `"hr"`, `"dy"`, `"mo"`, or `"yr"` will be substituted to `unit`.

### get_nvars<a id="nvars"></a>
```fortran
subroutine get_nvars(self, output)
    class(ctl), intent(in)  :: self
    integer   , intent(out) :: output
```
`output` is the number of variables defined in the target control file.

### get_var_idx<a id="var-idx"></a>
```fortran
subroutine get_var_idx(self, var, output)
    class(ctl)  , intent(in)  :: self
    character(*), intent(in)  :: var
    integer     , intent(out) :: output
```
`var` is the name of target variable.
`output` is its index.

### get_var_name<a id="var-name"></a>
```fortran
subroutine get_var_name(self, idx, output)
    class(ctl)  , intent(in)  :: self
    integer     , intent(in)  :: idx
    character(*), intent(out) :: output
```
`idx` is the index of target variable.
`output` is its name.

### get_var_description<a id="var-description"></a>
```fortran
subroutine get_var_description(self, output, idx, var)
    class(ctl)  , intent(in)  :: self
    character(*), intent(out) :: output
    integer     , intent(in), optional :: idx
    character(*), intent(in), optional :: var
```
`output` is the description (attribute value) of target variable.
`idx` is the index of target variable.
`var` is the name of target variable.
Variable can be specified by `idx` or `var`.







